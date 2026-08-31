# =============================================================================
# 03_desenho.R
# O QUE FAZ : Constroi os objetos de desenho amostral e valida as estimativas contra
#             SIDRA (PNS) e o relatorio oficial (Vigitel). Para se divergir.
# ENTRADAS  : data/derivado/*.rds, API do SIDRA, relatorio Vigitel em PDF
# SAIDAS    : data/derivado/design_*.rds, output/logs/validacao_*.csv
# =============================================================================
source(here::here("R", "00_setup.R"))
dir.create(here::here("data", "derivado"), showWarnings = FALSE, recursive = TRUE)
library(sidrar)

h1("03_design.R - desenho amostral e validacao")

# =============================================================================
# PARTE A - DESENHO DA PNS
# =============================================================================

h2("PNS ", ANO_PNS, " - objeto de desenho")

pns_br <- readRDS(here::here("data", "derivado", "pns_brasil.rds"))
cat("amostra nacional de adultos:", nrow(pns_br), "registros\n")

# Estrato = V0024, UPA = UPA_PNS, peso = V00291 (morador selecionado, calibrado).
# nest = TRUE porque os codigos de UPA se repetem entre estratos.
des_pns_br <- pns_br |>
  srvyr::as_survey_design(strata = strata, ids = psu, weights = weight, nest = TRUE)

cat("estratos:", length(unique(pns_br$strata)),
    "| UPAs:", length(unique(pns_br$psu)),
    "| populacao estimada:", format(round(sum(pns_br$weight)), big.mark = "."), "\n")

# Recorte de capitais SOBRE o desenho, nao sobre os dados
des_pns_cap <- des_pns_br |> srvyr::filter(area_type == "Capital")
cat("capitais - n:", nrow(des_pns_cap$variables),
    "| populacao estimada:", format(round(sum(des_pns_cap$variables$weight)), big.mark = "."), "\n")

# =============================================================================
# PARTE B - VALIDACAO DA PNS CONTRA O SIDRA
# =============================================================================
# As prevalencias de referencia sao baixadas da API do SIDRA. Cada tabela abaixo
# foi identificada pelos metadados do IBGE (api/v3/agregados), nao de memoria:
#   4418 var 4399  - % 18+ com diagnostico medico de hipertensao arterial
#   4487 var 4465  - % 18+ com diagnostico medico de diabetes
#   7666 var 10985 - distribuicao % de 18+ por avaliacao do estado de saude
#                    (classificacao 12258, categoria 104866 = "Ruim e muito ruim")
# Nao ha tabela SIDRA de tabagismo em adultos para a PNS; a validacao usa tres
# indicadores, o que atende a exigencia de "duas ou tres" do protocolo.

h2("Validacao externa da PNS (SIDRA)")

#' Baixa uma prevalencia publicada da PNS no SIDRA, no nivel Brasil, por sexo
sidra_pns <- function(table, variable, extra = "") {
  api <- glue::glue("/t/{table}/n1/all/v/{variable}/p/{ANO_PNS}/c2/all{extra}")
  r <- sidrar::get_sidra(api = api)
  tibble::tibble(
    sex_sidra = as.character(r[["Sexo"]]),
    published = as.numeric(r[["Valor"]])
  ) |>
    dplyr::mutate(sex = dplyr::case_when(
      sex_sidra == "Total" ~ "Total",
      sex_sidra == "Masculino" ~ "Masculino",
      sex_sidra == "Feminino" ~ "Feminino"
    )) |>
    dplyr::filter(!is.na(sex)) |>
    dplyr::select(sex, published)
}

#' Nossa estimativa ponderada de uma proporcao, total e por sexo, em %
our_prev <- function(design, var) {
  v <- rlang::sym(var)
  tot <- design |>
    srvyr::filter(!is.na(!!v)) |>
    srvyr::summarise(p = srvyr::survey_mean(!!v, vartype = "ci", na.rm = TRUE)) |>
    dplyr::mutate(sex = "Total")
  by <- design |>
    srvyr::filter(!is.na(!!v)) |>
    srvyr::group_by(sex) |>
    srvyr::summarise(p = srvyr::survey_mean(!!v, vartype = "ci", na.rm = TRUE)) |>
    dplyr::mutate(sex = as.character(sex))
  dplyr::bind_rows(tot, by) |>
    dplyr::transmute(sex, ours = p * 100, low = p_low * 100, upp = p_upp * 100)
}

# Comparamos as versoes *_official: exclusao do caso exclusivamente gestacional e
# nao-respondente no denominador como nao-caso. E essa a convencao do IBGE, e foi
# a propria validacao abaixo que a revelou - com as versoes que deixam o NA fora
# do denominador a diferenca chegava a 3,2 pp nas mulheres.
validacao <- dplyr::bind_rows(
  dplyr::full_join(
    our_prev(des_pns_br, "hypertension_official") |> dplyr::mutate(indicador = "Hipertensao (Q00201)"),
    sidra_pns(4418, 4399, "/c1/6795"), by = "sex"),
  dplyr::full_join(
    our_prev(des_pns_br, "diabetes_official") |> dplyr::mutate(indicador = "Diabetes (Q03001)"),
    sidra_pns(4487, 4465, "/c1/6795"), by = "sex"),
  dplyr::full_join(
    our_prev(des_pns_br, "poor_health_official") |> dplyr::mutate(indicador = "Saude ruim/muito ruim (N001)"),
    sidra_pns(7666, 10985, "/c12258/104866"), by = "sex")
) |>
  dplyr::mutate(
    diferenca = ours - published,
    dentro_do_IC = published >= low & published <= upp
  ) |>
  dplyr::select(indicador, sex, ours, low, upp, published, diferenca, dentro_do_IC)

print(as.data.frame(validacao), digits = 4)
grava_log(validacao, "validacao_pns_sidra")

# Criterio de parada: diferenca maior que 0,3 pp nao e arredondamento.
falhas <- validacao |> dplyr::filter(abs(diferenca) > 0.3)
if (nrow(falhas) > 0) {
  print(as.data.frame(falhas), digits = 4)
  stop("Validacao da PNS falhou: diferenca acima de 0,3 pp em ", nrow(falhas),
       " comparacao(oes). Nao prossiga com um desenho errado.", call. = FALSE)
}
cat("\nOK: todas as estimativas reproduzem os valores publicados dentro de 0,3 pp.\n")

# =============================================================================
# PARTE C - DESENHO DO VIGITEL
# =============================================================================
# O documento oficial "Orientacoes para analises de dados do Vigitel" (item 4.3 e
# exemplo de sintaxe) determina:
#     svyset [pweight=pesorake2025]
# Ou seja: ponderacao apenas, SEM estrato e SEM UPA declarados. O Vigitel nao
# divulga variaveis de conglomerado; o efeito de desenho e absorvido pelo peso de
# pos-estratificacao (rake) por sexo, faixa etaria e escolaridade. Reproduzimos
# exatamente isso - declarar estratos que a base nao tem produziria variancia
# incompativel com a publicada pelo Ministerio.

h2("Vigitel ", ANO_VIGITEL, " - objeto de desenho")

vig <- readRDS(here::here("data", "derivado", "vigitel.rds"))
cat("amostra:", nrow(vig), "entrevistas |",
    "populacao estimada:", format(round(sum(vig$weight)), big.mark = "."), "\n")

des_vig <- vig |>
  srvyr::as_survey_design(ids = 1, weights = weight)

# =============================================================================
# PARTE D - VALIDACAO DO VIGITEL
# =============================================================================
# Referencia: prevalencias do relatorio oficial Vigitel do ano, extraidas do PDF
# baixado em 01_download.R. Comparamos com as versoes *_official dos indicadores,
# que seguem a sintaxe do Ministerio (777/888 no denominador como nao-caso).

h2("Validacao externa do Vigitel")

# Os valores de referencia sao extraidos do texto do relatorio oficial, baixado
# em 01_download.R. Cada um vem de uma frase do tipo "No conjunto das 27 cidades,
# a frequencia ... foi de X%, sendo maior entre mulheres (Y%) do que entre homens
# (Z%)". Se alguma frase nao casar - por mudanca de redacao numa edicao futura -
# o script para, em vez de comparar contra um valor inventado.

extrair_publicado <- function(pdf, padroes) {
  txt <- paste(pdftools::pdf_text(pdf), collapse = " ")
  txt <- stringr::str_squish(txt)
  purrr::imap_dfr(padroes, function(p, nome) {
    m <- stringr::str_match(txt, p$regex)
    if (any(is.na(m))) {
      stop("Nao localizei no relatorio a frase de referencia de '", nome,
           "'. Confira a redacao do PDF antes de seguir.", call. = FALSE)
    }
    v <- as.numeric(gsub(",", ".", m[1, -1]))
    tibble::tibble(
      indicador = nome,
      sex       = c("Total", p$ordem),
      published = v
    )
  })
}

pdf_vig <- here::here("data/raw", "vigitel", glue::glue("vigitel-brasil-{ANO_VIGITEL}.pdf"))
if (!file.exists(pdf_vig)) {
  stop("Relatorio oficial do Vigitel ausente: ", basename(pdf_vig),
       ". Rode 01_download.R.", call. = FALSE)
}

padroes <- list(
  "Tabagismo (fumante)" = list(
    regex = "frequência de adultos fumantes foi de (\\d+,\\d)%, sendo maior no sexo masculino \\((\\d+,\\d)%\\) do que no feminino \\((\\d+,\\d)%\\)",
    ordem = c("Masculino", "Feminino")),
  "Hipertensao (hart)" = list(
    regex = "diagnóstico médico de hipertensão arterial foi de (\\d+,\\d)%, sendo maior entre mulheres \\((\\d+,\\d)%\\) do que entre homens \\((\\d+,\\d)%\\)",
    ordem = c("Feminino", "Masculino")),
  "Diabetes (diab)" = list(
    regex = "diagnóstico médico de diabetes foi de (\\d+,\\d)%, sendo maior entre as mulheres \\((\\d+,\\d)%\\) do que entre os homens \\((\\d+,\\d)%\\)",
    ordem = c("Feminino", "Masculino")),
  "Saude ruim (saruim)" = list(
    regex = "(\\d+,\\d)% dos indivíduos avaliaram negativamente o próprio estado de saúde, sendo essa proporção maior em mulheres \\((\\d+)%\\) do que em homens \\((\\d+,\\d)%\\)",
    ordem = c("Feminino", "Masculino"))
)

publicado_vig <- extrair_publicado(pdf_vig, padroes)

validacao_vig <- dplyr::bind_rows(
  our_prev(des_vig, "smoking_official")     |> dplyr::mutate(indicador = "Tabagismo (fumante)"),
  our_prev(des_vig, "hypertension_official")|> dplyr::mutate(indicador = "Hipertensao (hart)"),
  our_prev(des_vig, "diabetes_official")    |> dplyr::mutate(indicador = "Diabetes (diab)"),
  our_prev(des_vig, "poor_health_official") |> dplyr::mutate(indicador = "Saude ruim (saruim)")
) |>
  dplyr::left_join(publicado_vig, by = c("indicador", "sex")) |>
  dplyr::mutate(
    diferenca    = ours - published,
    dentro_do_IC = published >= low & published <= upp
  ) |>
  dplyr::select(indicador, sex, ours, low, upp, published, diferenca, dentro_do_IC)

print(as.data.frame(validacao_vig), digits = 4)
grava_log(validacao_vig, "validacao_vigitel")

# O relatorio publica uma casa decimal, entao o criterio e 0,1 pp de tolerancia
# mais a propria margem de arredondamento.
falhas_vig <- validacao_vig |> dplyr::filter(abs(diferenca) > 0.15)
if (nrow(falhas_vig) > 0) {
  print(as.data.frame(falhas_vig), digits = 4)
  stop("Validacao do Vigitel falhou em ", nrow(falhas_vig), " comparacao(oes).",
       call. = FALSE)
}
cat("\nOK: as estimativas do Vigitel reproduzem o relatorio oficial ",
    ANO_VIGITEL, ".\n", sep = "")

# =============================================================================
# PARTE E - GRAVACAO
# =============================================================================

saveRDS(des_pns_br,  here::here("data", "derivado", "design_pns_brasil.rds"))
saveRDS(des_pns_cap, here::here("data", "derivado", "design_pns.rds"))
saveRDS(des_vig,     here::here("data", "derivado", "design_vigitel.rds"))
cat("\nObjetos de desenho gravados em data/.\n")

message("03_design.R concluido.")
