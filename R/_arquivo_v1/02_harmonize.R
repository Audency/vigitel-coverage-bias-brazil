# =============================================================================
# 02_harmonize.R - Harmonizacao dos instrumentos
#
# Script mais delicado do pipeline. Trabalha indicador por indicador:
#   (1) imprime o enunciado literal da pergunta em cada inquerito,
#   (2) imprime as categorias de resposta,
#   (3) declara a codificacao binaria e as divergencias,
#   (4) so entao codifica.
#
# Nenhum enunciado e digitado de memoria: todos saem do dicionario oficial
# baixado em 01_download.R.
#
# A parte do Vigitel so roda se os microdados estiverem em data-raw/vigitel/.
# Sem eles, o script harmoniza a PNS e a PNAD-TIC, grava o que da, e marca as
# colunas do Vigitel com [XX] na tabela de equivalencia.
# =============================================================================

source(here::here("R", "00_setup.R"))
library(readxl)

h1("02_harmonize.R - harmonizacao dos instrumentos")

# =============================================================================
# PARTE A - DICIONARIO DA PNS
# =============================================================================

#' Le o dicionario oficial da PNS 2019 (.xls do IBGE) e devolve um data frame
#' longo: uma linha por par variavel x categoria. Usado para imprimir enunciados
#' e categorias literais, nunca digitados a mao.
read_pns_dict <- function(path = here::here("data-raw", "pns",
                                            "dicionario_PNS_microdados_2019.xls")) {
  stopifnot(file.exists(path))
  d <- readxl::read_excel(path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  names(d) <- c("pos", "size", "var", "quesito", "desc", "cat_code", "cat_desc")
  d |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_squish(as.character(.x)))) |>
    dplyr::mutate(
      variable = ifelse(!is.na(var) & stringr::str_detect(var, "^[A-Z]"), var, NA_character_)
    ) |>
    tidyr::fill(variable, .direction = "down") |>
    dplyr::mutate(
      question = ifelse(!is.na(var) & stringr::str_detect(var, "^[A-Z]"), desc, NA_character_)
    ) |>
    tidyr::fill(question, .direction = "down") |>
    dplyr::filter(!is.na(variable)) |>
    dplyr::select(variable, question, cat_code, cat_desc)
}

pns_dict <- read_pns_dict()

#' Enunciado literal de uma variavel da PNS
pns_question <- function(v) {
  q <- pns_dict$question[pns_dict$variable == v][1]
  if (is.na(q)) stop("Variavel ausente do dicionario: ", v)
  q
}

#' Categorias literais de uma variavel da PNS, como texto "1 = Sim; 2 = Nao"
pns_categories <- function(v) {
  r <- pns_dict |>
    dplyr::filter(variable == v, !is.na(cat_code)) |>
    dplyr::distinct(cat_code, cat_desc)
  if (nrow(r) == 0) return("(variavel numerica, sem categorias)")
  paste(paste0(r$cat_code, " = ", r$cat_desc), collapse = "; ")
}

# =============================================================================
# PARTE B - PNS: LEITURA E RECORTE
# =============================================================================

h2("PNS ", YEAR_PNS, " - leitura")

pns_raw <- readRDS(here::here("data-raw", "pns",
                              glue::glue("pns{YEAR_PNS}_selected.rds")))
cat("microdados brutos (morador selecionado):", nrow(pns_raw), "linhas x",
    ncol(pns_raw), "variaveis\n")

# Variaveis usadas. Toda a analise depende destas e de mais nenhuma.
vars_pns <- c(
  # desenho
  "V0024", "UPA_PNS", "V00291", "V00292", "V00293",
  # identificacao / recorte
  "V0001", "V0031", "V0026", "V0025A",
  # desfechos
  "P050", "Q00201", "Q00202", "Q03001", "Q03002", "N001",
  # peso e altura AUTORREFERIDOS, para a sensibilidade de obesidade (Tab. S4)
  "P00104", "P00404", "P00405",
  # posse de telefone
  "A018017", "A018019", "A01901",
  # covariaveis
  "C006", "C008", "C009", "VDD004A"
)
faltantes <- setdiff(vars_pns, names(pns_raw))
if (length(faltantes) > 0) stop("Variaveis ausentes nos microdados: ",
                                paste(faltantes, collapse = ", "))

pns_sel <- pns_raw |> dplyr::select(dplyr::all_of(vars_pns))
rm(pns_raw); invisible(gc())

# ---- Recorte de capitais ----------------------------------------------------
# A PNS 2019 nao divulga codigo de municipio, mas divulga V0031 "Tipo da area",
# cuja categoria 1 e "Capital". Isso identifica exatamente o universo do Vigitel
# (26 capitais + Distrito Federal) sem necessidade de aproximacao. Nao ha PROXY
# aqui: o recorte e exato no nivel de agregacao que o IBGE libera.

h2("Recorte: capitais e adultos 18+")

cat("V0031 - ", pns_question("V0031"), "\n  categorias: ", pns_categories("V0031"), "\n", sep = "")
cat("\ndistribuicao de V0031 nos microdados (nao ponderada):\n")
print(janitor::tabyl(pns_sel, V0031))

# A harmonizacao roda sobre o Brasil inteiro e o recorte de capitais e aplicado
# so no final. Motivo: a validacao do desenho (03) compara nossas estimativas com
# as prevalencias que o IBGE publica para o Brasil - sem a amostra nacional nao
# ha como fazer essa conferencia.
n0 <- nrow(pns_sel)
pns_ad <- pns_sel |> dplyr::filter(!is.na(C008), as.numeric(as.character(C008)) >= 18)
n1 <- nrow(pns_ad)
# Morador selecionado com peso valido: sem peso, o registro nao entra em nenhuma
# estimativa ponderada.
pns_ad <- pns_ad |> dplyr::filter(!is.na(V00291), V00291 > 0)
n2 <- nrow(pns_ad)
n3 <- sum(as.character(pns_ad$V0031) == "Capital")

fluxo_pns <- tibble::tibble(
  etapa = c("moradores selecionados (Brasil)", "com 18 anos ou mais",
            "com peso de morador selecionado valido", "residentes em capitais"),
  n     = c(n0, n1, n2, n3),
  perda = c(NA, n0 - n1, n1 - n2, n2 - n3)
)
print(as.data.frame(fluxo_pns))
write_log(fluxo_pns, "fluxo_amostral_pns")

# =============================================================================
# PARTE C - DESFECHOS, UM A UM
# =============================================================================

#' Sim/Nao -> 1/0, com "Ignorado" e "Nao aplicavel" -> NA.
#' Aceita tanto factor rotulado (labels = TRUE no download) quanto codigo numerico.
yes_no <- function(x) {
  s <- as.character(x)
  dplyr::case_when(
    s %in% c("Sim", "1") ~ 1L,
    s %in% c("Não", "Nao", "2") ~ 0L,
    TRUE ~ NA_integer_
  )
}

#' Imprime o bloco de inspecao de um indicador antes de codificar
show_indicator <- function(label, var_pns, extra = NULL) {
  h2("Indicador: ", label)
  cat("PNS  | variavel : ", var_pns, "\n", sep = "")
  cat("PNS  | enunciado: ", pns_question(var_pns), "\n", sep = "")
  cat("PNS  | categorias: ", pns_categories(var_pns), "\n", sep = "")
  if (!is.null(extra)) for (v in extra) {
    cat("PNS  | auxiliar  : ", v, " - ", pns_question(v), "\n", sep = "")
    cat("PNS  |   categorias: ", pns_categories(v), "\n", sep = "")
  }
  cat("Vigitel | ", if (VIGITEL_OK) "ver bloco do Vigitel abaixo" else
      "[XX] - microdados ausentes, enunciado e codificacao pendentes", "\n", sep = "")
}

# A flag e definida na Parte E; aqui so precisamos saber se ha arquivos.
VIGITEL_OK <- length(list.files(here::here("data-raw", "vigitel"),
                                pattern = "\\.(sav|dta|csv|xlsx)$",
                                ignore.case = TRUE)) > 0

# ---- 1. Tabagismo atual -----------------------------------------------------
show_indicator("Tabagismo atual", "P050")
cat("\nDistribuicao bruta:\n"); print(janitor::tabyl(pns_ad, P050))
cat("\nCODIFICACAO: 1 = fuma atualmente (diariamente OU menos que diariamente);",
    "\n             0 = nao fuma atualmente; Ignorado -> NA\n")
cat("DIVERGENCIA: a PNS pergunta por 'algum produto do tabaco'; o Vigitel,",
    "\n             historicamente, por cigarros. Conferir no bloco do Vigitel.\n")

pns_ad <- pns_ad |>
  dplyr::mutate(
    smoking = dplyr::case_when(
      as.character(P050) %in% c("Sim, diariamente", "Sim, menos que diariamente", "1", "2") ~ 1L,
      as.character(P050) %in% c("Não fumo atualmente", "Nao fumo atualmente", "3") ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# ---- 2. Hipertensao diagnosticada -------------------------------------------
show_indicator("Hipertensao diagnosticada", "Q00201", extra = "Q00202")
cat("\nDistribuicao bruta:\n"); print(janitor::tabyl(pns_ad, Q00201))
cat("\nCODIFICACAO: 1 = diagnostico medico de hipertensao; 0 = nao; Ignorado -> NA\n")
cat("DIVERGENCIA: a PNS tem a pergunta filtro Q00202 (hipertensao apenas na",
    "\n             gravidez). Codificamos DUAS versoes e comparamos abaixo;",
    "\n             a versao principal sera fixada apos conferir a convencao do",
    "\n             Vigitel para gestacional.\n")

pns_ad <- pns_ad |>
  dplyr::mutate(
    hypertension_all  = yes_no(Q00201),
    # exclui hipertensao exclusivamente gestacional.
    # Q00202 e pergunta filtro: so e feita a mulheres que responderam Sim em
    # Q00201. Para todos os demais ela vem NA por nao aplicabilidade, e NAO por
    # falta de resposta - coalesce para 0 para nao transformar homens com
    # hipertensao em missing.
    hypertension_excl = dplyr::if_else(
      hypertension_all == 1L & dplyr::coalesce(yes_no(Q00202), 0L) == 1L,
      0L, hypertension_all
    )
  )
cat("\nEfeito de excluir hipertensao gestacional (n nao ponderado):\n")
print(janitor::tabyl(pns_ad, hypertension_all, hypertension_excl))

# ---- 3. Diabetes diagnosticado ----------------------------------------------
show_indicator("Diabetes diagnosticado", "Q03001", extra = "Q03002")
cat("\nDistribuicao bruta:\n"); print(janitor::tabyl(pns_ad, Q03001))
cat("\nCODIFICACAO: 1 = diagnostico medico de diabetes; 0 = nao; Ignorado -> NA\n")
cat("DIVERGENCIA: mesma questao gestacional (Q03002). Duas versoes, como acima.\n")

pns_ad <- pns_ad |>
  dplyr::mutate(
    diabetes_all  = yes_no(Q03001),
    # mesma logica de filtro de Q00202 (ver acima)
    diabetes_excl = dplyr::if_else(
      diabetes_all == 1L & dplyr::coalesce(yes_no(Q03002), 0L) == 1L,
      0L, diabetes_all
    )
  )
cat("\nEfeito de excluir diabetes gestacional (n nao ponderado):\n")
print(janitor::tabyl(pns_ad, diabetes_all, diabetes_excl))

# ---- 4. Autoavaliacao ruim de saude -----------------------------------------
show_indicator("Autoavaliacao ruim de saude", "N001")
cat("\nDistribuicao bruta:\n"); print(janitor::tabyl(pns_ad, N001))
cat("\nCODIFICACAO: 1 = 'Ruim' ou 'Muito ruim'; 0 = demais; Ignorado -> NA\n")
cat("DIVERGENCIA: escala de 5 pontos em ambos os inqueritos; conferir se o",
    "\n             Vigitel usa 'Muito boa/Boa/Regular/Ruim/Muito ruim'.\n")

pns_ad <- pns_ad |>
  dplyr::mutate(
    poor_health = dplyr::case_when(
      as.character(N001) %in% c("Ruim", "Muito ruim", "4", "5") ~ 1L,
      as.character(N001) %in% c("Muito boa", "Boa", "Regular", "1", "2", "3") ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# =============================================================================
# PARTE D - POSSE DE TELEFONE E COVARIAVEIS (PNS)
# =============================================================================

# ---- 5. Obesidade autorreferida (sensibilidade, Tabela S4) ------------------
# O Vigitel calcula o IMC a partir de peso e altura DECLARADOS por telefone. A
# comparacao equivalente na PNS usa o peso e a altura tambem declarados
# (P00104, P00404). A versao aferida, do modulo de antropometria, entra como
# padrao-ouro no bloco seguinte - e o eixo "com e sem correcao" da Tabela S4.
h2("Obesidade autorreferida (Tabela S4)")
cat("P00104 - ", pns_question("P00104"), "\n", sep = "")
cat("P00404 - ", pns_question("P00404"), "\n", sep = "")
cat("P00405 - ", pns_question("P00405"), "\n", sep = "")
cat("\nCODIFICACAO: IMC = peso(kg) / altura(m)^2; obesidade = IMC >= 30.\n")
cat("Mesmos limites de plausibilidade da rotina oficial do Vigitel: IMC entre 7 e 115.\n")

pns_ad <- pns_ad |>
  dplyr::mutate(
    peso_ref   = suppressWarnings(as.numeric(as.character(P00104))),
    altura_ref = suppressWarnings(as.numeric(as.character(P00404))),
    bmi_self = dplyr::if_else(
      !is.na(peso_ref) & !is.na(altura_ref) & altura_ref > 0,
      peso_ref / (altura_ref / 100)^2, NA_real_
    ),
    bmi_self = dplyr::if_else(bmi_self >= 7 & bmi_self <= 115, bmi_self, NA_real_),
    obesity_self = dplyr::if_else(!is.na(bmi_self), as.integer(bmi_self >= 30), NA_integer_)
  )
cat("\nIMC autorreferido: ", sum(!is.na(pns_ad$bmi_self)), " calculaveis de ",
    nrow(pns_ad), " (", fmt_num(100*mean(!is.na(pns_ad$bmi_self)), 1), "%)\n", sep = "")

h2("Posse de telefone no domicilio (so PNS)")
cat("A018017 - ", pns_question("A018017"), "\n  ", pns_categories("A018017"), "\n", sep = "")
cat("A018019 - ", pns_question("A018019"), "\n  ", pns_categories("A018019"), "\n", sep = "")
cat("\nCODIFICACAO em tres categorias, como manda o protocolo:\n",
    "  'Fixo'            = tem fixo (com ou sem celular)\n",
    "  'Somente celular' = nao tem fixo, tem celular\n",
    "  'Nenhum'          = nao tem fixo nem celular\n", sep = "")

pns_ad <- pns_ad |>
  dplyr::mutate(
    has_landline = yes_no(A018017),
    has_mobile   = yes_no(A018019),
    # Acesso a internet no domicilio: define o quadro amostral "web" do cenario
    # S3 da simulacao (script 08).
    has_internet = yes_no(A01901),
    phone3 = dplyr::case_when(
      has_landline == 1L ~ "Fixo",
      has_landline == 0L & has_mobile == 1L ~ "Somente celular",
      has_landline == 0L & has_mobile == 0L ~ "Nenhum",
      TRUE ~ NA_character_
    ) |> factor(levels = PHONE_LEVELS)
  )
cat("\nDistribuicao bruta (nao ponderada):\n")
print(janitor::tabyl(pns_ad, phone3))

h2("Covariaveis")

# Escolaridade: VDD004A e o nivel de instrucao mais elevado alcancado. O Vigitel
# usa faixas de ANOS DE ESTUDO (0-8, 9-11, 12+), construidas sobre a contagem
# classica em que fundamental completo = 8 anos e medio completo = 11 anos.
# Ha, portanto, duas conversoes possiveis, e elas nao dao o mesmo resultado:
#
#   (A) por nivel de ensino   0-8  = ate fundamental completo
#                             9-11 = ensino medio (incompleto ou completo)
#                             12+  = ensino superior (incompleto ou completo)
#
#   (B) por anos no sistema de 9 anos   fundamental completo = 9 anos,
#                             medio completo = 12 anos, o que joga todo o
#                             "medio completo" para a faixa 12+.
#
# Adotamos (A). Razao: as faixas do Vigitel nasceram da contagem classica de
# anos de estudo, em que a fronteira 12+ separa quem chegou ao ensino superior.
# (B) inflaria a faixa 12+ com toda a populacao de ensino medio completo e
# quebraria a comparabilidade com o Vigitel, que e o objetivo do estudo.
# As duas versoes sao impressas abaixo para que a diferenca fique documentada.
cat("VDD004A - ", pns_question("VDD004A"), "\n  ", pns_categories("VDD004A"), "\n", sep = "")
cat("\nMAPEAMENTO ADOTADO (A) - alinhado ao nivel de ensino:\n",
    "  Sem instrucao, Fundamental incompleto, Fundamental completo -> 0-8\n",
    "  Medio incompleto, Medio completo                            -> 9-11\n",
    "  Superior incompleto, Superior completo                      -> 12+\n",
    "DIVERGENCIA: o Vigitel coleta anos de estudo declarados; a PNS coleta nivel\n",
    "  de instrucao. A conversao nivel -> faixa de anos e uma aproximacao\n",
    "  necessaria e esta registrada na Tabela S1.\n", sep = "")

uf_region <- tibble::tribble(
  ~uf_name,              ~region,
  "Rondônia", "Norte", "Acre", "Norte", "Amazonas", "Norte", "Roraima", "Norte",
  "Pará", "Norte", "Amapá", "Norte", "Tocantins", "Norte",
  "Maranhão", "Nordeste", "Piauí", "Nordeste", "Ceará", "Nordeste",
  "Rio Grande do Norte", "Nordeste", "Paraíba", "Nordeste", "Pernambuco", "Nordeste",
  "Alagoas", "Nordeste", "Sergipe", "Nordeste", "Bahia", "Nordeste",
  "Minas Gerais", "Sudeste", "Espírito Santo", "Sudeste",
  "Rio de Janeiro", "Sudeste", "São Paulo", "Sudeste",
  "Paraná", "Sul", "Santa Catarina", "Sul", "Rio Grande do Sul", "Sul",
  "Mato Grosso do Sul", "Centro-Oeste", "Mato Grosso", "Centro-Oeste",
  "Goiás", "Centro-Oeste", "Distrito Federal", "Centro-Oeste"
)
stopifnot(nrow(uf_region) == 27)

pns <- pns_ad |>
  dplyr::mutate(
    age     = as.numeric(as.character(C008)),
    age_grp = cut(age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                  labels = AGE_LEVELS, right = FALSE),
    sex = dplyr::case_when(
      as.character(C006) %in% c("Homem", "1") ~ "Masculino",
      as.character(C006) %in% c("Mulher", "2") ~ "Feminino",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Masculino", "Feminino")),
    # (A) mapeamento principal, alinhado ao nivel de ensino
    education = dplyr::case_when(
      as.character(VDD004A) %in% c("Sem instrução", "Fundamental incompleto ou equivalente",
                                   "Fundamental completo ou equivalente", "1", "2", "3") ~ "0-8",
      as.character(VDD004A) %in% c("Médio incompleto ou equivalente",
                                   "Médio completo ou equivalente", "4", "5") ~ "9-11",
      as.character(VDD004A) %in% c("Superior incompleto ou equivalente",
                                   "Superior completo", "6", "7") ~ "12+",
      TRUE ~ NA_character_
    ) |> factor(levels = EDU_LEVELS),
    # (B) mapeamento alternativo, pelo sistema de 9 anos - so para a Tabela S5
    # (sensibilidade ao conjunto de covariaveis). Nunca usado como principal.
    education_alt9 = dplyr::case_when(
      as.character(VDD004A) %in% c("Sem instrução", "Fundamental incompleto ou equivalente", "1", "2") ~ "0-8",
      as.character(VDD004A) %in% c("Fundamental completo ou equivalente",
                                   "Médio incompleto ou equivalente", "3", "4") ~ "9-11",
      as.character(VDD004A) %in% c("Médio completo ou equivalente", "Superior incompleto ou equivalente",
                                   "Superior completo", "5", "6", "7") ~ "12+",
      TRUE ~ NA_character_
    ) |> factor(levels = EDU_LEVELS),
    race = dplyr::case_when(
      as.character(C009) %in% c("Branca", "1") ~ "Branca",
      as.character(C009) %in% c("Preta", "2") ~ "Preta",
      as.character(C009) %in% c("Parda", "4") ~ "Parda",
      as.character(C009) %in% c("Amarela", "3") ~ "Amarela",
      as.character(C009) %in% c("Indígena", "5") ~ "Indigena",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Branca", "Preta", "Parda", "Amarela", "Indigena")),
    uf_name = as.character(V0001),
    urban = dplyr::case_when(
      as.character(V0026) %in% c("Urbano", "1") ~ "Urbano",
      as.character(V0026) %in% c("Rural", "2") ~ "Rural",
      TRUE ~ NA_character_
    ),
    # desenho
    strata = as.character(V0024),
    psu    = as.character(UPA_PNS),
    weight = as.numeric(V00291),
    survey = "PNS"
  ) |>
  dplyr::left_join(uf_region, by = "uf_name") |>
  dplyr::mutate(region = factor(region, levels = REGION_LEVELS))

stopifnot(!any(is.na(pns$region)))

cat("\nComparacao dos dois mapeamentos de escolaridade (n nao ponderado):\n")
print(janitor::tabyl(pns, education, education_alt9))

# ---- Versoes "oficiais" dos desfechos da PNS --------------------------------
# Descoberto na validacao de 03: as prevalencias publicadas pelo IBGE para a PNS
# 2019 sao reproduzidas exatamente quando (i) se exclui o caso exclusivamente
# gestacional e (ii) o nao-respondente permanece no denominador como nao-caso.
# Essa e a MESMA convencao das rotinas oficiais do Vigitel (777 -> nao-caso).
# Ou seja: os dois inqueritos concordam na convencao, e adota-la torna as
# estimativas simultaneamente comparaveis entre si e reproduziveis contra o que
# cada orgao publica. As versoes com NA fora do denominador ficam como
# sensibilidade (Tabela S5).
pns <- pns |>
  dplyr::mutate(
    smoking_official      = dplyr::coalesce(smoking, 0L),
    hypertension_official = dplyr::coalesce(hypertension_excl, 0L),
    diabetes_official     = dplyr::coalesce(diabetes_excl, 0L),
    poor_health_official  = dplyr::coalesce(poor_health, 0L)
  )

cat("\nCovariaveis codificadas (n nao ponderado, NA incluidos):\n")
for (v in c("sex", "age_grp", "education", "race", "region", "urban", "phone3")) {
  cat("\n--", v, "--\n"); print(janitor::tabyl(pns, !!rlang::sym(v)))
}

pns_out <- pns |>
  dplyr::mutate(area_type = as.character(V0031)) |>
  dplyr::select(survey, strata, psu, weight, area_type,
                uf_name, region, urban, sex, age, age_grp, education, education_alt9, race,
                phone3, has_landline, has_mobile, has_internet,
                # versoes oficiais (convencao IBGE/MS) - usadas na analise principal
                smoking_official, hypertension_official, diabetes_official, poor_health_official,
                bmi_self, obesity_self,
                # versoes com nao-resposta fora do denominador - sensibilidade (S5)
                smoking, hypertension_excl, diabetes_excl, poor_health,
                # sem exclusao gestacional - documentacao da decisao
                hypertension_all, diabetes_all)

# Duas gravacoes:
#   pns_brasil.rds - amostra nacional, usada SO na validacao do desenho (03),
#                    onde comparamos com as prevalencias publicadas pelo IBGE;
#   pns.rds        - recorte de capitais, base de toda a analise do artigo.
# O objeto de desenho tem que ser construido sobre a amostra completa e o recorte
# aplicado depois com subset(), para que a variancia use a estrutura correta de
# estratos e UPAs. Isso esta implementado em 03_design.R.
saveRDS(pns_out, here::here("data", "pns_brasil.rds"))
cat("\ndata/pns_brasil.rds gravado:", nrow(pns_out), "linhas x", ncol(pns_out), "variaveis\n")

pns_cap_out <- pns_out |> dplyr::filter(area_type == "Capital")
saveRDS(pns_cap_out, here::here("data", "pns.rds"))
cat("data/pns.rds (capitais) gravado:", nrow(pns_cap_out), "linhas\n")

# =============================================================================
# PARTE E - VIGITEL
# =============================================================================

h2("Vigitel ", YEAR_VIGITEL)

# ---- Dicionario do Vigitel --------------------------------------------------
# O dicionario oficial tem duas planilhas: "Variaveis_Vigitel" (nome, rotulo e
# categorias) e "Indicadores_Vigitel", que traz a SINTAXE STATA usada pelo
# Ministerio para gerar cada indicador. Usamos essa sintaxe como fonte da
# operacionalizacao - nada e reconstruido de memoria.

read_vigitel_dict <- function(path = list.files(here::here("data-raw", "vigitel"),
                                                pattern = "^dicionario-vigitel-.*\\.xlsx$",
                                                full.names = TRUE)[1]) {
  stopifnot(!is.na(path), file.exists(path))

  v <- readxl::read_excel(path, sheet = "Variáveis_Vigitel",
                          col_names = FALSE, .name_repair = "minimal")
  names(v) <- paste0("c", seq_len(ncol(v)))
  v <- v |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_squish(as.character(.x)))) |>
    dplyr::mutate(
      variable = ifelse(!is.na(c1) & !c1 %in% c("VARIÁVEIS VIGITEL", "Variable name"), c1, NA_character_)
    ) |>
    tidyr::fill(variable, .direction = "down") |>
    dplyr::mutate(
      question = ifelse(!is.na(c1) & !c1 %in% c("VARIÁVEIS VIGITEL", "Variable name"), c5, NA_character_)
    ) |>
    tidyr::fill(question, .direction = "down") |>
    dplyr::filter(!is.na(variable)) |>
    dplyr::select(variable, question, cat_code = c6, cat_desc = c7)

  i <- readxl::read_excel(path, sheet = "Indicadores_Vigitel",
                          col_names = FALSE, .name_repair = "minimal")
  names(i) <- paste0("c", seq_len(ncol(i)))
  i <- i |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_squish(as.character(.x)))) |>
    dplyr::filter(!is.na(c1), !c1 %in% c("INDICADORES VIGITEL", "variable name")) |>
    dplyr::select(indicator = c1, label = c5, routine = c8) |>
    dplyr::distinct(indicator, .keep_all = TRUE)

  list(vars = v, ind = i)
}

vig_dict <- read_vigitel_dict()

vig_question <- function(v) {
  q <- vig_dict$vars$question[vig_dict$vars$variable == v][1]
  if (is.na(q) || !nzchar(q)) paste0("(sem rotulo no dicionario: ", v, ")") else q
}
vig_categories <- function(v) {
  r <- vig_dict$vars |>
    dplyr::filter(variable == v, !is.na(cat_code)) |>
    dplyr::distinct(cat_code, cat_desc)
  if (nrow(r) == 0) return("(variavel numerica, sem categorias)")
  paste(paste0(r$cat_code, " = ", r$cat_desc), collapse = "; ")
}
vig_routine <- function(ind) {
  r <- vig_dict$ind$routine[vig_dict$ind$indicator == ind][1]
  if (is.na(r)) paste0("(sem rotina publicada para ", ind, ")") else r
}

# ---- Leitura dos microdados -------------------------------------------------

vig_file <- list.files(here::here("data-raw", "vigitel"),
                       pattern = glue::glue("^Vigitel-{YEAR_VIGITEL}.*\\.xlsx?$"),
                       full.names = TRUE, ignore.case = TRUE)[1]
if (is.na(vig_file)) {
  stop("Base do Vigitel nao encontrada em data-raw/vigitel/. Rode 01_download.R.",
       call. = FALSE)
}
cat("arquivo:", basename(vig_file), "\n")

# O .xls tem 158 MB e 229 colunas. Lemos o cabecalho, montamos col_types com
# "skip" para tudo que nao entra na analise e so entao lemos os dados: evita
# carregar ~200 colunas que nunca serao usadas.
vig_names <- names(readxl::read_excel(vig_file, n_max = 0))

# Alem das perguntas, lemos os indicadores JA CALCULADOS pelo Ministerio
# (fumante, hart, diab, saruim, obesid): eles servem de conferencia registro a
# registro da nossa codificacao - ver bloco de validacao interna adiante.
vig_keep <- intersect(c(
  "chave", "ano", "cidade", "q6", "q7", "q8_anos", "q9", "q11",
  "q60", "q69", "q74", "q75", "q76",
  "fet", "fesc", "fxesc", "pesorake", "pesorake2025", "q9_i", "q11_i",
  "fumante", "hart", "diab", "saruim", "obesid", "imc"
), vig_names)

vig_types <- ifelse(vig_names %in% vig_keep, "guess", "skip")
t0 <- Sys.time()
vig_raw <- readxl::read_excel(vig_file, col_types = vig_types, guess_max = 100000)
cat("microdados lidos em", round(difftime(Sys.time(), t0, units = "secs")), "s:",
    nrow(vig_raw), "linhas x", ncol(vig_raw), "variaveis (de",
    length(vig_names), "no arquivo)\n")

# Peso de pos-estratificacao (rake). O documento oficial "Orientacoes para
# analises de dados do Vigitel" (baixado em 01) determina, no item 4.3, que toda
# a serie 2006-2024 seja analisada com a variavel "pesorake2025", que incorpora a
# recalibragem pelo Censo 2022. Preferimos essa; se ela nao existir nesta edicao
# da base, caimos para a unica variavel de peso disponivel.
w_cand <- grep("^peso", names(vig_raw), value = TRUE, ignore.case = TRUE)
cat("candidatas a peso:", paste(w_cand, collapse = ", "), "\n")
w_var <- if ("pesorake2025" %in% w_cand) {
  "pesorake2025"
} else if (length(w_cand) == 1) {
  w_cand
} else {
  stop("Nao consegui decidir a variavel de peso. Candidatas: ",
       paste(w_cand, collapse = ", "), call. = FALSE)
}
cat("peso adotado:", w_var, "\n")
if (w_var != "pesorake2025") {
  cat(
    "NOTA METODOLOGICA: o arquivo da edicao ", YEAR_VIGITEL, " traz 'pesorake',\n",
    "calibrado pelas projecoes populacionais vigentes a epoca. O arquivo da serie\n",
    "2006-2024 traz 'pesorake2025', recalibrado pelo Censo 2022. Usamos o peso da\n",
    "edicao porque (i) e ele que reproduz o relatorio Vigitel ", YEAR_VIGITEL, ",\n",
    "criterio da validacao em 03, e (ii) e contemporaneo da calibragem da PNS ",
    YEAR_PNS, ",\n",
    "que e o inquerito de comparacao. O peso recalibrado fica como sensibilidade.\n",
    sep = ""
  )
}

vars_vig <- c("cidade", "q6", "q7", "q8_anos", "q9", "q11", "q60", "q69",
              "q74", "q75", "q76", w_var)
faltantes_vig <- setdiff(vars_vig, names(vig_raw))
if (length(faltantes_vig) > 0) {
  stop("Variaveis ausentes na base do Vigitel: ", paste(faltantes_vig, collapse = ", "),
       "\nVariaveis disponiveis: ", paste(head(names(vig_raw), 40), collapse = ", "),
       call. = FALSE)
}

vig <- vig_raw |> dplyr::select(dplyr::all_of(intersect(
  c(vars_vig, "chave", "ano", "fesc", "fxesc", "fet", "q9_i", "q11_i",
    # indicadores calculados pelo Ministerio, para a conferencia interna
    "fumante", "hart", "diab", "saruim", "obesid", "imc"),
  names(vig_raw)
)))
rm(vig_raw); invisible(gc())

# ---- Enunciados lado a lado e codificacao dos desfechos ---------------------

#' Compara enunciados dos dois inqueritos e imprime a rotina oficial do Vigitel
side_by_side <- function(label, var_pns, var_vig, ind_vig) {
  h2("Indicador: ", label, " - PNS x Vigitel")
  cat("PNS     | ", var_pns, ": ", pns_question(var_pns), "\n", sep = "")
  cat("PNS     |   categorias: ", pns_categories(var_pns), "\n", sep = "")
  cat("Vigitel | ", var_vig, ": ", vig_question(var_vig), "\n", sep = "")
  cat("Vigitel |   categorias: ", vig_categories(var_vig), "\n", sep = "")
  cat("Vigitel |   rotina oficial: ", vig_routine(ind_vig), "\n", sep = "")
}

side_by_side("Tabagismo atual", "P050", "q60", "fumante")
side_by_side("Hipertensao diagnosticada", "Q00201", "q75", "hart")
side_by_side("Diabetes diagnosticado", "Q03001", "q76", "diab")
side_by_side("Autoavaliacao ruim de saude", "N001", "q74", "saruim")

h2("Divergencia central: tratamento de 'nao sabe' / 'nao quis informar'")
cat(
  "O Vigitel codifica 777 = 'nao sabe' e 888 = 'nao quis informar'. As rotinas\n",
  "oficiais (ex.: cond(q75==1,1,0)) jogam esses casos no denominador como NAO-caso.\n",
  "Na PNS, 'Ignorado' foi codificado como NA, saindo do denominador.\n",
  "Sao convencoes diferentes. Geramos as DUAS versoes:\n",
  "  *_official  - segue a rotina do Ministerio (777/888 -> 0); usada em 03 para\n",
  "                reproduzir as prevalencias publicadas do Vigitel;\n",
  "  <indicador> - harmonizada com a PNS (777/888 -> NA); usada na comparacao (05).\n",
  sep = ""
)

num <- function(x) suppressWarnings(as.numeric(as.character(x)))

vig <- vig |>
  dplyr::mutate(
    q60n = num(q60), q74n = num(q74), q75n = num(q75), q76n = num(q76),

    # rotina oficial: gen fumante = cond(q60<3 & q60!=., 1, 0)
    smoking_official = dplyr::if_else(!is.na(q60n) & q60n < 3, 1L, 0L),
    smoking = dplyr::case_when(q60n %in% c(1, 2) ~ 1L,
                               q60n == 3 ~ 0L,
                               TRUE ~ NA_integer_),

    # gen hart = cond(q75 == 1, 1, 0)
    hypertension_official = dplyr::if_else(!is.na(q75n) & q75n == 1, 1L, 0L),
    hypertension = dplyr::case_when(q75n == 1 ~ 1L, q75n == 2 ~ 0L, TRUE ~ NA_integer_),

    # gen diab = cond(q76 == 1, 1, 0)
    diabetes_official = dplyr::if_else(!is.na(q76n) & q76n == 1, 1L, 0L),
    diabetes = dplyr::case_when(q76n == 1 ~ 1L, q76n == 2 ~ 0L, TRUE ~ NA_integer_),

    # gen saruim = cond(q74 == 4 | q74 == 5, 1, 0)
    poor_health_official = dplyr::if_else(!is.na(q74n) & q74n %in% c(4, 5), 1L, 0L),
    poor_health = dplyr::case_when(q74n %in% c(4, 5) ~ 1L,
                                   q74n %in% c(1, 2, 3) ~ 0L,
                                   TRUE ~ NA_integer_)
  )

# ---- Validacao interna: nossa codificacao x indicadores do Ministerio -------
# A base do Vigitel ja traz os indicadores calculados pelo proprio Ministerio
# (fumante, hart, diab, saruim). Comparamos registro a registro: se a nossa
# versao *_official divergir em UM caso sequer, a codificacao esta errada.

h2("Validacao interna: nossa codificacao x indicadores oficiais do arquivo")

check_pairs <- list(
  c("smoking_official",     "fumante"),
  c("hypertension_official", "hart"),
  c("diabetes_official",    "diab"),
  c("poor_health_official", "saruim")
)
conferencia <- purrr::map_dfr(check_pairs, function(p) {
  if (!p[2] %in% names(vig)) {
    return(tibble::tibble(nossa = p[1], oficial = p[2], n_divergente = NA_integer_,
                          situacao = "variavel oficial ausente do arquivo"))
  }
  ofic <- num(vig[[p[2]]])
  nossa <- vig[[p[1]]]
  dif <- sum(!is.na(ofic) & ofic != nossa, na.rm = TRUE) +
         sum(is.na(ofic) != is.na(nossa))
  tibble::tibble(nossa = p[1], oficial = p[2], n_divergente = as.integer(dif),
                 situacao = if (dif == 0) "identico" else "DIVERGE")
})
print(as.data.frame(conferencia))
write_log(conferencia, "conferencia_vigitel_oficial")

if (any(conferencia$situacao == "DIVERGE", na.rm = TRUE)) {
  stop("Nossa codificacao do Vigitel nao reproduz os indicadores oficiais do ",
       "arquivo. Corrija antes de seguir - nada adiante vale com desfecho errado.",
       call. = FALSE)
}

cat("\nCasos afetados pela divergencia (n nao ponderado):\n")
print(as.data.frame(tibble::tibble(
  indicador = c("tabagismo", "hipertensao", "diabetes", "saude ruim"),
  n_777_888 = c(sum(vig$q60n %in% c(777, 888), na.rm = TRUE),
                sum(vig$q75n %in% c(777, 888), na.rm = TRUE),
                sum(vig$q76n %in% c(777, 888), na.rm = TRUE),
                sum(vig$q74n %in% c(777, 888), na.rm = TRUE)),
  n_NA      = c(sum(is.na(vig$smoking)), sum(is.na(vig$hypertension)),
                sum(is.na(vig$diabetes)), sum(is.na(vig$poor_health)))
)))

# ---- Covariaveis do Vigitel -------------------------------------------------

h2("Covariaveis do Vigitel")
cat("q7  - ", vig_question("q7"), ": ", vig_categories("q7"), "\n", sep = "")
cat("q69 - cor: ", vig_categories("q69"), "\n", sep = "")
cat("fesc - ", vig_question("fesc"), ": ", vig_categories("fesc"), "\n", sep = "")
cat("\nA faixa fesc do Vigitel (0 a 8 / 9 a 11 / 12 anos e mais) e a definicao de\n",
    "referencia das faixas de escolaridade deste estudo: e ela que fixa a\n",
    "fronteira em 11 anos para o ensino medio completo, e foi por isso que o\n",
    "mapeamento da PNS foi feito por nivel de ensino, e nao pelo sistema de 9 anos.\n", sep = "")

# cidade -> capital -> UF -> regiao, pelo rotulo do dicionario
cidade_map <- vig_dict$vars |>
  dplyr::filter(variable == "cidade", !is.na(cat_code)) |>
  dplyr::distinct(cat_code, cat_desc) |>
  dplyr::mutate(
    cod  = as.numeric(cat_code),
    nome = tolower(iconv(cat_desc, to = "ASCII//TRANSLIT")),
    # O Vigitel rotula a capital do DF como "distrito federal"; a tabela de
    # capitais usa o nome do municipio, "Brasilia". Mesma unidade, rotulo
    # diferente - tratamos o apelido explicitamente em vez de deixar cair fora
    # do join, que descartaria uma das 27 capitais em silencio.
    nome = dplyr::recode(nome, "distrito federal" = "brasilia")
  )

capitals <- readRDS(here::here("data-raw", "censo", "capitals.rds")) |>
  dplyr::mutate(nome = tolower(iconv(capital, to = "ASCII//TRANSLIT")))

cidade_map <- cidade_map |> dplyr::left_join(capitals, by = "nome")
if (any(is.na(cidade_map$uf))) {
  stop("Capitais do Vigitel sem correspondencia: ",
       paste(cidade_map$cat_desc[is.na(cidade_map$uf)], collapse = ", "), call. = FALSE)
}
cat("\ncidades do Vigitel mapeadas para UF/regiao:", nrow(cidade_map), "de 27\n")

vigitel <- vig |>
  dplyr::mutate(
    cod = num(cidade),
    age = num(q6),
    age_grp = cut(age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                  labels = AGE_LEVELS, right = FALSE),
    sex = dplyr::case_when(num(q7) == 1 ~ "Masculino",
                           num(q7) == 2 ~ "Feminino",
                           TRUE ~ NA_character_) |>
      factor(levels = c("Masculino", "Feminino")),
    education = dplyr::case_when(num(fesc) == 1 ~ "0-8",
                                 num(fesc) == 2 ~ "9-11",
                                 num(fesc) == 3 ~ "12+",
                                 TRUE ~ NA_character_) |> factor(levels = EDU_LEVELS),
    race = dplyr::case_when(num(q69) == 1 ~ "Branca", num(q69) == 2 ~ "Preta",
                            num(q69) == 3 ~ "Amarela", num(q69) == 4 ~ "Parda",
                            num(q69) == 5 ~ "Indigena", TRUE ~ NA_character_) |>
      factor(levels = c("Branca", "Preta", "Parda", "Amarela", "Indigena")),
    weight = num(.data[[w_var]]),
    survey = "Vigitel"
  ) |>
  dplyr::left_join(cidade_map |> dplyr::select(cod, capital, uf, region), by = "cod") |>
  dplyr::mutate(region = factor(region, levels = REGION_LEVELS))

stopifnot(!any(is.na(vigitel$region)), !any(is.na(vigitel$weight)))

cat("\nCovariaveis do Vigitel (n nao ponderado):\n")
for (v in c("sex", "age_grp", "education", "race", "region")) {
  cat("\n--", v, "--\n"); print(janitor::tabyl(vigitel, !!rlang::sym(v)))
}

vigitel_out <- vigitel |>
  dplyr::select(survey, weight, capital, uf, region, sex, age, age_grp, education, race,
                smoking, smoking_official,
                hypertension, hypertension_official,
                diabetes, diabetes_official,
                poor_health, poor_health_official,
                dplyr::any_of(c("q9", "q11", "q9_i", "q11_i", "obesid", "imc")))

saveRDS(vigitel_out, here::here("data", "vigitel.rds"))
cat("\ndata/vigitel.rds gravado:", nrow(vigitel_out), "linhas x", ncol(vigitel_out), "variaveis\n")

# =============================================================================
# PARTE E2 - PNS: ANTROPOMETRIA AFERIDA (padrao-ouro da Tabela S4)
# =============================================================================
# Subamostra propria, com peso proprio (V00301). Nunca misturar com o arquivo do
# morador selecionado: sao desenhos diferentes.

h2("PNS ", YEAR_PNS, " - antropometria aferida")

anthro_raw <- readRDS(here::here("data-raw", "pns",
                                 glue::glue("pns{YEAR_PNS}_anthropometry.rds")))
cat("registros na subamostra de antropometria:", nrow(anthro_raw), "\n")
cat("W00103 - ", pns_question("W00103"), "\n", sep = "")
cat("W00203 - ", pns_question("W00203"), "\n", sep = "")

vars_anthro <- c("V0024", "UPA_PNS", "V00301", "V0001", "V0031", "C006", "C008",
                 "C009", "VDD004A", "W00103", "W00203")
falta_a <- setdiff(vars_anthro, names(anthro_raw))
if (length(falta_a) > 0) stop("Variaveis ausentes na antropometria: ",
                              paste(falta_a, collapse = ", "))

anthro <- anthro_raw |>
  dplyr::select(dplyr::all_of(vars_anthro)) |>
  dplyr::mutate(
    age = suppressWarnings(as.numeric(as.character(C008))),
    peso_af   = suppressWarnings(as.numeric(as.character(W00103))),
    altura_af = suppressWarnings(as.numeric(as.character(W00203))),
    bmi_measured = dplyr::if_else(
      !is.na(peso_af) & !is.na(altura_af) & altura_af > 0,
      peso_af / (altura_af / 100)^2, NA_real_
    ),
    bmi_measured = dplyr::if_else(bmi_measured >= 7 & bmi_measured <= 115,
                                  bmi_measured, NA_real_),
    obesity_measured = dplyr::if_else(!is.na(bmi_measured),
                                      as.integer(bmi_measured >= 30), NA_integer_),
    sex = dplyr::case_when(as.character(C006) %in% c("Homem", "1") ~ "Masculino",
                           as.character(C006) %in% c("Mulher", "2") ~ "Feminino",
                           TRUE ~ NA_character_) |>
      factor(levels = c("Masculino", "Feminino")),
    age_grp = cut(age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                  labels = AGE_LEVELS, right = FALSE),
    uf_name = as.character(V0001),
    strata = as.character(V0024), psu = as.character(UPA_PNS),
    weight = as.numeric(V00301), survey = "PNS antropometria",
    area_type = as.character(V0031)
  ) |>
  dplyr::left_join(uf_region, by = "uf_name") |>
  dplyr::mutate(region = factor(region, levels = REGION_LEVELS)) |>
  dplyr::filter(!is.na(age), age >= 18, !is.na(weight), weight > 0)

anthro_out <- anthro |>
  dplyr::select(survey, strata, psu, weight, area_type, uf_name, region,
                sex, age, age_grp, bmi_measured, obesity_measured)

cat("adultos com peso valido:", nrow(anthro_out),
    "| em capitais:", sum(anthro_out$area_type == "Capital"),
    "| com IMC aferido:", sum(!is.na(anthro_out$bmi_measured)), "\n")

saveRDS(anthro_out, here::here("data", "pns_anthro.rds"))
cat("data/pns_anthro.rds gravado.\n")

# =============================================================================
# PARTE F - TABELA DE EQUIVALENCIA (-> Tabela S1)
# =============================================================================

h2("Tabela de equivalencia (Tabela S1)")

n777 <- function(v) sum(num(vig[[v]]) %in% c(777, 888), na.rm = TRUE)

equivalencia <- tibble::tribble(
  ~indicador, ~var_pns, ~enunciado_pns, ~cat_pns,
  ~var_vigitel, ~enunciado_vigitel, ~cat_vigitel, ~rotina_vigitel,
  ~codificacao, ~divergencia,

  "Tabagismo atual", "P050", pns_question("P050"), pns_categories("P050"),
  "q60", vig_question("q60"), vig_categories("q60"), vig_routine("fumante"),
  "1 = fuma atualmente (diariamente ou menos que diariamente); 0 = nao fuma",
  paste("A PNS pergunta por 'algum produto do tabaco'; o Vigitel pergunta por",
        "cigarros. Diferenca de escopo do produto, nao de estrutura da resposta."),

  "Hipertensao diagnosticada", "Q00201", pns_question("Q00201"), pns_categories("Q00201"),
  "q75", vig_question("q75"), vig_categories("q75"), vig_routine("hart"),
  "1 = diagnostico medico referido; 0 = nao",
  glue::glue("A PNS tem filtro para hipertensao exclusivamente gestacional (Q00202), ",
             "que o Vigitel nao tem. O Vigitel tem 777 'nao sabe' ({n777('q75')} casos), ",
             "que a rotina oficial conta como nao-caso; na versao harmonizada eles saem ",
             "do denominador, como o 'Ignorado' da PNS."),

  "Diabetes diagnosticado", "Q03001", pns_question("Q03001"), pns_categories("Q03001"),
  "q76", vig_question("q76"), vig_categories("q76"), vig_routine("diab"),
  "1 = diagnostico medico referido; 0 = nao",
  glue::glue("Mesma questao gestacional (Q03002) e mesmo tratamento de 777 ",
             "({n777('q76')} casos) descrito para a hipertensao."),

  "Autoavaliacao ruim de saude", "N001", pns_question("N001"), pns_categories("N001"),
  "q74", vig_question("q74"), vig_categories("q74"), vig_routine("saruim"),
  "1 = 'Ruim' ou 'Muito ruim'; 0 = 'Muito boa', 'Boa' ou 'Regular'",
  glue::glue("Escala de 5 pontos identica nos dois inqueritos. O Vigitel acrescenta ",
             "777/888 ({n777('q74')} casos), tratados como acima."),

  "Escolaridade", "VDD004A", pns_question("VDD004A"), pns_categories("VDD004A"),
  "fesc", "escolaridade (faixas de anos de estudo)", vig_categories("fesc"), "(variavel derivada da base)",
  "0-8 = ate fundamental completo; 9-11 = ensino medio; 12+ = ensino superior",
  paste("A PNS coleta nivel de instrucao; o Vigitel coleta anos de estudo declarados.",
        "As faixas do Vigitel (0 a 8 / 9 a 11 / 12 anos e mais) fixam a fronteira em",
        "11 anos para o medio completo, e o mapeamento da PNS segue essa fronteira.",
        "A alternativa pelo sistema de 9 anos e testada na Tabela S5."),

  "Posse de telefone", "A018017 / A018019",
  paste(pns_question("A018017"), "|", pns_question("A018019")),
  paste(pns_categories("A018017"), "|", pns_categories("A018019")),
  "(nao aplicavel)", "(nao aplicavel)", "(nao aplicavel)", "(nao aplicavel)",
  "Fixo (com ou sem celular) / Somente celular / Nenhum",
  paste("Variavel existe apenas na PNS. E a base de toda a estimacao de nao",
        "cobertura: o Vigitel, por construcao, so entrevista quem tem telefone.")
)

print(as.data.frame(equivalencia[, c("indicador", "var_pns", "codificacao")]))
saveRDS(equivalencia, here::here("data", "equivalencia.rds"))
cat("\ndata/equivalencia.rds gravado.\n")

message("\n02_harmonize.R concluido: PNS (Brasil e capitais), Vigitel e equivalencia gravados.")
