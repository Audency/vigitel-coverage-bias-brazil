# =============================================================================
# 14_validacao_2023.R - Validacao da simulacao contra a transicao real do Vigitel
#
# A edicao de 2023 do Vigitel adotou cadastro duplo e divulga, na propria base,
# tres elementos que tornam a transicao auditavel:
#   tipo_fone         - quadro de origem de cada entrevista (FIXO ou CELULAR)
#   pesorake_fixo     - peso de pos-estratificacao do quadro fixo isolado
#   pesorake_celular  - peso de pos-estratificacao do quadro celular isolado
#   pesorake          - peso do cadastro duplo, efetivamente publicado
#
# Cada um dos tres pesos expande para a mesma populacao-alvo. Isso permite
# reconstruir, DENTRO de 2023, a estimativa que o desenho legado teria produzido
# e compara-la com a do desenho novo, com o tempo mantido constante - o que a
# comparacao 2019 x 2023 nao permitiria, por confundir desenho com quatro anos
# de mudanca epidemiologica.
#
# PREVISAO A TESTAR, derivada dos resultados de 2019: como os pesos ja sao
# pos-estratificados por idade, sexo e escolaridade nos tres bracos, a diferenca
# entre o quadro fixo e o cadastro duplo deve ser GRANDE para os indicadores com
# associacao residual a posse de telefone (tabagismo, autoavaliacao de saude) e
# PEQUENA para os que nao a tem (hipertensao, diabetes).
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("14_validacao_2023.R - transicao real do Vigitel")

ANO_DUAL <- 2023

# ---- Leitura ----------------------------------------------------------------

arq <- here::here("data-raw", "vigitel", glue::glue("Vigitel-{ANO_DUAL}-peso-rake.xlsx"))
if (!file.exists(arq)) {
  stop("Base do Vigitel ", ANO_DUAL, " ausente. Baixe de svs.aids.gov.br.", call. = FALSE)
}

nomes <- names(readxl::read_excel(arq, n_max = 0))
usar <- c("chave", "ano", "cidade", "tipo_fone",
          "pesorake", "pesorake_fixo", "pesorake_celular",
          "q6", "q7", "fesc", "fumante", "hart", "diab", "saruim")
faltam <- setdiff(usar, nomes)
if (length(faltam) > 0) stop("Variaveis ausentes: ", paste(faltam, collapse = ", "))

d23 <- readxl::read_excel(arq, col_types = ifelse(nomes %in% usar, "guess", "skip"),
                          guess_max = 60000)
num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Mapeamento cidade -> regiao, pelo mesmo dicionario usado em 02
vig_dict <- readxl::read_excel(
  list.files(here::here("data-raw", "vigitel"),
             pattern = "^dicionario-vigitel-.*\\.xlsx$", full.names = TRUE)[1],
  sheet = "Variáveis_Vigitel", col_names = FALSE, .name_repair = "minimal")
names(vig_dict) <- paste0("c", seq_len(ncol(vig_dict)))
vig_dict <- vig_dict |>
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_squish(as.character(.x)))) |>
  dplyr::mutate(v = ifelse(!is.na(c1) & !c1 %in% c("VARIÁVEIS VIGITEL", "Variable name"), c1, NA)) |>
  tidyr::fill(v) |>
  dplyr::filter(v == "cidade", !is.na(c6)) |>
  dplyr::distinct(cod = as.numeric(c6), nome = tolower(iconv(c7, to = "ASCII//TRANSLIT"))) |>
  dplyr::mutate(nome = dplyr::recode(nome, "distrito federal" = "brasilia"))

capitais <- readRDS(here::here("data-raw", "censo", "capitals.rds")) |>
  dplyr::mutate(nome = tolower(iconv(capital, to = "ASCII//TRANSLIT")))
mapa <- vig_dict |> dplyr::left_join(capitais, by = "nome") |>
  dplyr::select(cod, capital, region)
stopifnot(!any(is.na(mapa$region)))

d23 <- d23 |>
  dplyr::mutate(
    cod = num(cidade),
    quadro = dplyr::case_when(toupper(tipo_fone) == "FIXO" ~ "Fixo",
                              toupper(tipo_fone) == "CELULAR" ~ "Celular",
                              TRUE ~ NA_character_),
    sex = dplyr::case_when(num(q7) == 1 ~ "Masculino", num(q7) == 2 ~ "Feminino"),
    age = num(q6),
    age_grp = cut(age, c(18, 25, 35, 45, 55, 65, Inf), labels = AGE_LEVELS, right = FALSE),
    education = dplyr::case_when(num(fesc) == 1 ~ "0-8", num(fesc) == 2 ~ "9-11",
                                 num(fesc) == 3 ~ "12+"),
    dplyr::across(c(fumante, hart, diab, saruim), num)
  ) |>
  dplyr::left_join(mapa, by = "cod") |>
  dplyr::mutate(region = factor(region, levels = REGION_LEVELS))

stopifnot(!any(is.na(d23$quadro)), !any(is.na(d23$region)))

cat("Vigitel", ANO_DUAL, ":", nrow(d23), "entrevistas |",
    sum(d23$quadro == "Fixo"), "do quadro fixo,", sum(d23$quadro == "Celular"), "do celular\n")

DESFECHOS <- tibble::tribble(
  ~var,       ~label,
  "fumante",  "Tabagismo atual",
  "hart",     "Hipertensão diagnosticada",
  "diab",     "Diabetes diagnosticado",
  "saruim",   "Autoavaliação ruim de saúde"
)

# ---- Quem o desenho legado deixava de fora ----------------------------------

h2("Perfil das entrevistas por quadro de origem")

perfil <- d23 |>
  dplyr::filter(!is.na(age_grp), !is.na(education)) |>
  dplyr::group_by(quadro) |>
  dplyr::summarise(
    n = dplyr::n(),
    idade_media = mean(age, na.rm = TRUE),
    pct_18_34 = 100 * mean(age_grp %in% c("18-24", "25-34")),
    pct_65mais = 100 * mean(age_grp == "65+"),
    pct_0_8 = 100 * mean(education == "0-8"),
    pct_12mais = 100 * mean(education == "12+"),
    .groups = "drop"
  )
print(as.data.frame(perfil), digits = 3)
write_log(perfil, "vigitel2023_perfil_por_quadro")

# ---- Estimativas nos tres desenhos -----------------------------------------
# Cada peso ja expande para a populacao-alvo completa; as estimativas sao
# portanto diretamente comparaveis.

h2("Prevalencias nos tres desenhos")

est_peso <- function(dados, var, peso) {
  ok <- !is.na(dados[[var]]) & !is.na(dados[[peso]])
  y <- dados[[var]][ok]; w <- dados[[peso]][ok]
  sum(w * y) / sum(w) * 100
}

#' Bootstrap com reamostragem dentro de cada quadro: preserva a correlacao entre
#' o braco fixo e o cadastro duplo, que compartilham as mesmas entrevistas.
set.seed(20260803)
B <- N_BOOT
idx_fixo <- which(d23$quadro == "Fixo")
idx_cel  <- which(d23$quadro == "Celular")

boot_um <- function(var) {
  pontual <- c(
    fixo = est_peso(d23[idx_fixo, ], var, "pesorake_fixo"),
    celular = est_peso(d23[idx_cel, ], var, "pesorake_celular"),
    dual = est_peso(d23, var, "pesorake")
  )
  reps <- matrix(NA_real_, B, 3, dimnames = list(NULL, names(pontual)))
  for (b in seq_len(B)) {
    sf <- sample(idx_fixo, length(idx_fixo), replace = TRUE)
    sc <- sample(idx_cel, length(idx_cel), replace = TRUE)
    reps[b, "fixo"]    <- est_peso(d23[sf, ], var, "pesorake_fixo")
    reps[b, "celular"] <- est_peso(d23[sc, ], var, "pesorake_celular")
    reps[b, "dual"]    <- est_peso(d23[c(sf, sc), ], var, "pesorake")
  }
  dif_fc <- reps[, "fixo"] - reps[, "celular"]
  dif_fd <- reps[, "fixo"] - reps[, "dual"]
  tibble::tibble(
    p_fixo = pontual["fixo"], se_fixo = stats::sd(reps[, "fixo"]),
    p_cel  = pontual["celular"], se_cel = stats::sd(reps[, "celular"]),
    p_dual = pontual["dual"], se_dual = stats::sd(reps[, "dual"]),
    d_fixo_cel = pontual["fixo"] - pontual["celular"], se_fc = stats::sd(dif_fc),
    d_fixo_dual = pontual["fixo"] - pontual["dual"], se_fd = stats::sd(dif_fd)
  )
}

res23 <- purrr::pmap_dfr(list(DESFECHOS$var, DESFECHOS$label), function(v, rot) {
  cat("  ", rot, "... "); r <- boot_um(v); cat("ok\n")
  dplyr::mutate(r, var = v, label = rot, .before = 1)
}) |>
  dplyr::mutate(
    fc_low = d_fixo_cel - stats::qnorm(0.975) * se_fc,
    fc_upp = d_fixo_cel + stats::qnorm(0.975) * se_fc,
    fd_low = d_fixo_dual - stats::qnorm(0.975) * se_fd,
    fd_upp = d_fixo_dual + stats::qnorm(0.975) * se_fd
  )

print(as.data.frame(res23 |> dplyr::select(label, p_fixo, p_cel, p_dual,
                                           d_fixo_dual, fd_low, fd_upp)), digits = 3)
write_log(res23, "vigitel2023_tres_desenhos")

# ---- Confronto com a previsao de 2019 --------------------------------------
# A previsao: a diferenca residual entre o quadro fixo e o cadastro duplo deve
# ser maior para os indicadores cujo vies a pos-estratificacao NAO alcancava.

h2("Confronto com a previsao derivada de ", YEAR_VIGITEL)

part <- readRDS(here::here("data", "particao.rds"))
vies <- readRDS(here::here("data", "vies_nao_cobertura.rds")) |>
  dplyr::filter(dominio == "27 capitais")

previsao <- part |>
  dplyr::select(indicator, label, p_vig, verdadeiro = est_p_pns) |>
  dplyr::left_join(vies |> dplyr::select(indicator, sem_pond = est_p_fixo), by = "indicator") |>
  dplyr::mutate(
    pct_removido_2019 = 100 * (1 - abs(p_vig - verdadeiro) / abs(sem_pond - verdadeiro)),
    vies_residual_2019 = p_vig - verdadeiro
  ) |>
  dplyr::select(label, pct_removido_2019, vies_residual_2019)

confronto <- res23 |>
  dplyr::select(label, obs_2023 = d_fixo_dual, fd_low, fd_upp) |>
  dplyr::left_join(previsao, by = "label") |>
  dplyr::arrange(dplyr::desc(abs(obs_2023)))
print(as.data.frame(confronto), digits = 3)
write_log(confronto, "vigitel2023_confronto_previsao")

cat("\nOrdenacao prevista (menor % removido em 2019 = maior residuo esperado):\n  ",
    paste(previsao$label[order(previsao$pct_removido_2019)], collapse = " > "), "\n")
cat("Ordenacao observada em ", ANO_DUAL, " (|fixo - dual|):\n  ",
    paste(confronto$label, collapse = " > "), "\n", sep = "")

rho <- stats::cor(confronto$pct_removido_2019, abs(confronto$obs_2023),
                  method = "spearman")
cat("\nCorrelacao de Spearman entre % removido em 2019 e |diferenca| em ",
    ANO_DUAL, ": ", fmt_num(rho, 2),
    " (n = 4 indicadores; descritiva, nao teste)\n", sep = "")

# ---- Por regiao -------------------------------------------------------------

h2("Diferenca fixo x dual por regiao")

por_regiao <- purrr::pmap_dfr(list(DESFECHOS$var, DESFECHOS$label), function(v, rot) {
  purrr::map_dfr(REGION_LEVELS, function(reg) {
    dr <- d23[d23$region == reg, ]
    tibble::tibble(
      label = rot, region = reg,
      p_fixo = est_peso(dr[dr$quadro == "Fixo", ], v, "pesorake_fixo"),
      p_dual = est_peso(dr, v, "pesorake")
    ) |> dplyr::mutate(dif = p_fixo - p_dual)
  })
}) |>
  dplyr::mutate(region = factor(region, levels = REGION_LEVELS),
                label = factor(label, levels = INDICATORS$label_pt))
print(as.data.frame(por_regiao |> dplyr::select(label, region, dif)), digits = 3)
write_log(por_regiao, "vigitel2023_por_regiao")

# A simulacao previu ganho maior onde a cobertura fixa e menor
sim <- readRDS(here::here("data", "simulacao.rds"))
ganho_previsto <- sim |>
  dplyr::group_by(region) |>
  dplyr::summarise(rmse_S0 = mean(rmse[cenario == "S0"]),
                   rmse_S2 = mean(rmse[cenario == "S2"]), .groups = "drop") |>
  dplyr::mutate(reducao_prevista = rmse_S0 / rmse_S2)

obs_regiao <- por_regiao |>
  dplyr::group_by(region) |>
  dplyr::summarise(dif_media_abs = mean(abs(dif)), .groups = "drop")

comp_reg <- ganho_previsto |>
  dplyr::left_join(obs_regiao, by = "region") |>
  dplyr::arrange(dplyr::desc(reducao_prevista))
print(as.data.frame(comp_reg), digits = 3)
write_log(comp_reg, "vigitel2023_regiao_previsto_observado")

rho_reg <- stats::cor(comp_reg$reducao_prevista, comp_reg$dif_media_abs, method = "spearman")
cat("\nCorrelacao de Spearman entre reducao prevista de REQM e diferenca observada",
    "\npor regiao:", fmt_num(rho_reg, 2), "(n = 5 regioes; descritiva)\n")

saveRDS(list(tres_desenhos = res23, confronto = confronto,
             por_regiao = por_regiao, comp_reg = comp_reg),
        here::here("data", "validacao_2023.rds"))

message("14_validacao_2023.R concluido.")
