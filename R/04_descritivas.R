# =============================================================================
# 04_descritivas.R
# O QUE FAZ : Caracterizacao ponderada das duas amostras (idade, sexo,
#             escolaridade, raca/cor, regiao) com diferenca padronizada, e
#             conferencia da estrutura etaria contra o Censo. So calcula; a
#             montagem da Tabela 1 fica em 10_tabelas.R.
# ENTRADAS  : data/derivado/design_pns.rds, design_vigitel.rds,
#             data/raw/censo/censo{ANO}_pop_capitais.rds
# SAIDAS    : data/derivado/descritivas.rds, output/logs/estrutura_etaria.csv
# =============================================================================

source(here::here("R", "00_setup.R"))
dir.create(here::here("data", "derivado"), showWarnings = FALSE, recursive = TRUE)

h1("04_descritivas.R")

des_pns <- readRDS(here::here("data", "derivado", "design_pns.rds"))
des_vig <- readRDS(here::here("data", "derivado", "design_vigitel.rds"))

n_pns <- nrow(des_pns$variables); n_vig <- nrow(des_vig$variables)
cat("PNS capitais: n =", n_pns, "| Vigitel: n =", n_vig, "\n")

# ---- Estimadores ------------------------------------------------------------

dist_categorica <- function(design, var, inquerito) {
  v <- rlang::sym(var)
  design |>
    srvyr::filter(!is.na(!!v)) |>
    srvyr::group_by(!!v) |>
    srvyr::summarise(p = srvyr::survey_mean(vartype = "ci", proportion = TRUE),
                     n = srvyr::unweighted(dplyr::n())) |>
    dplyr::transmute(variavel = var, nivel = as.character(!!v), inquerito,
                     est = p * 100, low = p_low * 100, upp = p_upp * 100, n)
}

media_ponderada <- function(design, var, inquerito) {
  v <- rlang::sym(var)
  m <- design |>
    srvyr::filter(!is.na(!!v)) |>
    srvyr::summarise(est = srvyr::survey_mean(!!v, vartype = "ci", na.rm = TRUE),
                     va = srvyr::survey_var(!!v, na.rm = TRUE),
                     n = srvyr::unweighted(dplyr::n()))
  tibble::tibble(variavel = var, nivel = "Média (DP)", inquerito = inquerito,
                 est = m$est, low = m$est_low, upp = m$est_upp,
                 dp = sqrt(m$va), n = m$n)
}

VARS <- c("sex", "age_grp", "education", "race", "region")

categoricas <- dplyr::bind_rows(
  purrr::map_dfr(VARS, ~ dist_categorica(des_vig, .x, "Vigitel")),
  purrr::map_dfr(VARS, ~ dist_categorica(des_pns, .x, "PNS"))
)
idade <- dplyr::bind_rows(media_ponderada(des_vig, "age", "Vigitel"),
                          media_ponderada(des_pns, "age", "PNS"))

# ---- Diferenca padronizada --------------------------------------------------
smd_cat <- categoricas |>
  dplyr::select(variavel, nivel, inquerito, est) |>
  tidyr::pivot_wider(names_from = inquerito, values_from = est) |>
  dplyr::mutate(smd = smd_prop(Vigitel / 100, PNS / 100)) |>
  dplyr::select(variavel, nivel, smd)

smd_idade <- idade |>
  dplyr::select(variavel, nivel, inquerito, est, dp) |>
  tidyr::pivot_wider(names_from = inquerito, values_from = c(est, dp)) |>
  dplyr::mutate(smd = smd_media(est_Vigitel, est_PNS, dp_Vigitel, dp_PNS)) |>
  dplyr::select(variavel, nivel, smd)

descritivas <- list(
  categoricas = categoricas |> dplyr::left_join(smd_cat, by = c("variavel", "nivel")),
  idade       = idade |> dplyr::left_join(smd_idade, by = c("variavel", "nivel")),
  n = tibble::tibble(inquerito = c("Vigitel", "PNS"), n = c(n_vig, n_pns)),
  sem_resposta = tibble::tibble(
    variavel = VARS,
    vigitel = purrr::map_int(VARS, ~ sum(is.na(des_vig$variables[[.x]]))),
    pns     = purrr::map_int(VARS, ~ sum(is.na(des_pns$variables[[.x]])))
  ) |> dplyr::filter(vigitel > 0 | pns > 0)
)

h2("Distribuicoes ponderadas")
print(as.data.frame(descritivas$categoricas |> dplyr::select(-n)), digits = 3)
print(as.data.frame(descritivas$idade), digits = 3)
cat("\nSem resposta (excluidos do denominador da variavel):\n")
print(as.data.frame(descritivas$sem_resposta))

# ---- Estrutura etaria contra o Censo ---------------------------------------
# A Tabela 1 sinaliza |SMD| >= 0,10 na idade, o que nao deveria ocorrer se a
# calibragem do Vigitel (que inclui faixa etaria) reproduzisse a populacao.
# Confrontamos as duas distribuicoes ponderadas com o Censo para saber qual se
# afasta - o resultado entra na discussao, nao pode ficar so no console.

h2("Estrutura etaria contra o Censo ", ANO_CENSO)

censo <- readRDS(here::here("data", "raw", "censo",
                            glue::glue("censo{ANO_CENSO}_pop_capitais.rds")))
censo_idade <- censo |>
  dplyr::mutate(idade = suppressWarnings(as.numeric(stringr::str_extract(Idade, "^\\d+(?= anos?$)")))) |>
  dplyr::filter(!is.na(idade), idade >= 18) |>
  dplyr::bind_rows(censo |> dplyr::filter(Idade == "100 anos ou mais") |>
                     dplyr::mutate(idade = 100)) |>
  dplyr::mutate(age_grp = cut(idade, c(18, 25, 35, 45, 55, 65, Inf),
                              labels = FAIXAS_IDADE, right = FALSE)) |>
  dplyr::group_by(age_grp) |>
  dplyr::summarise(pop = sum(Valor), .groups = "drop") |>
  dplyr::mutate(censo = 100 * pop / sum(pop)) |>
  dplyr::select(age_grp, censo)

ponderada <- function(design, rotulo) {
  design$variables |>
    dplyr::filter(!is.na(age_grp)) |>
    dplyr::group_by(age_grp) |>
    dplyr::summarise(w = sum(weight), .groups = "drop") |>
    dplyr::mutate(!!rotulo := 100 * w / sum(w)) |>
    dplyr::select(age_grp, dplyr::all_of(rotulo))
}

estrutura <- censo_idade |>
  dplyr::left_join(ponderada(des_vig, "vigitel"), by = "age_grp") |>
  dplyr::left_join(ponderada(des_pns, "pns"), by = "age_grp") |>
  dplyr::mutate(dif_vigitel = vigitel - censo, dif_pns = pns - censo)
print(as.data.frame(estrutura), digits = 3)
cat(glue::glue("\nDesvio absoluto maximo: Vigitel {fmt_num(max(abs(estrutura$dif_vigitel)),1)} pp, ",
               "PNS {fmt_num(max(abs(estrutura$dif_pns)),1)} pp\n"))
grava_log(estrutura, "estrutura_etaria")

descritivas$estrutura_etaria <- estrutura
saveRDS(descritivas, here::here("data", "derivado", "descritivas.rds"))

message("04_descritivas.R concluido.")
