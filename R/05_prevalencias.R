# =============================================================================
# 05_prevalencias.R
# O QUE FAZ : Prevalencias ponderadas brutas e padronizadas por idade em cada
#             inquerito, total e por sexo; diferenca absoluta e razao de
#             prevalencias com IC95%; teste de interacao sexo x inquerito;
#             correcao de Holm sobre os quatro testes principais.
# ENTRADAS  : data/derivado/design_pns.rds, design_vigitel.rds,
#             output/logs/estrutura_etaria.csv
# SAIDAS    : data/derivado/prevalencias.rds, comparacoes.rds,
#             output/logs/{interacao,gap_padronizado}.csv
#
# CONVENCAO DE SINAL, sem excecao em todo o estudo: Delta = Vigitel - PNS.
# Os dois inqueritos sao amostras independentes da mesma populacao-alvo, entao
# a variancia da diferenca e a soma das variancias.
# =============================================================================

source(here::here("R", "00_setup.R"))
dir.create(here::here("data", "derivado"), showWarnings = FALSE, recursive = TRUE)

h1("05_prevalencias.R")

des_pns <- readRDS(here::here("data", "derivado", "design_pns.rds"))
des_vig <- readRDS(here::here("data", "derivado", "design_vigitel.rds"))

# Convencao oficial de cada orgao (ver 02 e 03): caso gestacional excluido e
# nao-respondente no denominador como nao-caso. E a mesma nos dois inqueritos.
DESFECHOS <- INDICADORES |> dplyr::mutate(var = paste0(indicador, "_official"))

# ---- Prevalencias -----------------------------------------------------------
prev <- function(design, var, inquerito, por = NULL) {
  v <- rlang::sym(var)
  d <- design |> srvyr::filter(!is.na(!!v))
  if (!is.null(por)) d <- d |> srvyr::group_by(!!rlang::sym(por))
  d |>
    srvyr::summarise(p = srvyr::survey_mean(!!v, vartype = c("se", "ci"),
                                            proportion = TRUE, na.rm = TRUE),
                     n = srvyr::unweighted(dplyr::n())) |>
    dplyr::mutate(estrato = if (is.null(por)) "Total" else as.character(.data[[por]]),
                  inquerito = inquerito,
                  est = p * 100, se = p_se * 100, low = p_low * 100, upp = p_upp * 100) |>
    dplyr::select(estrato, inquerito, est, se, low, upp, n)
}

prevalencias <- purrr::pmap_dfr(
  list(DESFECHOS$var, DESFECHOS$indicador, DESFECHOS$rotulo),
  function(var, ind, rot) {
    dplyr::bind_rows(
      prev(des_vig, var, "Vigitel"), prev(des_vig, var, "Vigitel", por = "sex"),
      prev(des_pns, var, "PNS"),     prev(des_pns, var, "PNS",     por = "sex")
    ) |> dplyr::mutate(indicador = ind, rotulo = rot, .before = 1)
  })

h2("Prevalencias ponderadas (%)")
print(as.data.frame(prevalencias |> dplyr::select(-n)), digits = 4)

# ---- Diferenca e razao ------------------------------------------------------
comparacoes <- prevalencias |>
  dplyr::select(indicador, rotulo, estrato, inquerito, est, se) |>
  tidyr::pivot_wider(names_from = inquerito, values_from = c(est, se)) |>
  dplyr::mutate(
    delta = est_Vigitel - est_PNS,
    se_delta = sqrt(se_Vigitel^2 + se_PNS^2),
    delta_low = delta - stats::qnorm(.975) * se_delta,
    delta_upp = delta + stats::qnorm(.975) * se_delta,
    p_delta = 2 * stats::pnorm(-abs(delta / se_delta)),
    rp = est_Vigitel / est_PNS,
    se_log_rp = sqrt((se_Vigitel / est_Vigitel)^2 + (se_PNS / est_PNS)^2),
    rp_low = exp(log(rp) - stats::qnorm(.975) * se_log_rp),
    rp_upp = exp(log(rp) + stats::qnorm(.975) * se_log_rp)
  )

principais <- comparacoes |> dplyr::filter(estrato == "Total")
stopifnot(nrow(principais) == nrow(DESFECHOS))
comparacoes <- comparacoes |>
  dplyr::left_join(principais |>
                     dplyr::transmute(indicador, estrato,
                                      p_holm = stats::p.adjust(p_delta, "holm")),
                   by = c("indicador", "estrato"))

h2("Diferencas (", SINAL, ", em pontos percentuais)")
print(as.data.frame(comparacoes |> dplyr::select(rotulo, estrato, delta, delta_low,
                                                 delta_upp, rp, p_delta, p_holm)), digits = 3)

# ---- Interacao sexo x inquerito --------------------------------------------
# Modelo sobre os dados empilhados com o desenho de cada inquerito preservado:
# a PNS entra com estratos e UPAs; cada entrevista do Vigitel entra como UPA
# propria, num estrato proprio - que e o desenho declarado pelo Ministerio.
h2("Interacao sexo x inquerito")

empilha <- function(var) {
  v <- rlang::sym(var)
  dplyr::bind_rows(
    des_pns$variables |>
      dplyr::transmute(y = !!v, sex, inquerito = "PNS", weight,
                       strata = paste0("PNS_", strata), psu = paste0("PNS_", psu)),
    des_vig$variables |>
      dplyr::transmute(y = !!v, sex, inquerito = "Vigitel", weight,
                       strata = "VIG", psu = paste0("VIG_", dplyr::row_number()))
  ) |>
    dplyr::filter(!is.na(y), !is.na(sex)) |>
    dplyr::mutate(inquerito = factor(inquerito, levels = c("PNS", "Vigitel")))
}

interacao <- purrr::pmap_dfr(
  list(DESFECHOS$var, DESFECHOS$indicador, DESFECHOS$rotulo),
  function(var, ind, rot) {
    dat <- empilha(var)
    des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~weight,
                             data = dat, nest = TRUE)
    fit <- survey::svyglm(y ~ inquerito * sex, design = des, family = stats::quasibinomial())
    co <- summary(fit)$coefficients
    i <- grep("^inqueritoVigitel:sex", rownames(co))
    tibble::tibble(indicador = ind, rotulo = rot, termo = rownames(co)[i],
                   beta = co[i, 1], p_int = co[i, 4])
  }) |>
  dplyr::mutate(p_int_holm = stats::p.adjust(p_int, "holm"))
print(as.data.frame(interacao), digits = 3)
grava_log(interacao, "interacao")

# ---- Prevalencias padronizadas por idade -----------------------------------
# O Vigitel se afasta da estrutura etaria do Censo mais que a PNS (04). A
# padronizacao direta para a MESMA distribuicao separa quanto do gap e
# composicao de idade. Variancia pela soma ponderada das variancias por faixa.
h2("Padronizacao direta por idade (padrao: Censo ", ANO_CENSO, ")")

pesos_padrao <- le_log("estrutura_etaria") |>
  dplyr::transmute(age_grp = as.character(age_grp), w = censo / 100)
stopifnot(abs(sum(pesos_padrao$w) - 1) < 1e-8)

prev_padronizada <- function(design, var, inquerito) {
  v <- rlang::sym(var)
  f <- design |>
    srvyr::filter(!is.na(!!v), !is.na(age_grp)) |>
    srvyr::group_by(age_grp) |>
    srvyr::summarise(p = srvyr::survey_mean(!!v, vartype = "se", na.rm = TRUE)) |>
    dplyr::mutate(age_grp = as.character(age_grp)) |>
    dplyr::left_join(pesos_padrao, by = "age_grp")
  tibble::tibble(inquerito = inquerito,
                 est = sum(f$w * f$p) * 100,
                 se = sqrt(sum((f$w * f$p_se)^2)) * 100)
}

padronizadas <- purrr::pmap_dfr(
  list(DESFECHOS$var, DESFECHOS$indicador, DESFECHOS$rotulo),
  function(var, ind, rot) {
    a <- prev_padronizada(des_vig, var, "Vigitel")
    b <- prev_padronizada(des_pns, var, "PNS")
    tibble::tibble(indicador = ind, rotulo = rot,
                   vig_padr = a$est, se_vig_padr = a$se,
                   pns_padr = b$est, se_pns_padr = b$se,
                   delta_padr = a$est - b$est,
                   se_padr = sqrt(a$se^2 + b$se^2)) |>
      dplyr::mutate(delta_padr_low = delta_padr - stats::qnorm(.975) * se_padr,
                    delta_padr_upp = delta_padr + stats::qnorm(.975) * se_padr)
  }) |>
  dplyr::left_join(principais |> dplyr::select(indicador, delta_bruto = delta), by = "indicador") |>
  dplyr::mutate(mudanca_pp = delta_padr - delta_bruto)

print(as.data.frame(padronizadas |> dplyr::select(rotulo, delta_bruto, delta_padr,
                                                  delta_padr_low, delta_padr_upp, mudanca_pp)),
      digits = 3)
grava_log(padronizadas, "gap_padronizado")

saveRDS(prevalencias, here::here("data", "derivado", "prevalencias.rds"))
saveRDS(list(comparacoes = comparacoes, interacao = interacao,
             padronizadas = padronizadas),
        here::here("data", "derivado", "comparacoes.rds"))

message("05_prevalencias.R concluido.")
