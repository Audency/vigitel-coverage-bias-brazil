# =============================================================================
# 05_prevalences.R - Tabela 2: prevalencias, diferencas e razoes
#
# CONVENCAO DE SINAL DO ESTUDO, sem excecao em nenhuma tabela ou figura:
#     Delta = Vigitel - PNS
# Delta negativo significa que o Vigitel subestima em relacao a PNS.
#
# Os dois inqueritos sao amostras independentes da mesma populacao-alvo (adultos
# das 27 capitais), entao a variancia da diferenca e a soma das variancias.
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("05_prevalences.R - prevalencias e diferencas")

des_pns <- readRDS(here::here("data", "design_pns.rds"))
des_vig <- readRDS(here::here("data", "design_vigitel.rds"))

# Analise principal na convencao oficial de cada orgao (ver 02 e 03): caso
# gestacional excluido e nao-respondente no denominador como nao-caso. E a mesma
# convencao nos dois inqueritos, o que os torna diretamente comparaveis.
OUTCOMES <- INDICATORS |>
  dplyr::mutate(var = paste0(indicator, "_official"))

# ---- Estimacao --------------------------------------------------------------

#' Prevalencia ponderada (%), erro-padrao e IC95% logit, por estrato
#' @param by NULL para o total, ou "sex"
prev <- function(design, var, survey_label, by = NULL) {
  v <- rlang::sym(var)
  d <- design |> srvyr::filter(!is.na(!!v))
  if (!is.null(by)) d <- d |> srvyr::group_by(!!rlang::sym(by))
  out <- d |>
    srvyr::summarise(
      p = srvyr::survey_mean(!!v, vartype = c("se", "ci"), proportion = TRUE, na.rm = TRUE),
      n = srvyr::unweighted(dplyr::n())
    )
  out |>
    dplyr::mutate(
      estrato = if (is.null(by)) "Total" else as.character(.data[[by]]),
      survey  = survey_label,
      est = p * 100, se = p_se * 100, low = p_low * 100, upp = p_upp * 100
    ) |>
    dplyr::select(estrato, survey, est, se, low, upp, n)
}

#' Todas as combinacoes de indicador x estrato x inquerito
estimativas <- purrr::pmap_dfr(
  list(OUTCOMES$var, OUTCOMES$indicator, OUTCOMES$label_pt),
  function(var, ind, rot) {
    dplyr::bind_rows(
      prev(des_vig, var, "Vigitel"), prev(des_vig, var, "Vigitel", by = "sex"),
      prev(des_pns, var, "PNS"),     prev(des_pns, var, "PNS",     by = "sex")
    ) |>
      dplyr::mutate(indicator = ind, label = rot, .before = 1)
  }
)

h2("Prevalencias ponderadas (%)")
print(as.data.frame(estimativas |> dplyr::select(-n)), digits = 4)

# ---- Diferenca e razao ------------------------------------------------------
# Amostras independentes: Var(Delta) = Var(Vigitel) + Var(PNS).
# Para a razao, delta-method na escala log.

comparacoes <- estimativas |>
  dplyr::select(indicator, label, estrato, survey, est, se) |>
  tidyr::pivot_wider(names_from = survey, values_from = c(est, se)) |>
  dplyr::mutate(
    delta      = est_Vigitel - est_PNS,
    se_delta   = sqrt(se_Vigitel^2 + se_PNS^2),
    delta_low  = delta - stats::qnorm(0.975) * se_delta,
    delta_upp  = delta + stats::qnorm(0.975) * se_delta,
    z          = delta / se_delta,
    p_delta    = 2 * stats::pnorm(-abs(z)),

    rp         = est_Vigitel / est_PNS,
    se_log_rp  = sqrt((se_Vigitel / est_Vigitel)^2 + (se_PNS / est_PNS)^2),
    rp_low     = exp(log(rp) - stats::qnorm(0.975) * se_log_rp),
    rp_upp     = exp(log(rp) + stats::qnorm(0.975) * se_log_rp)
  )

# Holm sobre os quatro testes principais (linhas Total dos quatro indicadores)
principais <- comparacoes |> dplyr::filter(estrato == "Total")
stopifnot(nrow(principais) == nrow(OUTCOMES))
comparacoes <- comparacoes |>
  dplyr::left_join(
    principais |>
      dplyr::transmute(indicator, estrato,
                       p_holm = stats::p.adjust(p_delta, method = "holm")),
    by = c("indicator", "estrato")
  )

h2("Diferencas (Delta = Vigitel - PNS, em pontos percentuais)")
print(as.data.frame(comparacoes |>
  dplyr::select(label, estrato, delta, delta_low, delta_upp, rp, p_delta, p_holm)), digits = 3)

# ---- Interacao sexo x inquerito ---------------------------------------------
# Modelo sobre os dados empilhados, com o desenho de cada inquerito preservado:
# a PNS entra com seus estratos e UPAs; cada entrevista do Vigitel entra como
# UPA propria, num estrato proprio - que e exatamente o desenho declarado pelo
# Ministerio (svyset [pweight=...], sem conglomerado).

h2("Teste de interacao sexo x inquerito")

empilhar <- function(var) {
  v <- rlang::sym(var)
  a <- des_pns$variables |>
    dplyr::transmute(y = !!v, sex, survey = "PNS", weight,
                     strata = paste0("PNS_", strata), psu = paste0("PNS_", psu))
  b <- des_vig$variables |>
    dplyr::transmute(y = !!v, sex, survey = "Vigitel", weight,
                     strata = "VIG", psu = paste0("VIG_", dplyr::row_number()))
  dplyr::bind_rows(a, b) |>
    dplyr::filter(!is.na(y), !is.na(sex)) |>
    dplyr::mutate(survey = factor(survey, levels = c("PNS", "Vigitel")))
}

interacoes <- purrr::pmap_dfr(
  list(OUTCOMES$var, OUTCOMES$indicator, OUTCOMES$label_pt),
  function(var, ind, rot) {
    dat <- empilhar(var)
    des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~weight,
                             data = dat, nest = TRUE)
    fit <- survey::svyglm(y ~ survey * sex, design = des, family = stats::quasibinomial())
    co  <- summary(fit)$coefficients
    linha <- grep("^surveyVigitel:sex", rownames(co))
    tibble::tibble(
      indicator = ind, label = rot,
      termo = rownames(co)[linha],
      beta  = co[linha, 1],
      p_int = co[linha, 4]
    )
  }
)
interacoes <- interacoes |>
  dplyr::mutate(p_int_holm = stats::p.adjust(p_int, method = "holm"))
print(as.data.frame(interacoes), digits = 3)
write_log(interacoes, "interacao_sexo_inquerito")

# ---- Sensibilidade: gap padronizado por idade -------------------------------
# A Tabela 1 mostrou que o Vigitel se afasta 5,1 pp do Censo 2022 na estrutura
# etaria, contra 1,5 pp da PNS. Padronizamos os dois inqueritos a MESMA
# distribuicao etaria (a do Censo, populacao das 27 capitais) para separar
# quanto do gap e composicao de idade. Padronizacao direta; variancia pela soma
# ponderada das variancias dentro de cada faixa.

h2("Sensibilidade: gap padronizado por idade (padrao = Censo ", YEAR_CENSO, ")")

pesos_padrao <- readr::read_csv(here::here("logs", "estrutura_etaria_vs_censo.csv"),
                                show_col_types = FALSE) |>
  dplyr::transmute(age_grp = as.character(age_grp), w = censo / 100)
stopifnot(abs(sum(pesos_padrao$w) - 1) < 1e-8)

prev_padronizada <- function(design, var, survey_label) {
  v <- rlang::sym(var)
  por_faixa <- design |>
    srvyr::filter(!is.na(!!v), !is.na(age_grp)) |>
    srvyr::group_by(age_grp) |>
    srvyr::summarise(p = srvyr::survey_mean(!!v, vartype = "se", na.rm = TRUE)) |>
    dplyr::mutate(age_grp = as.character(age_grp)) |>
    dplyr::left_join(pesos_padrao, by = "age_grp")
  tibble::tibble(
    survey = survey_label,
    est = sum(por_faixa$w * por_faixa$p) * 100,
    se  = sqrt(sum((por_faixa$w * por_faixa$p_se)^2)) * 100
  )
}

padronizado <- purrr::pmap_dfr(
  list(OUTCOMES$var, OUTCOMES$indicator, OUTCOMES$label_pt),
  function(var, ind, rot) {
    a <- prev_padronizada(des_vig, var, "Vigitel")
    b <- prev_padronizada(des_pns, var, "PNS")
    tibble::tibble(
      label = rot,
      vig_padr = a$est, pns_padr = b$est,
      delta_padr = a$est - b$est,
      se_padr = sqrt(a$se^2 + b$se^2)
    ) |>
      dplyr::mutate(
        delta_padr_low = delta_padr - stats::qnorm(0.975) * se_padr,
        delta_padr_upp = delta_padr + stats::qnorm(0.975) * se_padr
      )
  }
)

comp_padr <- comparacoes |>
  dplyr::filter(estrato == "Total") |>
  dplyr::select(label, delta_bruto = delta) |>
  dplyr::left_join(padronizado, by = "label") |>
  dplyr::mutate(mudanca_pp = delta_padr - delta_bruto)
print(as.data.frame(comp_padr |> dplyr::select(label, delta_bruto, delta_padr,
                                               delta_padr_low, delta_padr_upp, mudanca_pp)),
      digits = 3)
write_log(comp_padr, "gap_padronizado_por_idade")

# ---- Tabela 2 ---------------------------------------------------------------

corpo2 <- comparacoes |>
  dplyr::left_join(
    estimativas |>
      dplyr::select(indicator, estrato, survey, low, upp) |>
      tidyr::pivot_wider(names_from = survey, values_from = c(low, upp)),
    by = c("indicator", "estrato")
  ) |>
  dplyr::mutate(
    estrato = factor(estrato, levels = c("Total", "Masculino", "Feminino")),
    label   = factor(label, levels = INDICATORS$label_pt),
    col_vig = fmt_ci(est_Vigitel, low_Vigitel, upp_Vigitel, 1),
    col_pns = fmt_ci(est_PNS, low_PNS, upp_PNS, 1),
    col_dif = fmt_ci(delta, delta_low, delta_upp, 1),
    col_rp  = fmt_ci(rp, rp_low, rp_upp, 2),
    col_p   = dplyr::if_else(estrato == "Total", fmt_p(p_holm), NA_character_)
  ) |>
  dplyr::arrange(label, estrato) |>
  dplyr::select(label, estrato, col_vig, col_pns, col_dif, col_rp, col_p)

gt2 <- corpo2 |>
  gt::gt(groupname_col = "label", rowname_col = "estrato") |>
  gt::cols_label(
    col_vig = gt::md(glue::glue("**Vigitel {YEAR_VIGITEL}**<br>% (IC 95%)")),
    col_pns = gt::md(glue::glue("**PNS {YEAR_PNS}**<br>% (IC 95%)")),
    col_dif = gt::md("**Δ**<br>pp (IC 95%)"),
    col_rp  = gt::md("**RP**<br>(IC 95%)"),
    col_p   = gt::md("**p**<br>(Holm)")
  ) |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = dplyr::starts_with("col_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 2.** Prevalência dos indicadores em adultos (≥18 anos) das 27 capitais, ",
    "Vigitel {YEAR_VIGITEL} e PNS {YEAR_PNS}, com diferença absoluta e razão de prevalências"
  ))) |>
  gt::tab_footnote(
    footnote = gt::md(glue::glue("Δ = Vigitel − PNS, em pontos percentuais. Valor negativo indica prevalência menor no Vigitel.")),
    locations = gt::cells_column_labels(columns = col_dif)
  ) |>
  gt::tab_footnote(
    footnote = "RP = razão de prevalências (Vigitel / PNS). Intervalo de confiança pelo método delta na escala logarítmica.",
    locations = gt::cells_column_labels(columns = col_rp)
  ) |>
  gt::tab_footnote(
    footnote = paste(
      "Teste bilateral de H0: Δ = 0, tratando os dois inquéritos como amostras",
      "independentes, com correção de Holm sobre os quatro testes principais",
      "(linhas Total). As linhas por sexo não entram na correção; a assimetria",
      "entre sexos é avaliada pelo teste de interação, em nota de cada indicador."
    ),
    locations = gt::cells_column_labels(columns = col_p)
  )

# Uma nota por indicador com o p da interacao sexo x inquerito
for (i in seq_len(nrow(interacoes))) {
  gt2 <- gt2 |>
    gt::tab_footnote(
      footnote = glue::glue(
        "Interação sexo × inquérito: p = {fmt_p(interacoes$p_int[i])} ",
        "(p de Holm = {fmt_p(interacoes$p_int_holm[i])}), de modelo logístico sobre os ",
        "dados empilhados com o desenho de cada inquérito preservado."
      ),
      locations = gt::cells_row_groups(groups = interacoes$label[i])
    )
}

gt2 <- gt2 |>
  gt::tab_source_note(gt::md(glue::glue(
    "Fontes: Vigitel {YEAR_VIGITEL} (Ministério da Saúde) e PNS {YEAR_PNS} (IBGE), ",
    "restrita às capitais e ao Distrito Federal. Definições dos indicadores e ",
    "equivalência das perguntas na Tabela S1. Estimativas ponderadas pelo desenho ",
    "de cada inquérito."
  ))) |>
  gt_study_style()

save_table(gt2, "table2")
saveRDS(comparacoes, here::here("output", "tables", "table2_dados.rds"))
saveRDS(estimativas, here::here("data", "prevalencias.rds"))

message("05_prevalences.R concluido.")
