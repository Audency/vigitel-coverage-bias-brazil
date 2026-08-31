# =============================================================================
# 09_figures.R - Figuras do manuscrito e do suplemento
#
# Padrao unico (secao 11 do protocolo): theme_study() com grade horizontal
# apenas, sem titulo dentro da figura (o titulo vai na legenda do manuscrito),
# eixos rotulados com unidade, paleta fixada em 00_setup.R, exportacao em .pdf
# vetorial e .png a 300 dpi. Rotulos usam hifen-menos ASCII: o dispositivo PDF
# nativo (usado quando o cairo nao carrega) nao suporta o sinal U+2212., largura 180 mm (pagina inteira) ou 90 mm (meia).
#
# Figura 2 so e gerada se a simulacao (08) ja tiver rodado; sem ela, o script
# avisa e segue, em vez de falhar.
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("09_figures.R - figuras")

# =============================================================================
# FIGURA 1 - diferenca Vigitel - PNS por indicador e sexo
# =============================================================================

tab2 <- readRDS(here::here("output", "tables", "table2_dados.rds"))

# Ordenacao pela magnitude da diferenca total, nao alfabetica
ordem <- tab2 |>
  dplyr::filter(estrato == "Total") |>
  dplyr::arrange(delta) |>
  dplyr::pull(label)

dados_f1 <- tab2 |>
  dplyr::mutate(
    label   = factor(label, levels = ordem),
    estrato = factor(estrato, levels = c("Feminino", "Masculino", "Total"))
  )

PAL_ESTRATO <- c(Total = unname(PAL["ink"]),
                 Masculino = unname(PAL["vigitel"]),
                 Feminino = unname(PAL["accent2"]))

fig1 <- ggplot2::ggplot(dados_f1,
                        ggplot2::aes(x = delta, y = label, colour = estrato)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = unname(PAL["ink"])) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = delta_low, xmax = delta_upp),
    height = 0, linewidth = 0.5,
    position = ggplot2::position_dodge(width = 0.55)
  ) +
  ggplot2::geom_point(size = 1.9, position = ggplot2::position_dodge(width = 0.55)) +
  ggplot2::scale_colour_manual(values = PAL_ESTRATO, name = NULL,
                               breaks = c("Total", "Masculino", "Feminino")) +
  ggplot2::scale_x_continuous(
    breaks = scales::pretty_breaks(6),
    labels = function(x) fmt_num(x, 0)
  ) +
  ggplot2::labs(
    x = "Diferença de prevalência, pontos percentuais (Vigitel - PNS)",
    y = NULL
  ) +
  ggplot2::theme(
    legend.position = c(0.88, 0.16),
    legend.direction = "vertical",
    legend.text = ggplot2::element_text(size = 9),
    panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                               linewidth = 0.3, linetype = "dotted")
  )

save_figure(fig1, "figure1", width_mm = 180, height_mm = 95)

# =============================================================================
# FIGURA S1 - perfil por posse de telefone (idade e escolaridade)
# =============================================================================

pns <- readRDS(here::here("data", "pns.rds"))
des <- pns |>
  srvyr::as_survey_design(strata = strata, ids = psu, weights = weight, nest = TRUE)

dist_por_posse <- function(var, rotulo) {
  s <- rlang::sym(var)
  des |>
    srvyr::filter(!is.na(phone3), !is.na(!!s)) |>
    srvyr::group_by(phone3, !!s) |>
    srvyr::summarise(p = srvyr::survey_mean(vartype = "ci", proportion = TRUE),
                     .groups = "drop") |>
    dplyr::transmute(
      painel = rotulo,
      nivel  = factor(as.character(!!s), levels = levels(pns[[var]])),
      phone3 = factor(as.character(phone3), levels = PHONE_LEVELS),
      est = p * 100, low = p_low * 100, upp = p_upp * 100
    )
}

dados_s1 <- dplyr::bind_rows(
  dist_por_posse("age_grp",   "Faixa etária, anos"),
  dist_por_posse("education", "Escolaridade, anos de estudo")
) |>
  dplyr::mutate(painel = factor(painel, levels = c("Faixa etária, anos",
                                                   "Escolaridade, anos de estudo")))

figS1 <- ggplot2::ggplot(dados_s1,
                         ggplot2::aes(x = nivel, y = est, colour = phone3, group = phone3)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = low, ymax = upp),
                          linewidth = 0.5,
                          position = ggplot2::position_dodge(width = 0.45)) +
  ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(width = 0.45)) +
  ggplot2::facet_wrap(~painel, scales = "free_x", nrow = 1) +
  ggplot2::scale_colour_manual(values = PAL_PHONE, name = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num(x, 0)) +
  ggplot2::labs(x = NULL, y = "Percentual dentro do grupo de posse (%)") +
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = 0.3)
  )

save_figure(figS1, "figureS1", width_mm = 180, height_mm = 90)

# =============================================================================
# FIGURA S2 - particao do gap por regiao
# =============================================================================
# Complementa a Tabela 4 mostrando que o componente de nao cobertura e o residuo
# variam entre regioes, e que em varias delas atuam em sentidos opostos.

particao_regiao <- readRDS(here::here("data", "particao_regiao.rds"))

dados_s2 <- particao_regiao |>
  dplyr::select(label, dominio, `Gap total` = gap_total,
                `Componente de não cobertura` = componente, `Resíduo` = residuo) |>
  tidyr::pivot_longer(-c(label, dominio), names_to = "parcela", values_to = "valor") |>
  dplyr::mutate(parcela = factor(parcela, levels = c("Gap total",
                                                     "Componente de não cobertura",
                                                     "Resíduo")))

PAL_PARCELA <- c("Gap total" = unname(PAL["ink"]),
                 "Componente de não cobertura" = unname(PAL["pns"]),
                 "Resíduo" = unname(PAL["accent1"]))

figS2 <- ggplot2::ggplot(dados_s2,
                         ggplot2::aes(x = valor, y = dominio, colour = parcela)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = unname(PAL["ink"])) +
  ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(width = 0.6)) +
  ggplot2::facet_wrap(~label, nrow = 2) +
  ggplot2::scale_colour_manual(values = PAL_PARCELA, name = NULL) +
  ggplot2::scale_y_discrete(limits = rev(REGION_LEVELS)) +
  ggplot2::labs(x = "Pontos percentuais (Vigitel - PNS)", y = NULL) +
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                               linewidth = 0.3, linetype = "dotted")
  )

save_figure(figS2, "figureS2", width_mm = 180, height_mm = 130)

# =============================================================================
# FIGURA 2 - REQM por cenario de quadro amostral (depende do script 08)
# =============================================================================

arq_sim <- here::here("data", "simulacao.rds")

if (!file.exists(arq_sim)) {
  message("Figura 2 nao gerada: resultados da simulacao ausentes (rode 08_simulation.R).")
} else {
  sim <- readRDS(arq_sim)

  fig2 <- ggplot2::ggplot(
    sim |> dplyr::mutate(cenario = factor(cenario, levels = paste0("S", 0:4))),
    ggplot2::aes(x = cenario, y = rmse, colour = label, group = label)
  ) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::facet_wrap(~region, nrow = 1) +
    ggplot2::scale_colour_manual(values = PAL_INDICATOR, name = NULL) +
    ggplot2::scale_y_continuous(labels = function(x) fmt_num(x, 1)) +
    ggplot2::labs(x = "Cenário de quadro amostral",
                  y = "REQM, pontos percentuais") +
    ggplot2::theme(
      legend.position = "top",
      legend.justification = "left",
      panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = 0.3)
    )

  save_figure(fig2, "figure2", width_mm = 180, height_mm = 100)
}

cat("\nFiguras em output/figures/:\n")
print(list.files(here::here("output", "figures")))

message("09_figures.R concluido.")
