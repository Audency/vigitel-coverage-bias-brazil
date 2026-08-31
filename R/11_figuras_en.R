# =============================================================================
# 11_figuras_en.R
# O QUE FAZ : Versao em ingles das figuras do manuscrito e do suplemento. Le os
#             mesmos objetos derivados da versao PT; nada e recalculado exceto a
#             distribuicao da Figura S1, que sai do desenho da PNS como na
#             versao PT.
# ENTRADAS  : data/derivado/*.rds
# SAIDAS    : output/figures_en/figure{1,2,3}.{pdf,png}, figureS{1,2,3}.{pdf,png}
#
# Rotulos usam hifen-menos ASCII: o dispositivo PDF nativo, usado quando o cairo
# nao carrega, nao suporta o sinal de menos tipografico (U+2212).
# =============================================================================

source(here::here("R", "00_setup.R"))
source(here::here("R", "labels_en.R"))

h1("11_figuras_en.R")

comp  <- readRDS(here::here("data", "derivado", "comparacoes.rds"))
vies  <- readRDS(here::here("data", "derivado", "vies_ncob.rds"))
part  <- readRDS(here::here("data", "derivado", "particao.rds"))
sim   <- readRDS(here::here("data", "derivado", "simulacao.rds"))
val23 <- readRDS(here::here("data", "derivado", "validacao_2023.rds"))
pns   <- readRDS(here::here("data", "derivado", "pns.rds"))

salva_fig_en <- function(p, nome, larg = 180, alt = 120)
  salva_figura(p, nome, larg, alt, dir = DIR_FIG_EN)

sim_en <- sim |>
  dplyr::mutate(scenario = factor(as.character(cenario), levels = paste0("S", 0:4)),
                label = factor(tr(rotulo, IND_EN), levels = INDICADORES$rotulo_en),
                region_en = factor(tr(region, REGIOES_EN), levels = REG_EN))

# =============================================================================
# FIGURE 1 (manuscript) - difference by indicator and sex
# =============================================================================
# O manuscrito "Artigo versao" traz como Figura 1 a diferenca Vigitel - PNS por
# indicador e sexo, geometria da versao v1 do pipeline (R/_arquivo_v1/09_figures.R).
# Reproduzimos aqui em ingles, com os mesmos numeros de comparacoes.rds, nas
# mesmas dimensoes da imagem que esta no documento (180 x 95 mm).
h2("Figure 1 (manuscript version, by sex)")

ordem_sexo <- comp$comparacoes |>
  dplyr::filter(estrato == "Total") |>
  dplyr::arrange(delta) |>
  dplyr::pull(rotulo) |>
  tr(IND_EN)

f1s <- comp$comparacoes |>
  dplyr::mutate(label = factor(tr(rotulo, IND_EN), levels = ordem_sexo),
                stratum = factor(tr(estrato, SEXO_EN), levels = c("Women", "Men", "Overall")))

PAL_ESTRATO_EN <- c(Overall = unname(PAL["ink"]), Men = unname(PAL["vigitel"]),
                    Women = unname(PAL["rosa"]))

fig1s <- ggplot2::ggplot(f1s, ggplot2::aes(x = delta, y = label, colour = stratum)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = .4, colour = unname(PAL["ink"])) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = delta_low, xmax = delta_upp), height = 0,
                          linewidth = .5, position = ggplot2::position_dodge(width = .55)) +
  ggplot2::geom_point(size = 1.9, position = ggplot2::position_dodge(width = .55)) +
  ggplot2::scale_colour_manual(values = PAL_ESTRATO_EN, name = NULL,
                               breaks = c("Overall", "Men", "Women")) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6),
                              labels = function(x) fmt_num_en(x, 0)) +
  ggplot2::labs(x = "Prevalence difference, percentage points (Vigitel - PNS)", y = NULL) +
  ggplot2::theme(legend.position = "inside", legend.position.inside = c(.88, .16),
                 legend.direction = "vertical",
                 legend.text = ggplot2::element_text(size = 9),
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                                            linewidth = .3, linetype = "dotted"))
salva_fig_en(fig1s, "figure1_sex", 180, 95)

# =============================================================================
# FIGURE 1 - forest plot of the observed difference and its two parts
# =============================================================================
h2("Figure 1")

ordem <- tr(part$particao |> dplyr::arrange(gap_total) |> dplyr::pull(rotulo), IND_EN)

NIVEIS_F1 <- c("Observed difference (Vigitel - PNS)",
               "Non-coverage bias (within the PNS)",
               "Residual")

f1_dados <- dplyr::bind_rows(
  part$particao |> dplyr::transmute(
    label = tr(rotulo, IND_EN), part = NIVEIS_F1[1],
    est = gap_total, low = gap_low, upp = gap_upp),
  part$particao |> dplyr::transmute(
    label = tr(rotulo, IND_EN), part = NIVEIS_F1[2],
    est = componente, low = comp_low, upp = comp_upp),
  part$particao |> dplyr::transmute(
    label = tr(rotulo, IND_EN), part = NIVEIS_F1[3],
    est = residuo, low = residuo_low, upp = residuo_upp)
) |>
  dplyr::mutate(label = factor(label, levels = ordem),
                part = factor(part, levels = NIVEIS_F1))

PAL_PARCELA <- setNames(unname(PAL[c("ink", "pns", "verde")]), NIVEIS_F1)

fig1 <- ggplot2::ggplot(f1_dados, ggplot2::aes(est, label, colour = part)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = .4, colour = unname(PAL["ink"])) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = low, xmax = upp), height = 0,
                          linewidth = .5, position = ggplot2::position_dodge(.6)) +
  ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(.6)) +
  ggplot2::scale_colour_manual(values = PAL_PARCELA, name = NULL) +
  ggplot2::scale_x_continuous(labels = function(x) fmt_num_en(x, 0)) +
  ggplot2::labs(x = "Percentage points", y = NULL) +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 legend.text = ggplot2::element_text(size = 8.5),
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                                            linewidth = .3, linetype = "dotted")) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))
salva_fig_en(fig1, "figure1", 180, 105)

# =============================================================================
# FIGURE 2 - RMSE by scenario, regional panels
# =============================================================================
h2("Figure 2")

fig2 <- ggplot2::ggplot(sim_en, ggplot2::aes(scenario, rmse, colour = label, group = label)) +
  ggplot2::geom_line(linewidth = .5) + ggplot2::geom_point(size = 1.7) +
  ggplot2::facet_wrap(~region_en, nrow = 1) +
  ggplot2::scale_colour_manual(values = PAL_INDICADOR_EN, name = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num_en(x, 1)) +
  ggplot2::labs(x = "Sampling-frame scenario", y = "RMSE, percentage points") +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3))
salva_fig_en(fig2, "figure2", 180, 100)

# =============================================================================
# FIGURE 3 - predicted vs observed, with the identity line
# =============================================================================
h2("Figure 3")

f3 <- val23$confronto |>
  dplyr::mutate(label = factor(tr(rotulo, IND_EN), levels = INDICADORES$rotulo_en))
lim <- range(c(f3$vies_residual_2019, f3$fd_low, f3$fd_upp), na.rm = TRUE)
lim <- lim + c(-1, 1) * diff(lim) * 0.12

fig3 <- ggplot2::ggplot(f3, ggplot2::aes(vies_residual_2019, obs_2023, colour = label)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                       linewidth = .4, colour = unname(PAL["neutro"])) +
  ggplot2::geom_hline(yintercept = 0, linewidth = .3, colour = unname(PAL["grid"])) +
  ggplot2::geom_vline(xintercept = 0, linewidth = .3, colour = unname(PAL["grid"])) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = fd_low, ymax = fd_upp), width = 0, linewidth = .5) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::scale_colour_manual(values = PAL_INDICADOR_EN, name = NULL) +
  ggplot2::scale_x_continuous(labels = function(x) fmt_num_en(x, 0)) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num_en(x, 0)) +
  ggplot2::coord_equal(xlim = lim, ylim = lim) +
  ggplot2::labs(x = paste0("Predicted from ", ANO_VIGITEL, ", percentage points"),
                y = paste0("Observed in the ", ANO_VIG_DUAL, " transition, percentage points")) +
  ggplot2::theme(legend.position = "right", legend.text = ggplot2::element_text(size = 8),
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3))
salva_fig_en(fig3, "figure3", 180, 95)

# =============================================================================
# SUPPLEMENTARY FIGURES
# =============================================================================
h2("Supplementary figures")

# S1 - profile by telephone ownership
des <- pns |> srvyr::as_survey_design(strata = strata, ids = psu, weights = weight, nest = TRUE)
dist_posse <- function(var, painel) {
  s <- rlang::sym(var)
  des |>
    srvyr::filter(!is.na(phone3), !is.na(!!s)) |>
    srvyr::group_by(phone3, !!s) |>
    srvyr::summarise(p = srvyr::survey_mean(vartype = "ci", proportion = TRUE), .groups = "drop") |>
    dplyr::transmute(painel, nivel = factor(as.character(!!s), levels = levels(pns[[var]])),
                     phone3 = factor(tr(as.character(phone3), POSSE_EN), levels = POSSE_TEL_EN),
                     est = p * 100, low = p_low * 100, upp = p_upp * 100)
}
PAINEIS_S1 <- c("Age group, years", "Education, years of schooling")
s1 <- dplyr::bind_rows(dist_posse("age_grp", PAINEIS_S1[1]),
                       dist_posse("education", PAINEIS_S1[2])) |>
  dplyr::mutate(painel = factor(painel, levels = PAINEIS_S1))

figS1 <- ggplot2::ggplot(s1, ggplot2::aes(nivel, est, colour = phone3, group = phone3)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = low, ymax = upp), linewidth = .5,
                          position = ggplot2::position_dodge(.45)) +
  ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(.45)) +
  ggplot2::facet_wrap(~painel, scales = "free_x", nrow = 1) +
  ggplot2::scale_colour_manual(values = PAL_POSSE_EN, name = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num_en(x, 0)) +
  ggplot2::labs(x = NULL, y = "Percentage within the ownership group (%)") +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3))
salva_fig_en(figS1, "figureS1", 180, 90)

# S2 - bias by scenario and region
figS2 <- ggplot2::ggplot(sim_en, ggplot2::aes(scenario, vies, colour = label, group = label)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = .4, colour = unname(PAL["ink"])) +
  ggplot2::geom_line(linewidth = .5) + ggplot2::geom_point(size = 1.7) +
  ggplot2::facet_wrap(~region_en, nrow = 1) +
  ggplot2::scale_colour_manual(values = PAL_INDICADOR_EN, name = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num_en(x, 1)) +
  ggplot2::labs(x = "Sampling-frame scenario", y = "Bias, percentage points") +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3))
salva_fig_en(figS2, "figureS2", 180, 100)

# S3 - partition by region
NIVEIS_S3 <- c("Total gap", "Non-coverage component", "Residual")
s3 <- part$particao_regiao |>
  dplyr::transmute(label = factor(tr(rotulo, IND_EN), levels = INDICADORES$rotulo_en),
                   domain = tr(dominio, REGIOES_EN),
                   `Total gap` = gap_total, `Non-coverage component` = componente,
                   Residual = residuo) |>
  tidyr::pivot_longer(-c(label, domain), names_to = "part", values_to = "value") |>
  dplyr::mutate(part = factor(part, levels = NIVEIS_S3))
PAL_P3 <- setNames(unname(PAL[c("ink", "pns", "verde")]), NIVEIS_S3)

figS3 <- ggplot2::ggplot(s3, ggplot2::aes(value, domain, colour = part)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = .4, colour = unname(PAL["ink"])) +
  ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(.6)) +
  ggplot2::facet_wrap(~label, nrow = 2) +
  ggplot2::scale_colour_manual(values = PAL_P3, name = NULL) +
  ggplot2::scale_x_continuous(labels = function(x) fmt_num_en(x, 0)) +
  ggplot2::scale_y_discrete(limits = rev(REG_EN)) +
  ggplot2::labs(x = "Percentage points (Vigitel - PNS)", y = NULL) +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                                            linewidth = .3, linetype = "dotted"))
salva_fig_en(figS3, "figureS3", 180, 130)

cat("\nfiguras em output/figures_en/:\n"); print(list.files(DIR_FIG_EN))
message("11_figuras_en.R concluido.")
