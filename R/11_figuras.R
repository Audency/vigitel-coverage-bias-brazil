# =============================================================================
# 11_figuras.R
# O QUE FAZ : Monta as figuras do manuscrito e do suplemento a partir dos
#             objetos ja calculados. Padrao unico: grade horizontal apenas, sem
#             titulo dentro da figura (vai na legenda), eixos com unidade,
#             paleta fixada em 00_setup, .pdf vetorial e .png a 300 dpi.
# ENTRADAS  : data/derivado/*.rds
# SAIDAS    : output/figures/figura{1,2,3}.{pdf,png}, figuraS{1,2,3}.{pdf,png}
#
# Rotulos usam hifen-menos ASCII: o dispositivo PDF nativo, usado quando o cairo
# nao carrega, nao suporta o sinal de menos tipografico (U+2212).
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("11_figuras.R")

comp  <- readRDS(here::here("data", "derivado", "comparacoes.rds"))
vies  <- readRDS(here::here("data", "derivado", "vies_ncob.rds"))
part  <- readRDS(here::here("data", "derivado", "particao.rds"))
sim   <- readRDS(here::here("data", "derivado", "simulacao.rds"))
val23 <- readRDS(here::here("data", "derivado", "validacao_2023.rds"))
pns   <- readRDS(here::here("data", "derivado", "pns.rds"))

# =============================================================================
# FIGURA 1 - forest da diferenca observada e do componente de nao cobertura
# =============================================================================
# Junta numa so figura as duas quantidades centrais do artigo, para que o leitor
# veja de imediato que o componente de cobertura nao e uma fracao do gap.
h2("Figura 1")

ordem <- part$particao |> dplyr::arrange(gap_total) |> dplyr::pull(rotulo)

f1_dados <- dplyr::bind_rows(
  part$particao |> dplyr::transmute(
    rotulo, parcela = "Diferença observada (Vigitel - PNS)",
    est = gap_total, low = gap_low, upp = gap_upp),
  part$particao |> dplyr::transmute(
    rotulo, parcela = "Viés de não cobertura (dentro da PNS)",
    est = componente, low = comp_low, upp = comp_upp),
  part$particao |> dplyr::transmute(
    rotulo, parcela = "Resíduo", est = residuo, low = residuo_low, upp = residuo_upp)
) |>
  dplyr::mutate(rotulo = factor(rotulo, levels = ordem),
                parcela = factor(parcela, levels = c("Diferença observada (Vigitel - PNS)",
                                                     "Viés de não cobertura (dentro da PNS)",
                                                     "Resíduo")))

PAL_PARCELA <- setNames(unname(PAL[c("ink", "pns", "verde")]), levels(f1_dados$parcela))

fig1 <- ggplot2::ggplot(f1_dados, ggplot2::aes(est, rotulo, colour = parcela)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = .4, colour = unname(PAL["ink"])) +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = low, xmax = upp), height = 0,
                          linewidth = .5, position = ggplot2::position_dodge(.6)) +
  ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(.6)) +
  ggplot2::scale_colour_manual(values = PAL_PARCELA, name = NULL) +
  ggplot2::labs(x = "Pontos percentuais", y = NULL) +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 legend.text = ggplot2::element_text(size = 8.5),
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                                            linewidth = .3, linetype = "dotted")) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))
salva_figura(fig1, "figura1", 180, 105)

# =============================================================================
# FIGURA 2 - REQM por cenario, painel regional
# =============================================================================
h2("Figura 2")

fig2 <- ggplot2::ggplot(
  sim |> dplyr::mutate(cenario = factor(cenario, levels = paste0("S", 0:4)),
                       rotulo = factor(rotulo, levels = INDICADORES$rotulo)),
  ggplot2::aes(cenario, rmse, colour = rotulo, group = rotulo)) +
  ggplot2::geom_line(linewidth = .5) + ggplot2::geom_point(size = 1.7) +
  ggplot2::facet_wrap(~region, nrow = 1) +
  ggplot2::scale_colour_manual(values = PAL_INDICADOR, name = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num(x, 1)) +
  ggplot2::labs(x = "Cenário de quadro amostral", y = "REQM, pontos percentuais") +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3))
salva_figura(fig2, "figura2", 180, 100)

# =============================================================================
# FIGURA 3 - previsto x observado, com linha de identidade
# =============================================================================
h2("Figura 3")

f3 <- val23$confronto |> dplyr::mutate(rotulo = factor(rotulo, levels = INDICADORES$rotulo))
lim <- range(c(f3$vies_residual_2019, f3$fd_low, f3$fd_upp), na.rm = TRUE)
lim <- lim + c(-1, 1) * diff(lim) * 0.12

fig3 <- ggplot2::ggplot(f3, ggplot2::aes(vies_residual_2019, obs_2023, colour = rotulo)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                       linewidth = .4, colour = unname(PAL["neutro"])) +
  ggplot2::geom_hline(yintercept = 0, linewidth = .3, colour = unname(PAL["grid"])) +
  ggplot2::geom_vline(xintercept = 0, linewidth = .3, colour = unname(PAL["grid"])) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = fd_low, ymax = fd_upp), width = 0, linewidth = .5) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::scale_colour_manual(values = PAL_INDICADOR, name = NULL) +
  ggplot2::coord_equal(xlim = lim, ylim = lim) +
  ggplot2::labs(x = paste0("Previsto a partir de ", ANO_VIGITEL, ", pontos percentuais"),
                y = paste0("Observado na transição de ", ANO_VIG_DUAL, ", pontos percentuais")) +
  ggplot2::theme(legend.position = "right", legend.text = ggplot2::element_text(size = 8),
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3))
salva_figura(fig3, "figura3", 180, 95)

# =============================================================================
# FIGURAS SUPLEMENTARES
# =============================================================================
h2("Figuras suplementares")

# S1 - perfil por posse de telefone
des <- pns |> srvyr::as_survey_design(strata = strata, ids = psu, weights = weight, nest = TRUE)
dist_posse <- function(var, painel) {
  s <- rlang::sym(var)
  des |>
    srvyr::filter(!is.na(phone3), !is.na(!!s)) |>
    srvyr::group_by(phone3, !!s) |>
    srvyr::summarise(p = srvyr::survey_mean(vartype = "ci", proportion = TRUE), .groups = "drop") |>
    dplyr::transmute(painel, nivel = factor(as.character(!!s), levels = levels(pns[[var]])),
                     phone3 = factor(as.character(phone3), levels = POSSE_TEL),
                     est = p * 100, low = p_low * 100, upp = p_upp * 100)
}
s1 <- dplyr::bind_rows(dist_posse("age_grp", "Faixa etária, anos"),
                       dist_posse("education", "Escolaridade, anos de estudo")) |>
  dplyr::mutate(painel = factor(painel, levels = c("Faixa etária, anos", "Escolaridade, anos de estudo")))

figS1 <- ggplot2::ggplot(s1, ggplot2::aes(nivel, est, colour = phone3, group = phone3)) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = low, ymax = upp), linewidth = .5,
                          position = ggplot2::position_dodge(.45)) +
  ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(.45)) +
  ggplot2::facet_wrap(~painel, scales = "free_x", nrow = 1) +
  ggplot2::scale_colour_manual(values = PAL_POSSE, name = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num(x, 0)) +
  ggplot2::labs(x = NULL, y = "Percentual dentro do grupo de posse (%)") +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3))
salva_figura(figS1, "figuraS1", 180, 90)

# S2 - vies por cenario e regiao
figS2 <- ggplot2::ggplot(
  sim |> dplyr::mutate(cenario = factor(cenario, levels = paste0("S", 0:4)),
                       rotulo = factor(rotulo, levels = INDICADORES$rotulo)),
  ggplot2::aes(cenario, vies, colour = rotulo, group = rotulo)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = .4, colour = unname(PAL["ink"])) +
  ggplot2::geom_line(linewidth = .5) + ggplot2::geom_point(size = 1.7) +
  ggplot2::facet_wrap(~region, nrow = 1) +
  ggplot2::scale_colour_manual(values = PAL_INDICADOR, name = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num(x, 1)) +
  ggplot2::labs(x = "Cenário de quadro amostral", y = "Viés, pontos percentuais") +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3))
salva_figura(figS2, "figuraS2", 180, 100)

# S3 - particao por regiao
s3 <- part$particao_regiao |>
  dplyr::select(rotulo, dominio, `Gap total` = gap_total,
                `Componente de não cobertura` = componente, `Resíduo` = residuo) |>
  tidyr::pivot_longer(-c(rotulo, dominio), names_to = "parcela", values_to = "valor") |>
  dplyr::mutate(parcela = factor(parcela, levels = c("Gap total", "Componente de não cobertura", "Resíduo")),
                rotulo = factor(rotulo, levels = INDICADORES$rotulo))
PAL_P3 <- setNames(unname(PAL[c("ink", "pns", "verde")]), levels(s3$parcela))

figS3 <- ggplot2::ggplot(s3, ggplot2::aes(valor, dominio, colour = parcela)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = .4, colour = unname(PAL["ink"])) +
  ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(.6)) +
  ggplot2::facet_wrap(~rotulo, nrow = 2) +
  ggplot2::scale_colour_manual(values = PAL_P3, name = NULL) +
  ggplot2::scale_y_discrete(limits = rev(REGIOES)) +
  ggplot2::labs(x = "Pontos percentuais (Vigitel - PNS)", y = NULL) +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                                            linewidth = .3, linetype = "dotted"))
salva_figura(figS3, "figuraS3", 180, 130)

cat("\nfiguras em output/figures/:\n"); print(list.files(here::here("output", "figures")))
message("11_figuras.R concluido.")
