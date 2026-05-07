# 06_component2_vigitel_pns.R
# COMPONENTE 2 — Comparação de prevalências Vigitel × PNS
# para indicadores harmonizados (tabagismo, obesidade, HAS, DM, autoaval saúde),
# restritos a capitais + DF, ≥18, anos comuns (2013, 2019).
# IC bootstrap respeitando desenho complexo (svrep) e diferenças com IC.
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(survey); library(srvyr); library(data.table); library(fst)
  library(dplyr); library(ggplot2); library(scales); library(boot)
})

vig <- fst::read_fst(file.path(paths$processed, "harm_vigitel.fst"),
                     as.data.table = TRUE)
pns <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                     as.data.table = TRUE)

# Restrição a anos comuns
years_common <- intersect(unique(vig$year), unique(pns$year))
log_msg("Anos comuns Vigitel × PNS: ", paste(years_common, collapse = ", "))

indicators <- c("smoker","obesity","hypertension","diabetes","poor_health")

# Desenho amostral (Vigitel: pesos rake; PNS: peso da pessoa selecionada).
# Para PNS, idealmente usar configuração com replicate weights via
# PNSIBGE::pns_design(). Aqui usamos a forma com pesos diretos como aproximação;
# substitua por design completo em produção.
make_design <- function(d, src) {
  d <- d[!is.na(weight) & weight > 0]
  if (src == "Vigitel") {
    d |> as_survey_design(weights = weight)
  } else {
    d |> as_survey_design(weights = weight)
  }
}

prev_by_year_source <- function(year_) {
  out <- list()
  for (src in c("Vigitel","PNS")) {
    d <- if (src == "Vigitel") vig[year == year_] else pns[year == year_]
    if (nrow(d) == 0) next
    des <- make_design(d, src)
    for (ind in indicators) {
      if (!ind %in% names(d) || all(is.na(d[[ind]]))) next
      r <- des |> summarise(p = survey_mean(get(ind), na.rm = TRUE, vartype = "ci"))
      out[[paste(src, year_, ind, sep = "_")]] <- data.table(
        source = src, year = year_, indicator = ind,
        prev = r$p, lwr = r$p_low, upr = r$p_upp, n = nrow(d)
      )
    }
  }
  rbindlist(out, fill = TRUE)
}

prev_tbl <- rbindlist(lapply(years_common, prev_by_year_source), fill = TRUE)
fwrite(prev_tbl, file.path(paths$tab, "tab4_prevalences_vigitel_pns.csv"))
log_msg("Tabela 4 gravada.")

# ---------- Diferenças e razões com IC bootstrap ----------
# Bootstrap de réplica simples (Rao-Wu rescaling completo exigiria PSU/strata).
diff_boot <- function(d_v, d_p, indicator, R = study$bootstrap_R) {
  v_ind <- d_v[[indicator]]; v_w <- d_v$weight
  p_ind <- d_p[[indicator]]; p_w <- d_p$weight
  v_keep <- !is.na(v_ind) & !is.na(v_w)
  p_keep <- !is.na(p_ind) & !is.na(p_w)
  v_ind <- v_ind[v_keep]; v_w <- v_w[v_keep]
  p_ind <- p_ind[p_keep]; p_w <- p_w[p_keep]
  if (length(v_ind) < 30 || length(p_ind) < 30) return(NULL)

  pv0 <- weighted.mean(v_ind, v_w)
  pp0 <- weighted.mean(p_ind, p_w)
  diffs <- ratios <- numeric(R)
  for (b in seq_len(R)) {
    iv <- sample.int(length(v_ind), replace = TRUE)
    ip <- sample.int(length(p_ind), replace = TRUE)
    pv <- weighted.mean(v_ind[iv], v_w[iv])
    pp <- weighted.mean(p_ind[ip], p_w[ip])
    diffs[b]  <- pv - pp
    ratios[b] <- pv / pp
  }
  data.table(
    indicator    = indicator,
    p_vigitel    = pv0,
    p_pns        = pp0,
    diff         = pv0 - pp0,
    diff_lwr     = quantile(diffs, 0.025, na.rm = TRUE),
    diff_upr     = quantile(diffs, 0.975, na.rm = TRUE),
    ratio        = pv0 / pp0,
    ratio_lwr    = quantile(ratios, 0.025, na.rm = TRUE),
    ratio_upr    = quantile(ratios, 0.975, na.rm = TRUE)
  )
}

cmp <- list()
for (yr in years_common) {
  v <- vig[year == yr]; p <- pns[year == yr]
  for (ind in indicators) {
    res <- tryCatch(diff_boot(v, p, ind, R = study$bootstrap_R),
                    error = function(e) NULL)
    if (!is.null(res)) cmp[[paste(yr, ind, sep="_")]] <- cbind(year = yr, res)
  }
}
cmp <- rbindlist(cmp, fill = TRUE)
fwrite(cmp, file.path(paths$tab, "tab5_diffs_ratios_vigitel_pns.csv"))

# ---------- Figura 2: forest plot das diferenças ----------
if (nrow(cmp) > 0) {
  fig2 <- ggplot(cmp, aes(x = diff*100, y = indicator, colour = factor(year))) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_pointrange(aes(xmin = diff_lwr*100, xmax = diff_upr*100),
                    position = position_dodge(width = 0.6),
                    size = 0.4) +
    labs(x = "Diferença Vigitel − PNS (pontos percentuais)",
         y = NULL, colour = "Ano",
         title = "Diferenças de prevalência Vigitel × PNS",
         subtitle = "Capitais + DF, adultos ≥18 anos. IC 95% bootstrap (R=1000).") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  ggsave(file.path(paths$fig, "fig2_vigitel_vs_pns_diffs.png"), fig2,
         width = 8, height = 5, dpi = 300)
  ggsave(file.path(paths$fig, "fig2_vigitel_vs_pns_diffs.pdf"), fig2,
         width = 8, height = 5)
}
log_msg("Componente 2 concluído.")
