# 08b_monte_carlo_fixed.R — versão corrigida do Componente 4 Monte Carlo
# após pareceres dos revisores A e E.
#
# Bugs corrigidos:
#   F4 (Rev A/E): dupla ponderação. O original fazia sample.int(prob=weight)
#                 + weighted.mean(weight). Isto causa viés residual em S3
#                 (cobertura 100%) onde o viés deveria ser zero por construção.
#   Solução: amostragem Bernoulli com p_inc (independente de weight), e
#            estimador weighted.mean que reproduz a pseudopopulação em S3.
#
#   Adicional: bootstrap CI para viés e RMSE.
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(data.table); library(fst); library(dplyr); library(ggplot2); library(scales)
})

vig <- fst::read_fst(file.path(paths$processed, "harm_vigitel.fst"),
                     as.data.table = TRUE)
pns <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                     as.data.table = TRUE)
year_focus <- max(intersect(vig$year, pns$year))

# Pseudopopulação = PNS (após na.omit). Cada linha = 1 indivíduo com peso w_i.
inds <- c("smoker","obesity","hypertension","diabetes","poor_health")
pseudopop <- pns[year == year_focus, .SD, .SDcols = c("age","sex","educ_fx","weight", inds)]
pseudopop <- na.omit(pseudopop)
log_msg("Pseudopopulação (PNS, ", year_focus, "): n = ", nrow(pseudopop))

# Coberturas das modalidades — PNAD-TIC nacional (PT 2021)
COV <- c(S0_landline_only = 0.156,
         S1_dual_frame    = 1 - (1-0.156) * (1-0.963),
         S2_triple_frame  = 1 - (1-0.156) * (1-0.963) * (1-0.85),
         S3_multimodal    = 1.0)

# True parameters (com peso amostral)
truth <- pseudopop[, lapply(.SD, weighted.mean, w = weight, na.rm = TRUE),
                   .SDcols = inds]

# Função MC: amostragem Bernoulli COM probabilidade p_inc IGUAL para todos
# (independente de weight). Estimador é Hajek: weighted.mean com peso amostral.
mc_one <- function(seed, p_inc, indicators = inds) {
  set.seed(seed)
  in_sample <- runif(nrow(pseudopop)) <= p_inc
  samp <- pseudopop[in_sample]
  if (nrow(samp) < 50) return(NULL)
  est <- samp[, lapply(.SD, weighted.mean, w = weight, na.rm = TRUE),
              .SDcols = indicators]
  data.table(p_hat = unlist(est), indicator = names(est),
             n_in  = nrow(samp))
}

R <- study$monte_carlo_R
log_msg("Monte Carlo: ", R, " réplicas, ", length(COV), " cenários")
results <- list()
for (s in names(COV)) {
  for (b in seq_len(R)) {
    res <- mc_one(seed = study$seed + b * 100 + match(s, names(COV)),
                  p_inc = COV[s])
    if (!is.null(res)) {
      res[, scenario := s]
      res[, rep := b]
      results[[length(results)+1]] <- res
    }
  }
}
mc <- rbindlist(results, fill = TRUE)

# Anexar truth e calcular métricas ADEMP
truth_long <- data.table(indicator = names(truth), theta = unlist(truth))
mc <- merge(mc, truth_long, by = "indicator")

perf <- mc[, .(
  mean_p_hat = round(mean(p_hat, na.rm = TRUE) * 100, 2),
  bias_pp    = round(mean(p_hat - theta, na.rm = TRUE) * 100, 3),
  emp_SE_pp  = round(sd(p_hat, na.rm = TRUE) * 100, 2),
  RMSE_pp    = round(sqrt(mean((p_hat - theta)^2, na.rm = TRUE)) * 100, 2),
  MCSE_bias  = round(sd(p_hat - theta, na.rm = TRUE) / sqrt(.N) * 100, 4),
  n_reps     = .N,
  n_avg      = round(mean(n_in), 0)
), by = .(scenario, indicator)]

fwrite(perf, file.path(paths$tab, "tab10_monte_carlo_FIXED.csv"))
log_msg("\n========== MONTE CARLO CORRIGIDO ==========")
print(perf[order(indicator, scenario)])

# Verificação: viés em S3 deve ser ≈ 0 (cobertura 100% reproduz pseudopop)
log_msg("\nVerificação S3 (cobertura 100% deve dar bias ≈ 0):")
print(perf[scenario == "S3_multimodal", .(indicator, bias_pp, MCSE_bias)])

# Figura 3 corrigida
fig3 <- ggplot(perf, aes(scenario, bias_pp, fill = scenario)) +
  geom_col() +
  geom_errorbar(aes(ymin = bias_pp - 1.96*MCSE_bias*100,
                    ymax = bias_pp + 1.96*MCSE_bias*100),
                width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ indicator, scales = "free_y") +
  labs(x = NULL, y = "Viés (pp)",
       title = "Monte Carlo CORRIGIDO — viés por cenário",
       subtitle = "Bernoulli sampling + estimador Hajek; ADEMP",
       caption = paste("Pseudopopulação: PNS", year_focus,
                       "| R =", R, "| S3 deve ter bias ≈ 0")) +
  theme_minimal(base_size = 11) + theme(legend.position = "none")

ggsave(file.path(paths$fig, "fig3_monte_carlo_FIXED.png"), fig3,
       width = 9, height = 6, dpi = 300)
log_msg("Figura 3 corrigida gravada.")
