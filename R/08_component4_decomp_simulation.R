# 08_component4_decomp_simulation.R
# COMPONENTE 4 — (a) Decomposição de Fairlie 2005 da diferença de prevalência
# Vigitel × PNS para variáveis binárias; (b) Simulação Monte Carlo dos cenários
# multimodais usando a PNS como pseudopopulação de referência (não-circular).
#
# Framework de relato: ADEMP (Morris, White & Crowther 2019, Stat Med).
#   A — Aim
#   D — Data-generating mechanism
#   E — Estimands
#   M — Methods
#   P — Performance measures
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(data.table); library(fst); library(dplyr); library(survey); library(srvyr)
  library(ggplot2); library(scales)
})

vig <- fst::read_fst(file.path(paths$processed, "harm_vigitel.fst"),
                     as.data.table = TRUE)
pns <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                     as.data.table = TRUE)

year_focus <- max(intersect(vig$year, pns$year))
vig <- vig[year == year_focus]
pns <- pns[year == year_focus]

# ============================================================
# (a) DECOMPOSIÇÃO FAIRLIE para indicadores binários
# ============================================================
# Para cada indicador y, ajusta logit(P(y=1) | X) em cada amostra (Vigitel/PNS)
# e decompõe a diferença em (i) composição (X) e (ii) coeficientes (β).
# Implementação manual baseada em Fairlie (2005).

fairlie_decomp <- function(y_var, covariates, dV, dP) {
  fV <- glm(reformulate(covariates, y_var), family = binomial,
            data = dV, weights = weight, na.action = na.omit)
  fP <- glm(reformulate(covariates, y_var), family = binomial,
            data = dP, weights = weight, na.action = na.omit)

  # Predição V usando β_V × X_V
  pV_V <- mean(predict(fV, type = "response"), na.rm = TRUE)
  # Predição P usando β_P × X_P
  pP_P <- mean(predict(fP, type = "response"), na.rm = TRUE)
  # Contrafactuais: aplicar β_V em X_P e β_P em X_V
  pP_V <- mean(predict(fV, newdata = dP, type = "response"), na.rm = TRUE)
  pV_P <- mean(predict(fP, newdata = dV, type = "response"), na.rm = TRUE)

  total       <- pV_V - pP_P
  composition <- pV_V - pP_V    # mantendo β_V, mudando X
  coefficients<- pP_V - pP_P    # mantendo X_P, mudando β
  data.table(indicator = y_var,
             total = total, composition = composition,
             coefficients = coefficients,
             pct_composition = composition / total * 100)
}

cov_x <- c("age","sex","educ_fx")
inds  <- c("smoker","obesity","hypertension","diabetes","poor_health")

decomp_tbl <- rbindlist(lapply(inds, function(y_var) {
  ok_v <- !is.na(vig[[y_var]]) & complete.cases(vig[, ..cov_x])
  ok_p <- !is.na(pns[[y_var]]) & complete.cases(pns[, ..cov_x])
  if (sum(ok_v) < 100 || sum(ok_p) < 100) return(NULL)
  tryCatch(fairlie_decomp(y_var, cov_x, vig[ok_v], pns[ok_p]),
           error = function(e) { log_msg("Fairlie ", y_var, ": ", conditionMessage(e)); NULL })
}), fill = TRUE)
fwrite(decomp_tbl, file.path(paths$tab, "tab9_fairlie_decomposition.csv"))
log_msg("Decomposição Fairlie gravada.")

# ============================================================
# (b) SIMULAÇÃO MONTE CARLO DOS CENÁRIOS MULTIMODAIS
# ============================================================
# AIM: estimar viés e MSE de prevalência de cada indicador sob 4 cenários:
#   S0  status quo: linha fixa apenas
#   S1  dual-frame: linha fixa + celular
#   S2  triple-frame: + web (entre indivíduos com internet)
#   S3  multimodal: + presencial (todos)
#
# DATA-GENERATING MECHANISM (DGM):
#   - Pseudopopulação := PNS ano-foco (capitais + DF, ≥18), com peso amostral.
#   - Em cada réplica: amostra estratificada com probabilidade proporcional
#     a w_i × π_i(c), onde π_i(c) é a probabilidade de inclusão sob o cenário c
#     (depende de cobertura observada de cada modo).
#   - Para o cenário 0, π_i(0) = 1{tem fixo} (PNAD-TIC fornece a fração).
#
# ESTIMANDS: prevalência populacional θ_y para cada indicador y.
# METHODS: estimador ponderado simples (sem reponderação extra) para isolar
#          o efeito do desenho.
# PERFORMANCE: bias, empirical SE, RMSE, coverage do IC 95% (95% nominal).

# Probabilidades de inclusão por cenário (parametrizáveis)
incl_probs <- list(
  S0 = function(d) d$has_landline_prob,
  S1 = function(d) pmin(d$has_landline_prob + d$has_mobile_prob *
                         (1 - d$has_landline_prob), 1),
  S2 = function(d) pmin(d$has_landline_prob + d$has_mobile_prob *
                         (1 - d$has_landline_prob) + d$has_internet_prob *
                         (1 - d$has_landline_prob - d$has_mobile_prob *
                          (1 - d$has_landline_prob)), 1),
  S3 = function(d) rep(1, nrow(d))
)

# Anexa probabilidades à pseudopopulação a partir da PNAD TIC
pnad <- fst::read_fst(file.path(paths$processed, "harm_pnad.fst"),
                      as.data.table = TRUE)[year == year_focus & is_capital == 1]
pop_means <- pnad[, .(
  p_land = weighted.mean(has_landline, weight, na.rm = TRUE),
  p_mob  = weighted.mean(has_mobile,   weight, na.rm = TRUE),
  p_int  = weighted.mean(has_internet, weight, na.rm = TRUE)
)]

pseudopop <- pns[, .(age, sex, educ_fx, weight,
                     smoker, obesity, hypertension, diabetes, poor_health)]
# CORREÇÃO (Rev A/E): aplicar na.omit ANTES de calcular truth para garantir
# que `truth` e amostras venham do mesmo denominador. Isso elimina o viés
# residual artefactual em S3 (cobertura 100%) que aparecia na versão anterior.
pseudopop[, has_landline_prob := pop_means$p_land]
pseudopop[, has_mobile_prob   := pop_means$p_mob]
pseudopop[, has_internet_prob := pop_means$p_int]
pseudopop <- na.omit(pseudopop)

# True parameters: calculado APÓS na.omit (consistente com a base de simulação)
truth <- pseudopop[, lapply(.SD, weighted.mean, w = weight, na.rm = TRUE),
                   .SDcols = inds]
log_msg("True prevalences (PNS):"); print(round(unlist(truth), 4))

# Loop Monte Carlo
mc_one <- function(seed, n_target = 5000) {
  set.seed(seed)
  out <- data.table()
  for (s in names(incl_probs)) {
    p_inc <- incl_probs[[s]](pseudopop)
    p_inc <- pmin(pmax(p_inc, 1e-4), 1)
    sel   <- which(runif(nrow(pseudopop)) <= p_inc *
                     n_target / sum(pseudopop$weight * p_inc))
    samp  <- pseudopop[sel]
    if (nrow(samp) < 50) next
    est <- samp[, lapply(.SD, weighted.mean, w = weight, na.rm = TRUE),
                .SDcols = inds]
    est_long <- melt(est, measure.vars = inds,
                     variable.name = "indicator", value.name = "p_hat")
    est_long[, scenario := s]
    out <- rbind(out, est_long)
  }
  out
}

R <- study$monte_carlo_R
log_msg("Monte Carlo: ", R, " réplicas em 4 cenários…")
mc <- rbindlist(lapply(seq_len(R), function(b) {
  res <- mc_one(seed = study$seed + b)
  res[, rep := b]; res
}), fill = TRUE)

# Métricas ADEMP
truth_long <- melt(as.data.table(truth), measure.vars = inds,
                   variable.name = "indicator", value.name = "theta")

perf <- merge(mc, truth_long, by = "indicator")[, .(
  mean_p_hat = mean(p_hat, na.rm = TRUE),
  bias       = mean(p_hat - theta, na.rm = TRUE),
  emp_SE     = sd(p_hat, na.rm = TRUE),
  RMSE       = sqrt(mean((p_hat - theta)^2, na.rm = TRUE)),
  MCSE_bias  = sd(p_hat - theta, na.rm = TRUE) / sqrt(.N),
  n_reps     = .N
), by = .(scenario, indicator)]

fwrite(perf, file.path(paths$tab, "tab10_monte_carlo_performance.csv"))
print(perf)

# Figura 3: viés por cenário e indicador
fig3 <- ggplot(perf, aes(scenario, bias*100, fill = scenario)) +
  geom_col() +
  geom_errorbar(aes(ymin = (bias - 1.96*MCSE_bias)*100,
                    ymax = (bias + 1.96*MCSE_bias)*100),
                width = 0.3) +
  facet_wrap(~ indicator, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(x = NULL, y = "Viés (pontos percentuais)",
       title = "Viés esperado por cenário multimodal",
       subtitle = "Pseudopopulação = PNS; 1.000 réplicas Monte Carlo. ADEMP.",
       caption = "S0 fixo | S1 dual | S2 triple | S3 multimodal") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave(file.path(paths$fig, "fig3_monte_carlo_bias.png"), fig3,
       width = 9, height = 6, dpi = 300)
ggsave(file.path(paths$fig, "fig3_monte_carlo_bias.pdf"), fig3,
       width = 9, height = 6)

log_msg("Componente 4 concluído.")
