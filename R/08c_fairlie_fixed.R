# 08c_fairlie_fixed.R — Fairlie 2005 com debug e bootstrap CI
# Corrige tab9 zerada e adiciona IC 95% bootstrap.
# Bug original: glm com weights amostrais grande causa "non-integer #successes"
# warning e os contrafactuais predict() retornam NA quando educ_fx tem nível
# disjunto entre Vigitel e PNS após harmonização.
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(data.table); library(fst); library(dplyr); library(boot)
})

vig <- fst::read_fst(file.path(paths$processed, "harm_vigitel.fst"),
                     as.data.table = TRUE)
pns <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                     as.data.table = TRUE)
year_focus <- max(intersect(vig$year, pns$year))
v <- vig[year == year_focus]
p <- pns[year == year_focus]

# Função Fairlie reescrita com diagnóstico
fairlie_decomp_safe <- function(y_var, dV, dP, covs = c("age","sex","educ_fx")) {
  # Filtrar
  dV <- dV[!is.na(dV[[y_var]]) & complete.cases(dV[, ..covs])]
  dP <- dP[!is.na(dP[[y_var]]) & complete.cases(dP[, ..covs])]
  if (nrow(dV) < 100 || nrow(dP) < 100) {
    return(data.table(indicator = y_var, status = "n_insuf",
                      n_v = nrow(dV), n_p = nrow(dP)))
  }

  # Garantir mesmos níveis em educ_fx (causa #1 dos zeros)
  all_levels <- union(levels(factor(dV$educ_fx)), levels(factor(dP$educ_fx)))
  dV[, educ_fx := factor(as.character(educ_fx), levels = all_levels)]
  dP[, educ_fx := factor(as.character(educ_fx), levels = all_levels)]
  dV[, sex := factor(as.character(sex), levels = c("F","M"))]
  dP[, sex := factor(as.character(sex), levels = c("F","M"))]

  # Normalizar pesos (somar n) — evita warning de non-integer
  dV[, w_norm := weight * .N / sum(weight, na.rm = TRUE)]
  dP[, w_norm := weight * .N / sum(weight, na.rm = TRUE)]

  fV <- suppressWarnings(glm(reformulate(covs, y_var), family = quasibinomial,
                              data = dV, weights = w_norm))
  fP <- suppressWarnings(glm(reformulate(covs, y_var), family = quasibinomial,
                              data = dP, weights = w_norm))

  pVV <- weighted.mean(predict(fV, type = "response"), dV$weight, na.rm = TRUE)
  pPP <- weighted.mean(predict(fP, type = "response"), dP$weight, na.rm = TRUE)
  # Contrafactuais: aplicar coeficientes de uma fonte ao X da outra.
  # newdata precisa ter os mesmos níveis de fator que o modelo aprendeu.
  pPV <- weighted.mean(predict(fV, newdata = dP, type = "response"),
                       dP$weight, na.rm = TRUE)
  pVP <- weighted.mean(predict(fP, newdata = dV, type = "response"),
                       dV$weight, na.rm = TRUE)

  data.table(indicator   = y_var,
             prev_v      = pVV,
             prev_p      = pPP,
             total_diff  = pVV - pPP,
             composition = pVV - pPV,    # mantém β_V, muda X
             coefficients= pPV - pPP,    # mantém X_P, muda β
             pct_comp    = ifelse(abs(pVV - pPP) > 1e-6,
                                  (pVV - pPV) / (pVV - pPP) * 100, NA),
             n_v = nrow(dV), n_p = nrow(dP))
}

# Bootstrap CI
fairlie_boot <- function(y_var, dV, dP, R = 500, covs = c("age","sex","educ_fx")) {
  point <- fairlie_decomp_safe(y_var, dV, dP, covs)
  if ("status" %in% names(point)) return(point)
  set.seed(study$seed)
  comp_b <- coef_b <- numeric(R)
  for (b in seq_len(R)) {
    iv <- sample.int(nrow(dV), replace = TRUE)
    ip <- sample.int(nrow(dP), replace = TRUE)
    res <- tryCatch(
      fairlie_decomp_safe(y_var, dV[iv], dP[ip], covs),
      error = function(e) NULL
    )
    if (!is.null(res) && !"status" %in% names(res)) {
      comp_b[b] <- res$composition
      coef_b[b] <- res$coefficients
    } else { comp_b[b] <- NA; coef_b[b] <- NA }
  }
  point[, comp_lwr := quantile(comp_b, 0.025, na.rm = TRUE)]
  point[, comp_upr := quantile(comp_b, 0.975, na.rm = TRUE)]
  point[, coef_lwr := quantile(coef_b, 0.025, na.rm = TRUE)]
  point[, coef_upr := quantile(coef_b, 0.975, na.rm = TRUE)]
  point
}

# Rodar para cada indicador
log_msg("Fairlie + bootstrap (R=300) para 5 indicadores...")
inds <- c("smoker","obesity","hypertension","diabetes","poor_health")
res_all <- rbindlist(lapply(inds, function(i) {
  log_msg("  ", i, "...")
  fairlie_boot(i, v, p, R = 300)
}), fill = TRUE)

# Formatar para apresentação
fres <- res_all[, .(
  indicator,
  n_v, n_p,
  prev_v_pct       = round(prev_v * 100, 2),
  prev_p_pct       = round(prev_p * 100, 2),
  total_diff_pp    = round(total_diff * 100, 2),
  composition_pp   = round(composition * 100, 2),
  comp_lwr_pp      = round(comp_lwr * 100, 2),
  comp_upr_pp      = round(comp_upr * 100, 2),
  coefficients_pp  = round(coefficients * 100, 2),
  coef_lwr_pp      = round(coef_lwr * 100, 2),
  coef_upr_pp      = round(coef_upr * 100, 2),
  pct_composition  = round(pct_comp, 1)
)]

fwrite(fres, file.path(paths$tab, "tab9_fairlie_FIXED.csv"))
log_msg("\n========== FAIRLIE COM BOOTSTRAP ==========")
print(fres)
log_msg("\nInterpretação:")
log_msg("  composition: parte da diferença explicada por DIFERENÇA de COVARIÁVEIS")
log_msg("  coefficients: parte explicada por DIFERENÇA de RESPOSTA (proxy de viés)")
log_msg("  pct_composition: %da diferença atribuída a composição populacional")
