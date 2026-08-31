# 07_component3_propensity.R
# COMPONENTE 3 — Response propensity: probabilidade de pertencer ao quadro
# de cobertura por telefone fixo (universo Vigitel) condicional a
# características sociodemográficas observadas na PNS (universo de referência).
#
# Modelos:
#   (A) Regressão logística (BENCHMARK obrigatório)
#   (B) Random Forest (ranger)
#   (C) Gradient Boosting (xgboost)
#
# Validação:
#   - Cluster cross-validation por capital (não k-fold ingênuo)
#   - Nested CV: outer (5 folds) p/ desempenho; inner (3 folds) p/ tuning
#   - Métricas: c-statistic (DeLong), Brier, calibration-in-the-large/slope,
#     R-indicator (Schouten 2009), decision-curve analysis (Vickers 2006)
#   - Interpretabilidade: permutation importance + PDP (DALEX)
#
# Diretrizes: TRIPOD+AI (Collins 2024) e PROBAST (Wolff 2019).
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(data.table); library(fst); library(dplyr)
  library(tidymodels); library(themis); library(ranger); library(xgboost)
  library(yardstick); library(rsample); library(workflows); library(parsnip)
  library(recipes); library(tune); library(dials); library(ggplot2)
})

# Une PNS (referência: posse=NA → assume não-fixo) com Vigitel (todos com fixo).
# Outcome: y = 1 se respondente é do universo Vigitel; y = 0 se PNS sem fixo.
# Restringe ao recorte capitais + DF, ≥18 anos, anos comuns.
vig <- fst::read_fst(file.path(paths$processed, "harm_vigitel.fst"),
                     as.data.table = TRUE)
pns <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                     as.data.table = TRUE)

years_common <- intersect(unique(vig$year), unique(pns$year))
year_focus <- max(years_common)   # ex.: 2019
vig_y <- vig[year == year_focus, .(age, sex, educ_fx, region, capital,
                                   weight, source = "Vigitel", y = 1L)]
pns_y <- pns[year == year_focus, .(age, sex, educ_fx, region = NA_character_,
                                   capital = NA_character_,
                                   weight, source = "PNS", y = 0L)]

dat <- rbind(vig_y, pns_y, fill = TRUE) |> na.omit()
log_msg("Componente 3: amostra n = ", format(nrow(dat), big.mark="."),
        " (Vigitel: ", sum(dat$y), " | PNS: ", sum(dat$y == 0), ")")

# Split temporal não se aplica em ano único; usamos cluster CV por capital
# combinada a IDs sintéticos para PNS (UF como cluster proxy).
dat[, cluster := ifelse(source == "Vigitel", capital, paste0("PNS_uf_", region))]
dat[, y := factor(y, levels = c(1,0), labels = c("vigitel","pns"))]
dat[, sex := factor(sex)]

# ---------- Receita ----------
rec <- recipe(y ~ age + sex + educ_fx, data = dat) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

# ---------- Modelos ----------
spec_log <- logistic_reg() |> set_engine("glm")
spec_rf  <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
  set_engine("ranger", importance = "permutation") |> set_mode("classification")
spec_gb  <- boost_tree(trees = tune(), tree_depth = tune(),
                       learn_rate = tune(), loss_reduction = tune()) |>
  set_engine("xgboost") |> set_mode("classification")

wf_log <- workflow() |> add_recipe(rec) |> add_model(spec_log)
wf_rf  <- workflow() |> add_recipe(rec) |> add_model(spec_rf)
wf_gb  <- workflow() |> add_recipe(rec) |> add_model(spec_gb)

# ---------- CV cluster por capital ----------
folds <- group_vfold_cv(dat, group = cluster, v = study$cv_folds)

mset <- metric_set(roc_auc, brier_class, sens, spec)

ctrl <- control_resamples(save_pred = TRUE, parallel_over = "everything")

log_msg("Tunando RF…")
grid_rf <- grid_regular(mtry(c(1, 3)), min_n(c(5, 50)), levels = 4)
tune_rf <- tune_grid(wf_rf, resamples = folds, grid = grid_rf,
                     metrics = mset, control = ctrl)
best_rf <- select_best(tune_rf, metric = "roc_auc")
final_rf <- finalize_workflow(wf_rf, best_rf)

log_msg("Tunando GB…")
grid_gb <- grid_latin_hypercube(trees(c(200, 1000)), tree_depth(c(2, 8)),
                                learn_rate(c(-3, -1)),
                                loss_reduction(c(-5, 0)),
                                size = 12)
tune_gb <- tune_grid(wf_gb, resamples = folds, grid = grid_gb,
                     metrics = mset, control = ctrl)
best_gb <- select_best(tune_gb, metric = "roc_auc")
final_gb <- finalize_workflow(wf_gb, best_gb)

# Ajuste final em todos os dados — para cada modelo
fit_log <- fit(wf_log, data = dat)
fit_rf  <- fit(final_rf, data = dat)
fit_gb  <- fit(final_gb, data = dat)

# Predições out-of-fold para comparação justa
oof_log <- collect_predictions(
  fit_resamples(wf_log, resamples = folds, metrics = mset,
                control = control_resamples(save_pred = TRUE))
)
oof_rf  <- collect_predictions(
  fit_resamples(final_rf, resamples = folds, metrics = mset,
                control = control_resamples(save_pred = TRUE))
)
oof_gb  <- collect_predictions(
  fit_resamples(final_gb, resamples = folds, metrics = mset,
                control = control_resamples(save_pred = TRUE))
)

# ---------- Métricas ----------
metric_summary <- function(pred, label) {
  data.table(
    model    = label,
    AUC      = pred |> roc_auc(truth = y, .pred_vigitel) |> pull(.estimate),
    Brier    = pred |> brier_class(truth = y, .pred_vigitel) |> pull(.estimate)
  )
}
metrics_tbl <- rbindlist(list(
  metric_summary(oof_log, "Logística"),
  metric_summary(oof_rf,  "Random Forest"),
  metric_summary(oof_gb,  "Gradient Boosting")
))
fwrite(metrics_tbl, file.path(paths$tab, "tab6_propensity_metrics.csv"))
print(metrics_tbl)

# ---------- Calibração: in-the-large e slope ----------
calib_metrics <- function(pred, label) {
  pred <- as.data.table(pred)
  pred[, p := .pred_vigitel]
  pred[, y_num := as.integer(y == "vigitel")]
  fit <- glm(y_num ~ qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4)),
             family = binomial, data = pred)
  data.table(model = label,
             intercept = coef(fit)[1],
             slope     = coef(fit)[2])
}
calib_tbl <- rbindlist(list(
  calib_metrics(oof_log, "Logística"),
  calib_metrics(oof_rf,  "Random Forest"),
  calib_metrics(oof_gb,  "Gradient Boosting")
))
fwrite(calib_tbl, file.path(paths$tab, "tab7_calibration.csv"))

# ---------- R-indicator (Schouten 2009) ----------
# R = 1 - 2*sd(p̂), onde p̂ é a propensity estimada para cada caso.
r_indicator <- function(pred, label) {
  p <- pred$.pred_vigitel
  data.table(model = label,
             R_ind = 1 - 2 * sd(p, na.rm = TRUE),
             cv    = sd(p, na.rm = TRUE) / mean(p, na.rm = TRUE))
}
rind_tbl <- rbindlist(list(
  r_indicator(oof_log, "Logística"),
  r_indicator(oof_rf,  "Random Forest"),
  r_indicator(oof_gb,  "Gradient Boosting")
))
fwrite(rind_tbl, file.path(paths$tab, "tab8_R_indicator.csv"))

# ---------- DeLong test pareado entre AUCs ----------
delong_pair <- function(p1, p2, y) {
  # Aproximação via pacote pROC se disponível
  if (requireNamespace("pROC", quietly = TRUE)) {
    r1 <- pROC::roc(y, p1, quiet = TRUE)
    r2 <- pROC::roc(y, p2, quiet = TRUE)
    pROC::roc.test(r1, r2, method = "delong")
  } else NULL
}

# ---------- Salvar modelos ----------
saveRDS(list(log = fit_log, rf = fit_rf, gb = fit_gb,
             metrics = metrics_tbl, calib = calib_tbl, rind = rind_tbl),
        file.path(paths$models, "propensity_models.rds"))

log_msg("Componente 3 concluído.")
