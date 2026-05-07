# 09_run_all.R
# Orquestrador: executa o pipeline completo na ordem correta.
# Cada etapa é idempotente (checa cache) e pode ser executada isoladamente.
# ============================================================

source(here::here("config.R"))

steps <- c(
  "00_install_packages.R",
  "01_load_vigitel.R",
  "02_download_pns.R",
  "03_download_pnad_tic.R",
  "04_harmonize_indicators.R",
  "05_component1_coverage.R",
  "06_component2_vigitel_pns.R",
  "07_component3_propensity.R",
  "08_component4_decomp_simulation.R"
)

t0 <- Sys.time()
for (s in steps) {
  log_msg("==== Iniciando: ", s, " ====")
  tic <- Sys.time()
  tryCatch(
    source(file.path(PROJ_ROOT, "R", s), echo = FALSE),
    error = function(e) {
      log_msg("ERRO em ", s, ": ", conditionMessage(e))
    }
  )
  log_msg("==== Concluído ", s, " em ",
          round(difftime(Sys.time(), tic, units = "mins"), 2), " min ====")
}
log_msg("PIPELINE TOTAL: ",
        round(difftime(Sys.time(), t0, units = "mins"), 2), " min")
