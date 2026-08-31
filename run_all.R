# =============================================================================
# run_all.R
# O QUE FAZ : Executa o pipeline inteiro do zero, na ordem, parando no primeiro
#             erro. Registra o tempo de cada etapa.
# USO       : Rscript run_all.R            (tudo)
#             Rscript run_all.R 04 05      (apenas estas etapas)
# SAIDAS    : output/logs/run_all.csv
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

ETAPAS <- c("01_download", "02_harmoniza", "03_desenho", "04_descritivas",
            "05_prevalencias", "06_particao", "07_bootstrap", "08_simulacao",
            "09_validacao_2023", "10_tabelas", "11_figuras", "12_suplemento",
            "13_manuscrito")

if (length(args) > 0) {
  ETAPAS <- ETAPAS[substr(ETAPAS, 1, 2) %in% args]
  if (length(ETAPAS) == 0) stop("Nenhuma etapa corresponde a: ", paste(args, collapse = ", "))
}

cat(strrep("=", 78), "\nPIPELINE VIGITEL x PNS\n", strrep("=", 78), "\n", sep = "")
cat("etapas:", paste(ETAPAS, collapse = " -> "), "\n\n")

registro <- list()
t_total <- Sys.time()

for (etapa in ETAPAS) {
  arq <- file.path("R", paste0(etapa, ".R"))
  if (!file.exists(arq)) stop("Script ausente: ", arq, call. = FALSE)
  cat(strrep("-", 78), "\n>>> ", etapa, "\n", sep = "")
  t0 <- Sys.time()
  ok <- tryCatch({ source(arq, local = new.env(), echo = FALSE); TRUE },
                 error = function(e) { message("FALHOU em ", etapa, ": ", conditionMessage(e)); FALSE })
  dt <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  registro[[etapa]] <- data.frame(etapa = etapa, minutos = round(dt, 2),
                                  situacao = ifelse(ok, "ok", "FALHOU"))
  cat("    ", etapa, ifelse(ok, " ok ", " FALHOU "), "(", round(dt, 1), " min)\n", sep = "")
  if (!ok) {
    utils::write.csv(do.call(rbind, registro), "output/logs/run_all.csv", row.names = FALSE)
    stop("Pipeline interrompido em ", etapa, ". Nada foi contornado em silencio.", call. = FALSE)
  }
}

reg <- do.call(rbind, registro)
dir.create("output/logs", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(reg, "output/logs/run_all.csv", row.names = FALSE)

cat("\n", strrep("=", 78), "\n", sep = "")
print(reg, row.names = FALSE)
cat("\ntempo total:", round(as.numeric(difftime(Sys.time(), t_total, units = "mins")), 1), "min\n")
