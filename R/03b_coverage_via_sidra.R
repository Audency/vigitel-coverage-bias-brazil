# 03b_coverage_via_sidra.R
# Alternativa rápida ao download de microdados PNAD: usa o pacote sidrar
# para acessar tabelas agregadas IBGE/Sidra de cobertura telefônica e
# internet. Útil para Componente 1 (séries temporais regionais).
#
# Tabelas Sidra relevantes (módulo TIC - PNAD Contínua):
#   7269 — proporção de domicílios com telefone fixo (PNAD Contínua TIC)
#   7271 — proporção de domicílios com telefone móvel celular
#   7301 — proporção de domicílios com internet
#
# Sidra é navegado em https://sidra.ibge.gov.br/.
# A vantagem: ~ 100 KB por tabela em vez de 200 MB.
# A limitação: só fornece marginais (não permite análise individual).
# ============================================================

source(here::here("config.R"))

if (!"sidrar" %in% rownames(installed.packages())) {
  install.packages("sidrar", repos = "https://cloud.r-project.org/")
}
suppressPackageStartupMessages({
  library(sidrar); library(data.table); library(dplyr)
})

# Função genérica
get_sidra_safely <- function(table_id, period = "all", territory = "Brazil",
                              classific = NULL) {
  tryCatch({
    df <- get_sidra(x = table_id,
                    period   = period,
                    geo      = territory,
                    classific = classific,
                    format   = 4)
    setDT(df)
    df
  }, error = function(e) {
    log_msg("Sidra ", table_id, ": ", conditionMessage(e))
    NULL
  })
}

# Tentativa: tabela 7269 (telefone fixo, PNAD TIC)
# Por região
cov_landline_region <- get_sidra_safely(
  table_id = 7269,
  period = c("2016", "2017", "2018", "2019", "2021", "2022", "2023"),
  territory = "Region"
)

if (!is.null(cov_landline_region)) {
  fwrite(cov_landline_region,
         file.path(paths$tab, "sidra_landline_by_region.csv"))
  log_msg("Sidra 7269 salva: ", nrow(cov_landline_region), " linhas.")
} else {
  log_msg("Tabela 7269 indisponível. Verifique sidra.ibge.gov.br.")
}

# Tabela 7271 (celular)
cov_mobile_region <- get_sidra_safely(
  table_id = 7271,
  period = c("2016", "2017", "2018", "2019", "2021", "2022", "2023"),
  territory = "Region"
)
if (!is.null(cov_mobile_region)) {
  fwrite(cov_mobile_region,
         file.path(paths$tab, "sidra_mobile_by_region.csv"))
}

# Tabela 7301 (internet)
cov_internet_region <- get_sidra_safely(
  table_id = 7301,
  period = c("2016", "2017", "2018", "2019", "2021", "2022", "2023"),
  territory = "Region"
)
if (!is.null(cov_internet_region)) {
  fwrite(cov_internet_region,
         file.path(paths$tab, "sidra_internet_by_region.csv"))
}

log_msg("Componente 1 alternativo (Sidra) concluído.")
