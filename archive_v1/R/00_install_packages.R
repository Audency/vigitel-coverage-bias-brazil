# 00_install_packages.R
# Garante que todos os pacotes necessários estão presentes.
# Roda apenas o que falta. Idempotente.

required <- c(
  # IO / dados
  "data.table", "fst", "arrow", "readxl", "janitor",
  # Tidyverse
  "tidyverse", "stringi", "lubridate",
  # Survey
  "survey", "srvyr",
  # Inquéritos IBGE — download direto dos microdados
  "PNSIBGE", "PNADcIBGE",
  # Estatística e ML
  "ranger", "xgboost", "tidymodels", "themis",
  "mice", "boot",
  # Joinpoint manual (segmentação) — usaremos segmented
  "segmented",
  # Visualização
  "ggplot2", "scales", "patchwork", "ggsci", "ggdist",
  # Tabelas
  "gtsummary", "gt", "knitr", "kableExtra",
  # Outros
  "future", "furrr", "progressr", "logger"
)

ip <- rownames(installed.packages())
missing <- setdiff(required, ip)
if (length(missing) > 0) {
  message("Instalando: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org/", Ncpus = 4)
} else {
  message("Todos os pacotes já estão instalados.")
}
