# 01_load_vigitel.R
# Descompacta e carrega o Vigitel 2006-2024 (1 GB CSV) usando data.table::fread,
# faz limpeza mínima e salva em .fst para releitura rápida (~5–10× mais rápido).
#
# Variáveis-chave:
#   ano, cidade, q6 (idade), q7 (sexo), pesorake2025, regiao,
#   indicadores: fumante, excpeso_i, obesid_i, hart, diab, alcabu,
#                ativo_livre, saruim
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(data.table)
  library(fst)
  library(janitor)
  library(stringi)
})

# 1) Descompactar somente se ainda não foi feito
if (!file.exists(paths$vigitel_csv)) {
  log_msg("Descompactando ", basename(paths$vigitel_zip))
  unzip(paths$vigitel_zip, exdir = dirname(paths$vigitel_csv))
}

# 2) Ler com data.table (rapido, controlado, baixo consumo de RAM)
vars_keep <- c(
  "ano", "cidade", "q6", "q7", "pesorake2025", "regiao", "fxesc",
  "fumante", "exfuma", "mais20",
  "imc_i", "excpeso_i", "obesid_i",
  "hart", "diab", "dislip", "coracao", "osteo",
  "alcabu",
  "ativo_livre", "atilaz", "inativo",
  "saruim",
  "telefone",      # tipo de linha quando registrado
  "moradores", "morador"
)

log_msg("Lendo Vigitel CSV (≈1 GB) — pode levar 30–90 s")
vig <- fread(
  paths$vigitel_csv,
  select   = vars_keep,
  encoding = "Latin-1",
  showProgress = TRUE
)

# 3) Limpeza mínima
vig <- janitor::clean_names(vig)
setnames(vig, c("q6","q7","pesorake2025","fxesc"), c("idade","sexo","peso","educ_fx"))
vig[, cidade := stri_trans_general(cidade, "Latin-ASCII") |> tolower() |> trimws()]
vig <- vig[idade >= study$age_min]
vig <- vig[!is.na(peso) & peso > 0]

# Região canônica via lookup capital → região (oficial)
setnames(vig, "regiao", "region_raw", skip_absent = TRUE)
rl <- as.data.table(region_lookup)
setnames(rl, c("capital","region"), c("cidade","region_canon"))
vig <- merge(vig, rl, by = "cidade", all.x = TRUE, sort = FALSE)
vig[, region := as.character(region_canon)]
vig[, c("region_raw","region_canon") := NULL]

# 4) Sumario rapido
log_msg("Tamanho: ", format(nrow(vig), big.mark="."), " observações; ",
        ncol(vig), " variáveis.")
print(vig[, .N, by = ano][order(ano)])
print(vig[, .N, by = region])

# 5) Salvar em fst (compressão razoável + leitura rápida)
log_msg("Salvando ", paths$vigitel_fst)
fst::write_fst(vig, paths$vigitel_fst, compress = 75)
log_msg("Pronto.")
