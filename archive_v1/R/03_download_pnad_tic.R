# 03_download_pnad_tic.R
# Baixa o módulo TIC da PNAD Contínua via PNADcIBGE.
# Usado para o Componente 1 — séries temporais de cobertura telefônica
# (fixa, móvel, internet) por região e estrato sociodemográfico,
# restritas ao recorte capitais + DF.
#
# Observação: as variáveis-chave do módulo TIC podem mudar de ano para ano.
# Em 2016-2017 o módulo era anual (4º trimestre); de 2018 em diante,
# integra a PNAD Contínua anual (S01).
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(PNADcIBGE)
  library(data.table)
  library(stringi)
})

# Variáveis TIC mínimas (nomes podem variar entre edições)
# - S01001 (existência de telefone fixo no domicílio em alguns anos)
# - S01005 (existência de internet)
# - S01029 (telefone celular para uso pessoal)
# - V1022 (situação rural/urbana), UF, capital flag
# - VD3004 (escolaridade), V2007 (sexo), V2009 (idade), V2010 (cor)
# - VD5008 (rendimento)

vars_tic <- c(
  "Ano","Trimestre","UF","Capital","RM_RIDE","V1022",
  "V2005","V2007","V2008","V2009","V2010",
  "VD3004","VD3005",
  "VD5008",
  # Bloco TIC (varia entre anos)
  "S01001","S01005","S01016","S01017","S01018","S01029",
  # Pesos de pessoa
  "V1027","V1028"
)

download_pnad_tic <- function(year, quarter = 4, vars = vars_tic) {
  out_rds <- file.path(paths$pnad_dir, sprintf("pnad_tic_%d_T%d.rds", year, quarter))
  if (file.exists(out_rds)) {
    log_msg("PNAD TIC ", year, "T", quarter, " já no cache.")
    return(readRDS(out_rds))
  }
  log_msg("Baixando PNAD Contínua ", year, "T", quarter, "…")
  d <- tryCatch(
    PNADcIBGE::get_pnadc(
      year      = year,
      quarter   = quarter,
      topic     = "5",                  # tópico TIC quando aplicável
      selected  = FALSE,
      vars      = vars,
      labels    = FALSE,
      deflator  = FALSE,
      design    = FALSE,
      savedir   = paths$pnad_dir
    ),
    error = function(e) {
      # Fallback: sem topic (alguns anos a TIC vem na anual)
      log_msg("Tentando sem tópico…")
      PNADcIBGE::get_pnadc(
        year     = year,
        quarter  = quarter,
        selected = FALSE,
        vars     = NULL,                # baixa tudo
        labels   = FALSE,
        deflator = FALSE,
        design   = FALSE,
        savedir  = paths$pnad_dir
      )
    }
  )
  setDT(d)

  # Indicador binário: domicílio com telefone fixo (existência)
  if ("S01001" %in% names(d))      d[, has_landline := as.integer(S01001 == 1)]
  if ("S01029" %in% names(d))      d[, has_mobile   := as.integer(S01029 == 1)]
  if ("S01005" %in% names(d))      d[, has_internet := as.integer(S01005 == 1)]
  if ("Capital" %in% names(d))     d[, is_capital   := as.integer(!is.na(Capital))]

  saveRDS(d, out_rds, compress = "xz")
  log_msg("PNAD TIC ", year, "T", quarter, " salvo (n=", nrow(d), ").")
  d
}

if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  pnad_data <- list()
  for (yr in study$pnad_tic_years) {
    pnad_data[[as.character(yr)]] <- tryCatch(
      download_pnad_tic(yr, quarter = 4),
      error = function(e) { log_msg("Falha ", yr, ": ", conditionMessage(e)); NULL }
    )
  }
}
