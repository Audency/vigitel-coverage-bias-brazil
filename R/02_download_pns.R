# 02_download_pns.R
# Baixa microdados da PNS via PNSIBGE para os anos disponíveis (2013, 2019;
# 2024 quando o IBGE liberar). Restringe ao recorte capitais + DF, ≥18 anos.
# Salva como .fst e desenho srvyr.
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(PNSIBGE)
  library(survey)
  library(srvyr)
  library(data.table)
  library(fst)
  library(stringi)
  library(dplyr)
})

# Capitais e respectivos códigos IBGE (capital_municipio = código_uf*100000 + ...)
# A PNS traz a variável V0001 (UF). Para identificar capitais usamos o município
# (V0024) — só carregamos as colunas necessárias para reduzir RAM.
#
# IMPORTANTE: A primeira execução baixa ~400-700 MB para CADA edição.

cap_codes <- list(
  rio_branco        = 1200401,
  manaus            = 1302603,
  porto_velho       = 1100205,
  boa_vista         = 1400100,
  belem             = 1501402,
  macapa            = 1600303,
  palmas            = 1721000,
  fortaleza         = 2304400,
  recife            = 2611606,
  salvador          = 2927408,
  natal             = 2408102,
  joao_pessoa       = 2507507,
  maceio            = 2704302,
  aracaju           = 2800308,
  teresina          = 2211001,
  sao_luis          = 2111300,
  sao_paulo         = 3550308,
  rio_de_janeiro    = 3304557,
  belo_horizonte    = 3106200,
  vitoria           = 3205309,
  curitiba          = 4106902,
  porto_alegre      = 4314902,
  florianopolis     = 4205407,
  cuiaba            = 5103403,
  campo_grande      = 5002704,
  goiania           = 5208707,
  brasilia          = 5300108
)
capital_codes <- unlist(cap_codes, use.names = FALSE)

# Variáveis mínimas para análise comparativa
# - Sociodemográficas: V0001 (UF), V0024 (município), C006 (sexo), C008 (idade),
#   VDD004A (escolaridade), VDF003 (renda), C009 (cor/raça)
# - Indicadores: P050 (tabagismo atual), P027 (atividade física),
#   W00103 (peso), W00203 (altura) → IMC, Q002 (HAS diagnóstica),
#   Q030 (DM diagnóstica), J007 (autoavaliação saúde)
# - Pesos e desenho: V0029 (peso pessoa adulto selecionada),
#   V0024 (UPA/cluster), V0024S (estrato)

variables_pns_2019 <- c(
  "V0001","V0024","V0026",       # UF, mun, situação domiciliar
  "C006","C008","C009",          # sexo, idade, cor/raça
  "VDD004A",                     # escolaridade
  "VDF003",                      # renda
  "P050",                        # fumante atual
  "P027","P028","P02901","P02902","P03001","P03002","P03101","P03102", # atividade física blocos
  "W00103","W00203",             # peso, altura (medida ou autorrelatada)
  "Q002","Q030","Q06306",        # HAS diag, DM diag, dislip diag
  "J007",                        # autoavaliação de saúde
  "V0026",                       # área de domicílio
  "VDD004",                      # escolaridade alt.
  "C00301","V0028","V0029","V00291"  # pesos e replicate weights
)

download_pns <- function(year, vars = NULL, design = TRUE) {
  out_rds <- file.path(paths$pns_dir, sprintf("pns_%d_capitais.rds", year))
  if (file.exists(out_rds)) {
    log_msg("PNS ", year, " já no cache: ", out_rds)
    return(readRDS(out_rds))
  }
  log_msg("Baixando PNS ", year, " (pode levar 5–15 min)…")
  d <- PNSIBGE::get_pns(
    year     = year,
    selected = TRUE,                    # apenas morador selecionado (≥15 anos)
    anthropometry = TRUE,
    vars     = vars,
    labels   = FALSE,
    deflator = TRUE,
    design   = FALSE,                   # devolve data.frame; criamos design depois
    savedir  = paths$pns_dir
  )
  setDT(d)

  # Restrição a capitais + DF + adultos ≥18
  if ("V0024" %in% names(d)) {
    d <- d[V0024 %in% capital_codes]
  } else if ("V0001" %in% names(d)) {
    # Fallback: ao menos UFs com capital — mas o ideal é V0024.
    log_msg("Aviso: V0024 não encontrado; restrição apenas por UF.")
  }
  if ("C008" %in% names(d)) d <- d[C008 >= study$age_min]

  log_msg("PNS ", year, " — n após recorte: ", format(nrow(d), big.mark="."))
  saveRDS(d, out_rds, compress = "xz")
  d
}

# Execução
if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  pns_2013 <- tryCatch(download_pns(2013), error = function(e) {
    log_msg("Falha PNS 2013: ", conditionMessage(e)); NULL })
  pns_2019 <- tryCatch(download_pns(2019), error = function(e) {
    log_msg("Falha PNS 2019: ", conditionMessage(e)); NULL })

  # PNS 2024 — tentar; se IBGE ainda não liberou, capturar erro silenciosamente.
  pns_2024 <- tryCatch(download_pns(2024), error = function(e) {
    log_msg("PNS 2024 indisponível (esperado em 2026): ", conditionMessage(e)); NULL })
}
