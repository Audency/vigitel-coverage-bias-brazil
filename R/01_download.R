# =============================================================================
# 01_download.R
# O QUE FAZ : Baixa os microdados das fontes oficiais, confere integridade e
#             registra URL, data, tamanho e checksum SHA-256 de cada arquivo.
# ENTRADAS  : internet (ftp.ibge.gov.br, svs.aids.gov.br, servicodados.ibge.gov.br)
# SAIDAS    : data/raw/** (microdados), data/raw/MANIFEST.md,
#             output/logs/manifest.csv
#
# O servidor da SVSA limita banda e derruba streams longos. Baixamos em faixas
# de bytes paralelas, retomando por APPEND dentro de cada faixa: retomada sem
# append remonta bytes desalinhados e o CRC do zip falha.
# =============================================================================

source(here::here("R", "00_setup.R"))
library(PNSIBGE); library(PNADcIBGE); library(sidrar)

h1("01_download.R - microdados e checksums")

RAW <- here::here("data", "raw")
for (d in c("vigitel", "pns", "pnadc", "censo")) dir.create(file.path(RAW, d), recursive = TRUE, showWarnings = FALSE)

manifesto <- list()
registra <- function(fonte, ano, caminho, url) {
  manifesto[[length(manifesto) + 1]] <<- tibble::tibble(
    fonte = fonte, ano = ano, arquivo = basename(caminho),
    caminho = sub(here::here(), ".", caminho, fixed = TRUE),
    bytes = file.size(caminho),
    sha256 = digest::digest(caminho, algo = "sha256", file = TRUE),
    url = url, baixado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  invisible(NULL)
}

# ---- Vigitel: sem API, mas publico -----------------------------------------
PAG_VIG <- "https://svs.aids.gov.br/daent/cgdnt/vigitel/"

tamanho_remoto <- function(url) {
  h <- paste(system2("curl", c("-sIL", "--http1.1", "--max-time", "60", shQuote(url)),
                     stdout = TRUE), collapse = "\n")
  v <- as.numeric(stringr::str_match_all(h, "(?i)content-length: *(\\d+)")[[1]][, 2])
  if (length(v) == 0) NA_real_ else v[length(v)]
}

baixa_faixas <- function(url, destino, n_faixas = 8, max_tent = 200) {
  total <- tamanho_remoto(url)
  if (!is.na(total) && file.exists(destino) && file.size(destino) >= total) {
    message(glue::glue("  [ja existe] {basename(destino)} ({round(total/1024^2,1)} MB)"))
    return(invisible(destino))
  }
  if (is.na(total)) stop("Nao obtive o tamanho remoto de ", url, call. = FALSE)
  message(glue::glue("  [baixando] {basename(destino)} ({round(total/1024^2,1)} MB) em {n_faixas} faixas"))
  tmp <- file.path(dirname(destino), paste0(".", basename(destino), ".partes"))
  dir.create(tmp, showWarnings = FALSE)
  ch <- ceiling(total / n_faixas)
  script <- tempfile(fileext = ".sh")
  writeLines(c(
    "#!/bin/bash", "set -u",
    sprintf('URL=%s; TMP=%s; TOTAL=%d; N=%d; CH=%d', shQuote(url), shQuote(tmp), total, n_faixas, ch),
    'f(){ local i=$1 s=$(( $1*CH )) e; e=$(( s+CH-1 )); [ $e -ge $TOTAL ] && e=$(( TOTAL-1 ));',
    '  local w=$(( e-s+1 )) p="$TMP/p.$i"; touch "$p";',
    sprintf('  for t in $(seq 1 %d); do local h=$(stat -f%%z "$p");', max_tent),
    '    [ "$h" -ge "$w" ] && return 0;',
    '    curl -sS --http1.1 -r "$(( s+h ))-${e}" --max-time 300 "$URL" >> "$p" 2>/dev/null; done; return 1; }',
    'for i in $(seq 0 $((N-1))); do f $i & done; wait',
    'cat "$TMP"/p.* > "$1"'
  ), script)
  system2("bash", c(script, shQuote(destino)))
  unlink(tmp, recursive = TRUE)
  if (file.size(destino) != total) {
    stop("Download incompleto: ", basename(destino), " (", file.size(destino),
         " de ", total, " bytes)", call. = FALSE)
  }
  invisible(destino)
}

h2("Vigitel ", ANO_VIGITEL, " e ", ANO_VIG_DUAL)
for (ano in c(ANO_VIGITEL, ANO_VIG_DUAL)) {
  arq <- glue::glue("vigitel-{ano}-peso-rake.zip")
  url <- paste0(PAG_VIG, arq)
  dest <- file.path(RAW, "vigitel", arq)
  baixa_faixas(url, dest)
  teste <- system2("unzip", c("-t", shQuote(dest)), stdout = TRUE, stderr = TRUE)
  if (!any(grepl("No errors detected", teste))) {
    stop("CRC invalido em ", arq, ". Nao prossiga com arquivo corrompido.", call. = FALSE)
  }
  registra("Vigitel", ano, dest, url)
  system2("unzip", c("-o", "-q", shQuote(dest), "-d", shQuote(file.path(RAW, "vigitel"))))
}

# Documentacao oficial: dicionario, orientacoes de analise e relatorios
for (arq in c("dicionario-vigitel-2006-2024.xlsx", "orientacoes-vigitel-2006-2024.pdf",
              "nota-metodologica-vigitel-2006-2024.pdf",
              glue::glue("vigitel-brasil-{ANO_VIGITEL}.pdf"),
              glue::glue("vigitel-brasil-{ANO_VIG_DUAL}.pdf"))) {
  url <- paste0(PAG_VIG, arq); dest <- file.path(RAW, "vigitel", arq)
  baixa_faixas(url, dest, n_faixas = 4)
  registra("Vigitel (documentacao)", NA, dest, url)
}

# ---- PNS -------------------------------------------------------------------
h2("PNS ", ANO_PNS)
dest_pns <- file.path(RAW, "pns", glue::glue("pns{ANO_PNS}_selecionado.rds"))
if (!file.exists(dest_pns)) {
  d <- PNSIBGE::get_pns(year = ANO_PNS, selected = TRUE, anthropometry = FALSE,
                        vars = NULL, labels = TRUE, deflator = FALSE, design = FALSE,
                        savedir = file.path(RAW, "pns"))
  saveRDS(d, dest_pns)
}
registra("PNS (morador selecionado)", ANO_PNS, dest_pns,
         "https://ftp.ibge.gov.br/PNS/2019/Microdados/ via PNSIBGE::get_pns")

dest_ant <- file.path(RAW, "pns", glue::glue("pns{ANO_PNS}_antropometria.rds"))
if (!file.exists(dest_ant)) {
  d <- PNSIBGE::get_pns(year = ANO_PNS, selected = FALSE, anthropometry = TRUE,
                        vars = NULL, labels = TRUE, deflator = FALSE, design = FALSE,
                        savedir = file.path(RAW, "pns"))
  saveRDS(d, dest_ant)
}
registra("PNS (antropometria)", ANO_PNS, dest_ant,
         "https://ftp.ibge.gov.br/PNS/2019/Microdados/ via PNSIBGE::get_pns")

dic_pns <- list.files(file.path(RAW, "pns"), pattern = "dicionario.*\\.xls$", full.names = TRUE)
if (length(dic_pns) > 0) registra("PNS (dicionario)", ANO_PNS, dic_pns[1],
                                  "https://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/")

# ---- PNAD Continua TIC -----------------------------------------------------
# get_pnadc() le as 1.229 colunas do arquivo de 4 GB antes de filtrar `vars` e
# satura a memoria. Usamos o pacote para baixar e lemos so as colunas da analise,
# com as posicoes do programa de leitura SAS do IBGE.
h2("PNAD Continua TIC ", ANO_PNADC)
dest_tic <- file.path(RAW, "pnadc", glue::glue("pnadc{ANO_PNADC}_tic.rds"))
VARS_TIC <- c("Ano","Trimestre","UF","Capital","RM_RIDE","UPA","Estrato","V1008","V1014",
              "V1016","V1022","V1023","V1027","V1028","V1029","V2001","V2005","V2007",
              "V2009","V2010","VD2006","VD3004","VDI5002","VDI5003","S01021","S01022")
if (!file.exists(dest_tic)) {
  try(PNADcIBGE::get_pnadc(year = ANO_PNADC, topic = 4, vars = "V2007", labels = FALSE,
                           deflator = FALSE, design = FALSE,
                           savedir = file.path(RAW, "pnadc")), silent = TRUE)
  txt <- list.files(file.path(RAW, "pnadc"), pattern = "^PNADC_.*\\.txt$", full.names = TRUE)[1]
  inp <- list.files(file.path(RAW, "pnadc"), pattern = "^input_PNADC.*\\.txt$", full.names = TRUE)[1]
  stopifnot(!is.na(txt), !is.na(inp))
  linhas <- iconv(readLines(inp, warn = FALSE), "latin1", "UTF-8", sub = "")
  m <- stringr::str_match(linhas, "^\\s*@(\\d+)\\s+(\\S+)\\s+\\$?(\\d+)\\.\\s*(?:/\\*\\s*(.*?)\\s*\\*/)?")
  layout <- tibble::tibble(inicio = as.integer(m[,2]), nome = m[,3],
                           largura = as.integer(m[,4]), rotulo = m[,5]) |>
    dplyr::filter(!is.na(inicio))
  saveRDS(layout, file.path(RAW, "pnadc", "layout_pnadc.rds"))
  lay <- layout |> dplyr::filter(nome %in% VARS_TIC) |> dplyr::arrange(inicio)
  d <- readr::read_fwf(txt, readr::fwf_positions(lay$inicio, lay$inicio + lay$largura - 1, lay$nome),
                       col_types = readr::cols(.default = readr::col_character()), progress = FALSE)
  saveRDS(d, dest_tic)
}
registra("PNAD Continua TIC", ANO_PNADC, dest_tic,
         "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/.../Trimestre_4/ via PNADcIBGE")

# ---- Censo / SIDRA ---------------------------------------------------------
# Os 27 codigos sao identificadores administrativos do IBGE, nao resultados; o
# script confere os nomes que o SIDRA devolve.
h2("Censo ", ANO_CENSO, " (SIDRA)")
CAPITAIS <- tibble::tribble(
  ~code_muni, ~capital, ~uf, ~region,
  1200401,"Rio Branco","AC","Norte", 1302603,"Manaus","AM","Norte",
  1600303,"Macapa","AP","Norte", 1501402,"Belem","PA","Norte",
  1100205,"Porto Velho","RO","Norte", 1400100,"Boa Vista","RR","Norte",
  1721000,"Palmas","TO","Norte", 2704302,"Maceio","AL","Nordeste",
  2927408,"Salvador","BA","Nordeste", 2304400,"Fortaleza","CE","Nordeste",
  2111300,"Sao Luis","MA","Nordeste", 2507507,"Joao Pessoa","PB","Nordeste",
  2611606,"Recife","PE","Nordeste", 2211001,"Teresina","PI","Nordeste",
  2408102,"Natal","RN","Nordeste", 2800308,"Aracaju","SE","Nordeste",
  3205309,"Vitoria","ES","Sudeste", 3106200,"Belo Horizonte","MG","Sudeste",
  3304557,"Rio de Janeiro","RJ","Sudeste", 3550308,"Sao Paulo","SP","Sudeste",
  4106902,"Curitiba","PR","Sul", 4314902,"Porto Alegre","RS","Sul",
  4205407,"Florianopolis","SC","Sul", 5300108,"Brasilia","DF","Centro-Oeste",
  5208707,"Goiania","GO","Centro-Oeste", 5103403,"Cuiaba","MT","Centro-Oeste",
  5002704,"Campo Grande","MS","Centro-Oeste")
stopifnot(nrow(CAPITAIS) == 27, !any(duplicated(CAPITAIS$code_muni)))
saveRDS(CAPITAIS, file.path(RAW, "censo", "capitais.rds"))

# sidrar quebra com duas classificacoes sob R 4.5; montamos a consulta no
# formato da API, que o proprio pacote aceita.
api <- paste0("/t/9514/n6/", paste(CAPITAIS$code_muni, collapse = ","),
              "/v/93/p/all/c2/allxt/c287/all")
dest_censo <- file.path(RAW, "censo", glue::glue("censo{ANO_CENSO}_pop_capitais.rds"))
if (!file.exists(dest_censo)) saveRDS(sidrar::get_sidra(api = api), dest_censo)
censo <- readRDS(dest_censo)
devolvidos <- unique(as.character(censo[["Município (Código)"]]))
if (length(setdiff(as.character(CAPITAIS$code_muni), devolvidos)) > 0) {
  stop("SIDRA nao devolveu todas as capitais.", call. = FALSE)
}
registra("Censo (SIDRA 9514)", ANO_CENSO, dest_censo,
         paste0("https://apisidra.ibge.gov.br", api))

# ---- Manifesto -------------------------------------------------------------
h2("Manifesto")
man <- dplyr::bind_rows(manifesto)
print(as.data.frame(man |> dplyr::select(fonte, ano, arquivo, bytes)), right = FALSE)
grava_log(man, "manifest")

linhas_md <- c(
  "# MANIFEST — procedência dos microdados",
  "",
  glue::glue("Gerado por `R/01_download.R` em {format(Sys.time(), '%Y-%m-%d %H:%M')}."),
  "Checksums SHA-256 permitem verificar que a análise usou exatamente estes arquivos.",
  "",
  "| Fonte | Ano | Arquivo | Bytes | SHA-256 | URL | Baixado em |",
  "|---|---|---|---|---|---|---|",
  glue::glue("| {man$fonte} | {ifelse(is.na(man$ano),'—',man$ano)} | `{man$arquivo}` | ",
             "{format(man$bytes, big.mark='.')} | `{substr(man$sha256,1,16)}…` | {man$url} | {man$baixado_em} |")
)
writeLines(linhas_md, file.path(RAW, "MANIFEST.md"))
cat("\ndata/raw/MANIFEST.md gravado com", nrow(man), "arquivos\n")
message("01_download.R concluido.")
