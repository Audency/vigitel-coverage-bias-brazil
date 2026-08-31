# =============================================================================
# 01_download.R - Download dos microdados
#
# Cada bloco checa se o arquivo ja existe antes de baixar. Rodar duas vezes nao
# repete download. Os objetos sao salvos crus em data-raw/, sem nenhuma
# transformacao - toda harmonizacao acontece em 02_harmonize.R.
#
# design = FALSE em todos os get_*: baixamos o data.frame com as variaveis de
# desenho (estrato, UPA, pesos) preservadas e construimos os objetos de desenho
# em 03_design.R com srvyr, como manda a secao 4 do protocolo.
# =============================================================================

source(here::here("R", "00_setup.R"))
library(PNSIBGE)
library(PNADcIBGE)
library(sidrar)

h1("01_download.R - microdados")

download_log <- list()

#' Executa um download apenas se o destino nao existir; registra no log
fetch_once <- function(name, path, expr) {
  if (file.exists(path)) {
    message(glue::glue("[pula] {name}: ja existe em {basename(path)}"))
    obj <- readRDS(path)
  } else {
    message(glue::glue("[baixa] {name} ..."))
    t0  <- Sys.time()
    obj <- force(expr)
    saveRDS(obj, path)
    message(glue::glue("[ok]   {name} em {round(difftime(Sys.time(), t0, units = 'mins'), 1)} min"))
  }
  invisible(obj)
}

#' Linha do resumo de download
log_row <- function(source, year, path, obj = NULL) {
  tibble::tibble(
    source    = source,
    year      = year,
    file      = basename(path),
    size_mb   = round(file.size(path) / 1024^2, 1),
    n_rows    = if (is.null(obj)) NA_integer_ else nrow(obj),
    n_cols    = if (is.null(obj)) NA_integer_ else ncol(obj),
    downloaded = format(file.mtime(path), "%Y-%m-%d %H:%M")
  )
}

# ---- 1. PNS 2019 - morador selecionado --------------------------------------
# selected = TRUE: questionario do morador selecionado, onde estao os quatro
# desfechos (modulos N, P, Q) e o peso de pessoa selecionada exigido pelo
# protocolo. labels = TRUE preserva os rotulos do dicionario, necessarios para
# imprimir os enunciados literais das perguntas em 02_harmonize.R.

h2("PNS ", YEAR_PNS, " - morador selecionado")

path_pns <- here::here("data-raw", "pns", glue::glue("pns{YEAR_PNS}_selected.rds"))
pns_raw <- fetch_once(
  "PNS morador selecionado", path_pns,
  PNSIBGE::get_pns(
    year         = YEAR_PNS,
    selected     = TRUE,
    anthropometry = FALSE,
    vars         = NULL,        # tudo: o dicionario e inspecionado em 02
    labels       = TRUE,
    deflator     = FALSE,       # nenhuma analise monetaria neste estudo
    design       = FALSE,
    savedir      = here::here("data-raw", "pns")
  )
)
download_log$pns <- log_row("PNS (morador selecionado)", YEAR_PNS, path_pns, pns_raw)
cat("dim:", dim(pns_raw), "\n")

# ---- 2. PNS 2019 - subamostra de antropometria ------------------------------
# Peso e altura medidos, para a analise de sensibilidade de obesidade (Tab. S4).
# Subamostra propria, com peso e desenho proprios - nunca misturar com o acima.

h2("PNS ", YEAR_PNS, " - antropometria (sensibilidade obesidade)")

path_anthro <- here::here("data-raw", "pns", glue::glue("pns{YEAR_PNS}_anthropometry.rds"))
pns_anthro_raw <- fetch_once(
  "PNS antropometria", path_anthro,
  PNSIBGE::get_pns(
    year         = YEAR_PNS,
    selected     = FALSE,
    anthropometry = TRUE,
    vars         = NULL,
    labels       = TRUE,
    deflator     = FALSE,
    design       = FALSE,
    savedir      = here::here("data-raw", "pns")
  )
)
download_log$pns_anthro <- log_row("PNS (antropometria)", YEAR_PNS, path_anthro, pns_anthro_raw)
cat("dim:", dim(pns_anthro_raw), "\n")

# ---- 3. PNAD Continua - suplemento TIC --------------------------------------
# topic = 4: microdados anuais por tema do 4o trimestre, onde vem o suplemento de
# Tecnologia da Informacao e Comunicacao. Dele so precisamos de posse de telefone
# fixo e celular por domicilio, regiao e estrato - parametros da simulacao (08).
#
# O arquivo bruto tem ~4 GB e ~470 variaveis. Restringimos a leitura ao que
# entra na analise: sem isso a leitura estoura a memoria e demora dezenas de
# minutos para carregar colunas que nunca serao usadas. As variaveis de desenho
# sao mantidas automaticamente pelo pacote.
#
#   S01022  Este domicilio tem telefone fixo convencional?
#   S01021  Numero de moradores que tem telefone movel celular para uso pessoal
#   V2007 sexo | V2009 idade | V2010 cor ou raca | VD3004 nivel de instrucao
#   VDI5002 rendimento efetivo domiciliar per capita (estrato socioeconomico)
#   V1022 situacao urbana/rural | V1023 tipo de area (1 = Capital)

vars_pnadc <- c(
  "Ano", "Trimestre", "UF", "Capital", "RM_RIDE", "UPA", "Estrato",
  "V1008", "V1014", "V1016", "V1022", "V1023", "V1027", "V1028", "V1029",
  "V2001", "V2005", "V2007", "V2009", "V2010",
  "VD2006", "VD3004", "VDI5002", "VDI5003",
  "S01021", "S01022"
)

h2("PNAD Continua TIC ", YEAR_PNADC_TIC)

pnadc_dir <- here::here("data-raw", "pnadc")

#' Baixa os arquivos da PNADc TIC via PNADcIBGE, sem le-los.
#' get_pnadc() le o arquivo de largura fixa inteiro (~4 GB, ~470 colunas) antes
#' de aplicar o filtro de variaveis, o que satura a memoria desta maquina. Como
#' precisamos de 26 colunas, baixamos com o pacote e lemos so essas colunas.
fetch_pnadc_files <- function(dir) {
  if (length(list.files(dir, pattern = "^PNADC_.*\\.txt$")) > 0 &&
      length(list.files(dir, pattern = "^input_PNADC.*\\.txt$")) > 0) {
    message("[pula] arquivos da PNADc ja baixados")
    return(invisible(TRUE))
  }
  message("[baixa] arquivos da PNADc TIC via PNADcIBGE ...")
  # design = FALSE + vars minimo: o objetivo aqui e apenas o efeito colateral de
  # popular savedir com os microdados e o input de leitura.
  try(PNADcIBGE::get_pnadc(year = YEAR_PNADC_TIC, topic = 4, vars = "V2007",
                           labels = FALSE, deflator = FALSE, design = FALSE,
                           savedir = dir), silent = TRUE)
  invisible(TRUE)
}

#' Le o programa de leitura SAS do IBGE e devolve nome, posicao inicial e
#' largura de cada variavel. Os enunciados vem no proprio arquivo, entre /* */.
parse_sas_input <- function(path) {
  linhas <- readLines(path, warn = FALSE, encoding = "latin1")
  linhas <- iconv(linhas, from = "latin1", to = "UTF-8", sub = "")
  m <- stringr::str_match(linhas, "^\\s*@(\\d+)\\s+(\\S+)\\s+\\$?(\\d+)\\.\\s*(?:/\\*\\s*(.*?)\\s*\\*/)?")
  tibble::tibble(
    start = as.integer(m[, 2]),
    name  = m[, 3],
    width = as.integer(m[, 4]),
    label = m[, 5]
  ) |> dplyr::filter(!is.na(start))
}

#' Le apenas as colunas pedidas de um arquivo de largura fixa do IBGE
read_fwf_subset <- function(txt, layout, vars) {
  faltando <- setdiff(vars, layout$name)
  if (length(faltando) > 0) stop("Variaveis fora do layout: ", paste(faltando, collapse = ", "))
  lay <- layout |> dplyr::filter(name %in% vars) |> dplyr::arrange(start)
  readr::read_fwf(
    txt,
    col_positions = readr::fwf_positions(
      start = lay$start, end = lay$start + lay$width - 1, col_names = lay$name
    ),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

path_tic <- here::here("data-raw", "pnadc", glue::glue("pnadc{YEAR_PNADC_TIC}_tic.rds"))

if (file.exists(path_tic)) {
  message("[pula] PNADc TIC: ja existe em ", basename(path_tic))
  pnadc_raw <- readRDS(path_tic)
} else {
  fetch_pnadc_files(pnadc_dir)
  txt_pnadc   <- list.files(pnadc_dir, pattern = "^PNADC_.*\\.txt$", full.names = TRUE)[1]
  input_pnadc <- list.files(pnadc_dir, pattern = "^input_PNADC.*\\.txt$", full.names = TRUE)[1]
  stopifnot(!is.na(txt_pnadc), !is.na(input_pnadc))

  layout_pnadc <- parse_sas_input(input_pnadc)
  cat("layout da PNADc:", nrow(layout_pnadc), "variaveis;",
      "lendo", length(vars_pnadc), "\n")
  # Guarda o layout: dele saem os enunciados literais da PNADc na Tabela S1
  saveRDS(layout_pnadc, file.path(pnadc_dir, "layout_pnadc.rds"))
  print(as.data.frame(layout_pnadc |> dplyr::filter(name %in% c("S01021", "S01022"))))

  t0 <- Sys.time()
  pnadc_raw <- read_fwf_subset(txt_pnadc, layout_pnadc, vars_pnadc)
  message(glue::glue("[ok]   PNADc TIC em {round(difftime(Sys.time(), t0, units = 'mins'), 1)} min"))
  saveRDS(pnadc_raw, path_tic)
}
download_log$pnadc <- log_row("PNAD Continua TIC", YEAR_PNADC_TIC, path_tic, pnadc_raw)
cat("dim:", dim(pnadc_raw), "\n")

# ---- 4. Censo - populacao adulta por sexo, faixa etaria e capital ------------
# Tabela SIDRA 9514: populacao residente por sexo e idade, nivel municipio.
# Os 27 codigos de municipio abaixo sao identificadores administrativos do IBGE,
# nao resultados; ainda assim o script confere os nomes devolvidos pelo SIDRA.

h2("Censo ", YEAR_CENSO, " - populacao por sexo e idade nas capitais")

capitals <- tibble::tribble(
  ~code_muni, ~capital,          ~uf, ~region,
  1200401,    "Rio Branco",      "AC", "Norte",
  1302603,    "Manaus",          "AM", "Norte",
  1600303,    "Macapa",          "AP", "Norte",
  1501402,    "Belem",           "PA", "Norte",
  1100205,    "Porto Velho",     "RO", "Norte",
  1400100,    "Boa Vista",       "RR", "Norte",
  1721000,    "Palmas",          "TO", "Norte",
  2704302,    "Maceio",          "AL", "Nordeste",
  2927408,    "Salvador",        "BA", "Nordeste",
  2304400,    "Fortaleza",       "CE", "Nordeste",
  2111300,    "Sao Luis",        "MA", "Nordeste",
  2507507,    "Joao Pessoa",     "PB", "Nordeste",
  2611606,    "Recife",          "PE", "Nordeste",
  2211001,    "Teresina",        "PI", "Nordeste",
  2408102,    "Natal",           "RN", "Nordeste",
  2800308,    "Aracaju",         "SE", "Nordeste",
  3205309,    "Vitoria",         "ES", "Sudeste",
  3106200,    "Belo Horizonte",  "MG", "Sudeste",
  3304557,    "Rio de Janeiro",  "RJ", "Sudeste",
  3550308,    "Sao Paulo",       "SP", "Sudeste",
  4106902,    "Curitiba",        "PR", "Sul",
  4314902,    "Porto Alegre",    "RS", "Sul",
  4205407,    "Florianopolis",   "SC", "Sul",
  5300108,    "Brasilia",        "DF", "Centro-Oeste",
  5208707,    "Goiania",         "GO", "Centro-Oeste",
  5103403,    "Cuiaba",          "MT", "Centro-Oeste",
  5002704,    "Campo Grande",    "MS", "Centro-Oeste"
)
stopifnot(nrow(capitals) == 27, !any(duplicated(capitals$code_muni)))
saveRDS(capitals, here::here("data-raw", "censo", "capitals.rds"))

path_censo <- here::here("data-raw", "censo", glue::glue("censo{YEAR_CENSO}_pop_capitais.rds"))
# A interface de argumentos do sidrar quebra com duas classificacoes sob R 4.5
# ("'length = 2' in coercion to 'logical(1)'"), entao montamos a consulta no
# formato da API do SIDRA, que o proprio pacote aceita via argumento `api`:
#   t/9514  tabela   n6/<codigos>  municipios   v/93  populacao residente
#   c2/allxt  sexo sem o total     c287/all     grupos de idade
sidra_api <- paste0("/t/9514/n6/", paste(capitals$code_muni, collapse = ","),
                    "/v/93/p/all/c2/allxt/c287/all")

censo_raw <- tryCatch(
  fetch_once("Censo/SIDRA 9514", path_censo, sidrar::get_sidra(api = sidra_api)),
  error = function(e) {
    warning("SIDRA falhou: ", conditionMessage(e),
            "\nO Censo entra apenas na conferencia de pos-estratificacao; ",
            "os demais scripts nao dependem dele. Rode 01 de novo mais tarde.",
            call. = FALSE)
    NULL
  }
)
if (!is.null(censo_raw)) {
  download_log$censo <- log_row("Censo (SIDRA 9514)", YEAR_CENSO, path_censo, censo_raw)
  cat("dim:", dim(censo_raw), "\n")
  # Conferencia: o SIDRA tem que devolver os 27 municipios pedidos, e os nomes
  # devolvidos por ele tem que bater com a lista de capitais acima.
  devolvidos <- sort(unique(as.character(censo_raw[["Município (Código)"]])))
  cat("municipios devolvidos:", length(devolvidos), "de 27\n")
  faltando_muni <- setdiff(as.character(capitals$code_muni), devolvidos)
  if (length(faltando_muni) > 0) {
    warning("Capitais ausentes na resposta do SIDRA: ",
            paste(faltando_muni, collapse = ", "), call. = FALSE)
  } else {
    cat("conferencia de codigos: todas as 27 capitais presentes\n")
  }
}

# ---- 5. Resumo --------------------------------------------------------------

h2("Resumo do download")

download_summary <- dplyr::bind_rows(download_log)
print(as.data.frame(download_summary))
write_log(download_summary, "download_summary")

# ---- 6. Vigitel -------------------------------------------------------------
# O Vigitel nao tem pacote de R nem API, mas as bases sao publicas na pagina da
# SVSA/MS. Nenhuma URL e digitada de memoria: o script le a pagina oficial,
# encontra o link do ano pedido pelo padrao de nome e baixa a partir dai. Se o
# link mudar de nome, o script para e diz o que encontrou, em vez de errar em
# silencio.

h2("Vigitel ", YEAR_VIGITEL)

vigitel_dir  <- here::here("data-raw", "vigitel")
vigitel_page <- "https://svs.aids.gov.br/daent/cgdnt/vigitel/"

#' Le a pagina da SVSA e devolve os links de arquivo (href) com URL absoluta
list_vigitel_links <- function(page = vigitel_page) {
  html  <- paste(readLines(url(page), warn = FALSE), collapse = "\n")
  hrefs <- unique(stringr::str_match_all(html, 'href="([^"]+)"')[[1]][, 2])
  hrefs <- hrefs[stringr::str_detect(hrefs, "\\.(zip|xls|xlsx)$")]
  tibble::tibble(
    file = basename(hrefs),
    url  = ifelse(stringr::str_detect(hrefs, "^https?://"), hrefs, paste0(page, hrefs))
  )
}

#' Baixa um arquivo se ainda nao existir.
#' O servidor da SVSA derruba a conexao no meio de arquivos grandes e nao
#' negocia HTTP/2 de forma estavel, entao usamos curl com HTTP/1.1 e retomada
#' (-C -), repetindo ate o tamanho local bater com o Content-Length remoto.
#' download.file() do R nao retoma transferencia parcial e falharia aqui.
download_once <- function(url, dest, max_tries = 40) {
  remote_size <- suppressWarnings(as.numeric(stringr::str_match(
    paste(system2("curl", c("-sIL", "--http1.1", "--max-time", "60", shQuote(url)),
                  stdout = TRUE), collapse = "\n"),
    "(?i)content-length: *(\\d+)"
  )[, 2]))
  remote_size <- remote_size[length(remote_size)]

  if (file.exists(dest) && (is.na(remote_size) || file.size(dest) >= remote_size)) {
    message(glue::glue("[pula] {basename(dest)}: ja existe ({round(file.size(dest)/1024^2, 1)} MB)"))
    return(invisible(dest))
  }

  message(glue::glue("[baixa] {basename(dest)} ({round(remote_size/1024^2, 1)} MB) ..."))
  for (i in seq_len(max_tries)) {
    system2("curl", c("-sL", "--http1.1", "-C", "-", "--max-time", "120",
                      "--speed-time", "30", "--speed-limit", "1000",
                      "-o", shQuote(dest), shQuote(url)))
    if (file.exists(dest) && (is.na(remote_size) || file.size(dest) >= remote_size)) {
      message(glue::glue("[ok]   {basename(dest)} em {i} tentativa(s)"))
      return(invisible(dest))
    }
  }
  stop("Download incompleto apos ", max_tries, " tentativas: ", basename(dest),
       " (", file.size(dest), " de ", remote_size, " bytes)", call. = FALSE)
}

links <- list_vigitel_links()
cat("arquivos publicados na pagina da SVSA:", nrow(links), "\n")

# Base do ano: padrao "vigitel-<ano>-peso-rake.zip" (peso rake = pos-estratificacao)
alvo_base <- links |> dplyr::filter(file == glue::glue("vigitel-{YEAR_VIGITEL}-peso-rake.zip"))
# Dicionarios: a pagina publica mais de um (um por recorte de serie). Baixamos
# todos - ordenar por nome pegaria "dicionario-vigitel-2021" na frente do da
# serie 2006-2024, que e o que cobre o ano do estudo.
alvo_dic <- links |> dplyr::filter(stringr::str_detect(file, "^dicionario-vigitel-.*\\.xlsx?$"))

if (nrow(alvo_base) == 0) {
  stop(glue::glue(
    "Nao encontrei 'vigitel-{YEAR_VIGITEL}-peso-rake.zip' em {vigitel_page}.
     Arquivos .zip disponiveis la:
     {paste(links$file[stringr::str_detect(links$file, '\\\\.zip$')], collapse = ', ')}"
  ), call. = FALSE)
}

cat("base:      ", alvo_base$url, "\n")
cat("dicionarios:", paste(alvo_dic$file, collapse = ", "), "\n")

zip_base <- download_once(alvo_base$url, file.path(vigitel_dir, alvo_base$file))
for (k in seq_len(nrow(alvo_dic))) {
  download_once(alvo_dic$url[k], file.path(vigitel_dir, alvo_dic$file[k]))
}
# Nota metodologica e orientacoes de uso: entram na descricao do desenho (03)
for (f in c("nota-metodologica-vigitel-2006-2024.pdf", "orientacoes-vigitel-2006-2024.pdf")) {
  try(download_once(paste0(vigitel_page, f), file.path(vigitel_dir, f)), silent = TRUE)
}

utils::unzip(zip_base, exdir = vigitel_dir)

vigitel_files <- list.files(vigitel_dir, full.names = TRUE, recursive = TRUE)
vigitel_inventory <- tibble::tibble(
  file    = basename(vigitel_files),
  size_mb = round(file.size(vigitel_files) / 1024^2, 2),
  mtime   = format(file.mtime(vigitel_files), "%Y-%m-%d %H:%M")
)
cat("\nArquivos do Vigitel em data-raw/vigitel/:\n")
print(as.data.frame(vigitel_inventory))
write_log(vigitel_inventory, "vigitel_inventory")

download_log$vigitel <- tibble::tibble(
  source = "Vigitel (SVSA/MS)", year = YEAR_VIGITEL, file = alvo_base$file,
  size_mb = round(file.size(zip_base) / 1024^2, 1),
  n_rows = NA_integer_, n_cols = NA_integer_,
  downloaded = format(file.mtime(zip_base), "%Y-%m-%d %H:%M")
)
download_summary <- dplyr::bind_rows(download_log)
write_log(download_summary, "download_summary")

message("\n01_download.R concluido.")
