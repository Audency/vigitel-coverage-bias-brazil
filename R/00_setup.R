# =============================================================================
# 00_setup.R
# O QUE FAZ : Carrega pacotes, fixa semente e opcoes, define parametros do
#             estudo, paleta e tema grafico, e helpers usados por todos os
#             scripts. Grava o registro do ambiente computacional.
# ENTRADAS  : nenhuma
# SAIDAS    : output/logs/sessioninfo.txt, output/logs/pacotes.csv
# =============================================================================

pkgs <- c("tidyverse", "survey", "srvyr", "PNSIBGE", "PNADcIBGE", "sidrar",
          "gt", "gtsummary", "patchwork", "here", "janitor", "glue",
          "Frames2", "scales", "sessioninfo", "readxl", "officer", "pdftools",
          "digest")
faltando <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) {
  message("Instalando: ", paste(faltando, collapse = ", "))
  install.packages(faltando, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(tidyverse); library(survey); library(srvyr); library(here)
  library(janitor); library(glue); library(gt); library(scales)
})

SEED <- 20260803
set.seed(SEED)
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE,
        stringsAsFactors = FALSE, scipen = 999,
        dplyr.summarise.inform = FALSE, timeout = 3600)

# ---- Parametros do estudo (unico lugar onde se editam) ----------------------
ANO_PNS      <- 2019   # unica edicao da PNS com modulo de posse de telefone
ANO_VIGITEL  <- 2019   # edicao pareada; era do quadro exclusivamente fixo
ANO_VIG_DUAL <- 2023   # primeira edicao com cadastro duplo (valida a previsao)
ANO_PNADC    <- 2019
ANO_CENSO    <- 2022

# Quatro desfechos principais. Obesidade so como sensibilidade: no Vigitel peso e
# altura sao autorreferidos por telefone e na PNS ha medida aferida em subamostra,
# o que confunde vies de mensuracao com vies de cobertura.
INDICADORES <- tibble::tribble(
  ~indicador,     ~rotulo,                        ~var_vigitel, ~rotulo_en,
  "smoking",      "Tabagismo atual",              "fumante",    "Current smoking",
  "hypertension", "Hipertensão diagnosticada",    "hart",       "Diagnosed hypertension",
  "diabetes",     "Diabetes diagnosticado",       "diab",       "Diagnosed diabetes",
  "poor_health",  "Autoavaliação ruim de saúde",  "saruim",     "Poor self-rated health"
)

FAIXAS_IDADE <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
FAIXAS_ESC   <- c("0-8", "9-11", "12+")
POSSE_TEL    <- c("Fixo", "Somente celular", "Nenhum")
REGIOES      <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

N_BOOT     <- 1000   # replicas do bootstrap de Rao-Wu
N_REPLICAS <- 1000   # replicas por celula da simulacao
LIMIAR_COCHRAN <- 0.40
SINAL <- "Δ = Vigitel − PNS"

# ---- Paleta e tema ----------------------------------------------------------
PAL <- c(ink = "#1A1A1A", grid = "#D9D9D9", vigitel = "#0072B2", pns = "#D55E00",
         verde = "#009E73", rosa = "#CC79A7", ambar = "#E69F00",
         azul = "#56B4E9", neutro = "#7F7F7F")
PAL_POSSE <- c("Fixo" = unname(PAL["vigitel"]),
               "Somente celular" = unname(PAL["ambar"]),
               "Nenhum" = unname(PAL["neutro"]))
PAL_INDICADOR <- setNames(unname(PAL[c("vigitel", "pns", "verde", "rosa")]),
                          INDICADORES$rotulo)

tema_estudo <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = .3),
      axis.line.x = ggplot2::element_line(colour = unname(PAL["ink"]), linewidth = .3),
      axis.ticks.x = ggplot2::element_line(colour = unname(PAL["ink"]), linewidth = .3),
      text = ggplot2::element_text(colour = unname(PAL["ink"])),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank()
    )
}
ggplot2::theme_set(tema_estudo())

source(here::here("R", "funcoes.R"))

# ---- Registro do ambiente ---------------------------------------------------
dir.create(here::here("output", "logs"), showWarnings = FALSE, recursive = TRUE)
utils::capture.output(sessioninfo::session_info(),
                      file = here::here("output", "logs", "sessioninfo.txt"))
readr::write_csv(
  tibble::tibble(pacote = pkgs,
                 versao = vapply(pkgs, function(p)
                   tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_),
                   character(1))),
  here::here("output", "logs", "pacotes.csv")
)

message(glue::glue(
  "00_setup: R {getRversion()} | semente {SEED} | PNS {ANO_PNS} | ",
  "Vigitel {ANO_VIGITEL} e {ANO_VIG_DUAL}"
))
