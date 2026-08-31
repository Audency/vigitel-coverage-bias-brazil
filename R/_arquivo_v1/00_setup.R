# =============================================================================
# 00_setup.R - Ambiente, pacotes, parametros globais e paleta
# Estudo Vigitel x PNS - vies de nao cobertura e estrategias multimodais
#
# Rode este script no inicio de toda sessao. Todos os demais scripts comecam
# com: source(here::here("R", "00_setup.R"))
# =============================================================================

# ---- 1. Pacotes -------------------------------------------------------------
# Instala apenas o que estiver ausente. Nunca reinstala o que ja existe.

# NOTA: o protocolo cita "frames2"; o pacote no CRAN e "Frames2" (F maiusculo),
# versao 0.2.1, que exporta Hartley(), FB(), PEL(), BKA(), CalDF(), CalSF() e a
# funcao de comparacao Compare() - e o pacote pretendido.
pkgs <- c(
  "tidyverse", "survey", "srvyr", "PNSIBGE", "PNADcIBGE", "sidrar",
  "gt", "gtsummary", "patchwork", "here", "janitor", "glue",
  "Frames2", "scales", "sessioninfo"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  message("Instalando pacotes ausentes: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

# Frames2 so e usado no script 08 (estimacao dual/multiframe). Registramos a
# disponibilidade para que 08 decida entre o pacote e implementacao propria.
HAS_FRAMES2 <- requireNamespace("Frames2", quietly = TRUE)
if (!HAS_FRAMES2) {
  warning(
    "Pacote 'Frames2' indisponivel. O script 08_simulation.R usara ",
    "implementacao propria do estimador dual-frame (Hartley/Fuller-Burmeister), ",
    "com a escolha justificada no proprio script.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(srvyr)
  library(here)
  library(janitor)
  library(glue)
  library(gt)
  library(scales)
})

# ---- 2. Opcoes globais ------------------------------------------------------

set.seed(20260803)                              # semente unica do projeto
options(
  survey.lonely.psu = "adjust",                 # estratos com 1 UPA
  survey.adjust.domain.lonely = TRUE,
  stringsAsFactors = FALSE,
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  timeout = 3600                                # downloads do IBGE sao lentos
)

# ---- 3. Parametros do estudo ------------------------------------------------
# Editar SOMENTE aqui. Nenhum script abaixo redefine ano ou lista de indicadores.

YEAR_PNS      <- 2019   # PNS 2019: unica edicao com modulo de posse de telefone
YEAR_VIGITEL  <- 2019   # edicao pareada ao ano da PNS
YEAR_PNADC_TIC <- 2019  # suplemento TIC do 4o trimestre
YEAR_CENSO    <- 2022

# Quatro desfechos principais. Obesidade entra apenas como sensibilidade (Tab. S4)
# porque no Vigitel peso e altura sao autorrelatados por telefone e na PNS ha
# medida antropometrica em subamostra - a divergencia de instrumento e de outra
# natureza que a das demais perguntas.
INDICATORS <- tibble::tribble(
  ~indicator,        ~label_pt,                          ~label_en,
  "smoking",         "Tabagismo atual",                  "Current smoking",
  "hypertension",    "Hipertensão diagnosticada",        "Diagnosed hypertension",
  "diabetes",        "Diabetes diagnosticado",           "Diagnosed diabetes",
  "poor_health",     "Autoavaliação ruim de saúde",      "Poor self-rated health"
)

INDICATOR_SENS <- tibble::tribble(
  ~indicator,  ~label_pt,    ~label_en,
  "obesity",   "Obesidade",  "Obesity"
)

AGE_LEVELS  <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
EDU_LEVELS  <- c("0-8", "9-11", "12+")
PHONE_LEVELS <- c("Fixo", "Somente celular", "Nenhum")
REGION_LEVELS <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

N_REPLICATES <- 1000   # replicas por cenario x regiao x indicador (script 08)
N_BOOT       <- 1000   # replicas bootstrap do vies de nao cobertura (script 06)
COCHRAN_LIMIT <- 0.40  # limiar do vicio relativo de Cochran

# Convencao de sinal do estudo, usada em todas as tabelas e figuras:
DELTA_LABEL_PT <- "Δ = Vigitel − PNS"
DELTA_LABEL_EN <- "Δ = Vigitel − PNS"

# ---- 4. Paleta unica --------------------------------------------------------
# Base Okabe-Ito (segura para daltonismo). Nenhuma figura usa cor default do
# ggplot; toda cor sai daqui.

PAL <- c(
  ink       = "#1A1A1A",   # texto, eixos
  grid      = "#D9D9D9",   # grade
  vigitel   = "#0072B2",   # azul
  pns       = "#D55E00",   # laranja-vermelho
  accent1   = "#009E73",   # verde
  accent2   = "#CC79A7",   # rosa
  accent3   = "#E69F00",   # ambar
  accent4   = "#56B4E9",   # azul claro
  neutral   = "#7F7F7F"
)

PAL_SURVEY    <- c("Vigitel" = unname(PAL["vigitel"]), "PNS" = unname(PAL["pns"]))
PAL_SEX       <- c("Masculino" = unname(PAL["vigitel"]), "Feminino" = unname(PAL["accent2"]))
PAL_INDICATOR <- c(
  "Tabagismo atual"             = unname(PAL["vigitel"]),
  "Hipertensão diagnosticada"   = unname(PAL["pns"]),
  "Diabetes diagnosticado"      = unname(PAL["accent1"]),
  "Autoavaliação ruim de saúde" = unname(PAL["accent2"])
)
PAL_PHONE <- c(
  "Fixo"            = unname(PAL["vigitel"]),
  "Somente celular" = unname(PAL["accent3"]),
  "Nenhum"          = unname(PAL["neutral"])
)

# Tema unico das figuras (secao 11 do protocolo: grade horizontal apenas)
theme_study <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.border      = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]), linewidth = 0.3),
      axis.line.x       = ggplot2::element_line(colour = unname(PAL["ink"]), linewidth = 0.3),
      axis.ticks.x      = ggplot2::element_line(colour = unname(PAL["ink"]), linewidth = 0.3),
      text              = ggplot2::element_text(colour = unname(PAL["ink"])),
      strip.text        = ggplot2::element_text(face = "bold", hjust = 0),
      legend.background = ggplot2::element_blank(),
      legend.key.size   = ggplot2::unit(0.8, "lines"),
      plot.title        = ggplot2::element_blank()   # titulo vai na legenda do MS
    )
}
ggplot2::theme_set(theme_study())

# ---- 5. Funcoes auxiliares --------------------------------------------------

source(here::here("R", "functions.R"))

# ---- 6. Registro da sessao --------------------------------------------------
# Gravado a cada execucao; vai para o Texto S4 do suplemento.

dir.create(here::here("logs"), showWarnings = FALSE, recursive = TRUE)
utils::capture.output(
  sessioninfo::session_info(),
  file = here::here("logs", "sessioninfo.txt")
)

message(glue::glue(
  "
  Setup concluido.
  Projeto      : {here::here()}
  R            : {getRversion()}
  Semente      : 20260803
  PNS          : {YEAR_PNS}    Vigitel: {YEAR_VIGITEL}    PNAD-TIC: {YEAR_PNADC_TIC}
  Indicadores  : {paste(INDICATORS$indicator, collapse = ', ')} (+ {INDICATOR_SENS$indicator} como sensibilidade)
  frames2      : {ifelse(HAS_FRAMES2, 'disponivel', 'AUSENTE - ver aviso acima')}
  sessioninfo  : logs/sessioninfo.txt
  "
))
