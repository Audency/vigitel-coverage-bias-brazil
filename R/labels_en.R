# =============================================================================
# labels_en.R
# O QUE FAZ : Dicionarios de traducao PT -> EN e formatadores numericos em
#             ingles (ponto decimal, virgula de milhar). Carregado pelos
#             scripts *_en.R. Nao contem nenhum numero do estudo: so rotulos.
# ENTRADAS  : nenhuma (requer 00_setup.R carregado)
# SAIDAS    : nenhuma
# =============================================================================

# ---- Formatacao em ingles ---------------------------------------------------
fmt_num_en <- function(x, dig = 1) fmt_num(x, dig, lang = "en")

#' IC95% como "12.3 (10.1-14.5)". Com limite negativo o travessao gruda no
#' sinal, entao usamos " to " nesse caso (mesma regra da versao PT).
fmt_ic_en <- function(est, low, upp, dig = 1) {
  neg <- (!is.na(low) & low < 0) | (!is.na(upp) & upp < 0)
  ifelse(is.na(est), NA_character_,
         paste0(fmt_num_en(est, dig), " (", fmt_num_en(low, dig),
                ifelse(neg, " to ", "–"), fmt_num_en(upp, dig), ")"))
}

fmt_p_en <- function(p) fmt_p(p, lang = "en")

big_en <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

# ---- Dicionarios ------------------------------------------------------------
IND_EN <- setNames(INDICADORES$rotulo_en, INDICADORES$rotulo)

REGIOES_EN <- c("Norte" = "North", "Nordeste" = "Northeast", "Sudeste" = "Southeast",
                "Sul" = "South", "Centro-Oeste" = "Central-West")
REG_EN <- unname(REGIOES_EN)

DOMINIO_EN <- c("27 capitais" = "27 capitals", REGIOES_EN)

SEXO_EN <- c("Total" = "Overall", "Masculino" = "Men", "Feminino" = "Women")

RACA_EN <- c("Branca" = "White", "Preta" = "Black", "Parda" = "Brown (parda)",
             "Amarela" = "Asian", "Indigena" = "Indigenous",
             "Indígena" = "Indigenous")

POSSE_EN <- c("Fixo" = "Landline", "Somente celular" = "Mobile only",
              "Nenhum" = "No telephone")
POSSE_TEL_EN <- unname(POSSE_EN)

VAR_EN <- c(age = "Age, years", sex = "Sex", age_grp = "Age group, years",
            education = "Education, years of schooling", race = "Race/skin colour",
            region = "Region")

NIVEL_EN <- c("Média (DP)" = "Mean (SD)", SEXO_EN, RACA_EN, REGIOES_EN,
              setNames(FAIXAS_IDADE, FAIXAS_IDADE), setNames(FAIXAS_ESC, FAIXAS_ESC))

PCT_TXT_EN <- c("excede o gap observado" = "exceeds the observed gap",
                "sinal oposto ao gap" = "opposite sign to the gap")

CENARIO_EN <- c("S0" = "S0 landline only", "S1" = "S1 mobile only",
                "S2" = "S2 dual frame", "S3" = "S3 triple frame (+web)",
                "S4" = "S4 full multimodal")

# Rotulos dos indicadores como aparecem nos logs de validacao externa
VALID_IND_EN <- c(
  "Hipertensao (Q00201)"          = "Hypertension (Q00201)",
  "Diabetes (Q03001)"             = "Diabetes (Q03001)",
  "Saude ruim/muito ruim (N001)"  = "Poor/very poor self-rated health (N001)",
  "Tabagismo (fumante)"           = "Smoking (fumante)",
  "Hipertensao (hart)"            = "Hypertension (hart)",
  "Diabetes (diab)"               = "Diabetes (diab)",
  "Saude ruim (saruim)"           = "Poor self-rated health (saruim)")

QUADRO_EN <- c("fixo" = "Landline", "celular" = "Mobile", "web" = "Web",
               "presencial" = "In person")

FONTE_TIC_EN <- c("PNS (capitais)" = "PNS (capitals)",
                  "PNAD-C TIC (capitais)" = "PNAD-C ICT (capitals)")

#' Traducao por dicionario, preservando o que nao esta mapeado em vez de gerar NA.
tr <- function(x, dic) {
  x <- as.character(x)
  out <- unname(dic[x])
  ifelse(is.na(out), x, out)
}

# ---- Paleta com chaves em ingles --------------------------------------------
PAL_INDICADOR_EN <- setNames(unname(PAL[c("vigitel", "pns", "verde", "rosa")]),
                             INDICADORES$rotulo_en)
PAL_POSSE_EN <- setNames(unname(PAL_POSSE), POSSE_TEL_EN)

# ---- Diretorios de saida ----------------------------------------------------
DIR_TAB_EN <- here::here("output", "tables_en")
DIR_FIG_EN <- here::here("output", "figures_en")
dir.create(DIR_TAB_EN, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIG_EN, showWarnings = FALSE, recursive = TRUE)

SOURCE_BASE_EN <- glue::glue(
  "Sources: Vigitel {ANO_VIGITEL} (Brazilian Ministry of Health) and National Health Survey ",
  "(PNS) {ANO_PNS} (IBGE), restricted to the 26 state capitals and the Federal District. ",
  "Estimates weighted by the sampling design of each survey."
)
