# ============================================================
# config.R — Paths, constantes e parâmetros do estudo
# Protocolo: cobertura, representatividade e viés de seleção em
# inquéritos telefônicos de saúde no Brasil.
# ============================================================

PROJ_ROOT <- "/Users/lshva8/Desktop/AUDENCIO/PROADIS- SUS Hospital Einterin/Audencio e Carla/analise_R"

paths <- list(
  vigitel_zip       = file.path(PROJ_ROOT, "..", "Base de dados", "vigitel-2006-2024-peso-rake-csv.zip"),
  vigitel_csv       = file.path(PROJ_ROOT, "data", "raw", "vigitel-2006-2024-peso-rake.csv"),
  vigitel_dict      = file.path(PROJ_ROOT, "..", "Base de dados", "dicionario-vigitel-2006-2024.xlsx"),
  vigitel_fst       = file.path(PROJ_ROOT, "data", "processed", "vigitel.fst"),
  pns_dir           = file.path(PROJ_ROOT, "data", "raw", "pns"),
  pnad_dir          = file.path(PROJ_ROOT, "data", "raw", "pnad"),
  processed         = file.path(PROJ_ROOT, "data", "processed"),
  fig               = file.path(PROJ_ROOT, "outputs", "figures"),
  tab               = file.path(PROJ_ROOT, "outputs", "tables"),
  models            = file.path(PROJ_ROOT, "outputs", "models"),
  logs              = file.path(PROJ_ROOT, "logs")
)

invisible(lapply(paths[c("processed","fig","tab","models","logs","pns_dir","pnad_dir")],
                 dir.create, showWarnings = FALSE, recursive = TRUE))

# Constantes do estudo
study <- list(
  vigitel_years   = 2006:2024,
  pns_years       = c(2013, 2019),                      # 2024 quando disponível
  pnad_tic_years  = c(2016:2019, 2021, 2022, 2023, 2024),
  capitals        = c("aracaju","belem","belo horizonte","boa vista","brasilia",
                      "campo grande","cuiaba","curitiba","florianopolis","fortaleza",
                      "goiania","joao pessoa","macapa","maceio","manaus","natal",
                      "palmas","porto alegre","porto velho","recife","rio branco",
                      "rio de janeiro","salvador","sao luis","sao paulo","teresina",
                      "vitoria"),
  age_min         = 18,
  indicators      = c("smoker","heavy_drinking","obesity","hypertension",
                      "diabetes","poor_self_rated","physical_activity"),
  bootstrap_R     = 1000,
  monte_carlo_R   = 1000,
  cv_folds        = 5,        # cluster-CV outer
  cv_inner        = 3,
  seed            = 20260507
)

set.seed(study$seed)

# Regiões → capitais
regions <- list(
  Norte    = c("manaus","belem","porto velho","rio branco","macapa","boa vista","palmas"),
  Nordeste = c("salvador","fortaleza","recife","sao luis","maceio","natal",
               "joao pessoa","teresina","aracaju"),
  Sudeste  = c("sao paulo","rio de janeiro","belo horizonte","vitoria"),
  Sul      = c("porto alegre","curitiba","florianopolis"),
  CentroOeste = c("brasilia","distrito federal","goiania","cuiaba","campo grande")
)
region_lookup <- stack(regions); names(region_lookup) <- c("capital","region")
region_lookup$capital <- as.character(region_lookup$capital)

# Helper de log com timestamp
log_msg <- function(...) {
  message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...)))
}
