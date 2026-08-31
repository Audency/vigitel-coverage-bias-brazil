#!/usr/bin/env Rscript
# ==========================================================================
# PIPELINE COMPLETO — Cobertura, representatividade e viés de seleção
# em inquéritos telefônicos de saúde no Brasil.
#
# Vigitel × PNS × PNAD Contínua TIC. Pipeline reproduzível em arquivo único.
# Versão final pós-revisão por 6 + 5 revisores.
#
# AUTORES: Audêncio Victor, Carla F. do Nascimento, Bruna S. Breternitz,
#          Michele L. P. Ferrer, Étienne L. Duim
# DATA:    Maio 2026 — v0.5
# ==========================================================================
#
# COMO USAR
# ---------
#   # Configurar diretório do projeto:
#   PROJ_ROOT <- "<seu/caminho>/analise_R"
#   setwd(PROJ_ROOT)
#   source("pipeline_completo.R")
#   run_all()
#
#   # OU rodar etapas individuais:
#   step_install_packages()
#   step_load_vigitel()
#   step_download_pns()
#   step_harmonize()
#   step_component1_coverage()
#   step_component2_vigitel_pns()
#   step_component3_propensity()
#   step_component4_fairlie()
#   step_component4_monte_carlo()
#
# DEPENDÊNCIAS
# ------------
#   R ≥ 4.3, ~3.5 GB de espaço em disco para microdados.
#   Pacotes: data.table, fst, tidyverse, survey, srvyr, PNSIBGE, PNADcIBGE,
#            ranger, yardstick, rsample, segmented, ggplot2, scales, sidrar.
#
# FLUXO DE EXECUÇÃO TÍPICO (1ª vez)
# ---------------------------------
#   1. step_install_packages()       (~ 5–10 min, idempotente)
#   2. step_load_vigitel()           (~ 1–2 min — descompacta CSV de 1 GB)
#   3. step_download_pns(2019)       (~ 1–2 min — baixa 28 MB + parsing 455 MB)
#   4. step_harmonize()              (~ 30 s)
#   5. step_component1_coverage()    (~ 30 s)
#   6. step_component2_vigitel_pns() (~ 2–3 min — bootstrap R=1000)
#   7. step_component3_propensity()  (~ 5–10 min — RF + GLM em 5 folds)
#   8. step_component4_fairlie()     (~ 3–5 min — bootstrap R=300 × 5 ind.)
#   9. step_component4_monte_carlo() (~ 1–2 min — R=1000 réplicas × 4 cenários)
#
# Re-execuções subsequentes são rápidas porque cada step verifica cache.
# ==========================================================================

# ============================================================
#  CONFIGURAÇÃO
# ============================================================
PROJ_ROOT <- if (exists("PROJ_ROOT")) PROJ_ROOT else here::here()

paths <- list(
  vigitel_zip = file.path(PROJ_ROOT, "..", "Base de dados",
                          "vigitel-2006-2024-peso-rake-csv.zip"),
  vigitel_csv = file.path(PROJ_ROOT, "data", "raw",
                          "vigitel-2006-2024-peso-rake.csv"),
  vigitel_fst = file.path(PROJ_ROOT, "data", "processed", "vigitel.fst"),
  pns_dir     = file.path(PROJ_ROOT, "data", "raw", "pns"),
  pnad_dir    = file.path(PROJ_ROOT, "data", "raw", "pnad"),
  processed   = file.path(PROJ_ROOT, "data", "processed"),
  fig         = file.path(PROJ_ROOT, "outputs", "figures"),
  tab         = file.path(PROJ_ROOT, "outputs", "tables"),
  models      = file.path(PROJ_ROOT, "outputs", "models"),
  logs        = file.path(PROJ_ROOT, "logs")
)

invisible(lapply(paths[c("processed","fig","tab","models","logs",
                         "pns_dir","pnad_dir")],
                 dir.create, showWarnings = FALSE, recursive = TRUE))

study <- list(
  vigitel_years    = 2006:2024,
  pns_years        = c(2013, 2019),
  pnad_tic_years   = c(2016:2019, 2021, 2022, 2023),
  capitals         = c("aracaju","belem","belo horizonte","boa vista","brasilia",
                       "campo grande","cuiaba","curitiba","distrito federal",
                       "florianopolis","fortaleza","goiania","joao pessoa",
                       "macapa","maceio","manaus","natal","palmas","porto alegre",
                       "porto velho","recife","rio branco","rio de janeiro",
                       "salvador","sao luis","sao paulo","teresina","vitoria"),
  age_min          = 18,
  indicators       = c("smoker","obesity","hypertension","diabetes","poor_health"),
  bootstrap_R      = 1000,
  monte_carlo_R    = 1000,
  fairlie_R        = 300,
  cv_folds         = 5,
  seed             = 20260507
)

set.seed(study$seed)

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

log_msg <- function(...) {
  message(sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...)))
}

# ============================================================
#  STEP 0 — INSTALAÇÃO DE PACOTES (idempotente)
# ============================================================
step_install_packages <- function() {
  required <- c("data.table","fst","arrow","readxl","janitor","stringi",
                "lubridate","tidyverse","survey","srvyr","PNSIBGE","PNADcIBGE",
                "ranger","xgboost","tidymodels","yardstick","rsample","themis",
                "mice","boot","segmented","sidrar","ggplot2","scales","patchwork",
                "gtsummary","gt","knitr","kableExtra","here")
  ip <- rownames(installed.packages())
  miss <- setdiff(required, ip)
  if (length(miss) > 0) {
    message("Instalando: ", paste(miss, collapse = ", "))
    install.packages(miss, repos = "https://cloud.r-project.org/", Ncpus = 4)
  } else message("Todos pacotes presentes.")
}

# ============================================================
#  STEP 1 — CARREGAR VIGITEL 2006–2024
# ============================================================
step_load_vigitel <- function(force = FALSE) {
  if (file.exists(paths$vigitel_fst) && !force) {
    log_msg("Vigitel já em cache. Pular.")
    return(invisible(NULL))
  }
  suppressPackageStartupMessages({
    library(data.table); library(fst); library(janitor); library(stringi)
  })
  if (!file.exists(paths$vigitel_csv)) {
    log_msg("Descompactando ", basename(paths$vigitel_zip))
    unzip(paths$vigitel_zip, exdir = dirname(paths$vigitel_csv))
  }
  vars_keep <- c("ano","cidade","q6","q7","pesorake2025","regiao","fxesc",
                 "fumante","exfuma","mais20","imc_i","excpeso_i","obesid_i",
                 "hart","diab","dislip","coracao","osteo","alcabu",
                 "ativo_livre","atilaz","inativo","saruim","telefone")
  log_msg("Lendo Vigitel CSV (~1 GB) — pode levar 30-90 s")
  vig <- fread(paths$vigitel_csv, select = vars_keep, encoding = "Latin-1",
               showProgress = TRUE)
  vig <- janitor::clean_names(vig)
  setnames(vig, c("q6","q7","pesorake2025","fxesc"),
                c("idade","sexo","peso","educ_fx"))
  vig[, cidade := stri_trans_general(cidade,"Latin-ASCII") |> tolower() |> trimws()]
  vig <- vig[idade >= study$age_min & !is.na(peso) & peso > 0]
  setnames(vig, "regiao", "region_raw", skip_absent = TRUE)
  rl <- as.data.table(region_lookup); setnames(rl, c("cidade","region_canon"))
  vig <- merge(vig, rl, by = "cidade", all.x = TRUE, sort = FALSE)
  vig[, region := as.character(region_canon)]
  vig[, c("region_raw","region_canon") := NULL]
  log_msg("Vigitel n=", format(nrow(vig), big.mark="."))
  fst::write_fst(vig, paths$vigitel_fst, compress = 75)
  invisible(vig)
}

# ============================================================
#  STEP 2 — DOWNLOAD PNS
# ============================================================
step_download_pns <- function(year = 2019, force = FALSE) {
  out_rds <- file.path(paths$pns_dir, sprintf("pns_%d_capitais.rds", year))
  if (file.exists(out_rds) && !force) {
    log_msg("PNS ", year, " já em cache.")
    return(readRDS(out_rds))
  }
  suppressPackageStartupMessages({ library(PNSIBGE); library(data.table) })
  log_msg("Baixando PNS ", year, " — 28 MB zip + parse 455 MB")
  d <- PNSIBGE::get_pns(year = year, selected = TRUE, anthropometry = FALSE,
                        labels = FALSE, deflator = FALSE, design = FALSE,
                        savedir = paths$pns_dir)
  setDT(d)
  log_msg("PNS ", year, " n=", format(nrow(d), big.mark="."))
  saveRDS(d, out_rds, compress = "xz")
  d
}

# ============================================================
#  STEP 3 — HARMONIZAÇÃO (Vigitel + PNS → mesmo schema)
# ============================================================
step_harmonize <- function() {
  suppressPackageStartupMessages({
    library(data.table); library(fst); library(stringi)
  })

  yn <- function(x) {
    if (is.numeric(x)) return(as.integer(x == 1))
    s <- stri_trans_general(as.character(x), "Latin-ASCII") |> tolower() |> trimws()
    out <- rep(NA_integer_, length(s))
    out[s %in% c("sim","yes","1")]      <- 1L
    out[s %in% c("nao","no","0","2")]   <- 0L
    out
  }

  # ---------- Vigitel ----------
  vig <- fst::read_fst(paths$vigitel_fst, as.data.table = TRUE)
  h_vig <- vig[, .(
    source       = "Vigitel",
    year         = ano,
    capital      = stri_trans_general(cidade,"Latin-ASCII") |> tolower() |> trimws(),
    region,
    age          = as.integer(idade),
    sex          = factor(ifelse(stri_trans_general(sexo,"Latin-ASCII") |> tolower() == "feminino",
                                 "F","M")),
    educ_fx      = factor(educ_fx, levels = 1:3,
                          labels = c("0-8 anos","9-11 anos","≥12 anos")),
    weight       = as.numeric(peso),
    smoker       = yn(fumante),
    obesity      = yn(obesid_i),
    overweight   = yn(excpeso_i),
    hypertension = yn(hart),
    diabetes     = yn(diab),
    heavy_drink  = yn(alcabu),
    phys_active  = {
      a <- stri_trans_general(as.character(ativo_livre),"Latin-ASCII") |> tolower() |> trimws()
      ifelse(a == "ativo", 1L,
             ifelse(a %in% c("inativo/ins","inativo","insuf"), 0L, NA_integer_))
    },
    poor_health  = yn(saruim),
    has_landline = 1L
  )]
  fst::write_fst(h_vig, file.path(paths$processed, "harm_vigitel.fst"), 75)

  # ---------- PNS ----------
  h_pns_list <- list()
  for (yr in study$pns_years) {
    f <- file.path(paths$pns_dir, sprintf("pns_%d_capitais.rds", yr))
    if (!file.exists(f)) next
    pns <- as.data.table(readRDS(f)); cols <- names(pns)

    smoker      <- if ("P050" %in% cols) as.integer(pns$P050 %in% c(1,2)) else NA_integer_
    hyperten    <- if ("Q00201" %in% cols) as.integer(pns$Q00201 == 1)
                   else if ("Q002" %in% cols) as.integer(pns$Q002 == 1) else NA_integer_
    diabetes    <- if ("Q03001" %in% cols) as.integer(pns$Q03001 == 1)
                   else if ("Q030" %in% cols) as.integer(pns$Q030 == 1) else NA_integer_
    poor_health <- if ("N001" %in% cols) as.integer(as.integer(pns$N001) %in% c(4,5)) else NA_integer_
    if (all(c("W00103","W00203") %in% cols)) {
      bmi <- as.numeric(pns$W00103) / (as.numeric(pns$W00203)/100)^2
      obesity <- as.integer(bmi >= 30)
    } else obesity <- NA_integer_

    weight_var <- intersect(cols, c("V00291","V0029","V00292","V0030","V0028"))[1]
    pesos <- if (!is.na(weight_var)) as.numeric(pns[[weight_var]]) else rep(1, nrow(pns))

    out <- data.table(
      source       = "PNS",
      year         = yr,
      capital      = NA_character_,
      region       = NA_character_,
      age          = if ("C008" %in% cols) as.integer(pns$C008) else NA_integer_,
      sex          = if ("C006" %in% cols) factor(ifelse(pns$C006 == 2, "F","M")) else NA,
      educ_fx      = if ("VDD004A" %in% cols) {
                       v <- as.integer(pns$VDD004A)
                       factor(ifelse(v %in% 1:2, "0-8 anos",
                              ifelse(v %in% 3:4, "9-11 anos",
                              ifelse(v %in% 5:7, "≥12 anos", NA))),
                              levels = c("0-8 anos","9-11 anos","≥12 anos"))
                     } else NA,
      weight       = pesos, smoker, obesity, overweight = NA_integer_,
      hypertension = hyperten, diabetes, heavy_drink = NA_integer_,
      phys_active  = NA_integer_, poor_health,
      has_landline = NA_integer_,
      uf_code      = if ("V0001" %in% cols) as.integer(pns$V0001) else NA_integer_
    )
    h_pns_list[[as.character(yr)]] <- out[!is.na(age) & age >= study$age_min]
  }
  if (length(h_pns_list)) {
    h_pns <- rbindlist(h_pns_list, fill = TRUE)
    fst::write_fst(h_pns, file.path(paths$processed, "harm_pns.fst"), 75)
  }

  log_msg("Harmonização concluída.")
  invisible(NULL)
}

# ============================================================
#  STEP 4 — COMPONENTE 1: COBERTURA TELEFÔNICA + AAPC
# ============================================================
step_component1_coverage <- function() {
  suppressPackageStartupMessages({
    library(data.table); library(ggplot2); library(scales)
  })

  cobertura_fixa <- fread("
year,region,p_landline
2016,Brasil,32.6
2016,Norte,9.5
2016,Nordeste,11.7
2016,Sudeste,46.5
2016,Sul,40.1
2016,Centro-Oeste,28.4
2017,Brasil,29.8
2017,Norte,8.8
2017,Nordeste,10.4
2017,Sudeste,43.3
2017,Sul,37.1
2017,Centro-Oeste,25.6
2018,Brasil,27.0
2018,Norte,8.0
2018,Nordeste,9.4
2018,Sudeste,40.0
2018,Sul,33.6
2018,Centro-Oeste,22.5
2019,Brasil,23.1
2019,Norte,7.0
2019,Nordeste,8.2
2019,Sudeste,34.4
2019,Sul,28.2
2019,Centro-Oeste,18.7
2021,Brasil,15.6
2021,Norte,5.5
2021,Nordeste,5.9
2021,Sudeste,22.7
2021,Sul,19.5
2021,Centro-Oeste,11.6
2022,Brasil,13.6
2022,Norte,4.8
2022,Nordeste,5.2
2022,Sudeste,19.6
2022,Sul,17.0
2022,Centro-Oeste,10.0
2023,Brasil,11.7
2023,Norte,4.0
2023,Nordeste,4.4
2023,Sudeste,16.9
2023,Sul,14.6
2023,Centro-Oeste,8.8
")
  fwrite(cobertura_fixa, file.path(paths$tab, "tab1_cobertura_fixa.csv"))

  aapc_tbl <- cobertura_fixa[region != "Brasil",
    .(AAPC_pct = round((exp(coef(lm(log(p_landline) ~ year, data=.SD))[2]) - 1) * 100, 2),
      n_anos   = .N), by = region]

  boot_aapc <- function(df, R = 500) {
    out <- numeric(R)
    for (b in seq_len(R)) {
      idx <- sample(seq_len(nrow(df)), replace = TRUE)
      out[b] <- (exp(coef(lm(log(p_landline) ~ year, data = df[idx]))[2]) - 1) * 100
    }
    sd(out, na.rm = TRUE)
  }
  aapc_tbl[, SE := sapply(region, function(r) boot_aapc(cobertura_fixa[region == r]))]
  fwrite(aapc_tbl, file.path(paths$tab, "tab2_AAPC_landline_published.csv"))

  # z-test entre AAPCs
  regions_v <- unique(aapc_tbl$region)
  ztest <- expand.grid(r1 = regions_v, r2 = regions_v, stringsAsFactors = FALSE)
  ztest <- as.data.table(ztest)[r1 < r2]
  ztest[, c("AAPC1","AAPC2","SE1","SE2") := list(
    aapc_tbl$AAPC_pct[match(r1, aapc_tbl$region)],
    aapc_tbl$AAPC_pct[match(r2, aapc_tbl$region)],
    aapc_tbl$SE[match(r1, aapc_tbl$region)],
    aapc_tbl$SE[match(r2, aapc_tbl$region)]
  )]
  ztest[, z := round((AAPC1 - AAPC2) / sqrt(SE1^2 + SE2^2), 3)]
  ztest[, p := round(2*pnorm(-abs(z)), 4)]
  fwrite(ztest, file.path(paths$tab, "tab3_AAPC_ztest.csv"))

  # Figura 1
  fig1 <- ggplot(cobertura_fixa[region != "Brasil"],
                 aes(year, p_landline/100, colour = region)) +
    geom_line(linewidth = 1) + geom_point(size = 2.4) +
    geom_line(data = cobertura_fixa[region == "Brasil"],
              aes(year, p_landline/100), linetype = "dashed",
              colour = "black", linewidth = 0.8) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.50)) +
    scale_x_continuous(breaks = c(2016:2019, 2021:2023)) +
    scale_colour_brewer(palette = "Set1") +
    labs(x = NULL, y = "Domicílios com telefone fixo (%)", colour = NULL,
         title = "Declínio da telefonia fixa nos domicílios brasileiros, 2016–2023",
         subtitle = "PNAD Contínua TIC, IBGE — por grande região",
         caption = "Linha tracejada: Brasil. Adultos ≥18, todas as áreas (não restrito a capitais).") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")

  ggsave(file.path(paths$fig, "fig1_landline_by_region_published.png"),
         fig1, width = 9, height = 5.5, dpi = 300)
  log_msg("Componente 1 concluído.")
  print(aapc_tbl)
  invisible(list(coverage = cobertura_fixa, aapc = aapc_tbl, ztest = ztest))
}

# ============================================================
#  STEP 5 — COMPONENTE 2: VIGITEL × PNS COM BOOTSTRAP
# ============================================================
step_component2_vigitel_pns <- function(year_focus = 2019, R = study$bootstrap_R) {
  suppressPackageStartupMessages({
    library(data.table); library(fst)
  })
  v <- fst::read_fst(file.path(paths$processed, "harm_vigitel.fst"),
                     as.data.table = TRUE)[year == year_focus]
  p <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                     as.data.table = TRUE)[year == year_focus]

  set.seed(study$seed)
  inds <- study$indicators
  cmp <- list()
  for (ind in inds) {
    v_v <- v[[ind]]; v_w <- v$weight
    p_v <- p[[ind]]; p_w <- p$weight
    v_keep <- !is.na(v_v) & !is.na(v_w); p_keep <- !is.na(p_v) & !is.na(p_w)
    v_v <- v_v[v_keep]; v_w <- v_w[v_keep]
    p_v <- p_v[p_keep]; p_w <- p_w[p_keep]
    if (length(v_v) < 30 || length(p_v) < 30) next
    pv <- weighted.mean(v_v, v_w); pp <- weighted.mean(p_v, p_w)
    diffs <- numeric(R)
    for (b in seq_len(R)) {
      iv <- sample.int(length(v_v), replace = TRUE)
      ip <- sample.int(length(p_v), replace = TRUE)
      diffs[b] <- weighted.mean(v_v[iv], v_w[iv]) - weighted.mean(p_v[ip], p_w[ip])
    }
    cmp[[ind]] <- data.table(
      indicator = ind,
      p_vigitel = round(pv*100, 2), p_pns = round(pp*100, 2),
      diff_pp   = round((pv-pp)*100, 2),
      lwr_pp    = round(quantile(diffs, 0.025)*100, 2),
      upr_pp    = round(quantile(diffs, 0.975)*100, 2),
      n_vig     = length(v_v), n_pns = length(p_v)
    )
  }
  res <- rbindlist(cmp)
  fwrite(res, file.path(paths$tab, "tab5_diffs_FIXED.csv"))
  log_msg("Componente 2 concluído.")
  print(res)
  invisible(res)
}

# ============================================================
#  STEP 6 — COMPONENTE 3: RESPONSE PROPENSITY
# ============================================================
step_component3_propensity <- function(year_focus = 2019,
                                        n_max_vig = 50000) {
  suppressPackageStartupMessages({
    library(data.table); library(fst); library(rsample)
    library(ranger); library(yardstick)
  })

  v <- fst::read_fst(file.path(paths$processed, "harm_vigitel.fst"),
                     as.data.table = TRUE)
  p <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                     as.data.table = TRUE)
  set.seed(study$seed)

  v19 <- v[year == year_focus, .(age, sex, educ_fx, weight, y = 1L)]
  p19 <- p[year == year_focus, .(age, sex, educ_fx, weight, y = 0L)]
  v_sample <- v19[sample(.N, min(n_max_vig, nrow(v19)))]
  dat <- rbind(v_sample, p19)
  dat <- dat[complete.cases(dat[, .(age, sex, educ_fx)])]
  dat[, sex := factor(sex)]
  dat[, educ_fx := factor(educ_fx)]
  dat[, y := factor(y, levels = c(0,1), labels = c("pns","vigitel"))]

  # Pesos normalizados POR FONTE (corrige separação espúria)
  dat[, w_norm := weight * .N / sum(weight), by = y]

  folds <- vfold_cv(dat, v = study$cv_folds, strata = y)
  auc_log <- auc_rf <- numeric(study$cv_folds)
  brier_log <- brier_rf <- numeric(study$cv_folds)

  for (k in seq_len(study$cv_folds)) {
    tr <- analysis(folds$splits[[k]])
    te <- assessment(folds$splits[[k]])

    m_log <- suppressWarnings(glm(y ~ age + sex + educ_fx, data = tr,
                                   family = quasibinomial, weights = tr$w_norm))
    p_log <- predict(m_log, te, type = "response")

    m_rf <- ranger(y ~ age + sex + educ_fx, data = tr, probability = TRUE,
                    num.trees = 300, case.weights = tr$w_norm, seed = k)
    p_rf <- predict(m_rf, te)$predictions[, "vigitel"]

    y_num <- as.integer(te$y == "vigitel")
    auc_log[k]   <- yardstick::roc_auc_vec(te$y, p_log, event_level = "second")
    auc_rf[k]    <- yardstick::roc_auc_vec(te$y, p_rf,  event_level = "second")
    brier_log[k] <- mean((p_log - y_num)^2)
    brier_rf[k]  <- mean((p_rf  - y_num)^2)
  }

  result <- data.table(
    model = c("Logística","Random Forest"),
    AUC_mean = round(c(mean(auc_log), mean(auc_rf)), 3),
    AUC_sd   = round(c(sd(auc_log),   sd(auc_rf)), 3),
    Brier_manual = round(c(mean(brier_log), mean(brier_rf)), 4),
    Brier_random_ref = 0.25
  )
  fwrite(result, file.path(paths$tab, "tab6_propensity_FINAL.csv"))
  log_msg("Componente 3 concluído.")
  print(result)
  invisible(result)
}

# ============================================================
#  STEP 7 — COMPONENTE 4a: FAIRLIE DECOMPOSITION + BOOTSTRAP CI
# ============================================================
step_component4_fairlie <- function(year_focus = 2019, R = study$fairlie_R) {
  suppressPackageStartupMessages({
    library(data.table); library(fst)
  })

  v <- fst::read_fst(file.path(paths$processed, "harm_vigitel.fst"),
                     as.data.table = TRUE)[year == year_focus]
  p <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                     as.data.table = TRUE)[year == year_focus]

  fairlie_safe <- function(y_var, dV, dP, covs = c("age","sex","educ_fx")) {
    dV <- dV[!is.na(dV[[y_var]]) & complete.cases(dV[, ..covs])]
    dP <- dP[!is.na(dP[[y_var]]) & complete.cases(dP[, ..covs])]
    if (nrow(dV) < 100 || nrow(dP) < 100)
      return(data.table(indicator = y_var, status = "n_insuf"))

    all_lvl <- union(levels(factor(dV$educ_fx)), levels(factor(dP$educ_fx)))
    dV[, educ_fx := factor(as.character(educ_fx), levels = all_lvl)]
    dP[, educ_fx := factor(as.character(educ_fx), levels = all_lvl)]
    dV[, sex := factor(as.character(sex), levels = c("F","M"))]
    dP[, sex := factor(as.character(sex), levels = c("F","M"))]
    dV[, w_norm := weight * .N / sum(weight, na.rm = TRUE)]
    dP[, w_norm := weight * .N / sum(weight, na.rm = TRUE)]

    fV <- suppressWarnings(glm(reformulate(covs, y_var), family = quasibinomial,
                                data = dV, weights = w_norm))
    fP <- suppressWarnings(glm(reformulate(covs, y_var), family = quasibinomial,
                                data = dP, weights = w_norm))

    pVV <- weighted.mean(predict(fV, type = "response"), dV$weight, na.rm=TRUE)
    pPP <- weighted.mean(predict(fP, type = "response"), dP$weight, na.rm=TRUE)
    pPV <- weighted.mean(predict(fV, newdata = dP, type = "response"),
                         dP$weight, na.rm=TRUE)

    data.table(indicator = y_var,
               prev_v = pVV, prev_p = pPP,
               total_diff   = pVV - pPP,
               composition  = pVV - pPV,
               coefficients = pPV - pPP,
               n_v = nrow(dV), n_p = nrow(dP))
  }

  fairlie_boot <- function(y_var, dV, dP, R = 300, covs = c("age","sex","educ_fx")) {
    point <- fairlie_safe(y_var, dV, dP, covs)
    if ("status" %in% names(point)) return(point)
    set.seed(study$seed)
    comp_b <- coef_b <- numeric(R)
    for (b in seq_len(R)) {
      iv <- sample.int(nrow(dV), replace = TRUE)
      ip <- sample.int(nrow(dP), replace = TRUE)
      res <- tryCatch(fairlie_safe(y_var, dV[iv], dP[ip], covs), error = function(e) NULL)
      if (!is.null(res) && !"status" %in% names(res)) {
        comp_b[b] <- res$composition; coef_b[b] <- res$coefficients
      } else { comp_b[b] <- NA; coef_b[b] <- NA }
    }
    point[, comp_lwr := quantile(comp_b, 0.025, na.rm = TRUE)]
    point[, comp_upr := quantile(comp_b, 0.975, na.rm = TRUE)]
    point[, coef_lwr := quantile(coef_b, 0.025, na.rm = TRUE)]
    point[, coef_upr := quantile(coef_b, 0.975, na.rm = TRUE)]
    point
  }

  log_msg("Fairlie + bootstrap (R=", R, ") para ", length(study$indicators), " indicadores...")
  res_all <- rbindlist(lapply(study$indicators, function(i) {
    log_msg("  ", i)
    fairlie_boot(i, v, p, R = R)
  }), fill = TRUE)

  fres <- res_all[, .(
    indicator, n_v, n_p,
    prev_v_pct       = round(prev_v * 100, 2),
    prev_p_pct       = round(prev_p * 100, 2),
    total_diff_pp    = round(total_diff * 100, 2),
    composition_pp   = round(composition * 100, 2),
    comp_lwr_pp      = round(comp_lwr * 100, 2),
    comp_upr_pp      = round(comp_upr * 100, 2),
    coefficients_pp  = round(coefficients * 100, 2),
    coef_lwr_pp      = round(coef_lwr * 100, 2),
    coef_upr_pp      = round(coef_upr * 100, 2),
    pct_composition  = round(composition / total_diff * 100, 1)
  )]
  fwrite(fres, file.path(paths$tab, "tab9_fairlie_FIXED.csv"))
  log_msg("Componente 4a concluído.")
  print(fres)
  invisible(fres)
}

# ============================================================
#  STEP 8 — COMPONENTE 4b: MONTE CARLO ADEMP (corrigido)
# ============================================================
step_component4_monte_carlo <- function(year_focus = 2019, R = study$monte_carlo_R) {
  suppressPackageStartupMessages({
    library(data.table); library(fst); library(ggplot2); library(scales)
  })

  pns <- fst::read_fst(file.path(paths$processed, "harm_pns.fst"),
                       as.data.table = TRUE)[year == year_focus]
  inds <- study$indicators

  pseudopop <- pns[, .SD, .SDcols = c("age","sex","educ_fx","weight", inds)]
  pseudopop <- na.omit(pseudopop)
  log_msg("Pseudopopulação (PNS, ", year_focus, "): n = ", nrow(pseudopop))

  # Coberturas estimadas (PNAD-TIC 2021 Brasil)
  COV <- c(S0_landline_only = 0.156,
           S1_dual_frame    = 1 - (1 - 0.156) * (1 - 0.963),
           S2_triple_frame  = 1 - (1 - 0.156) * (1 - 0.963) * (1 - 0.85),
           S3_multimodal    = 1.0)

  truth <- pseudopop[, lapply(.SD, weighted.mean, w = weight, na.rm = TRUE),
                     .SDcols = inds]

  mc_one <- function(seed, p_inc) {
    set.seed(seed)
    in_sample <- runif(nrow(pseudopop)) <= p_inc
    samp <- pseudopop[in_sample]
    if (nrow(samp) < 50) return(NULL)
    est <- samp[, lapply(.SD, weighted.mean, w = weight, na.rm = TRUE), .SDcols = inds]
    data.table(p_hat = unlist(est), indicator = names(est), n_in = nrow(samp))
  }

  log_msg("Monte Carlo: ", R, " réplicas × ", length(COV), " cenários...")
  results <- list()
  for (s in names(COV)) for (b in seq_len(R)) {
    res <- mc_one(study$seed + b * 100 + match(s, names(COV)), COV[s])
    if (!is.null(res)) {
      res[, scenario := s]; res[, rep := b]
      results[[length(results) + 1]] <- res
    }
  }
  mc <- rbindlist(results, fill = TRUE)
  truth_long <- data.table(indicator = names(truth), theta = unlist(truth))
  mc <- merge(mc, truth_long, by = "indicator")

  perf <- mc[, .(
    mean_p_hat = round(mean(p_hat, na.rm=TRUE) * 100, 2),
    bias_pp    = round(mean(p_hat - theta, na.rm=TRUE) * 100, 3),
    emp_SE_pp  = round(sd(p_hat, na.rm=TRUE) * 100, 2),
    RMSE_pp    = round(sqrt(mean((p_hat - theta)^2, na.rm=TRUE)) * 100, 2),
    MCSE_bias  = round(sd(p_hat - theta, na.rm=TRUE) / sqrt(.N) * 100, 4),
    n_reps     = .N, n_avg = round(mean(n_in), 0)
  ), by = .(scenario, indicator)]

  fwrite(perf, file.path(paths$tab, "tab10_monte_carlo_FIXED.csv"))

  fig3 <- ggplot(perf, aes(scenario, bias_pp, fill = scenario)) +
    geom_col() +
    geom_errorbar(aes(ymin = bias_pp - 1.96*MCSE_bias*100,
                      ymax = bias_pp + 1.96*MCSE_bias*100), width = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    facet_wrap(~ indicator, scales = "free_y") +
    labs(x = NULL, y = "Viés (pp)",
         title = "Monte Carlo CORRIGIDO — viés por cenário",
         subtitle = "Bernoulli sampling + estimador Hajek; ADEMP",
         caption = paste("Pseudopopulação: PNS", year_focus, "| R =", R)) +
    theme_minimal(base_size = 11) + theme(legend.position = "none")
  ggsave(file.path(paths$fig, "fig3_monte_carlo_FIXED.png"), fig3,
         width = 9, height = 6, dpi = 300)

  log_msg("Componente 4b concluído.")
  print(perf)
  invisible(perf)
}

# ============================================================
#  RUN_ALL — Pipeline completo end-to-end
# ============================================================
run_all <- function() {
  t0 <- Sys.time()
  step_install_packages()
  step_load_vigitel()
  for (yr in study$pns_years)
    tryCatch(step_download_pns(yr),
             error = function(e) log_msg("Falha PNS ", yr, ": ", conditionMessage(e)))
  step_harmonize()
  step_component1_coverage()
  step_component2_vigitel_pns()
  step_component3_propensity()
  step_component4_fairlie()
  step_component4_monte_carlo()
  log_msg("PIPELINE COMPLETO em ",
          round(difftime(Sys.time(), t0, units = "mins"), 2), " min.")
}

# ============================================================
#  Quando rodado como script (não sourced):
# ============================================================
if (!interactive() && sys.nframe() == 0) {
  run_all()
}
