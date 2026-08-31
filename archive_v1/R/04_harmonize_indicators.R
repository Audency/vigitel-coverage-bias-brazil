# 04_harmonize_indicators.R
# Harmoniza definições de indicadores entre Vigitel, PNS e PNAD.
# Saída: tabelas com colunas idade, sexo, esc_fx, renda_fx, region, capital,
# peso amostral, indicadores binários comparáveis.
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(data.table)
  library(fst)
  library(dplyr)
  library(stringi)
})

# ---------- VIGITEL ----------
harmonize_vigitel <- function(vig) {
  setDT(vig)
  # Helper: converte "Sim"/"Nao" (com variações) para 1/0
  yn <- function(x) {
    if (is.numeric(x)) return(as.integer(x == 1))
    s <- stri_trans_general(as.character(x), "Latin-ASCII") |> tolower() |> trimws()
    out <- rep(NA_integer_, length(s))
    out[s %in% c("sim","yes","1")] <- 1L
    out[s %in% c("nao","no","0","2")] <- 0L
    out
  }
  out <- vig[, .(
    source       = "Vigitel",
    year         = ano,
    capital      = cidade,
    region,
    age          = as.integer(idade),
    sex          = factor(ifelse(stri_trans_general(sexo, "Latin-ASCII") |>
                                   tolower() == "feminino", "F","M")),
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
      a <- stri_trans_general(as.character(ativo_livre), "Latin-ASCII") |>
        tolower() |> trimws()
      ifelse(a == "ativo", 1L, ifelse(a %in% c("inativo/ins","inativo","insuf"), 0L, NA_integer_))
    },
    poor_health  = yn(saruim),
    has_landline = 1L                 # por construção amostral
  )]
  out[, capital := stri_trans_general(capital, "Latin-ASCII") |> tolower() |> trimws()]
  out
}

# ---------- PNS ----------
# Códigos baseados em PNS 2019 (PNSIBGE labels=FALSE):
#  P050: tabagismo (1 = diariamente; 2 = menos que diariamente; 3 = não fuma)
#  Q002: HAS diagnóstica (1 = sim)
#  Q030: DM diagnóstica (1 = sim)
#  J007: autoaval saúde (1 muito boa, 2 boa, 3 regular, 4 ruim, 5 muito ruim)
#  W00103/W00203: peso/altura → IMC ≥30 → obesidade
#  C006 sexo, C008 idade, C009 raça, VDD004A escolaridade

harmonize_pns <- function(pns, year) {
  setDT(pns)
  cols <- names(pns)

  # Códigos verificados no PNS 2019 microdata real
  smoker      <- if ("P050" %in% cols)   as.integer(pns$P050 %in% c(1,2)) else NA_integer_
  # HAS diagnóstica: PNS 2019 usa Q00201 (item de Q002)
  hyperten    <- if ("Q00201" %in% cols) as.integer(pns$Q00201 == 1)
                 else if ("Q002" %in% cols) as.integer(pns$Q002 == 1) else NA_integer_
  # DM diagnóstica: PNS 2019 usa Q03001; PNS 2013 pode usar Q030/Q03001
  diabetes    <- if ("Q03001" %in% cols) as.integer(pns$Q03001 == 1)
                 else if ("Q030" %in% cols)  as.integer(pns$Q030 == 1)  else NA_integer_
  # Autoavaliação de saúde: PNS 2019 usa N001 (1=muito boa, 2=boa, 3=regular,
  # 4=ruim, 5=muito ruim). J007 é outra variável (plano de saúde).
  poor_health <- if ("N001" %in% cols) as.integer(as.integer(pns$N001) %in% c(4,5))
                 else if ("J007" %in% cols && max(as.integer(pns$J007), na.rm=TRUE) >= 4)
                   as.integer(as.integer(pns$J007) %in% c(4,5))
                 else NA_integer_
  # Obesidade: se W00103/W00203 (peso/altura MEDIDOS) estiverem disponíveis,
  # usa-se IMC medido; caso contrário NA. ATENÇÃO: comparação com Vigitel
  # (autorrelato) é DIFERENCIAL DE MODO. Reportar separadamente.
  # No subset 'selected sem anthropometry' (n≈90k) essas variáveis NÃO existem;
  # já no subset 'selected+anthropometry' (n≈6.7k) sim. Documentar.
  if (all(c("W00103","W00203") %in% cols)) {
    bmi <- as.numeric(pns$W00103) / (as.numeric(pns$W00203)/100)^2
    obesity <- as.integer(bmi >= 30)
  } else { obesity <- NA_integer_ }

  # Peso PNS: V00291 (peso pessoa selecionada com pós-estratificação por
  # projeção populacional) — recomendado pelo IBGE para análises de prevalência
  # com universo "selected adult" (≥15 anos). V0029 é peso pessoa amostral sem
  # calibração final; V0030 é peso domiciliar (não usar para análise individual).
  # Confirmação por Rev B (Vigitel/PNS specialist) e dicionário PNS 2019.
  weight_var <- intersect(cols, c("V00291","V0029","V00292","V0030","V0028"))[1]
  pesos <- if (!is.na(weight_var)) as.numeric(pns[[weight_var]]) else rep(1, nrow(pns))

  out <- data.table(
    source       = "PNS",
    year         = year,
    capital      = NA_character_,
    region       = NA_character_,
    age          = if ("C008" %in% cols) as.integer(pns$C008)
                   else if ("C00301" %in% cols) as.integer(pns$C00301) else NA_integer_,
    sex          = if ("C006" %in% cols) factor(ifelse(pns$C006 == 2, "F","M"))
                   else NA,
    # PNS VDD004A: 1=Sem instr, 2=Fund.incomp, 3=Fund.comp, 4=Med.incomp,
    # 5=Med.comp, 6=Sup.incomp, 7=Sup.comp.
    # Mapear para 3 brackets comparáveis ao Vigitel: 0-8 / 9-11 / ≥12 anos.
    educ_fx      = if ("VDD004A" %in% cols) {
                     v <- as.integer(pns$VDD004A)
                     factor(ifelse(v %in% 1:2, "0-8 anos",
                            ifelse(v %in% 3:4, "9-11 anos",
                            ifelse(v %in% 5:7, "≥12 anos", NA))),
                            levels = c("0-8 anos","9-11 anos","≥12 anos"))
                   } else NA,
    weight       = pesos,
    smoker, obesity, overweight = NA_integer_,
    hypertension = hyperten, diabetes, heavy_drink = NA_integer_,
    phys_active  = NA_integer_, poor_health,
    has_landline = NA_integer_,
    uf_code      = if ("V0001" %in% cols) as.integer(pns$V0001) else NA_integer_,
    mun_code     = if ("V0024" %in% cols) as.integer(pns$V0024) else NA_integer_
  )
  out[!is.na(age) & age >= study$age_min]
}

# ---------- PNAD TIC ----------
harmonize_pnad <- function(pnad, year) {
  setDT(pnad)
  out <- data.table(
    source       = "PNAD-TIC",
    year         = year,
    capital      = NA_character_,
    region       = NA_character_,
    age          = if ("V2009" %in% names(pnad)) as.integer(pnad$V2009) else NA_integer_,
    sex          = if ("V2007" %in% names(pnad)) factor(ifelse(pnad$V2007 == 2, "F","M")) else NA,
    educ_fx      = if ("VD3004" %in% names(pnad)) factor(pnad$VD3004) else NA,
    weight       = if ("V1028" %in% names(pnad)) as.numeric(pnad$V1028)
                   else if ("V1027" %in% names(pnad)) as.numeric(pnad$V1027)
                   else NA_real_,
    has_landline = if ("has_landline" %in% names(pnad)) pnad$has_landline else NA_integer_,
    has_mobile   = if ("has_mobile"   %in% names(pnad)) pnad$has_mobile   else NA_integer_,
    has_internet = if ("has_internet" %in% names(pnad)) pnad$has_internet else NA_integer_,
    uf_code      = if ("UF" %in% names(pnad)) as.integer(pnad$UF) else NA_integer_,
    is_capital   = if ("is_capital" %in% names(pnad)) pnad$is_capital else NA_integer_
  )
  out[age >= study$age_min]
}

# Pipeline: lê os arquivos processados e devolve uma única lista harmonizada
build_harmonized <- function() {
  # Vigitel
  if (file.exists(paths$vigitel_fst)) {
    vig <- fst::read_fst(paths$vigitel_fst, as.data.table = TRUE)
    h_vig <- harmonize_vigitel(vig)
    fst::write_fst(h_vig, file.path(paths$processed, "harm_vigitel.fst"), 75)
  } else { h_vig <- NULL }

  # PNS
  h_pns <- list()
  for (yr in study$pns_years) {
    f <- file.path(paths$pns_dir, sprintf("pns_%d_capitais.rds", yr))
    if (file.exists(f)) {
      d <- readRDS(f)
      h_pns[[as.character(yr)]] <- harmonize_pns(d, yr)
    }
  }
  if (length(h_pns)) {
    h_pns <- rbindlist(h_pns, fill = TRUE)
    fst::write_fst(h_pns, file.path(paths$processed, "harm_pns.fst"), 75)
  }

  # PNAD TIC
  h_pnad <- list()
  for (yr in study$pnad_tic_years) {
    f <- file.path(paths$pnad_dir, sprintf("pnad_tic_%d_T4.rds", yr))
    if (file.exists(f)) {
      d <- readRDS(f)
      h_pnad[[as.character(yr)]] <- harmonize_pnad(d, yr)
    }
  }
  if (length(h_pnad)) {
    h_pnad <- rbindlist(h_pnad, fill = TRUE)
    fst::write_fst(h_pnad, file.path(paths$processed, "harm_pnad.fst"), 75)
  }

  log_msg("Harmonização concluída. Arquivos em ", paths$processed)
  invisible(list(vigitel = h_vig, pns = h_pns, pnad = h_pnad))
}

if (sys.nframe() == 0L || identical(environment(), globalenv())) {
  build_harmonized()
}
