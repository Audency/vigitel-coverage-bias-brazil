# 05_component1_coverage.R
# COMPONENTE 1 — Evolução temporal da cobertura telefônica
# (fixa, móvel, internet) entre 2006-2024 por região, capitais + DF.
# Usa PNAD Contínua TIC (harmonizada). Tendências por joinpoint via segmented;
# AAPC e comparação entre regiões via z-test.
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(survey); library(srvyr); library(data.table); library(fst)
  library(dplyr); library(ggplot2); library(scales); library(segmented)
})

dat <- fst::read_fst(file.path(paths$processed, "harm_pnad.fst"),
                     as.data.table = TRUE)
# Restrição: capitais + DF, ≥18
dat <- dat[is_capital == 1L & age >= study$age_min]

# Mapa UF → região
uf_to_region <- data.table(
  uf_code = c(11,12,13,14,15,16,17,
              21,22,23,24,25,26,27,28,29,
              31,32,33,35,
              41,42,43,
              50,51,52,53),
  region  = c(rep("Norte",7), rep("Nordeste",9), rep("Sudeste",4),
              rep("Sul",3), rep("Centro-Oeste",4))
)
dat <- merge(dat, uf_to_region, by = "uf_code", all.x = TRUE, sort = FALSE)
setnames(dat, "region.y", "region", skip_absent = TRUE)

# Desenho amostral simplificado (PNAD contínua tem replicate weights;
# para análise descritiva agregada usamos pesos diretos com cuidado).
des <- dat |>
  as_survey_design(weights = weight)

# Cobertura por ano e região
cov_yr_reg <- des |>
  group_by(year, region) |>
  summarise(
    p_landline = survey_mean(has_landline, na.rm = TRUE, vartype = "ci"),
    p_mobile   = survey_mean(has_mobile,   na.rm = TRUE, vartype = "ci"),
    p_internet = survey_mean(has_internet, na.rm = TRUE, vartype = "ci"),
    n          = unweighted(n())
  ) |> as.data.table()

fwrite(cov_yr_reg, file.path(paths$tab, "tab1_coverage_year_region.csv"))
log_msg("Tabela 1 gravada.")

# Cobertura nacional (capitais + DF)
cov_yr <- des |>
  group_by(year) |>
  summarise(
    p_landline = survey_mean(has_landline, na.rm = TRUE, vartype = "ci"),
    p_mobile   = survey_mean(has_mobile,   na.rm = TRUE, vartype = "ci"),
    p_internet = survey_mean(has_internet, na.rm = TRUE, vartype = "ci"),
    n          = unweighted(n())
  ) |> as.data.table()
fwrite(cov_yr, file.path(paths$tab, "tab1b_coverage_year_total.csv"))

# ---------- Joinpoint via segmented ----------
# Para cada região, ajusta log(p_landline) ~ year com até 2 breakpoints.
fit_joinpoint <- function(df, max_breaks = 2) {
  df <- df[!is.na(p_landline) & p_landline > 0]
  if (nrow(df) < 5) return(NULL)
  m0 <- lm(log(p_landline) ~ year, data = df)
  best <- list(model = m0, k = 0, bic = BIC(m0))
  for (k in 1:max_breaks) {
    fit <- tryCatch(
      segmented::segmented(m0, seg.Z = ~year, npsi = k),
      error = function(e) NULL
    )
    if (!is.null(fit) && BIC(fit) < best$bic) best <- list(model = fit, k = k, bic = BIC(fit))
  }
  best
}

aapc_from_segmented <- function(fit, df) {
  # AAPC global: derivada do slope médio ponderado pelos comprimentos dos segmentos
  if (inherits(fit$model, "segmented")) {
    slopes <- segmented::slope(fit$model)$year[, "Est."]
    # Aproximação simples — ponderar por comprimento
    bp     <- fit$model$psi[, "Est."]
    yrs    <- range(df$year)
    cuts   <- c(yrs[1], bp, yrs[2])
    lens   <- diff(cuts); lens[lens < 0] <- 0
    wgt    <- lens / sum(lens)
    aapc   <- sum(wgt * (exp(slopes) - 1)) * 100
  } else {
    s <- coef(fit$model)["year"]
    aapc <- (exp(s) - 1) * 100
  }
  aapc
}

regions <- unique(cov_yr_reg$region)
aapc_tbl <- data.table()
for (r in regions) {
  df <- cov_yr_reg[region == r]
  fit <- fit_joinpoint(df, max_breaks = 2)
  if (is.null(fit)) next
  aapc <- aapc_from_segmented(fit, df)
  aapc_tbl <- rbind(aapc_tbl, data.table(
    region = r, k_joinpoints = fit$k,
    AAPC_pct = round(aapc, 2),
    BIC = round(fit$bic, 1)
  ))
}
fwrite(aapc_tbl, file.path(paths$tab, "tab2_AAPC_landline.csv"))
print(aapc_tbl)

# ---------- z-test entre AAPCs (Kim et al. 2004 aproximação) ----------
# Sob H0 de igualdade, z = (AAPC1 − AAPC2) / sqrt(SE1^2 + SE2^2). Aqui usamos
# bootstrap entre anos para estimar SE de AAPC.
boot_aapc <- function(df, R = 500) {
  out <- numeric(R)
  for (b in seq_len(R)) {
    idx <- sample(seq_len(nrow(df)), replace = TRUE)
    fit <- fit_joinpoint(df[idx], max_breaks = 1)
    out[b] <- if (!is.null(fit)) aapc_from_segmented(fit, df[idx]) else NA_real_
  }
  sd(out, na.rm = TRUE)
}
aapc_tbl[, SE := sapply(region, function(r) {
  boot_aapc(cov_yr_reg[region == r], R = 200)
})]

ztest <- expand.grid(r1 = regions, r2 = regions, stringsAsFactors = FALSE) |>
  filter(r1 < r2)
ztest <- as.data.table(ztest)
ztest[, c("z","p") := {
  i <- match(r1, aapc_tbl$region); j <- match(r2, aapc_tbl$region)
  d <- aapc_tbl$AAPC_pct[i] - aapc_tbl$AAPC_pct[j]
  s <- sqrt(aapc_tbl$SE[i]^2 + aapc_tbl$SE[j]^2)
  list(round(d/s, 3), round(2*pnorm(-abs(d/s)), 4))
}]
fwrite(ztest, file.path(paths$tab, "tab3_AAPC_ztest.csv"))

# ---------- Figura 1: cobertura de fixa por região, 2006-2024 ----------
fig1 <- ggplot(cov_yr_reg, aes(year, p_landline, colour = region, fill = region)) +
  geom_ribbon(aes(ymin = p_landline_low, ymax = p_landline_upp),
              alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2016, 2024, 2)) +
  labs(x = NULL, y = "Cobertura domiciliar de telefonia fixa",
       colour = NULL, fill = NULL,
       title = "Declínio da telefonia fixa em capitais brasileiras + DF",
       subtitle = "PNAD Contínua TIC, IBGE",
       caption = "Capitais + DF, adultos ≥18 anos.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(paths$fig, "fig1_landline_by_region.png"), fig1,
       width = 8, height = 5, dpi = 300)
ggsave(file.path(paths$fig, "fig1_landline_by_region.pdf"), fig1,
       width = 8, height = 5)
log_msg("Figura 1 gravada.")
