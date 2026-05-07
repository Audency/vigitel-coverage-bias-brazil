# 05b_coverage_published.R
# Componente 1 alternativo — usando valores publicados pela PNAD Contínua TIC
# (IBGE) em formato hardcoded. Mais leve e direto que microdados.
# Fontes: IBGE/PNAD Contínua TIC 2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024.
# ============================================================

source(here::here("config.R"))
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(scales); library(segmented)
})

# Domicílios com telefone fixo (% por região), PNAD-TIC IBGE
# Tabela compilada de relatórios anuais. Brasil rural+urbano (não só capitais).
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

cobertura_movel <- fread("
year,region,p_mobile
2019,Brasil,94.4
2021,Brasil,96.3
2023,Brasil,97.5
")

fwrite(cobertura_fixa,  file.path(paths$tab, "tab1_cobertura_fixa.csv"))
fwrite(cobertura_movel, file.path(paths$tab, "tab1b_cobertura_movel.csv"))

# AAPC por região (Norte, Nordeste, Sudeste, Sul, Centro-Oeste) com regressão
# log-linear simples sobre log(p_landline) ~ year
aapc_tbl <- cobertura_fixa[region != "Brasil",
  .(AAPC_pct = round((exp(coef(lm(log(p_landline) ~ year, data=.SD))[2]) - 1) * 100, 2),
    n_anos   = .N),
  by = region]
fwrite(aapc_tbl, file.path(paths$tab, "tab2_AAPC_landline_published.csv"))
print(aapc_tbl)

# Brasil global
aapc_br <- (exp(coef(lm(log(p_landline) ~ year, data=cobertura_fixa[region=="Brasil"]))[2]) - 1) * 100
log_msg("AAPC Brasil: ", round(aapc_br, 2), "% ao ano")

# z-test entre AAPCs (aproximação simples — ignora autocorrelação)
# SE estimado por bootstrap entre anos
boot_aapc <- function(df, R = 500) {
  out <- numeric(R)
  for (b in seq_len(R)) {
    idx <- sample(seq_len(nrow(df)), replace = TRUE)
    out[b] <- (exp(coef(lm(log(p_landline) ~ year, data = df[idx]))[2]) - 1) * 100
  }
  sd(out, na.rm = TRUE)
}
aapc_tbl[, SE := sapply(region, function(r) boot_aapc(cobertura_fixa[region == r]))]

regions_v <- unique(aapc_tbl$region)
ztest <- expand.grid(r1 = regions_v, r2 = regions_v, stringsAsFactors = FALSE)
ztest <- as.data.table(ztest)[r1 < r2]
ztest[, c("AAPC1","AAPC2","SE1","SE2") := list(
  aapc_tbl$AAPC_pct[match(r1, aapc_tbl$region)],
  aapc_tbl$AAPC_pct[match(r2, aapc_tbl$region)],
  aapc_tbl$SE[match(r1, aapc_tbl$region)],
  aapc_tbl$SE[match(r2, aapc_tbl$region)]
)]
ztest[, z := (AAPC1 - AAPC2) / sqrt(SE1^2 + SE2^2)]
ztest[, p := round(2*pnorm(-abs(z)), 4)]
ztest[, z := round(z, 3)]
fwrite(ztest, file.path(paths$tab, "tab3_AAPC_ztest.csv"))
print(ztest[, .(r1, r2, AAPC1, AAPC2, z, p)])

# ---------- Figura 1 ----------
fig1 <- ggplot(cobertura_fixa[region != "Brasil"],
               aes(year, p_landline/100, colour = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4) +
  geom_line(data = cobertura_fixa[region == "Brasil"],
            aes(year, p_landline/100), linetype = "dashed",
            colour = "black", linewidth = 0.8) +
  geom_text(data = cobertura_fixa[region == "Brasil" & year == 2023],
            aes(label = "Brasil"), nudge_y = 0.012, colour = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.50)) +
  scale_x_continuous(breaks = c(2016:2019, 2021:2023)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = NULL, y = "Domicílios com telefone fixo (%)",
       colour = NULL,
       title = "Declínio da telefonia fixa nos domicílios brasileiros, 2016–2023",
       subtitle = "PNAD Contínua TIC, IBGE — por grande região",
       caption = "Linha tracejada: Brasil. Adultos ≥18, todas as áreas (não restrito a capitais).") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0))

ggsave(file.path(paths$fig, "fig1_landline_by_region_published.png"),
       fig1, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(paths$fig, "fig1_landline_by_region_published.pdf"),
       fig1, width = 9, height = 5.5)
log_msg("Figura 1 (publicada) gravada em ", paths$fig)

# Razão de declínio Norte vs Sudeste
last <- cobertura_fixa[year == 2023]
cat("\nCobertura 2023:\n"); print(last)
cat("Razão Sudeste/Norte 2023:", round(last[region=="Sudeste"]$p_landline /
                                       last[region=="Norte"]$p_landline, 2), "\n")
