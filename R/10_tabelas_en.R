# =============================================================================
# 10_tabelas_en.R
# O QUE FAZ : Versao em ingles das seis tabelas do manuscrito. Le exatamente os
#             mesmos objetos derivados da versao PT e nao recalcula nada: so
#             traduz rotulos, notas e formatacao numerica (ponto decimal).
# ENTRADAS  : data/derivado/*.rds
# SAIDAS    : output/tables_en/table{1..6}.{rds,docx,html} + *_data.rds
# =============================================================================

source(here::here("R", "00_setup.R"))
source(here::here("R", "labels_en.R"))

h1("10_tabelas_en.R")

desc  <- readRDS(here::here("data", "derivado", "descritivas.rds"))
comp  <- readRDS(here::here("data", "derivado", "comparacoes.rds"))
prev  <- readRDS(here::here("data", "derivado", "prevalencias.rds"))
vies  <- readRDS(here::here("data", "derivado", "vies_ncob.rds"))
part  <- readRDS(here::here("data", "derivado", "particao.rds"))
sim   <- readRDS(here::here("data", "derivado", "simulacao.rds"))
val23 <- readRDS(here::here("data", "derivado", "validacao_2023.rds"))

salva_en <- function(g, nome, dados = NULL) salva_tabela(g, nome, dados, dir = DIR_TAB_EN)

# =============================================================================
# TABLE 1 - sample characteristics
# =============================================================================
h2("Table 1")

n_vig <- desc$n$n[desc$n$inquerito == "Vigitel"]
n_pns <- desc$n$n[desc$n$inquerito == "PNS"]

NIVEIS_T1 <- c("Mean (SD)", "Men", "Women", FAIXAS_IDADE, FAIXAS_ESC,
               "White", "Black", "Brown (parda)", "Asian", "Indigenous", REG_EN)

corpo_cat <- desc$categoricas |>
  dplyr::mutate(celula = fmt_ic_en(est, low, upp, 1)) |>
  dplyr::select(variavel, nivel, inquerito, celula, smd) |>
  tidyr::pivot_wider(names_from = inquerito, values_from = celula)

corpo_idade <- desc$idade |>
  dplyr::mutate(celula = paste0(fmt_num_en(est, 1), " (", fmt_num_en(dp, 1), ")")) |>
  dplyr::select(variavel, nivel, inquerito, celula, smd) |>
  tidyr::pivot_wider(names_from = inquerito, values_from = celula)

t1_dados <- dplyr::bind_rows(corpo_idade, corpo_cat) |>
  dplyr::mutate(group = factor(tr(variavel, VAR_EN), levels = unname(VAR_EN)),
                level = factor(tr(nivel, NIVEL_EN), levels = NIVEIS_T1)) |>
  dplyr::arrange(group, level) |>
  dplyr::select(group, level, Vigitel, PNS, smd)

stopifnot(!any(is.na(t1_dados$level)), !any(is.na(t1_dados$group)))

tab1 <- t1_dados |>
  gt::gt(groupname_col = "group", rowname_col = "level") |>
  gt::cols_label(
    Vigitel = gt::md(glue::glue("**Vigitel {ANO_VIGITEL}**<br>27 capitals<br>n = {big_en(n_vig)}<br>% (95% CI)")),
    PNS = gt::md(glue::glue("**PNS {ANO_PNS}**<br>27 capitals<br>n = {big_en(n_pns)}<br>% (95% CI)")),
    smd = gt::md("**SMD**")) |>
  gt::fmt_number(columns = smd, decimals = 2, dec_mark = ".") |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = c(Vigitel, PNS, smd)) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Table 1.** Characteristics of the adult samples (aged 18 years or over) of the 27 capitals, ",
    "Vigitel {ANO_VIGITEL} and PNS {ANO_PNS}"))) |>
  gt::tab_footnote("The age row reports the weighted mean and standard deviation; all other rows report percentages with 95% confidence intervals.",
                   locations = gt::cells_column_labels(columns = Vigitel)) |>
  gt::tab_footnote("SMD = standardised mean difference between the two surveys, computed row by row for proportions and from the mean and standard deviation for age. Absolute values of 0.10 or above indicate relevant imbalance.",
                   locations = gt::cells_column_labels(columns = smd)) |>
  gt::tab_footnote(paste(
    "The Vigitel post-stratification weight is calibrated by age group, education and sex",
    "(Ministry of Health, Guidance for the analysis of Vigitel data, item 4.5). A standardised",
    "difference close to zero for these three variables follows from the calibration and does not",
    "indicate that the covered populations are equivalent. Race/skin colour and region do not enter",
    "the calibration."),
    locations = gt::cells_row_groups(groups = "Education, years of schooling")) |>
  gt::tab_source_note(gt::md(paste(SOURCE_BASE_EN, "Missing responses were excluded from the denominator of the respective variable."))) |>
  estilo_gt() |>
  gt::tab_style(gt::cell_text(weight = "bold"),
                gt::cells_body(columns = smd, rows = abs(smd) >= 0.10))
salva_en(tab1, "table1", t1_dados)

# =============================================================================
# TABLE 2 - crude and standardised prevalences
# =============================================================================
h2("Table 2")

t2_dados <- comp$comparacoes |>
  dplyr::left_join(prev |> dplyr::select(indicador, estrato, inquerito, low, upp) |>
                     tidyr::pivot_wider(names_from = inquerito, values_from = c(low, upp)),
                   by = c("indicador", "estrato")) |>
  dplyr::left_join(comp$padronizadas |> dplyr::select(indicador, vig_padr, pns_padr,
                                                      delta_padr, delta_padr_low, delta_padr_upp),
                   by = "indicador") |>
  dplyr::mutate(
    stratum = factor(tr(estrato, SEXO_EN), levels = c("Overall", "Men", "Women")),
    label = factor(tr(rotulo, IND_EN), levels = INDICADORES$rotulo_en),
    c_vig = fmt_ic_en(est_Vigitel, low_Vigitel, upp_Vigitel, 1),
    c_pns = fmt_ic_en(est_PNS, low_PNS, upp_PNS, 1),
    c_dif = fmt_ic_en(delta, delta_low, delta_upp, 2),
    c_rp  = fmt_ic_en(rp, rp_low, rp_upp, 2),
    c_padr = dplyr::if_else(stratum == "Overall",
                            fmt_ic_en(delta_padr, delta_padr_low, delta_padr_upp, 2), NA_character_),
    c_p = dplyr::if_else(stratum == "Overall", fmt_p_en(p_holm), NA_character_)) |>
  dplyr::arrange(label, stratum)

tab2 <- t2_dados |>
  dplyr::select(label, stratum, c_vig, c_pns, c_dif, c_padr, c_rp, c_p) |>
  gt::gt(groupname_col = "label", rowname_col = "stratum") |>
  gt::cols_label(
    c_vig = gt::md(glue::glue("**Vigitel {ANO_VIGITEL}**<br>% (95% CI)")),
    c_pns = gt::md(glue::glue("**PNS {ANO_PNS}**<br>% (95% CI)")),
    c_dif = gt::md("**Crude Δ**<br>pp (95% CI)"),
    c_padr = gt::md("**Age-standardised Δ**<br>pp (95% CI)"),
    c_rp = gt::md("**PR**<br>(95% CI)"),
    c_p = gt::md("**p**<br>(Holm)")) |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = dplyr::starts_with("c_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Table 2.** Prevalence of the indicators among adults (aged 18 years or over) of the 27 ",
    "capitals, with crude and age-standardised absolute differences and prevalence ratios"))) |>
  gt::tab_footnote("Δ = Vigitel − PNS, in percentage points (pp). A negative value indicates a lower prevalence in Vigitel.",
                   locations = gt::cells_column_labels(columns = c_dif)) |>
  gt::tab_footnote(glue::glue("Direct standardisation to the age structure of the adult population of the 27 capitals in the {ANO_CENSO} Census, applied to both surveys."),
                   locations = gt::cells_column_labels(columns = c_padr)) |>
  gt::tab_footnote("PR = prevalence ratio (Vigitel divided by PNS); interval obtained by the delta method on the logarithmic scale.",
                   locations = gt::cells_column_labels(columns = c_rp)) |>
  gt::tab_footnote("Two-sided test of the null hypothesis of no difference, treating the surveys as independent samples, with Holm correction over the four principal tests (Overall rows). The sex-specific rows do not enter the correction; asymmetry between sexes is assessed by the interaction test reported in a footnote to each indicator.",
                   locations = gt::cells_column_labels(columns = c_p))
for (i in seq_len(nrow(comp$interacao))) {
  tab2 <- tab2 |> gt::tab_footnote(
    glue::glue("Sex × survey interaction: p = {fmt_p_en(comp$interacao$p_int[i])} ",
               "(Holm-adjusted p = {fmt_p_en(comp$interacao$p_int_holm[i])}), from a logistic model on the ",
               "stacked data with the design of each survey preserved."),
    locations = gt::cells_row_groups(groups = tr(comp$interacao$rotulo[i], IND_EN)))
}
tab2 <- tab2 |>
  gt::tab_source_note(gt::md(paste(SOURCE_BASE_EN, "Indicator definitions and item equivalence are given in Table S1."))) |>
  estilo_gt()
salva_en(tab2, "table2", t2_dados)

# =============================================================================
# TABLE 3 - non-coverage bias
# =============================================================================
h2("Table 3")

t3_dados <- vies |>
  dplyr::mutate(label = factor(tr(rotulo, IND_EN), levels = INDICADORES$rotulo_en),
                domain = factor(tr(dominio, DOMINIO_EN), levels = unname(DOMINIO_EN)),
                c_fixo = fmt_ic_en(est_p_fixo, p_fixo_low, p_fixo_upp, 1),
                c_sem = fmt_ic_en(est_p_semfixo, p_sem_low, p_sem_upp, 1),
                c_f = fmt_ic_en(est_f_semfixo, f_low, f_upp, 1),
                c_vies = fmt_ic_en(est_vies, vies_low, vies_upp, 2),
                c_coch = paste0(fmt_num_en(cochran_vigitel, 2),
                                dplyr::if_else(acima_limiar, " *", ""))) |>
  dplyr::arrange(label, domain)

tab3 <- t3_dados |>
  dplyr::select(label, domain, c_fixo, c_sem, c_f, c_vies, c_coch) |>
  gt::gt(groupname_col = "label", rowname_col = "domain") |>
  gt::cols_label(c_fixo = gt::md("**With a landline**<br>% (95% CI)"),
                 c_sem = gt::md("**Without a landline**<br>% (95% CI)"),
                 c_f = gt::md("**Population without<br>a landline**<br>% (95% CI)"),
                 c_vies = gt::md("**Non-coverage bias**<br>pp (95% CI)"),
                 c_coch = gt::md("**Cochran's relative<br>bias**")) |>
  gt::cols_align("center", columns = dplyr::starts_with("c_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Table 3.** Non-coverage bias of the landline frame, estimated within the PNS {ANO_PNS}, ",
    "by indicator and region — adults (aged 18 years or over) of the 27 capitals"))) |>
  gt::tab_footnote("Non-coverage bias = the proportion of the population without a landline multiplied by the difference in prevalence between those with and those without a landline. A positive value indicates that a survey restricted to the landline frame would overestimate the population prevalence; a negative value, that it would underestimate it.",
                   locations = gt::cells_column_labels(columns = c_vies)) |>
  gt::tab_footnote(glue::glue(
    "Cochran's relative bias = the absolute bias divided by the standard error of the estimate. Above ",
    "{fmt_num_en(LIMIAR_COCHRAN, 2)} the nominal 95% level of the intervals degrades; cells above the ",
    "threshold are marked with an asterisk ({sum(vies$acima_limiar)} of {nrow(vies)}). The ratio shown uses ",
    "the standard error that Vigitel actually has for the indicator, an operational reading of the ",
    "criterion; the ratio using the internal standard error of the PNS is given in Table S4."),
    locations = gt::cells_column_labels(columns = c_coch)) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Source: PNS {ANO_PNS} (IBGE), adults of the 26 state capitals and the Federal District. No quantity ",
    "in this table depends on Vigitel: estimation is entirely internal to the PNS. Intervals from a ",
    "Rao-Wu bootstrap over the sampling design, with {big_en(N_BOOT)} replicates and a fixed seed."))) |>
  estilo_gt()
salva_en(tab3, "table3", t3_dados)

# =============================================================================
# TABLE 4 - partition
# =============================================================================
h2("Table 4")

t4_dados <- part$particao |>
  dplyr::mutate(label = factor(tr(rotulo, IND_EN), levels = INDICADORES$rotulo_en)) |>
  dplyr::arrange(label) |>
  dplyr::mutate(c_gap = fmt_num_en(gap_total, 2),
                c_comp = fmt_ic_en(componente, comp_low, comp_upp, 2),
                c_res = fmt_ic_en(residuo, residuo_low, residuo_upp, 2),
                c_pct = tr(pct_txt, PCT_TXT_EN),
                c_rem = paste0(fmt_num_en(pct_removido, 0), "%"))

tab4 <- t4_dados |>
  dplyr::select(label, c_gap, c_comp, c_res, c_pct, c_rem) |>
  gt::gt(rowname_col = "label") |>
  gt::cols_label(c_gap = gt::md("**Total gap**<br>pp"),
                 c_comp = gt::md("**Non-coverage<br>component**<br>pp (95% CI)"),
                 c_res = gt::md("**Residual**<br>pp (95% CI)"),
                 c_pct = gt::md("**Share of the gap<br>attributable to<br>non-coverage**"),
                 c_rem = gt::md("**Coverage bias removed<br>by post-stratification**")) |>
  gt::cols_align("center", columns = dplyr::starts_with("c_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Table 4.** Partition of the difference between Vigitel {ANO_VIGITEL} and PNS {ANO_PNS} into a ",
    "non-coverage component and a residual — adults (aged 18 years or over) of the 27 capitals"))) |>
  gt::tab_footnote("Total gap = prevalence in Vigitel minus prevalence in the PNS, in percentage points. It reproduces the crude difference column of Table 2.",
                   locations = gt::cells_column_labels(columns = c_gap)) |>
  gt::tab_footnote("Component estimated entirely within the PNS (Table 3): the bias that a survey restricted to the landline frame would incur by excluding the population without a landline.",
                   locations = gt::cells_column_labels(columns = c_comp)) |>
  gt::tab_footnote("Residual = total gap minus the non-coverage component, that is, the share of the difference not attributable to telephone non-coverage. It jointly absorbs mode of data collection, self-report, instrument differences, non-response and weight calibration; this design does not allow them to be separated. The interval is obtained by summing the variances of Vigitel and of the corresponding quantity in the PNS, the latter from the same bootstrap that produced the component, so as to respect the correlation between the two parts.",
                   locations = gt::cells_column_labels(columns = c_res)) |>
  gt::tab_footnote("When the component has the opposite sign to the observed gap, or exceeds it in magnitude, the ratio between the two is no longer interpretable as a fraction and the situation is described in words.",
                   locations = gt::cells_column_labels(columns = c_pct)) |>
  gt::tab_footnote("Share of the coverage bias that the Vigitel post-stratification removes, comparing the estimate a landline frame would produce without weighting with the one actually published, taking the PNS as the reference.",
                   locations = gt::cells_column_labels(columns = c_rem)) |>
  gt::tab_source_note(gt::md(paste(SOURCE_BASE_EN, "Sign convention: Δ = Vigitel − PNS."))) |>
  estilo_gt()
salva_en(tab4, "table4", t4_dados)

# =============================================================================
# TABLE 5 - simulation performance
# =============================================================================
h2("Table 5")

t5_dados <- sim |>
  dplyr::group_by(region, cenario, rotulo) |>
  dplyr::summarise(cobertura = mean(cobertura), vies = mean(vies), empse = mean(empse),
                   rmse = mean(rmse), mcse = mean(mcse_rmse), .groups = "drop") |>
  dplyr::mutate(region_en = factor(tr(region, REGIOES_EN), levels = REG_EN),
                label = factor(tr(rotulo, IND_EN), levels = INDICADORES$rotulo_en),
                scenario = factor(as.character(cenario), levels = paste0("S", 0:4))) |>
  dplyr::arrange(label, region_en, scenario)

tab5 <- t5_dados |>
  dplyr::transmute(label, region_en, scenario,
                   Coverage = fmt_num_en(cobertura, 1),
                   Bias = fmt_num_en(vies, 3),
                   `Empirical SE` = fmt_num_en(empse, 3),
                   RMSE = paste0(fmt_num_en(rmse, 3), " (", fmt_num_en(mcse, 3), ")")) |>
  gt::gt(groupname_col = "label") |>
  gt::cols_label(region_en = gt::md("**Region**"), scenario = gt::md("**Scenario**"),
                 Coverage = gt::md("**Coverage**<br>%"), Bias = gt::md("**Bias**"),
                 `Empirical SE` = gt::md("**Empirical SE**"), RMSE = gt::md("**RMSE (MCSE)**")) |>
  gt::cols_align("center", columns = -c(label, region_en)) |>
  gt::tab_header(title = gt::md(
    "**Table 5.** Performance of the sampling-frame scenarios in the simulation, by region and indicator")) |>
  gt::tab_footnote(glue::glue(
    "Simulation under the ADEMP framework with {big_en(N_REPLICAS)} replicates per scenario, region and ",
    "indicator, with a fixed seed. Values in percentage points. RMSE = root mean squared error, the ",
    "primary performance measure; in parentheses, the Monte Carlo standard error, which measures the ",
    "uncertainty due to the finite number of replicates and not sampling uncertainty."),
    locations = gt::cells_column_labels(columns = RMSE)) |>
  gt::tab_footnote("S0, landline only; S1, mobile only; S2, dual frame; S3, triple frame including internet access; S4, full multimodal. Scenario S1 is not a subset of S0: it has its own non-coverage profile, and bias of opposite sign between them is a legitimate result.",
                   locations = gt::cells_column_labels(columns = scenario)) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Pseudo-population: PNS {ANO_PNS} restricted to the 27 capitals and expanded by the sampling weights. ",
    "Telephone ownership is not simulated: the ownership observed in the PNS is used. Parameters of the ",
    "data-generating mechanism are given in Table S5."))) |>
  estilo_gt()
salva_en(tab5, "table5", t5_dados)

# =============================================================================
# TABLE 6 - 2023 validation
# =============================================================================
h2("Table 6")

t6_dados <- val23$confronto |>
  dplyr::mutate(label = factor(tr(rotulo, IND_EN), levels = INDICADORES$rotulo_en)) |>
  dplyr::arrange(label)

tab6 <- t6_dados |>
  dplyr::transmute(label,
                   predicted = fmt_num_en(vies_residual_2019, 2),
                   observed = fmt_ic_en(obs_2023, fd_low, fd_upp, 2),
                   agrees = dplyr::if_else(sign(vies_residual_2019) == sign(obs_2023), "yes", "no"),
                   removed = paste0(fmt_num_en(pct_removido_2019, 0), "%")) |>
  gt::gt(rowname_col = "label") |>
  gt::cols_label(predicted = gt::md(glue::glue("**Predicted**<br>from {ANO_VIGITEL}<br>pp")),
                 observed = gt::md(glue::glue("**Observed**<br>in the {ANO_VIG_DUAL} transition<br>pp (95% CI)")),
                 agrees = gt::md("**Sign<br>agreement**"),
                 removed = gt::md(glue::glue("**Bias removed by<br>post-stratification<br>in {ANO_VIGITEL}**"))) |>
  gt::cols_align("center", columns = -label) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Table 6.** Validation of the prediction against the real transition of the system: difference ",
    "predicted from {ANO_VIGITEL} and observed at the adoption of the dual frame in {ANO_VIG_DUAL}"))) |>
  gt::tab_footnote(glue::glue(
    "Predicted: the residual estimated in {ANO_VIGITEL}, that is, the difference between the estimate ",
    "published by Vigitel and the population prevalence measured in the PNS."),
    locations = gt::cells_column_labels(columns = predicted)) |>
  gt::tab_footnote(glue::glue(
    "Observed: the difference between the legacy design reconstructed within the {ANO_VIG_DUAL} edition ",
    "(landline-frame interviews with the pesorake_fixo weight) and the one published under the dual frame ",
    "(pesorake weight), holding time constant. Intervals from a bootstrap resampling within each frame, ",
    "{big_en(N_BOOT)} replicates, respecting the correlation between the two arms, which share interviews."),
    locations = gt::cells_column_labels(columns = observed)) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Pearson correlation between predicted and observed: ",
    "{fmt_num_en(stats::cor(t6_dados$vies_residual_2019, t6_dados$obs_2023), 2)}. With four indicators, ",
    "the correlation is descriptive and does not constitute a hypothesis test."))) |>
  estilo_gt()
salva_en(tab6, "table6", t6_dados)

cat("\ntabelas em output/tables_en/:\n"); print(list.files(DIR_TAB_EN, pattern = "^table.*\\.docx$"))
message("10_tabelas_en.R concluido.")
