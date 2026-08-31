# =============================================================================
# 12_suplemento_en.R
# O QUE FAZ : Versao em ingles do material suplementar (S1..S7 + figuras S1..S3
#             + textos). Le os mesmos objetos e logs da versao PT; nenhum numero
#             e recalculado. Os enunciados dos questionarios sao traduzidos pelos
#             autores; os codigos de variavel e as rotinas oficiais ficam como no
#             original.
# ENTRADAS  : data/derivado/*.rds, output/logs/*.csv, output/figures_en/*
# SAIDAS    : output/supplement/supplementary_material_en.docx,
#             output/tables_en/tableS*.{rds,docx,html}
# =============================================================================

source(here::here("R", "00_setup.R"))
source(here::here("R", "labels_en.R"))
library(officer)

h1("12_suplemento_en.R")
dir.create(here::here("output", "supplement"), showWarnings = FALSE, recursive = TRUE)

de_para <- readRDS(here::here("data", "derivado", "de_para.rds"))
vies    <- readRDS(here::here("data", "derivado", "vies_ncob.rds"))
sim     <- readRDS(here::here("data", "derivado", "simulacao.rds"))
pns     <- readRDS(here::here("data", "derivado", "pns.rds"))
vig     <- readRDS(here::here("data", "derivado", "vigitel.rds"))

W_NS <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
add_gt <- function(doc, g) {
  ti <- g[["_heading"]]$title
  if (!is.null(ti)) doc <- officer::body_add_par(doc, gsub("\\*\\*", "", as.character(ti)),
                                                 style = "Table Caption")
  raiz <- xml2::read_xml(paste0("<f xmlns:w='", W_NS, "'>", gt::as_word(g), "</f>"))
  for (no in xml2::xml_children(raiz))
    if (xml2::xml_name(no) == "tbl") doc <- officer::body_add_xml(doc, str = as.character(no))
  doc
}
p_ <- function(d, t, s = "Normal") officer::body_add_par(d, t, style = s)
h_ <- function(d, t, n = 1) officer::body_add_par(d, t, style = paste("heading", n))
br_ <- function(d) officer::body_add_par(d, "", style = "Normal")

simples <- function(dados, titulo, nota = NULL) {
  g <- dados |> gt::gt() |> gt::tab_header(title = gt::md(titulo)) |>
    gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |> estilo_gt()
  if (!is.null(nota)) g <- g |> gt::tab_source_note(gt::md(nota))
  g
}
salva_en <- function(g, nome, dados = NULL) salva_tabela(g, nome, dados, dir = DIR_TAB_EN)

# =============================================================================
# S1 - item-equivalence dictionary
# =============================================================================
h2("S1 - item equivalence")

# Traducao dos enunciados e categorias. Os codigos de variavel e a sintaxe
# oficial do Vigitel vem de de_para.rds e nao sao traduzidos.
de_para_en <- tibble::tribble(
  ~indicador, ~ind_en, ~enun_pns_en, ~cat_pns_en, ~enun_vig_en, ~cat_vig_en, ~cod_en, ~div_en,

  "Tabagismo atual", "Current smoking",
  "Do you currently smoke any tobacco product?",
  "1 = Yes, daily; 2 = Yes, less than daily; 3 = I do not currently smoke; 9 = Not reported",
  "smoker",
  "1 = yes, daily; 2 = yes, but not daily; 3 = no",
  "1 = currently smokes (daily or less than daily); 0 = does not smoke",
  "The PNS asks about 'any tobacco product'; Vigitel asks about cigarettes. A difference in the scope of the product, not in the structure of the response.",

  "Hipertensao diagnosticada", "Diagnosed hypertension",
  "Has a doctor ever given you a diagnosis of arterial hypertension (high blood pressure)?",
  "1 = Yes; 2 = No; 9 = Not reported",
  "high blood pressure",
  "1 = yes; 2 = no; 777 = does not know",
  "1 = reported medical diagnosis; 0 = no",
  "The PNS has a filter for hypertension occurring exclusively during pregnancy (Q00202), which Vigitel does not have. Vigitel has code 777 'does not know' (44 cases), which the official routine counts as non-cases; in the harmonised version they are removed from the denominator, as with the PNS 'Not reported'.",

  "Diabetes diagnosticado", "Diagnosed diabetes",
  "Has a doctor ever given you a diagnosis of diabetes?",
  "1 = Yes; 2 = No; 9 = Not reported",
  "diabetes",
  "1 = yes; 2 = no; 777 = does not know",
  "1 = reported medical diagnosis; 0 = no",
  "The same pregnancy-related question (Q03002) and the same treatment of code 777 (58 cases) described for hypertension.",

  "Autoavaliacao ruim de saude", "Poor self-rated health",
  "In general, how do you rate your health?",
  "1 = Very good; 2 = Good; 3 = Fair; 4 = Poor; 5 = Very poor; 9 = Not reported",
  "health status",
  "1 = very good; 2 = good; 3 = fair; 4 = poor; 5 = very poor; 777 = does not know; 888 = declined to answer",
  "1 = 'Poor' or 'Very poor'; 0 = 'Very good', 'Good' or 'Fair'",
  "The five-point scale is identical in the two surveys. Vigitel adds codes 777/888 (562 cases), handled as above.",

  "Escolaridade", "Education",
  "Highest level of education attained (persons aged 5 years or over), standardised to the nine-year primary school system",
  "1 = No schooling; 2 = Incomplete primary or equivalent; 3 = Complete primary or equivalent; 4 = Incomplete secondary or equivalent; 5 = Complete secondary or equivalent; 6 = Incomplete tertiary or equivalent; 7 = Complete tertiary",
  "education (bands of years of schooling)",
  "1 = 0 to 8 years; 2 = 9 to 11 years; 3 = 12 years or more",
  "0-8 = up to complete primary; 9-11 = secondary; 12+ = tertiary",
  "The PNS collects level of education; Vigitel collects self-reported years of schooling. The Vigitel bands (0 to 8 / 9 to 11 / 12 or more) place the boundary for complete secondary schooling at 11 years, and the PNS mapping follows that boundary. The alternative under the nine-year system is tested in the sensitivity analysis.",

  "Posse de telefone", "Telephone ownership",
  "Does this household have a conventional landline telephone? | Does this household have a mobile telephone?",
  "1 = Yes; 2 = No; 9 = Not reported | 1 = Yes; 2 = No; 9 = Not reported",
  "(not applicable)",
  "(not applicable)",
  "Landline (with or without a mobile) / Mobile only / None",
  "The variable exists only in the PNS. It is the basis of the whole non-coverage estimation: Vigitel, by construction, interviews only those who have a telephone."
)

stopifnot(setequal(de_para_en$indicador, de_para$indicador))

tS1 <- de_para |>
  dplyr::left_join(de_para_en, by = "indicador") |>
  dplyr::transmute(Indicator = ind_en,
                   PNS = paste0(var_pns, ": ", enun_pns_en, " [", cat_pns_en, "]"),
                   Vigitel = paste0(var_vigitel, ": ", enun_vig_en, " [", cat_vig_en, "]",
                                    dplyr::if_else(rotina_vigitel == "(nao aplicavel)", "",
                                                   paste0(" — official routine: ", rotina_vigitel))),
                   `Coding adopted` = cod_en,
                   `Observed divergence` = div_en) |>
  simples("**Table S1.** Item-equivalence dictionary between the two instruments",
          paste("Variable codes and the official Vigitel syntax are reproduced verbatim from the",
                "official data dictionaries of each survey. The item wording and the response",
                "categories were translated from Portuguese by the authors; the originals are",
                "reproduced in the Portuguese version of this supplement."))
salva_en(tS1, "tableS1")

# =============================================================================
# S2 - external validation
# =============================================================================
h2("S2 - external validation")
val_pns <- le_log("validacao_pns_sidra"); val_vig <- le_log("validacao_vigitel")
tS2 <- dplyr::bind_rows(
  val_pns |> dplyr::transmute(Source = "PNS 2019 vs SIDRA (IBGE)", Indicator = tr(indicador, VALID_IND_EN),
                              Stratum = tr(sex, SEXO_EN), Ours = fmt_num_en(ours, 2),
                              Published = fmt_num_en(published, 2), `Difference (pp)` = fmt_num_en(diferenca, 3)),
  val_vig |> dplyr::transmute(Source = "Vigitel 2019 vs official report", Indicator = tr(indicador, VALID_IND_EN),
                              Stratum = tr(sex, SEXO_EN), Ours = fmt_num_en(ours, 2),
                              Published = fmt_num_en(published, 2), `Difference (pp)` = fmt_num_en(diferenca, 3))
) |>
  simples("**Table S2.** External validation: our own estimates against the published values",
          glue::glue("Maximum absolute difference: {fmt_num_en(max(abs(val_pns$diferenca)),3)} pp in the PNS and ",
                     "{fmt_num_en(max(abs(val_vig$diferenca)),3)} pp in Vigitel. No discrepancy reaches 0.5 pp."))
salva_en(tS2, "tableS2")

# =============================================================================
# S3 - weight diagnostics
# =============================================================================
h2("S3 - weight diagnostics")
diag_peso <- function(w, rotulo, n) {
  tibble::tibble(Survey = rotulo, n = big_en(n),
                 `Sum of weights` = big_en(round(sum(w))),
                 Minimum = fmt_num_en(min(w), 1), Median = fmt_num_en(stats::median(w), 1),
                 Maximum = fmt_num_en(max(w), 1),
                 `Max/min ratio` = big_en(round(max(w) / min(w))),
                 `Kish design effect` = fmt_num_en(length(w) * sum(w^2) / sum(w)^2, 2))
}
tS3_dados <- dplyr::bind_rows(diag_peso(vig$weight, glue::glue("Vigitel {ANO_VIGITEL}"), nrow(vig)),
                              diag_peso(pns$weight, glue::glue("PNS {ANO_PNS} (capitals)"), nrow(pns)))
print(as.data.frame(tS3_dados))
tS3 <- tS3_dados |>
  simples("**Table S3.** Diagnostics of the sampling weights",
          paste("The Kish design effect measures the loss of precision due to the variability of the",
                "weights: a value of 1 indicates uniform weights. The ratio between the largest and the",
                "smallest weight quantifies the magnitude of the correction each survey needs to apply."))
salva_en(tS3, "tableS3", tS3_dados)

# =============================================================================
# S4 - non-coverage bias by region, both Cochran readings
# =============================================================================
h2("S4 - bias by region")
tS4 <- vies |>
  dplyr::transmute(Indicator = tr(rotulo, IND_EN), Domain = tr(dominio, DOMINIO_EN),
                   `Population (%)` = fmt_num_en(est_p_pns, 1),
                   `With a landline (%)` = fmt_ic_en(est_p_fixo, p_fixo_low, p_fixo_upp, 1),
                   `Without a landline (%)` = fmt_ic_en(est_p_semfixo, p_sem_low, p_sem_upp, 1),
                   `Without a landline, % of pop.` = fmt_ic_en(est_f_semfixo, f_low, f_upp, 1),
                   `Bias (pp)` = fmt_ic_en(est_vies, vies_low, vies_upp, 2),
                   `Cochran (PNS SE)` = fmt_num_en(cochran_pns, 2),
                   `Cochran (Vigitel SE)` = fmt_num_en(cochran_vigitel, 2)) |>
  simples(glue::glue("**Table S4.** Non-coverage bias by indicator and region, PNS {ANO_PNS}"),
          glue::glue("The last two columns give Cochran's relative bias under the two possible denominators. ",
                     "The degradation threshold is {fmt_num_en(LIMIAR_COCHRAN,2)} in both. Intervals from a ",
                     "Rao-Wu bootstrap with {big_en(N_BOOT)} replicates."))
salva_en(tS4, "tableS4")

# =============================================================================
# S5 - parameters of the data-generating mechanism
# =============================================================================
h2("S5 - data-generating mechanism")
mec <- le_log("mecanismo_posse_desfecho"); cob <- le_log("cobertura_quadros_regiao")
tic <- le_log("conferencia_cobertura_pns_tic")
tS5 <- dplyr::bind_rows(
  cob |> tidyr::pivot_longer(-region, names_to = "linha", values_to = "v") |>
    dplyr::transmute(Block = "Coverage of each frame, by region (%)",
                     Item = paste0(tr(linha, QUADRO_EN), " — ", tr(region, REGIOES_EN)),
                     Value = fmt_num_en(v, 1)),
  mec |> dplyr::transmute(Block = "Residual association between outcome and landline ownership",
                          Item = tr(indicador, IND_EN),
                          Value = paste0("OR ", fmt_ic_en(or, ic_low, ic_upp, 2), "; p = ", fmt_p_en(p))),
  tic |> dplyr::transmute(Block = "Cross-check of coverage between independent sources (%)",
                          Item = tr(fonte, FONTE_TIC_EN),
                          Value = paste0("landline ", fmt_num_en(fixo_pct, 1), "; mobile ", fmt_num_en(celular_pct, 1)))
) |>
  simples("**Table S5.** Parameters of the data-generating mechanism of the simulation",
          paste("Telephone ownership is not simulated: the pseudo-population uses the ownership observed",
                "in the PNS, so that the dependence between ownership, sociodemographic characteristics",
                "and outcome is the one present in the data. The residual association documents that",
                "dependence: were it null for every outcome, the bias would be zero by construction and",
                "the simulation would be uninformative."))
salva_en(tS5, "tableS5")

# =============================================================================
# S6 - full simulation results
# =============================================================================
h2("S6 - full simulation")
tS6 <- sim |>
  dplyr::transmute(Indicator = tr(rotulo, IND_EN), Region = tr(region, REGIOES_EN),
                   Scenario = as.character(cenario),
                   `Coverage (%)` = fmt_num_en(cobertura, 1), True = fmt_num_en(verdadeiro, 2),
                   `Bias (MCSE)` = paste0(fmt_num_en(vies, 3), " (", fmt_num_en(mcse_vies, 3), ")"),
                   `Empirical SE (MCSE)` = paste0(fmt_num_en(empse, 3), " (", fmt_num_en(mcse_empse, 3), ")"),
                   `RMSE (MCSE)` = paste0(fmt_num_en(rmse, 3), " (", fmt_num_en(mcse_rmse, 3), ")")) |>
  simples("**Table S6.** Full simulation results, with Monte Carlo standard errors",
          glue::glue("{big_en(N_REPLICAS)} replicates per scenario, region and indicator, with a fixed seed. ",
                     "Values in percentage points. MCSE = Monte Carlo standard error, which measures the ",
                     "uncertainty due to the finite number of replicates and not sampling uncertainty."))
salva_en(tS6, "tableS6")

# =============================================================================
# S7 - 2023 validation by region (the negative result)
# =============================================================================
h2("S7 - 2023 validation by region")
v23 <- readRDS(here::here("data", "derivado", "validacao_2023.rds"))
tS7_dados <- v23$comp_reg
tS7 <- tS7_dados |>
  dplyr::transmute(Region = tr(region, REGIOES_EN),
                   `RMSE S0 (simulation)` = fmt_num_en(rmse_S0, 2),
                   `RMSE S2 (simulation)` = fmt_num_en(rmse_S2, 2),
                   `Predicted reduction` = fmt_num_en(reducao_prevista, 2),
                   `Mean difference observed in 2023 (pp)` = fmt_num_en(dif_media_abs, 2)) |>
  simples("**Table S7.** Predicted and observed by region: the negative result of the regional stratification",
          glue::glue("The simulation predicted a larger gain where landline coverage is lower. The real ",
                     "{ANO_VIG_DUAL} transition reversed that ordering (Spearman correlation ",
                     "{fmt_num_en(stats::cor(tS7_dados$reducao_prevista, tS7_dados$dif_media_abs, method='spearman'),2)}; ",
                     "with five regions this is descriptive and not a test). The hypothesis on record is that ",
                     "the population still keeping a landline in {ANO_VIG_DUAL} differs from that of {ANO_PNS}, ",
                     "on which the simulation was calibrated."))
salva_en(tS7, "tableS7", tS7_dados)

# =============================================================================
# DOCUMENT ASSEMBLY
# =============================================================================
h2("Assembling the document")

doc <- officer::read_docx() |>
  p_("Supplementary material", "heading 1") |>
  p_(paste("Non-coverage bias of the landline frame in the Vigitel estimates:",
           "tables, figures and supporting documentation.")) |>
  br_() |> h_("Supplementary tables", 1)

for (n in paste0("tableS", 1:7)) {
  doc <- add_gt(doc, readRDS(file.path(DIR_TAB_EN, paste0(n, ".rds")))) |> br_()
}

fig <- function(d, nome, legenda, alt = 3.6) {
  d |> p_(legenda, "Image Caption") |>
    officer::body_add_img(file.path(DIR_FIG_EN, paste0(nome, ".png")),
                          width = 6.5, height = alt) |> br_()
}
doc <- doc |> h_("Supplementary figures", 1) |>
  fig("figureS1", glue::glue("Figure S1. Distribution of age group and education according to household telephone ownership, PNS {ANO_PNS}, adults of the 27 capitals. Weighted estimates with 95% confidence intervals."), 3.2) |>
  fig("figureS2", glue::glue("Figure S2. Bias by sampling-frame scenario and region, complementing Figure 2. Values in percentage points; {big_en(N_REPLICAS)} replicates per cell.")) |>
  fig("figureS3", "Figure S3. Partition of the difference between Vigitel and the PNS into a non-coverage component and a residual, by region and indicator.", 4.6)

# Text S1 - source code of the pipeline
doc <- doc |> h_("Supplementary texts", 1) |> h_("Text S1. Source code", 2) |>
  p_(paste("The full pipeline is organised into thirteen numbered scripts, executed in order by",
           "run_all.R with a fixed seed. The simulation script is reproduced below; the remaining",
           "scripts accompany the study repository. Code comments are in Portuguese, as written."))
for (l in readLines(here::here("R", "08_simulacao.R"), warn = FALSE)) doc <- p_(doc, l)

doc <- doc |> br_() |> h_("Text S2. Computational environment", 2)
for (l in readLines(here::here("output", "logs", "sessioninfo.txt"), warn = FALSE)) doc <- p_(doc, l)

saida <- here::here("output", "supplement", "supplementary_material_en.docx")
print(doc, target = saida)
cat("\nsupplement:", saida, "|", round(file.size(saida) / 1024), "KB\n")

message("12_suplemento_en.R concluido.")
