# =============================================================================
# run_en.R
# O QUE FAZ : Gera a versao em ingles das tabelas, figuras e do suplemento a
#             partir dos objetos ja calculados em data/derivado/. Nao recalcula
#             estimativa nenhuma: exige que o pipeline principal ja tenha rodado.
# USO       : Rscript run_en.R
# SAIDAS    : output/tables_en/, output/figures_en/,
#             output/supplement/supplementary_material_en.docx
# =============================================================================

necessarios <- c("descritivas", "comparacoes", "prevalencias", "vies_ncob",
                 "particao", "simulacao", "validacao_2023", "de_para", "pns", "vigitel")
faltando <- necessarios[!file.exists(file.path("data", "derivado", paste0(necessarios, ".rds")))]
if (length(faltando) > 0)
  stop("Rode o pipeline principal antes: faltam ", paste(faltando, collapse = ", "), call. = FALSE)

for (etapa in c("10_tabelas_en", "11_figuras_en", "12_suplemento_en")) {
  cat(strrep("-", 78), "\n>>> ", etapa, "\n", sep = "")
  source(file.path("R", paste0(etapa, ".R")), local = new.env(), echo = FALSE)
}
cat("\nversao em ingles concluida.\n")
