# =============================================================================
# 12_suplemento.R
# O QUE FAZ : Monta o material suplementar num arquivo unico, na ordem S1..S7:
#             dicionario de-para, validacao externa, diagnostico dos pesos, vies
#             por regiao, parametros do mecanismo gerador, simulacao completa e
#             o resultado negativo da previsao regional.
# ENTRADAS  : data/derivado/*.rds, output/tables/*, output/figures/*
# SAIDAS    : output/supplement/material_suplementar.docx,
#             output/tables/tabelaS*.{rds,docx,html}
# =============================================================================

source(here::here("R", "00_setup.R"))
library(officer)

h1("12_suplemento.R")
dir.create(here::here("output", "supplement"), showWarnings = FALSE, recursive = TRUE)

de_para <- readRDS(here::here("data", "derivado", "de_para.rds"))
vies    <- readRDS(here::here("data", "derivado", "vies_ncob.rds"))
sim     <- readRDS(here::here("data", "derivado", "simulacao.rds"))
pns     <- readRDS(here::here("data", "derivado", "pns.rds"))
vig     <- readRDS(here::here("data", "derivado", "vigitel.rds"))
DESFECHOS <- INDICADORES |> dplyr::mutate(var = paste0(indicador, "_official"))

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

# =============================================================================
# S1 - dicionario de-para
# =============================================================================
h2("S1 - de-para")
tS1 <- de_para |>
  dplyr::transmute(Indicador = indicador,
                   PNS = paste0(var_pns, ": ", enunciado_pns, " [", cat_pns, "]"),
                   Vigitel = paste0(var_vigitel, ": ", enunciado_vigitel, " [", cat_vigitel, "]",
                                    dplyr::if_else(rotina_vigitel == "(nao aplicavel)", "",
                                                   paste0(" — rotina oficial: ", rotina_vigitel))),
                   `Recodificação adotada` = codificacao,
                   `Divergência observada` = divergencia) |>
  simples("**Tabela S1.** Dicionário de-para: equivalência das perguntas entre os instrumentos",
          "Enunciados e categorias transcritos dos dicionários oficiais de cada inquérito, não redigidos pelos autores. A rotina oficial é a sintaxe publicada pelo Ministério da Saúde para gerar cada indicador do Vigitel.")
salva_tabela(tS1, "tabelaS1")

# =============================================================================
# S2 - validacao externa
# =============================================================================
h2("S2 - validacao externa")
val_pns <- le_log("validacao_pns_sidra"); val_vig <- le_log("validacao_vigitel")
tS2 <- dplyr::bind_rows(
  val_pns |> dplyr::transmute(Fonte = "PNS 2019 x SIDRA (IBGE)", Indicador = indicador,
                              Estrato = sex, Nosso = fmt_num(ours, 2),
                              Publicado = fmt_num(published, 2), `Diferença (pp)` = fmt_num(diferenca, 3)),
  val_vig |> dplyr::transmute(Fonte = "Vigitel 2019 x relatório oficial", Indicador = indicador,
                              Estrato = sex, Nosso = fmt_num(ours, 2),
                              Publicado = fmt_num(published, 2), `Diferença (pp)` = fmt_num(diferenca, 3))
) |>
  simples("**Tabela S2.** Validação externa: estimativas próprias contra os valores publicados",
          glue::glue("Diferença máxima absoluta: {fmt_num(max(abs(val_pns$diferenca)),3)} pp na PNS e ",
                     "{fmt_num(max(abs(val_vig$diferenca)),3)} pp no Vigitel. Nenhuma divergência atinge 0,5 pp."))
salva_tabela(tS2, "tabelaS2")

# =============================================================================
# S3 - diagnostico dos pesos
# =============================================================================
h2("S3 - diagnostico dos pesos")
diag_peso <- function(w, rotulo, n) {
  tibble::tibble(Inquérito = rotulo, n = format(n, big.mark = "."),
                 `Soma dos pesos` = format(round(sum(w)), big.mark = "."),
                 `Mínimo` = fmt_num(min(w), 1), `Mediana` = fmt_num(stats::median(w), 1),
                 `Máximo` = fmt_num(max(w), 1),
                 `Razão máx/mín` = format(round(max(w) / min(w)), big.mark = "."),
                 `Efeito de desenho de Kish` = fmt_num(length(w) * sum(w^2) / sum(w)^2, 2))
}
tS3_dados <- dplyr::bind_rows(diag_peso(vig$weight, glue::glue("Vigitel {ANO_VIGITEL}"), nrow(vig)),
                              diag_peso(pns$weight, glue::glue("PNS {ANO_PNS} (capitais)"), nrow(pns)))
print(as.data.frame(tS3_dados))
tS3 <- tS3_dados |>
  simples("**Tabela S3.** Diagnóstico dos pesos amostrais",
          "O efeito de desenho de Kish mede a perda de precisão devida à variabilidade dos pesos: valor 1 indica pesos uniformes. A razão entre o maior e o menor peso quantifica a magnitude da correção que cada inquérito precisa aplicar.")
salva_tabela(tS3, "tabelaS3", tS3_dados)

# =============================================================================
# S4 - vies de nao cobertura por regiao, com as duas leituras de Cochran
# =============================================================================
h2("S4 - vies por regiao")
tS4 <- vies |>
  dplyr::transmute(Indicador = as.character(rotulo), Domínio = as.character(dominio),
                   `População (%)` = fmt_num(est_p_pns, 1),
                   `Com fixo (%)` = fmt_ic(est_p_fixo, p_fixo_low, p_fixo_upp, 1),
                   `Sem fixo (%)` = fmt_ic(est_p_semfixo, p_sem_low, p_sem_upp, 1),
                   `Sem fixo, % da pop.` = fmt_ic(est_f_semfixo, f_low, f_upp, 1),
                   `Viés (pp)` = fmt_ic(est_vies, vies_low, vies_upp, 2),
                   `Cochran (EP PNS)` = fmt_num(cochran_pns, 2),
                   `Cochran (EP Vigitel)` = fmt_num(cochran_vigitel, 2)) |>
  simples(glue::glue("**Tabela S4.** Viés de não cobertura por indicador e região, PNS {ANO_PNS}"),
          glue::glue("As duas últimas colunas trazem o vício relativo de Cochran sob os dois denominadores possíveis. O limiar de degradação é {fmt_num(LIMIAR_COCHRAN,2)} em ambos. Intervalos por bootstrap de Rao-Wu com {format(N_BOOT, big.mark='.')} réplicas."))
salva_tabela(tS4, "tabelaS4")

# =============================================================================
# S5 - parametros do mecanismo gerador
# =============================================================================
h2("S5 - mecanismo gerador")
mec <- le_log("mecanismo_posse_desfecho"); cob <- le_log("cobertura_quadros_regiao")
tic <- le_log("conferencia_cobertura_pns_tic")
tS5 <- dplyr::bind_rows(
  cob |> tidyr::pivot_longer(-region, names_to = "linha", values_to = "v") |>
    dplyr::transmute(Bloco = "Cobertura de cada quadro, por região (%)",
                     Item = paste0(linha, " — ", region), Valor = fmt_num(v, 1)),
  mec |> dplyr::transmute(Bloco = "Associação residual entre desfecho e posse de telefone fixo",
                          Item = indicador,
                          Valor = paste0("OR ", fmt_ic(or, ic_low, ic_upp, 2), "; p = ", fmt_p(p))),
  tic |> dplyr::transmute(Bloco = "Conferência da cobertura entre fontes independentes (%)",
                          Item = fonte,
                          Valor = paste0("fixo ", fmt_num(fixo_pct, 1), "; celular ", fmt_num(celular_pct, 1)))
) |>
  simples("**Tabela S5.** Parâmetros do mecanismo gerador da simulação",
          paste("A posse de telefone não é simulada: a pseudopopulação usa a posse observada na PNS, de modo",
                "que a dependência entre posse, características sociodemográficas e desfecho é a que existe",
                "nos dados. A associação residual documenta essa dependência: fosse ela nula para todos os",
                "desfechos, o viés seria zero por construção e a simulação não informaria nada."))
salva_tabela(tS5, "tabelaS5")

# =============================================================================
# S6 - simulacao completa
# =============================================================================
h2("S6 - simulacao completa")
tS6 <- sim |>
  dplyr::transmute(Indicador = as.character(rotulo), Região = as.character(region), Cenário = as.character(cenario),
                   `Cobertura (%)` = fmt_num(cobertura, 1), Verdadeiro = fmt_num(verdadeiro, 2),
                   `Viés (EMC)` = paste0(fmt_num(vies, 3), " (", fmt_num(mcse_vies, 3), ")"),
                   `EP empírico (EMC)` = paste0(fmt_num(empse, 3), " (", fmt_num(mcse_empse, 3), ")"),
                   `REQM (EMC)` = paste0(fmt_num(rmse, 3), " (", fmt_num(mcse_rmse, 3), ")")) |>
  simples("**Tabela S6.** Resultados completos da simulação, com erro de Monte Carlo",
          glue::glue("{format(N_REPLICAS, big.mark='.')} réplicas por cenário, região e indicador, semente fixa. Valores em pontos percentuais. EMC = erro de Monte Carlo, que mede a incerteza devida ao número finito de réplicas e não a incerteza amostral."))
salva_tabela(tS6, "tabelaS6")

# =============================================================================
# S7 - validacao 2023 por regiao (resultado negativo)
# =============================================================================
h2("S7 - validacao 2023 por regiao")
v23 <- readRDS(here::here("data", "derivado", "validacao_2023.rds"))
tS7_dados <- v23$comp_reg
tS7 <- tS7_dados |>
  dplyr::transmute(Região = as.character(region),
                   `REQM S0 (simulação)` = fmt_num(rmse_S0, 2),
                   `REQM S2 (simulação)` = fmt_num(rmse_S2, 2),
                   `Redução prevista` = fmt_num(reducao_prevista, 2),
                   `Diferença média observada em 2023 (pp)` = fmt_num(dif_media_abs, 2)) |>
  simples("**Tabela S7.** Previsto e observado por região: o resultado negativo da estratificação regional",
          glue::glue("A simulação previa ganho maior onde a cobertura de telefonia fixa é menor. A transição real de {ANO_VIG_DUAL} inverteu essa ordenação (correlação de Spearman {fmt_num(stats::cor(tS7_dados$reducao_prevista, tS7_dados$dif_media_abs, method='spearman'),2)}; com cinco regiões, descritiva e não teste). A hipótese registrada é que a população que ainda mantinha telefonia fixa em {ANO_VIG_DUAL} difere daquela de {ANO_PNS}, sobre a qual a simulação foi calibrada."))
salva_tabela(tS7, "tabelaS7", tS7_dados)

# =============================================================================
# MONTAGEM
# =============================================================================
h2("Montando o documento")

doc <- officer::read_docx() |>
  p_("Material suplementar", "heading 1") |>
  p_(glue::glue("Viés de não cobertura da telefonia fixa nas estimativas do Vigitel: ",
                "tabelas, figuras e documentação de apoio.")) |>
  br_() |> h_("Tabelas suplementares", 1)

for (n in paste0("tabelaS", 1:7)) {
  doc <- add_gt(doc, readRDS(here::here("output", "tables", paste0(n, ".rds")))) |> br_()
}

fig <- function(d, nome, legenda, alt = 3.6) {
  d |> p_(legenda, "Image Caption") |>
    officer::body_add_img(here::here("output", "figures", paste0(nome, ".png")),
                          width = 6.5, height = alt) |> br_()
}
doc <- doc |> h_("Figuras suplementares", 1) |>
  fig("figuraS1", glue::glue("Figura S1. Distribuição de faixa etária e de escolaridade segundo a posse de telefone no domicílio, PNS {ANO_PNS}, adultos das 27 capitais. Estimativas ponderadas com intervalo de confiança de 95%."), 3.2) |>
  fig("figuraS2", glue::glue("Figura S2. Viés por cenário de quadro amostral e região, complementando a Figura 2. Valores em pontos percentuais; {format(N_REPLICAS, big.mark='.')} réplicas por célula.")) |>
  fig("figuraS3", "Figura S3. Partição da diferença entre Vigitel e PNS em componente de não cobertura e resíduo, por região e indicador.", 4.6)

# Texto S1 - codigo-fonte do pipeline
doc <- doc |> h_("Textos suplementares", 1) |> h_("Texto S1. Código-fonte", 2) |>
  p_(paste("O pipeline completo está organizado em treze scripts numerados, executados em ordem por",
           "run_all.R, com semente fixa. Reproduz-se abaixo o script da simulação; os demais",
           "acompanham o depósito do estudo."))
for (l in readLines(here::here("R", "08_simulacao.R"), warn = FALSE)) doc <- p_(doc, l)

doc <- doc |> br_() |> h_("Texto S2. Ambiente computacional", 2)
for (l in readLines(here::here("output", "logs", "sessioninfo.txt"), warn = FALSE)) doc <- p_(doc, l)

saida <- here::here("output", "supplement", "material_suplementar.docx")
print(doc, target = saida)
cat("\nsuplemento:", saida, "|", round(file.size(saida) / 1024), "KB\n")

message("12_suplemento.R concluido.")
