# =============================================================================
# 10_supplement.R - Material suplementar em arquivo unico
#
# Ordem e numeracao fixadas pelo protocolo (secao 12):
#   Tabela S1  Equivalencia das perguntas entre Vigitel e PNS
#   Tabela S2  Perfil sociodemografico por posse de telefone
#   Tabela S3  Prevalencias e vies de nao cobertura por regiao
#   Tabela S4  Obesidade - sensibilidade, com e sem correcao
#   Tabela S5  Sensibilidade ao conjunto de covariaveis
#   Tabela S6  Parametros do mecanismo gerador da simulacao
#   Tabela S7  Resultados completos da simulacao, com Monte Carlo SE
#   Figura S1  Distribuicao de idade e escolaridade por posse de telefone
#   Figura S2  Vies por cenario e regiao (complemento da Figura 2)
#   Figura S3  Particao do gap por regiao
#   Texto S1   Checklist STROBE
#   Texto S2   Checklist RECORD
#   Texto S3   Codigo da simulacao
#   Texto S4   sessionInfo()
#
# Ao final, o script confere quais itens ainda nao sao citados no corpo do
# manuscrito e lista os pendentes.
# =============================================================================

source(here::here("R", "00_setup.R"))
library(officer)

h1("10_supplement.R - material suplementar")

dir.create(here::here("output", "supplement"), showWarnings = FALSE, recursive = TRUE)

W_NS <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
add_gt <- function(doc, gt_tbl) {
  titulo <- gt_tbl[["_heading"]]$title
  if (!is.null(titulo)) {
    doc <- officer::body_add_par(doc, gsub("\\*\\*", "", as.character(titulo)),
                                 style = "Table Caption")
  }
  raiz <- xml2::read_xml(paste0("<gtfrag xmlns:w='", W_NS, "'>", gt::as_word(gt_tbl), "</gtfrag>"))
  for (no in xml2::xml_children(raiz)) {
    if (xml2::xml_name(no) == "tbl") doc <- officer::body_add_xml(doc, str = as.character(no))
  }
  doc
}
p  <- function(doc, txt, style = "Normal") officer::body_add_par(doc, txt, style = style)
h  <- function(doc, txt, n = 1) officer::body_add_par(doc, txt, style = paste("heading", n))
br <- function(doc) officer::body_add_par(doc, "", style = "Normal")

# =============================================================================
# TABELA S1 - equivalencia das perguntas
# =============================================================================

h2("Tabela S1 - equivalencia das perguntas")

equiv <- readRDS(here::here("data", "equivalencia.rds"))

gtS1 <- equiv |>
  dplyr::transmute(
    indicador,
    PNS = paste0(var_pns, ": ", enunciado_pns, "\n[", cat_pns, "]"),
    Vigitel = paste0(var_vigitel, ": ", enunciado_vigitel, "\n[", cat_vigitel, "]",
                     dplyr::if_else(rotina_vigitel == "(nao aplicavel)", "",
                                    paste0("\nRotina oficial: ", rotina_vigitel))),
    `Codificação adotada` = codificacao,
    `Divergência observada` = divergencia
  ) |>
  gt::gt(rowname_col = "indicador") |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela S1.** Equivalência das perguntas entre Vigitel {YEAR_VIGITEL} e PNS {YEAR_PNS}, com divergências"
  ))) |>
  gt::cols_align("left", columns = dplyr::everything()) |>
  gt::tab_footnote(
    footnote = paste(
      "Enunciados e categorias transcritos dos dicionários oficiais de cada",
      "inquérito, não redigidos pelos autores. A rotina oficial é a sintaxe",
      "publicada pelo Ministério da Saúde para gerar cada indicador do Vigitel;",
      "a codificação deste estudo a reproduz sem divergência em nenhum dos",
      "registros da base."
    ),
    locations = gt::cells_column_labels(columns = Vigitel)
  ) |>
  gt_study_style()

save_table(gtS1, "tableS1")

# =============================================================================
# TABELA S4 - obesidade, com e sem correcao
# =============================================================================

h2("Tabela S4 - obesidade")

vig    <- readRDS(here::here("data", "vigitel.rds"))
pns    <- readRDS(here::here("data", "pns.rds"))
anthro <- readRDS(here::here("data", "pns_anthro.rds")) |>
  dplyr::filter(area_type == "Capital")

des_vig <- vig |> srvyr::as_survey_design(ids = 1, weights = weight)
des_pns <- pns |> srvyr::as_survey_design(strata = strata, ids = psu,
                                          weights = weight, nest = TRUE)
des_ant <- anthro |> srvyr::as_survey_design(strata = strata, ids = psu,
                                             weights = weight, nest = TRUE)

um_prev <- function(design, var, rotulo) {
  v <- rlang::sym(var)
  design |>
    srvyr::filter(!is.na(!!v)) |>
    srvyr::summarise(p = srvyr::survey_mean(!!v, vartype = c("se", "ci"),
                                            proportion = TRUE, na.rm = TRUE),
                     n = srvyr::unweighted(dplyr::n())) |>
    dplyr::transmute(fonte = rotulo, est = p * 100, se = p_se * 100,
                     low = p_low * 100, upp = p_upp * 100, n)
}

# O Vigitel calcula obesidade sobre peso e altura DECLARADOS por telefone.
vig <- vig |>
  dplyr::mutate(obesity_official = dplyr::if_else(
    !is.na(suppressWarnings(as.numeric(obesid))),
    as.integer(suppressWarnings(as.numeric(obesid))), NA_integer_))
des_vig <- vig |> srvyr::as_survey_design(ids = 1, weights = weight)

obes <- dplyr::bind_rows(
  um_prev(des_vig, "obesity_official", "Vigitel — peso e altura declarados"),
  um_prev(des_pns, "obesity_self",     "PNS — peso e altura declarados"),
  um_prev(des_ant, "obesity_measured", "PNS — peso e altura aferidos")
)
print(as.data.frame(obes), digits = 4)

# Duas comparacoes distintas
delta_sem <- obes$est[1] - obes$est[2]   # like-for-like: ambos declarados
se_sem    <- sqrt(obes$se[1]^2 + obes$se[2]^2)
delta_com <- obes$est[1] - obes$est[3]   # Vigitel declarado x padrao-ouro aferido
se_com    <- sqrt(obes$se[1]^2 + obes$se[3]^2)
vies_autorrelato <- obes$est[2] - obes$est[3]
se_autorrelato   <- sqrt(obes$se[2]^2 + obes$se[3]^2)

comp_obes <- tibble::tibble(
  comparacao = c("Δ sem correção (Vigitel declarado − PNS declarado)",
                 "Δ com correção (Vigitel declarado − PNS aferido)",
                 "Viés do autorrelato na PNS (declarado − aferido)"),
  delta = c(delta_sem, delta_com, vies_autorrelato),
  se    = c(se_sem, se_com, se_autorrelato)
) |>
  dplyr::mutate(low = delta - stats::qnorm(0.975) * se,
                upp = delta + stats::qnorm(0.975) * se)
print(as.data.frame(comp_obes), digits = 3)
write_log(obes, "obesidade_prevalencias")
write_log(comp_obes, "obesidade_comparacoes")

gtS4 <- dplyr::bind_rows(
  obes |> dplyr::transmute(bloco = "Prevalência de obesidade (IMC ≥ 30)",
                           linha = fonte,
                           valor = fmt_ci(est, low, upp, 1),
                           n = format(n, big.mark = ".")),
  comp_obes |> dplyr::transmute(bloco = "Diferenças, pontos percentuais",
                                linha = comparacao,
                                valor = fmt_ci(delta, low, upp, 2),
                                n = NA_character_)
) |>
  gt::gt(groupname_col = "bloco", rowname_col = "linha") |>
  gt::cols_label(valor = gt::md("**Estimativa (IC 95%)**"), n = gt::md("**n**")) |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = c(valor, n)) |>
  gt::tab_header(title = gt::md(
    "**Tabela S4.** Obesidade: análise de sensibilidade com e sem correção do autorrelato"
  )) |>
  gt::tab_footnote(
    footnote = paste(
      "A comparação 'sem correção' confronta duas medidas declaradas e é a",
      "comparação equivalente à das Tabelas 2 e 4. A comparação 'com correção'",
      "confronta a estimativa declarada do Vigitel com a medida aferida da PNS, e",
      "portanto soma o viés de não cobertura ao viés de autorrelato."
    ),
    locations = gt::cells_row_groups(groups = "Diferenças, pontos percentuais")
  ) |>
  gt::tab_source_note(gt::md(glue::glue(
    "A subamostra de antropometria da PNS {YEAR_PNS} tem {nrow(anthro)} adultos nas capitais, ",
    "dos quais {sum(!is.na(anthro$obesity_measured))} com índice de massa corporal aferido. ",
    "É um n muito menor que o das demais análises, e os intervalos de confiança refletem isso: ",
    "esta tabela sustenta a direção do efeito, não a sua magnitude precisa."
  ))) |>
  gt_study_style()

save_table(gtS4, "tableS4")

# =============================================================================
# TABELA S5 - sensibilidade ao conjunto de covariaveis
# =============================================================================

h2("Tabela S5 - sensibilidade ao conjunto de covariaveis")

# (a) associacao residual entre desfecho e posse, sob tres conjuntos de ajuste
OUTCOMES <- INDICATORS |> dplyr::mutate(var = paste0(indicator, "_official"))
CONJUNTOS <- list(
  "Sem ajuste"                              = character(0),
  "Idade e sexo"                            = c("age_grp", "sex"),
  "Idade, sexo, escolaridade e região"      = c("age_grp", "sex", "education", "region"),
  "Acima + escolaridade alternativa (9 anos)" = c("age_grp", "sex", "education_alt9", "region")
)

sens_cov <- purrr::imap_dfr(CONJUNTOS, function(covs, nome) {
  purrr::pmap_dfr(list(OUTCOMES$var, OUTCOMES$label_pt), function(var, rot) {
    rhs <- paste(c(covs, var), collapse = " + ")
    m <- survey::svyglm(stats::as.formula(paste("has_landline ~", rhs)),
                        design = des_pns, family = stats::quasibinomial())
    s <- summary(m)$coefficients
    linha <- grep(paste0("^", var), rownames(s))
    tibble::tibble(conjunto = nome, indicador = rot,
                   or = exp(s[linha, 1]),
                   low = exp(s[linha, 1] - 1.96 * s[linha, 2]),
                   upp = exp(s[linha, 1] + 1.96 * s[linha, 2]),
                   p = s[linha, 4])
  })
})
print(as.data.frame(sens_cov), digits = 3)
write_log(sens_cov, "sensibilidade_covariaveis")

gtS5 <- sens_cov |>
  dplyr::mutate(cell = paste0(fmt_ci(or, low, upp, 2)),
                indicador = factor(indicador, levels = INDICATORS$label_pt)) |>
  dplyr::select(conjunto, indicador, cell) |>
  tidyr::pivot_wider(names_from = indicador, values_from = cell) |>
  gt::gt(rowname_col = "conjunto") |>
  gt::tab_header(title = gt::md(
    "**Tabela S5.** Sensibilidade da associação entre desfecho e posse de telefone fixo ao conjunto de covariáveis de ajuste"
  )) |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = dplyr::everything()) |>
  gt::tab_footnote(
    footnote = paste(
      "Razão de chances de ter telefone fixo no domicílio associada a cada",
      "desfecho, com intervalo de confiança de 95%, em modelo logístico com",
      "desenho complexo. A razão sem ajuste é a associação bruta; a ajustada por",
      "idade, sexo, escolaridade e região é a associação residual às variáveis de",
      "calibragem do Vigitel. Quanto mais a razão se aproxima de 1 com o ajuste,",
      "mais o viés de não cobertura é de composição e mais a pós-estratificação o",
      "alcança."
    ),
    locations = gt::cells_stubhead()
  ) |>
  gt::tab_source_note(gt::md(glue::glue(
    "A última linha substitui o mapeamento de escolaridade adotado (fronteira em 11 anos, ",
    "alinhada às faixas do Vigitel) pelo mapeamento alternativo do ensino fundamental de nove ",
    "anos, para mostrar que a conclusão não depende dessa escolha. Fonte: PNS {YEAR_PNS}."
  ))) |>
  gt_study_style()

save_table(gtS5, "tableS5")

# =============================================================================
# TABELAS S3, S6 e S7
# =============================================================================

h2("Tabelas S3, S6 e S7")

# S3 - prevalencias e vies por regiao (versao longa da Tabela 3)
vies <- readRDS(here::here("data", "vies_nao_cobertura.rds"))
gtS3 <- vies |>
  dplyr::mutate(
    label = factor(label, levels = INDICATORS$label_pt),
    dominio = factor(dominio, levels = c("27 capitais", REGION_LEVELS)),
    col_pop = fmt_num(est_p_pns, 1),
    col_fix = fmt_ci(est_p_fixo, p_fixo_low, p_fixo_upp, 1),
    col_sem = fmt_ci(est_p_semfixo, p_sem_low, p_sem_upp, 1),
    col_f   = fmt_ci(est_f_semfixo, f_low, f_upp, 1),
    col_v   = fmt_ci(est_vies, vies_low, vies_upp, 2),
    col_c1  = fmt_num(cochran_pns, 2),
    col_c2  = fmt_num(cochran_vigitel, 2)
  ) |>
  dplyr::arrange(label, dominio) |>
  dplyr::select(label, dominio, col_pop, col_fix, col_sem, col_f, col_v, col_c1, col_c2) |>
  gt::gt(groupname_col = "label", rowname_col = "dominio") |>
  gt::cols_label(
    col_pop = gt::md("**População**<br>%"),
    col_fix = gt::md("**Com fixo**<br>% (IC 95%)"),
    col_sem = gt::md("**Sem fixo**<br>% (IC 95%)"),
    col_f   = gt::md("**Sem fixo**<br>% da pop."),
    col_v   = gt::md("**Viés**<br>pp (IC 95%)"),
    col_c1  = gt::md("**Cochran**<br>(EP da PNS)"),
    col_c2  = gt::md("**Cochran**<br>(EP do Vigitel)")
  ) |>
  gt::cols_align("center", columns = dplyr::starts_with("col_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela S3.** Prevalências e viés de não cobertura por região, PNS {YEAR_PNS}"
  ))) |>
  gt::tab_footnote(
    footnote = glue::glue(
      "As duas últimas colunas trazem o vício relativo de Cochran sob os dois denominadores ",
      "possíveis: o erro-padrão interno da PNS e o erro-padrão que o Vigitel efetivamente tem. ",
      "O limiar de degradação é {fmt_num(COCHRAN_LIMIT, 2)} em ambos."
    ),
    locations = gt::cells_column_labels(columns = col_c2)
  ) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Fonte: PNS {YEAR_PNS} (IBGE). Intervalos do viés por bootstrap de Rao-Wu com ",
    "{format(N_BOOT, big.mark = '.')} réplicas."
  ))) |>
  gt_study_style()
save_table(gtS3, "tableS3")

# S6 - parametros do mecanismo gerador
mec  <- readr::read_csv(here::here("logs", "mecanismo_posse_desfecho.csv"), show_col_types = FALSE)
cobr <- readr::read_csv(here::here("logs", "cobertura_quadros_regiao.csv"), show_col_types = FALSE)
conf_tic <- readr::read_csv(here::here("logs", "conferencia_cobertura_pns_tic.csv"), show_col_types = FALSE)

gtS6 <- dplyr::bind_rows(
  cobr |>
    tidyr::pivot_longer(-region, names_to = "linha", values_to = "v") |>
    dplyr::transmute(bloco = "Cobertura de cada quadro amostral, por região (%)",
                     linha = paste0(linha, " — ", region),
                     valor = fmt_num(v, 1)),
  mec |> dplyr::transmute(bloco = "Associação residual desfecho ~ posse de telefone fixo (OR)",
                          linha = indicador,
                          valor = paste0(fmt_ci(or, ic_low, ic_upp, 2), "; p = ", fmt_p(p))),
  conf_tic |> dplyr::transmute(bloco = "Conferência da cobertura entre fontes independentes (%)",
                               linha = fonte,
                               valor = paste0("fixo ", fmt_num(fixo_pct, 1),
                                              "; celular ", fmt_num(celular_pct, 1)))
) |>
  gt::gt(groupname_col = "bloco", rowname_col = "linha") |>
  gt::cols_label(valor = gt::md("**Valor**")) |>
  gt::cols_align("center", columns = valor) |>
  gt::tab_header(title = gt::md(
    "**Tabela S6.** Parâmetros do mecanismo gerador da simulação"
  )) |>
  gt::tab_footnote(
    footnote = paste(
      "A posse de telefone não é simulada: a pseudopopulação usa a posse observada",
      "na PNS, de modo que a dependência entre posse, características",
      "sociodemográficas e desfecho é a que existe nos dados. A associação residual",
      "acima documenta essa dependência, como exige o desenho da simulação: fosse",
      "ela nula para todos os desfechos, o viés seria zero por construção e a",
      "simulação não informaria nada."
    ),
    locations = gt::cells_row_groups(groups = "Associação residual desfecho ~ posse de telefone fixo (OR)")
  ) |>
  gt_study_style()
save_table(gtS6, "tableS6")

# S7 - simulacao completa com Monte Carlo SE
sim <- readRDS(here::here("data", "simulacao.rds"))
gtS7 <- sim |>
  dplyr::transmute(
    label, region, cenario,
    Cobertura = fmt_num(cobertura, 1),
    Verdadeiro = fmt_num(verdadeiro, 2),
    `Viés` = paste0(fmt_num(vies, 3), " (", fmt_num(mcse_vies, 3), ")"),
    `EP empírico` = paste0(fmt_num(empse, 3), " (", fmt_num(mcse_empse, 3), ")"),
    REQM = paste0(fmt_num(rmse, 3), " (", fmt_num(mcse_rmse, 3), ")")
  ) |>
  gt::gt(groupname_col = "label") |>
  gt::cols_label(region = gt::md("**Região**"), cenario = gt::md("**Cenário**")) |>
  gt::cols_align("center", columns = -c(label, region)) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela S7.** Resultados completos da simulação, com erro de Monte Carlo entre parênteses"
  ))) |>
  gt::tab_footnote(
    footnote = glue::glue(
      "{format(N_REPLICATES, big.mark = '.')} réplicas por cenário × região × indicador, ",
      "semente fixa. Valores em pontos percentuais. O erro de Monte Carlo entre parênteses ",
      "mede a incerteza devida ao número finito de réplicas, e não a incerteza amostral."
    ),
    locations = gt::cells_column_labels(columns = REQM)
  ) |>
  gt_study_style()
save_table(gtS7, "tableS7")

# =============================================================================
# FIGURA S2 - vies por cenario e regiao
# =============================================================================

h2("Figura S2 - vies por cenario e regiao")

figS2 <- ggplot2::ggplot(
  sim, ggplot2::aes(x = cenario, y = vies, colour = label, group = label)
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, colour = unname(PAL["ink"])) +
  ggplot2::geom_line(linewidth = 0.5) +
  ggplot2::geom_point(size = 1.7) +
  ggplot2::facet_wrap(~region, nrow = 1) +
  ggplot2::scale_colour_manual(values = PAL_INDICATOR, name = NULL) +
  ggplot2::scale_y_continuous(labels = function(x) fmt_num(x, 1)) +
  ggplot2::labs(x = "Cenário de quadro amostral", y = "Viés, pontos percentuais") +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                                            linewidth = 0.3))
save_figure(figS2, "figureS2", width_mm = 180, height_mm = 100)

# A figura de particao por regiao passa a ser a S3
particao_regiao <- readRDS(here::here("data", "particao_regiao.rds"))
dados_s3 <- particao_regiao |>
  dplyr::select(label, dominio, `Gap total` = gap_total,
                `Componente de não cobertura` = componente, `Resíduo` = residuo) |>
  tidyr::pivot_longer(-c(label, dominio), names_to = "parcela", values_to = "valor") |>
  dplyr::mutate(parcela = factor(parcela, levels = c("Gap total",
                                                     "Componente de não cobertura", "Resíduo")))
PAL_PARCELA <- c("Gap total" = unname(PAL["ink"]),
                 "Componente de não cobertura" = unname(PAL["pns"]),
                 "Resíduo" = unname(PAL["accent1"]))
figS3 <- ggplot2::ggplot(dados_s3, ggplot2::aes(x = valor, y = dominio, colour = parcela)) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = unname(PAL["ink"])) +
  ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(width = 0.6)) +
  ggplot2::facet_wrap(~label, nrow = 2) +
  ggplot2::scale_colour_manual(values = PAL_PARCELA, name = NULL) +
  ggplot2::scale_y_discrete(limits = rev(REGION_LEVELS)) +
  ggplot2::labs(x = "Pontos percentuais (Vigitel - PNS)", y = NULL) +
  ggplot2::theme(legend.position = "top", legend.justification = "left",
                 panel.grid.major.y = ggplot2::element_line(colour = unname(PAL["grid"]),
                                                            linewidth = 0.3, linetype = "dotted"))
save_figure(figS3, "figureS3", width_mm = 180, height_mm = 130)

# =============================================================================
# MONTAGEM DO DOCUMENTO
# =============================================================================

h2("Montando o documento")

doc <- officer::read_docx() |>
  p("Material suplementar", "heading 1") |>
  p(glue::glue(
    "Representatividade e viés de não cobertura do Vigitel {YEAR_VIGITEL} em relação à ",
    "Pesquisa Nacional de Saúde {YEAR_PNS}: tabelas, figuras e documentação de apoio."
  )) |>
  br()

add_tabela <- function(doc, nome) add_gt(doc, readRDS(here::here("output", "tables", paste0(nome, ".rds"))))
add_figura <- function(doc, nome, legenda, altura = 4.2) {
  doc |>
    p(legenda, "Image Caption") |>
    officer::body_add_img(here::here("output", "figures", paste0(nome, ".png")),
                          width = 6.5, height = altura) |>
    br()
}

doc <- doc |>
  h("Tabelas suplementares", 1) |>
  add_tabela("tableS1") |> br() |>
  add_tabela("tableS2") |> br() |>
  add_tabela("tableS3") |> br() |>
  add_tabela("tableS4") |> br() |>
  add_tabela("tableS5") |> br() |>
  add_tabela("tableS6") |> br() |>
  add_tabela("tableS7") |> br() |>
  h("Figuras suplementares", 1) |>
  add_figura("figureS1",
             paste("Figura S1. Distribuição de faixa etária e de escolaridade segundo a posse",
                   "de telefone no domicílio, PNS", YEAR_PNS, "— adultos das 27 capitais.",
                   "Estimativas ponderadas, com intervalo de confiança de 95%."), 3.2) |>
  add_figura("figureS2",
             paste("Figura S2. Viés por cenário de quadro amostral e região, complementando a",
                   "Figura 2 do manuscrito. Valores em pontos percentuais;",
                   format(N_REPLICATES, big.mark = "."), "réplicas por célula."), 3.6) |>
  add_figura("figureS3",
             paste("Figura S3. Partição da diferença entre Vigitel e PNS em componente de não",
                   "cobertura e resíduo, por região e indicador.")) |>
  h("Textos suplementares", 1)

# ---- Texto S1 e S2: checklists ----------------------------------------------
# Os itens sao listados com o local correspondente; a numeracao de pagina so
# pode ser preenchida sobre o manuscrito diagramado, e por isso fica marcada.

strobe <- tibble::tribble(
  ~Item, ~Recomendacao, ~Onde,
  "1",  "Título e resumo indicam o desenho do estudo", "Título; Resumo",
  "2",  "Contexto científico e justificativa", "Introdução",
  "3",  "Objetivos e hipóteses", "Introdução, último parágrafo",
  "4",  "Desenho do estudo", "Métodos — Desenho",
  "5",  "Contexto, locais e datas", "Métodos — Fontes de dados",
  "6",  "Critérios de elegibilidade e seleção", "Métodos — Amostras; Quadro 1",
  "7",  "Variáveis: desfechos, exposições, confundidores", "Métodos; Tabela S1",
  "8",  "Fontes de dados e mensuração", "Métodos; Tabela S1",
  "9",  "Vieses: fontes potenciais e como foram tratados", "Métodos — Viés de não cobertura; Tabela 3",
  "10", "Tamanho do estudo", "Métodos — Amostras",
  "11", "Tratamento de variáveis quantitativas", "Métodos — Harmonização",
  "12", "Métodos estatísticos", "Métodos — Análise; Tabelas 2 a 4",
  "13", "Participantes em cada etapa", "Quadro 1 (fluxo amostral)",
  "14", "Dados descritivos", "Tabela 1",
  "15", "Dados dos desfechos", "Tabela 2",
  "16", "Resultados principais", "Tabelas 2 a 4; Figura 1",
  "17", "Outras análises (subgrupos, sensibilidade)", "Tabelas S4 e S5; seção de sensibilidade",
  "18", "Resultados-chave em relação aos objetivos", "Discussão, primeiro parágrafo",
  "19", "Limitações", "Discussão — Limitações",
  "20", "Interpretação", "Discussão",
  "21", "Generalização", "Discussão",
  "22", "Financiamento", "Declarações"
)

record <- tibble::tribble(
  ~Item, ~Recomendacao, ~Onde,
  "1.1", "População e bases de dados usadas para selecioná-la", "Métodos — Fontes de dados",
  "1.2", "Códigos e algoritmos de seleção da população", "Métodos; R/02_harmonize.R",
  "1.3", "Validação da seleção da população", "Métodos — Validação; Quadros 2 e 3",
  "2.1", "Códigos e algoritmos de classificação de variáveis", "Tabela S1; R/02_harmonize.R",
  "3.1", "Ligação entre bases de dados", "Não se aplica — bases analisadas separadamente",
  "4.1", "Métodos de limpeza e tratamento dos dados", "Métodos — Harmonização",
  "6.1", "Fluxo de seleção da população", "Quadro 1",
  "12.1", "Acesso à base e disponibilidade do código", "Declarações — Disponibilidade",
  "13.1", "Uso de dados coletados para outra finalidade", "Métodos — Fontes de dados",
  "19.1", "Limitações do uso de dados secundários", "Discussão — Limitações"
)

gt_checklist <- function(d, titulo, nota) {
  d |> gt::gt() |>
    gt::tab_header(title = gt::md(titulo)) |>
    gt::cols_label(Item = gt::md("**Item**"), Recomendacao = gt::md("**Recomendação**"),
                   Onde = gt::md("**Onde no manuscrito**")) |>
    gt::tab_source_note(gt::md(nota)) |>
    gt_study_style()
}

doc <- doc |>
  h("Texto S1. Checklist STROBE", 2) |>
  add_gt(gt_checklist(strobe, "**Texto S1.** Checklist STROBE para estudos observacionais",
    "A coluna indica a seção do manuscrito onde o item é atendido. O número de página só pode ser preenchido sobre a versão diagramada e está marcado como [XX] no arquivo de submissão.")) |>
  br() |>
  h("Texto S2. Checklist RECORD", 2) |>
  add_gt(gt_checklist(record, "**Texto S2.** Checklist RECORD para estudos com dados coletados rotineiramente",
    "Extensão RECORD do STROBE. O item 3.1 não se aplica: as bases são analisadas separadamente, sem ligação de registros entre elas.")) |>
  br()

# ---- Texto S3: codigo da simulacao ------------------------------------------
codigo <- readLines(here::here("R", "08_simulation.R"), warn = FALSE)
doc <- doc |> h("Texto S3. Código da simulação", 2) |>
  p(glue::glue(
    "Reprodução integral de R/08_simulation.R ({length(codigo)} linhas). O pipeline completo, ",
    "incluindo download, harmonização e validação, está organizado em dez scripts numerados."
  ))
for (linha in codigo) doc <- officer::body_add_par(doc, linha, style = "Normal")
doc <- br(doc)

# ---- Texto S4: sessionInfo --------------------------------------------------
sess <- readLines(here::here("logs", "sessioninfo.txt"), warn = FALSE)
doc <- doc |> h("Texto S4. Ambiente computacional (sessionInfo)", 2)
for (linha in sess) doc <- officer::body_add_par(doc, linha, style = "Normal")

saida <- here::here("output", "supplement", "supplementary_material.docx")
print(doc, target = saida)
cat("\nSuplemento gravado:", saida, "\n")
cat("tamanho:", round(file.size(saida) / 1024), "KB\n")

# =============================================================================
# CHECAGEM DE CITACAO NO CORPO DO MANUSCRITO
# =============================================================================
# Cada item do suplemento precisa ser citado ao menos uma vez no corpo do texto.
# Conferimos contra o documento de resultados gerado pelo pipeline.

h2("Itens do suplemento sem citacao no corpo do manuscrito")

itens <- c(paste0("Tabela S", 1:7), paste0("Figura S", 1:3), paste0("Texto S", 1:4))
ms <- here::here("output", "resultados_vigitel_pns.docx")

if (file.exists(ms)) {
  tmp <- tempfile(); utils::unzip(ms, files = "word/document.xml", exdir = tmp)
  corpo <- paste(readLines(file.path(tmp, "word", "document.xml"),
                           warn = FALSE, encoding = "UTF-8"), collapse = " ")
  corpo <- gsub("<[^>]+>", "", corpo)
  citacao <- tibble::tibble(
    item = itens,
    citado = vapply(itens, function(i) grepl(i, corpo, fixed = TRUE), logical(1))
  )
  print(as.data.frame(citacao))
  pendentes <- citacao$item[!citacao$citado]
  if (length(pendentes) > 0) {
    cat("\nPENDENTE - itens ainda nao citados no corpo:\n  ",
        paste(pendentes, collapse = ", "), "\n", sep = "")
    cat("Cada um precisa de pelo menos uma chamada no texto antes da submissao.\n")
  } else {
    cat("\nOK: todos os itens do suplemento sao citados no corpo.\n")
  }
  write_log(citacao, "citacao_suplemento")
} else {
  message("Documento de resultados ausente; checagem de citacao nao realizada.")
}

message("10_supplement.R concluido.")
