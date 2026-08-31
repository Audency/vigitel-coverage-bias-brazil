# =============================================================================
# 13_exporta_para_docx.R - Exporta as tabelas ja formatadas para o script que
# atualiza o manuscrito em Word. Devolve texto pronto para celula, nao numero
# cru, para que a formatacao decidida no pipeline (virgula decimal, IC, sinais)
# chegue intacta ao documento.
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("13_exporta_para_docx.R")

dir_out <- here::here("output", "docx_tabelas")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# ---- Tabela 1 ---------------------------------------------------------------
t1 <- readRDS(here::here("output", "tables", "table1_dados.rds")) |>
  dplyr::transmute(
    Variavel = dplyr::if_else(as.character(nivel) == "Média (DP)",
                              as.character(grupo),
                              paste0("   ", as.character(nivel))),
    Vigitel = cell_Vigitel,
    PNS     = cell_PNS,
    SMD     = fmt_num(smd, 2)
  )
# Cabecalhos de bloco, para o leitor nao perder o agrupamento
t1_blocos <- readRDS(here::here("output", "tables", "table1_dados.rds")) |>
  dplyr::distinct(grupo) |> dplyr::pull(grupo) |> as.character()
readr::write_csv(t1, file.path(dir_out, "tabela1.csv"))

# ---- Tabela 3 (vies de nao cobertura) --------------------------------------
t3 <- readRDS(here::here("output", "tables", "table3_dados.rds")) |>
  dplyr::mutate(
    label = factor(label, levels = INDICATORS$label_pt),
    dominio = factor(dominio, levels = c("27 capitais", REGION_LEVELS))
  ) |>
  dplyr::arrange(label, dominio) |>
  dplyr::transmute(
    Indicador = as.character(label),
    Dominio   = as.character(dominio),
    `Com fixo`  = fmt_ci(est_p_fixo, p_fixo_low, p_fixo_upp, 1),
    `Sem fixo`  = fmt_ci(est_p_semfixo, p_sem_low, p_sem_upp, 1),
    `Sem fixo (% pop)` = fmt_ci(est_f_semfixo, f_low, f_upp, 1),
    `Vies (pp)` = fmt_ci(est_vies, vies_low, vies_upp, 2),
    `Cochran`   = fmt_num(cochran_vigitel, 2)
  )
readr::write_csv(t3, file.path(dir_out, "tabela3.csv"))

# ---- Tabela 4 (particao) ----------------------------------------------------
t4 <- readRDS(here::here("output", "tables", "table4_dados.rds")) |>
  dplyr::mutate(label = factor(label, levels = INDICATORS$label_pt)) |>
  dplyr::arrange(label) |>
  dplyr::transmute(
    Indicador = as.character(label),
    `Gap total (pp)` = fmt_num(gap_total, 2),
    `Componente de nao cobertura (pp, IC 95%)` = fmt_ci(componente, comp_low, comp_upp, 2),
    `Residuo (pp, IC 95%)` = fmt_ci(residuo, residuo_low, residuo_upp, 2),
    `Parcela atribuivel a nao cobertura` = pct_txt
  )
readr::write_csv(t4, file.path(dir_out, "tabela4.csv"))

# ---- Textos que substituem paragrafos --------------------------------------
vies <- readRDS(here::here("data", "vies_nao_cobertura.rds"))
tab2 <- readRDS(here::here("output", "tables", "table2_dados.rds"))
part <- readRDS(here::here("data", "particao.rds"))
sim  <- readRDS(here::here("data", "simulacao.rds"))
mec  <- readr::read_csv(here::here("logs", "mecanismo_posse_desfecho.csv"), show_col_types = FALSE)

g  <- function(i, c) tab2[[c]][tab2$indicator == i & tab2$estrato == "Total"]
gi <- function(i) fmt_ci(g(i, "delta"), g(i, "delta_low"), g(i, "delta_upp"), 2)
v27 <- function(i, c) vies[[c]][vies$indicator == i & vies$dominio == "27 capitais"]
vr  <- function(i, r, c) vies[[c]][vies$indicator == i & vies$dominio == r]

remocao <- part |>
  dplyr::select(indicator, p_vig, verdadeiro = est_p_pns) |>
  dplyr::left_join(vies |> dplyr::filter(dominio == "27 capitais") |>
                     dplyr::select(indicator, sem_pond = est_p_fixo), by = "indicator") |>
  dplyr::mutate(pct = 100 * (1 - abs(p_vig - verdadeiro) / abs(sem_pond - verdadeiro)))
rm_ <- function(i) remocao$pct[remocao$indicator == i]

reqm <- sim |> dplyr::group_by(region, cenario) |>
  dplyr::summarise(rmse = mean(rmse), .groups = "drop") |>
  tidyr::pivot_wider(names_from = cenario, values_from = rmse) |>
  dplyr::mutate(red = S0 / S1)
red <- function(r) reqm$red[reqm$region == r]

textos <- tibble::tibble(
  chave = c("resumo_resultados", "resumo_conclusao", "recorte_capitais",
            "mecanismo_gerador", "cochran"),
  texto = c(
    glue::glue(
      "Resultados: As estimativas de ambos os inquéritos reproduzem os valores oficiais ",
      "publicados. O viés de não cobertura da telefonia fixa é grande: ",
      "{fmt_num(v27('hypertension','est_vies'),2)} pp para hipertensão no conjunto das capitais e ",
      "{fmt_num(vr('hypertension','Norte','est_vies'),2)} pp no Norte, com vício relativo de Cochran ",
      "acima do limiar de 0,40 em todas as {nrow(vies)} combinações de indicador e região. As ",
      "diferenças efetivamente observadas entre os inquéritos, porém, são pequenas e de sentidos ",
      "opostos: {gi('smoking')} pp para tabagismo atual e {gi('hypertension')} pp para hipertensão, ",
      "sem diferença detectável para diabetes ({gi('diabetes')}) e autoavaliação ruim de saúde ",
      "({gi('poor_health')}); apenas dois dos quatro indicadores diferem de zero após correção de ",
      "Holm. O teste formal de interação sexo × inquérito não foi significativo em nenhum indicador. ",
      "A pós-estratificação removia {fmt_num(rm_('diabetes'),0)}% do viés de cobertura em diabetes e ",
      "{fmt_num(rm_('hypertension'),0)}% em hipertensão, mas apenas {fmt_num(rm_('smoking'),0)}% em ",
      "tabagismo, ordem que acompanha a associação residual entre desfecho e posse de telefone após ",
      "ajuste pelas variáveis de calibragem. Na simulação, abandonar o quadro exclusivamente fixo ",
      "reduz a raiz do erro quadrático médio por fator de {fmt_num(red('Norte'),1)} no Norte e ",
      "{fmt_num(red('Nordeste'),1)} no Nordeste, contra {fmt_num(red('Sudeste'),1)} no Sudeste."
    ),
    paste(
      "Conclusão: Cobertura e viés não são a mesma quantidade. O quadro de telefonia fixa do Vigitel",
      "estava gravemente comprometido, mas suas estimativas publicadas permaneceram próximas da",
      "referência domiciliar porque a pós-estratificação absorvia a maior parte do viés de não",
      "cobertura — de forma desigual entre indicadores, e sem que essa desigualdade pudesse ser",
      "detectada de dentro do inquérito. A incorporação da telefonia móvel, adotada a partir de",
      "2023, corrige a origem do problema e beneficia sobretudo as regiões de menor cobertura fixa,",
      "o que recomenda alocação amostral diferenciada em vez de uniforme. Para as dezessete edições",
      "anteriores, as estimativas de viés aqui apresentadas fornecem a base para interpretar",
      "tendências que atravessem a mudança metodológica."
    ),
    paste(
      "A análise principal restringiu a PNS ao universo do Vigitel, formado pelas 26 capitais",
      "estaduais e pelo Distrito Federal. Embora a base pública da PNS não divulgue o código",
      "municipal, ela divulga a variável de tipo de área, cuja categoria Capital identifica",
      "exatamente esse conjunto de municípios; o recorte é, portanto, exato no nível de agregação",
      "liberado pelo IBGE, e não uma aproximação. A restrição não é detalhe de execução: as",
      "prevalências fora das capitais diferem das capitais em magnitude comparável à do próprio",
      "efeito investigado, de modo que comparar o Vigitel à PNS nacional introduziria um artefato do",
      "tamanho do achado e, para a autoavaliação de saúde, inverteria o sinal da diferença."
    ),
    glue::glue(
      "No mecanismo gerador, a posse de telefone não foi simulada por modelo: utilizou-se a posse ",
      "observada na PNS para cada registro da pseudopopulação, de modo que a dependência entre posse ",
      "de telefone, características sociodemográficas e desfecho é a que existe nos dados, e não a ",
      "que uma especificação paramétrica lhe atribuiria. A exigência de que a posse dependa do ",
      "desfecho — sem a qual o viés seria nulo por construção — foi verificada e é reportada ",
      "explicitamente: ajustando por escolaridade, faixa etária e região, a razão de chances de ter ",
      "telefone fixo associada ao desfecho é de {fmt_num(mec$or[mec$indicador=='Tabagismo atual'],2)} ",
      "para tabagismo e {fmt_num(mec$or[mec$indicador=='Autoavaliação ruim de saúde'],2)} para ",
      "autoavaliação de saúde, e não se distingue de 1 para hipertensão ",
      "({fmt_num(mec$or[mec$indicador=='Hipertensão diagnosticada'],2)}) e diabetes ",
      "({fmt_num(mec$or[mec$indicador=='Diabetes diagnosticado'],2)}) (Tabela S6). A cobertura dos ",
      "quadros foi conferida contra a PNAD Contínua TIC do mesmo ano."
    ),
    glue::glue(
      "Segundo o critério de Cochran, o vício relativo excedeu o limiar de 0,40 na totalidade das ",
      "{nrow(vies)} combinações de indicador e região, tanto quando calculado com o erro-padrão do ",
      "Vigitel (de {fmt_num(min(vies$cochran_vigitel),2)} a {fmt_num(max(vies$cochran_vigitel),2)}) ",
      "quanto com o erro-padrão interno da PNS (de {fmt_num(min(vies$cochran_pns),2)} a ",
      "{fmt_num(max(vies$cochran_pns),2)}) (Tabela S3). Em nenhuma das células, portanto, o viés de ",
      "não cobertura mostrou-se desprezível frente à precisão da estimativa."
    )
  )
)
readr::write_csv(textos, file.path(dir_out, "textos.csv"))

cat("exportado para", dir_out, ":\n")
print(list.files(dir_out))
message("13_exporta_para_docx.R concluido.")
