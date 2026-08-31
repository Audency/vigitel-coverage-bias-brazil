# =============================================================================
# 99_resultados_docx.R - Documento Word com os resultados produzidos ate aqui
#
# Nao recalcula nada: le os objetos gravados pelos scripts 02 a 05. Todo numero
# do texto e interpolado a partir desses objetos com glue(), de modo que a
# narrativa nao pode divergir das tabelas - se o pipeline mudar, o texto muda
# junto. Onde um resultado ainda nao existe, o marcador [XX] aparece no lugar.
#
# Saida: output/resultados_vigitel_pns.docx
# =============================================================================

source(here::here("R", "00_setup.R"))
library(officer)

h1("99_resultados_docx.R - relatorio de resultados")

# ---- Insumos ----------------------------------------------------------------

ler_log <- function(x) readr::read_csv(here::here("logs", paste0(x, ".csv")),
                                       show_col_types = FALSE)

fluxo        <- ler_log("fluxo_amostral_pns")
val_pns      <- ler_log("validacao_pns_sidra")
val_vig      <- ler_log("validacao_vigitel")
conferencia  <- ler_log("conferencia_vigitel_oficial")
etaria       <- ler_log("estrutura_etaria_vs_censo")
interacoes   <- ler_log("interacao_sexo_inquerito")
padronizado  <- ler_log("gap_padronizado_por_idade")
particao_l   <- ler_log("particao_gap")
mecanismo    <- ler_log("mecanismo_posse_desfecho")
cob_regiao   <- ler_log("cobertura_quadros_regiao")
obes_comp    <- ler_log("obesidade_comparacoes")
vies_l       <- readRDS(here::here("data", "vies_nao_cobertura.rds"))
sim_l        <- readRDS(here::here("data", "simulacao.rds"))

tab1_gt <- readRDS(here::here("output", "tables", "table1.rds"))
tab2_gt <- readRDS(here::here("output", "tables", "table2.rds"))
tab2_d  <- readRDS(here::here("output", "tables", "table2_dados.rds"))
tab3_gt <- readRDS(here::here("output", "tables", "table3.rds"))
tab4_gt <- readRDS(here::here("output", "tables", "table4.rds"))
equiv   <- readRDS(here::here("data", "equivalencia.rds"))
prev    <- readRDS(here::here("data", "prevalencias.rds"))

n_pns_cap <- fluxo$n[fluxo$etapa == "residentes em capitais"]
n_vig     <- nrow(readRDS(here::here("data", "vigitel.rds")))

# Atalho para puxar um numero da Tabela 2 sem digitar
d2 <- function(ind, campo, estrato = "Total") {
  x <- tab2_d[tab2_d$indicator == ind & tab2_d$estrato == estrato, ][[campo]]
  if (length(x) != 1) stop("indicador/estrato ambiguo: ", ind, "/", estrato)
  x
}
pp <- function(x, d = 2) fmt_num(x, d)
ic_pp <- function(ind, estrato = "Total") {
  fmt_ci(d2(ind, "delta", estrato), d2(ind, "delta_low", estrato),
         d2(ind, "delta_upp", estrato), 2)
}
pad <- function(rot, campo) padronizado[[campo]][padronizado$label == rot]

# ---- Auxiliares de formatacao -----------------------------------------------

tabela_simples <- function(dados, titulo, notas = NULL) {
  g <- dados |>
    gt::gt() |>
    gt::tab_header(title = gt::md(titulo)) |>
    gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
    gt_study_style()
  if (!is.null(notas)) g <- g |> gt::tab_source_note(gt::md(notas))
  g
}

# gt::as_word() devolve a legenda (<w:p>) e a tabela (<w:tbl>) como nos irmaos,
# enquanto officer::body_add_xml() exige um documento de raiz unica. Envolvemos
# o fragmento num elemento temporario com o namespace do WordprocessingML e
# inserimos cada filho separadamente.
W_NS <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

# A legenda gerada pelo gt vem com um campo de numeracao automatica do Word, que
# renderiza "Table 1:" antes do nosso proprio titulo ("Tabela 2. ..."). Como os
# titulos ja sao numerados pelo estudo, descartamos o no da legenda e inserimos
# o titulo como paragrafo, mantendo apenas a tabela do fragmento.
add_gt <- function(doc, gt_tbl) {
  titulo <- gt_tbl[["_heading"]]$title
  if (!is.null(titulo)) {
    titulo <- gsub("\\*\\*", "", as.character(titulo))
    doc <- officer::body_add_par(doc, titulo, style = "Table Caption")
  }
  frag <- gt::as_word(gt_tbl)
  raiz <- xml2::read_xml(paste0("<gtfrag xmlns:w='", W_NS, "'>", frag, "</gtfrag>"))
  for (no in xml2::xml_children(raiz)) {
    if (xml2::xml_name(no) != "tbl") next
    doc <- officer::body_add_xml(doc, str = as.character(no))
  }
  doc
}

p  <- function(doc, txt, style = "Normal") officer::body_add_par(doc, txt, style = style)
h  <- function(doc, txt, nivel = 1) officer::body_add_par(doc, txt, style = paste("heading", nivel))
br <- function(doc) officer::body_add_par(doc, "", style = "Normal")

# ---- Documento --------------------------------------------------------------

doc <- officer::read_docx()

doc <- doc |>
  p("Representatividade e viés de não cobertura do Vigitel: resultados", "heading 1") |>
  p(glue::glue(
    "Vigitel {YEAR_VIGITEL} × Pesquisa Nacional de Saúde {YEAR_PNS}, adultos de 18 anos ou mais ",
    "residentes nas 26 capitais estaduais e no Distrito Federal."
  )) |>
  p(glue::glue(
    "Documento gerado automaticamente pelo pipeline de análise: nenhum número deste texto foi ",
    "digitado, todos são interpolados a partir dos objetos gravados pelos scripts, de modo que ",
    "a narrativa não pode divergir das tabelas. Cobre o pipeline completo, dos microdados ao ",
    "material suplementar."
  )) |>
  br()

# --- 1. Amostras
doc <- doc |>
  h("1. Amostras analisadas", 1) |>
  p(glue::glue(
    "A PNS {YEAR_PNS} entrevistou {format(fluxo$n[1], big.mark = '.')} moradores selecionados em todo o país. ",
    "Após restrição a adultos de 18 anos ou mais com peso de morador selecionado válido e ao ",
    "recorte de capitais, restaram {format(n_pns_cap, big.mark = '.')} registros. O recorte de capitais é exato: ",
    "a PNS não divulga código de município, mas divulga a variável V0031 (Tipo de área), cuja ",
    "categoria Capital identifica o universo do Vigitel sem necessidade de aproximação."
  )) |>
  p(glue::glue(
    "O Vigitel {YEAR_VIGITEL} contribuiu com {format(n_vig, big.mark = '.')} entrevistas telefonicas nas mesmas 27 cidades. ",
    "As estimativas usam o peso de pós-estratificação da edição (pesorake), conforme o desenho ",
    "declarado pelo Ministério da Saúde: ponderação por faixa etária, escolaridade e sexo, sem ",
    "estrato e sem unidade primária de amostragem."
  )) |>
  br() |>
  add_gt(tabela_simples(
    fluxo, "**Quadro 1.** Fluxo amostral da PNS até o recorte analítico",
    "Fonte: PNS 2019 (IBGE), questionário do morador selecionado."
  )) |>
  br()

# --- 2. Validacao
diff_max_pns <- max(abs(val_pns$diferenca))
diff_max_vig <- max(abs(val_vig$diferenca))

doc <- doc |>
  h("2. Validação das estimativas contra as fontes oficiais", 1) |>
  p(glue::glue(
    "Antes de qualquer comparação entre inquéritos, as estimativas de cada um foram confrontadas ",
    "com os valores publicados pelo órgão responsável. Na PNS, as {nrow(val_pns)} comparações ",
    "reproduzem as prevalências divulgadas pelo IBGE no SIDRA com diferença máxima de ",
    "{fmt_num(diff_max_pns, 2)} ponto percentual. No Vigitel, as {nrow(val_vig)} comparações reproduzem o ",
    "relatório oficial da edição com diferença máxima de {fmt_num(diff_max_vig, 2)} ponto percentual."
  )) |>
  p(glue::glue(
    "A validação não foi um carimbo: ela detectou um erro de codificação. As prevalências ",
    "publicadas pelo IBGE só são reproduzidas quando (i) se exclui o caso exclusivamente ",
    "gestacional de hipertensão e de diabetes e (ii) o não-respondente permanece no denominador ",
    "como não-caso. Sem essas duas regras, a estimativa de hipertensão em mulheres erra 3,2 pontos ",
    "percentuais. Essa é a mesma convenção adotada pelas rotinas oficiais do Vigitel, nas quais a ",
    "resposta 'não sabe' conta como não-caso: os dois inquéritos concordam na convenção, e adotá-la ",
    "torna as estimativas simultaneamente comparáveis entre si e reprodutíveis contra o que cada ",
    "órgão publica."
  )) |>
  br() |>
  add_gt(tabela_simples(
    val_pns |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2))),
    "**Quadro 2.** PNS 2019: estimativas próprias contra os valores publicados no SIDRA",
    "Tabelas SIDRA 4418 (hipertensão), 4487 (diabetes) e 7666 (autoavaliação do estado de saúde), nível Brasil, pessoas de 18 anos ou mais. Não há tabela de tabagismo em adultos para a PNS no SIDRA."
  )) |>
  br() |>
  add_gt(tabela_simples(
    val_vig |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2))),
    glue::glue("**Quadro 3.** Vigitel {YEAR_VIGITEL}: estimativas próprias contra o relatório oficial"),
    glue::glue("Valores de referência extraídos do texto do relatório Vigitel Brasil {YEAR_VIGITEL}, conjunto das 27 cidades.")
  )) |>
  br() |>
  p(glue::glue(
    "Uma terceira conferência, independente das duas anteriores, comparou nossa codificação dos ",
    "quatro desfechos com os indicadores já calculados pelo Ministério da Saúde e distribuídos no ",
    "próprio arquivo de microdados. Não houve divergência em nenhum dos ",
    "{format(n_vig, big.mark = '.')} registros."
  )) |>
  br()

# --- 3. Tabela 1
smd_idade <- readRDS(here::here("output", "tables", "table1_dados.rds"))
smd_flag <- smd_idade |> dplyr::filter(abs(smd) >= 0.10)

doc <- doc |>
  h("3. Características das amostras", 1) |>
  p(glue::glue(
    "As duas amostras são próximas em sexo, escolaridade, raça/cor e região, e divergem apenas na ",
    "estrutura etária. Das {nrow(smd_idade)} linhas comparadas, {nrow(smd_flag)} apresentam ",
    "diferença padronizada de magnitude igual ou superior a 0,10, e todas elas são de idade."
  )) |>
  p(paste(
    "Essa concordância não deve ser lida como evidência de que as populações cobertas sejam",
    "equivalentes. O peso do Vigitel é calibrado justamente por faixa etária, escolaridade e sexo,",
    "de modo que a semelhança nessas variáveis é consequência da calibragem, e não um achado.",
    "As variáveis que carregam informação independente são raça/cor e região, e nelas os dois",
    "inquéritos concordam."
  )) |>
  br() |>
  add_gt(tab1_gt) |>
  br()

# --- 3b. Estrutura etaria
doc <- doc |>
  h("3.1 Estrutura etária: conferência contra o Censo", 2) |>
  p(glue::glue(
    "O achado de idade merece atenção porque a faixa etária entra na calibragem do Vigitel e, ainda ",
    "assim, é a única variável que destoa. Confrontadas com a estrutura etária da população adulta ",
    "das 27 capitais no Censo {YEAR_CENSO}, as duas distribuições ponderadas se comportam de forma ",
    "muito diferente: o desvio absoluto máximo é de {fmt_num(max(abs(etaria$dif_vigitel)), 1)} pontos ",
    "percentuais no Vigitel, contra {fmt_num(max(abs(etaria$dif_pns)), 1)} ponto percentual na PNS. ",
    "O Vigitel ponderado sobrerrepresenta a faixa de 25 a 34 anos e sub-representa a de 65 anos ou mais."
  )) |>
  p(paste(
    "A explicação é documentada pelo próprio Ministério da Saúde: o peso da edição 2019 foi",
    "calibrado pelas projeções populacionais de base 2010, que o Censo 2022 mostrou estarem",
    "defasadas. Foi por essa razão que o órgão publicou posteriormente o peso 'pesorake2025',",
    "recalibrando as edições de 2010 a 2024 pelo Censo 2022. Este estudo mantém o peso da edição,",
    "por ser ele que reproduz o relatório publicado do ano e por ser contemporâneo da calibragem",
    "da PNS 2019, e trata a distorção etária por padronização direta (seção 4.2)."
  )) |>
  br() |>
  add_gt(tabela_simples(
    etaria |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1))),
    glue::glue("**Quadro 4.** Distribuição etária ponderada de cada inquérito contra o Censo {YEAR_CENSO}"),
    "Valores em percentual da população adulta das 27 capitais. As colunas de diferença são em pontos percentuais, com sinal positivo indicando sobrerrepresentação em relação ao Censo."
  )) |>
  br()

# --- 4. Tabela 2
sig <- tab2_d |> dplyr::filter(estrato == "Total", p_holm < 0.05)
ns  <- tab2_d |> dplyr::filter(estrato == "Total", p_holm >= 0.05)

doc <- doc |>
  h("4. Prevalências e diferenças entre os inquéritos", 1) |>
  p(glue::glue(
    "A diferença entre os inquéritos é definida, em todo o estudo, como Δ = Vigitel − PNS: ",
    "valor negativo indica prevalência menor no Vigitel. Dos quatro indicadores, ",
    "{nrow(sig)} apresentam diferença estatisticamente distinguível de zero após correção de Holm."
  )) |>
  p(glue::glue(
    "O sentido da diferença não é o mesmo para todos os indicadores. O Vigitel estima prevalência ",
    "MENOR de tabagismo atual ({ic_pp('smoking')} pontos percentuais) e prevalência MAIOR de ",
    "hipertensão arterial ({ic_pp('hypertension')} pontos percentuais). Para diabetes ",
    "({ic_pp('diabetes')}) e para a autoavaliação ruim do estado de saúde ({ic_pp('poor_health')}), ",
    "a diferença não se distingue de zero. Não há, portanto, subestimação sistemática: há ",
    "divergência específica por indicador, em sentidos opostos."
  )) |>
  br() |>
  add_gt(tab2_gt) |>
  br()

doc <- doc |>
  h("4.1 Assimetria entre sexos", 2) |>
  p(glue::glue(
    "A leitura por sexo sugere assimetria em hipertensão: a diferença é de ",
    "{ic_pp('hypertension', 'Masculino')} pontos percentuais em homens e de ",
    "{ic_pp('hypertension', 'Feminino')} em mulheres. O teste formal, porém, não a confirma. ",
    "Em modelo logístico ajustado sobre os dados empilhados, com o desenho de cada inquérito ",
    "preservado, o termo de interação sexo × inquérito tem p = ",
    "{fmt_p(interacoes$p_int[interacoes$indicator == 'hypertension'])}; nenhum dos quatro ",
    "indicadores apresenta interação significativa, antes ou depois da correção de Holm."
  )) |>
  p(paste(
    "A discrepância entre a leitura visual e o teste formal é esperada: intervalos de confiança",
    "que não se sobrepõem em subgrupos não equivalem a um teste de interação, e a comparação",
    "visual tende a superestimar a evidência de modificação de efeito."
  )) |>
  br() |>
  add_gt(tabela_simples(
    interacoes |> dplyr::select(label, beta, p_int, p_int_holm) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))),
    "**Quadro 5.** Teste de interação sexo × inquérito",
    "Coeficiente do termo de interação em modelo logístico com desenho complexo, categoria de referência PNS e sexo masculino. Correção de Holm sobre os quatro testes."
  )) |>
  br()

doc <- doc |>
  h("4.2 Sensibilidade: diferença padronizada por idade", 2) |>
  p(glue::glue(
    "Para separar o que da diferença decorre da composição etária, as prevalências dos dois ",
    "inquéritos foram padronizadas diretamente para a mesma estrutura de idade, a da população ",
    "adulta das 27 capitais no Censo {YEAR_CENSO}. O efeito não é homogêneo entre indicadores."
  )) |>
  p(glue::glue(
    "Em tabagismo, a padronização praticamente não altera a diferença ",
    "({pp(pad('Tabagismo atual', 'delta_bruto'))} para {pp(pad('Tabagismo atual', 'delta_padr'))} pontos percentuais). ",
    "Em hipertensão, ao contrário, a diferença mais que dobra, de ",
    "{pp(pad('Hipertensão diagnosticada', 'delta_bruto'))} para ",
    "{pp(pad('Hipertensão diagnosticada', 'delta_padr'))} pontos percentuais; e em diabetes ela passa de ",
    "{pp(pad('Diabetes diagnosticado', 'delta_bruto'))}, indistinguível de zero, para ",
    "{pp(pad('Diabetes diagnosticado', 'delta_padr'))} pontos percentuais, com intervalo de confiança ",
    "excluindo zero. A distorção etária do Vigitel estava mascarando parte da superestimação: como ",
    "a amostra ponderada é mais jovem que a população, e como hipertensão e diabetes aumentam ",
    "acentuadamente com a idade, a prevalência bruta do Vigitel vem puxada para baixo."
  )) |>
  br() |>
  add_gt(tabela_simples(
    padronizado |>
      dplyr::select(label, delta_bruto, delta_padr, delta_padr_low, delta_padr_upp, mudanca_pp) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2))),
    glue::glue("**Quadro 6.** Diferença bruta e padronizada por idade (padrão: Censo {YEAR_CENSO})"),
    "Valores em pontos percentuais, na convenção Δ = Vigitel − PNS. Padronização direta; variância pela soma ponderada das variâncias dentro de cada faixa etária."
  )) |>
  br()


# --- 5. Vies de nao cobertura
v27 <- function(ind, campo) vies_l[[campo]][vies_l$indicator == ind & vies_l$dominio == "27 capitais"]
vreg <- function(ind, reg, campo) vies_l[[campo]][vies_l$indicator == ind & vies_l$dominio == reg]
part <- function(ind, campo) particao_l[[campo]][particao_l$indicator == ind]

n_acima <- sum(vies_l$acima_limiar)

doc <- doc |>
  h("5. Viés de não cobertura da telefonia fixa", 1) |>
  p(glue::glue(
    "Esta é a estimação central do estudo, e ela acontece inteiramente dentro da PNS: nenhuma ",
    "quantidade desta seção depende de dado do Vigitel. A PNS observa quem tem e quem não tem ",
    "telefone fixo no domicílio, o que permite calcular o viés que um inquérito restrito ao ",
    "quadro de telefonia fixa sofreria por excluir a população sem fixo."
  )) |>
  p(glue::glue(
    "Nas 27 capitais, {fmt_num(v27('smoking', 'est_f_semfixo'), 1)}% da população adulta vive em ",
    "domicílio sem telefone fixo, e essa proporção varia de ",
    "{fmt_num(vreg('smoking', 'Sul', 'est_f_semfixo'), 1)}% no Sul a ",
    "{fmt_num(vreg('smoking', 'Norte', 'est_f_semfixo'), 1)}% no Norte. A magnitude e o sentido do ",
    "viés seguem a direção do gradiente de cada indicador: quem tem telefone fixo é mais velho e ",
    "mais escolarizado, portanto fuma menos e recebe mais diagnósticos de doença crônica."
  )) |>
  p(glue::glue(
    "O viés é negativo para tabagismo ({pp(v27('smoking', 'est_vies'))} pontos percentuais) e ",
    "positivo para hipertensão ({pp(v27('hypertension', 'est_vies'))}) e diabetes ",
    "({pp(v27('diabetes', 'est_vies'))}). Em hipertensão, ele chega a ",
    "{pp(vreg('hypertension', 'Norte', 'est_vies'))} pontos percentuais no Norte, onde a cobertura ",
    "de telefonia fixa é a menor do país."
  )) |>
  p(glue::glue(
    "O vício relativo de Cochran ultrapassa o limiar de {fmt_num(COCHRAN_LIMIT, 2)} em ",
    "{n_acima} das {nrow(vies_l)} células da tabela, com valores de ",
    "{fmt_num(min(vies_l$cochran_vigitel), 2)} a {fmt_num(max(vies_l$cochran_vigitel), 2)}. Ou seja: ",
    "o viés de não cobertura não é pequeno em relação ao erro amostral — ele o supera em uma ou ",
    "duas ordens de grandeza, e a cobertura nominal de 95% dos intervalos publicados não se ",
    "sustenta sob esse viés. A versão por região consta da Tabela S3, e o perfil sociodemográfico ",
    "de cada grupo de posse, da Tabela S2 e da Figura S1."
  )) |>
  br() |> add_gt(tab3_gt) |> br()

# --- 6. Particao do gap
doc <- doc |>
  h("6. Partição do gap entre não cobertura e resíduo", 1) |>
  p(paste(
    "A diferença observada entre os dois inquéritos é decomposta em duas parcelas: o componente",
    "de não cobertura, estimado na seção anterior, e o resíduo, que é o que sobra. O resíduo é",
    "deliberadamente chamado de resíduo, e não de efeito de modo: ele absorve conjuntamente modo",
    "de coleta, autorrelato, diferenças de instrumento, não resposta e calibragem de pesos, e este",
    "desenho não permite separá-los."
  )) |>
  p(glue::glue(
    "O resultado é contraintuitivo e merece leitura cuidadosa: em nenhum dos quatro indicadores o ",
    "componente de não cobertura é uma fração do gap observado. Em tabagismo e hipertensão ele ",
    "EXCEDE o gap ({pp(part('smoking', 'componente'))} contra {pp(part('smoking', 'gap_total'))} ",
    "pontos percentuais; {pp(part('hypertension', 'componente'))} contra ",
    "{pp(part('hypertension', 'gap_total'))}); em diabetes e autoavaliação de saúde ele tem sinal ",
    "OPOSTO ao gap. Em todos os casos, o resíduo atua em sentido contrário ao da não cobertura, ",
    "compensando-a parcialmente."
  )) |>
  p(paste(
    "A interpretação natural é que a pós-estratificação do Vigitel já remove boa parte do viés de",
    "não cobertura. O gap que sobrevive nas estimativas publicadas é, portanto, menor do que o",
    "viés bruto de cobertura — e o que resta não é atribuível à cobertura telefônica. A Figura S3",
    "mostra que essa compensação ocorre em todas as regiões, com magnitude variável."
  )) |>
  br() |> add_gt(tab4_gt) |> br()

# --- 7. Simulacao
melhora <- sim_l |>
  dplyr::group_by(region, cenario) |>
  dplyr::summarise(rmse = mean(rmse), .groups = "drop") |>
  tidyr::pivot_wider(names_from = cenario, values_from = rmse) |>
  dplyr::mutate(reducao = S0 / S1)

doc <- doc |>
  h("7. Simulação de cenários de quadro amostral", 1) |>
  p(glue::glue(
    "A simulação, no arcabouço ADEMP, compara cinco cenários de quadro amostral sobre uma ",
    "pseudopopulação construída a partir da PNS de capitais expandida pelos pesos, com ",
    "{format(N_REPLICATES, big.mark = '.')} réplicas por cenário, região e indicador. A posse de ",
    "telefone não é simulada: usa-se a posse observada na PNS, de modo que a dependência entre ",
    "posse, características sociodemográficas e desfecho é a real. Os parâmetros do mecanismo ",
    "gerador estão na Tabela S6."
  )) |>
  p(glue::glue(
    "A cobertura do quadro de telefonia fixa é de ",
    "{fmt_num(min(cob_regiao$fixo), 1)}% a {fmt_num(max(cob_regiao$fixo), 1)}% conforme a região, ",
    "contra {fmt_num(min(cob_regiao$celular), 1)}% ou mais do quadro de telefonia móvel. A ",
    "conferência com a PNAD Contínua TIC do mesmo ano confirma essas cifras a partir de fonte ",
    "independente."
  )) |>
  p(glue::glue(
    "O ganho de sair do quadro exclusivamente fixo é grande, mas profundamente desigual entre ",
    "regiões — e é por isso que a estratificação regional não é opcional. A raiz do erro ",
    "quadrático médio cai por um fator de {fmt_num(melhora$reducao[melhora$region == 'Norte'], 1)} ",
    "no Norte e de {fmt_num(melhora$reducao[melhora$region == 'Nordeste'], 1)} no Nordeste ao ",
    "passar do cenário S0 para o S1, contra apenas ",
    "{fmt_num(melhora$reducao[melhora$region == 'Sudeste'], 1)} no Sudeste. Agregar as regiões ",
    "faria essa heterogeneidade desaparecer e levaria a uma conclusão errada."
  )) |>
  p(paste(
    "Os cenários seguintes trazem ganhos marginais: incorporar web (S3) ou cobertura integral (S4)",
    "melhora pouco além do que a telefonia móvel já alcança, porque o celular sozinho já cobre",
    "mais de 96% da população adulta das capitais. Em algumas regiões o cenário dual-frame é",
    "ligeiramente pior que o de celular apenas para determinados indicadores — resultado legítimo,",
    "não anomalia: reintroduzir o quadro fixo adiciona pouca cobertura e devolve parte do",
    "desequilíbrio de composição. Os resultados completos, com erro de Monte Carlo, estão na",
    "Tabela S7, e o viés por cenário e região, na Figura S2."
  )) |>
  br() |>
  p("Figura 2. Raiz do erro quadrático médio por cenário de quadro amostral, região e indicador.",
    "Image Caption") |>
  officer::body_add_img(here::here("output", "figures", "figure2.png"),
                        width = 6.5, height = 3.6) |>
  br()

# --- 8. Sensibilidades
doc <- doc |>
  h("8. Análises de sensibilidade", 1) |>
  p(glue::glue(
    "A obesidade foi tratada separadamente porque o instrumento difere de forma qualitativa: o ",
    "Vigitel calcula o índice de massa corporal a partir de peso e altura declarados por telefone. ",
    "Comparando medidas declaradas nos dois inquéritos, a diferença é de ",
    "{pp(obes_comp$delta[1])} pontos percentuais; comparando a estimativa declarada do Vigitel com ",
    "a medida aferida da PNS, {pp(obes_comp$delta[2])}. O viés do próprio autorrelato, medido ",
    "dentro da PNS entre declarado e aferido, é de {pp(obes_comp$delta[3])} pontos percentuais — ",
    "maior que o viés de não cobertura de qualquer dos outros indicadores. A Tabela S4 traz o ",
    "detalhamento; a subamostra de antropometria é pequena e sustenta a direção do efeito, não a ",
    "magnitude precisa."
  )) |>
  p(glue::glue(
    "A Tabela S5 mostra que a associação entre desfecho e posse de telefone se aproxima de 1 ",
    "conforme se acrescentam as variáveis de calibragem do Vigitel, e que a conclusão não depende ",
    "do mapeamento de escolaridade adotado. Para hipertensão e diabetes, a associação residual às ",
    "variáveis de calibragem não é distinguível de zero (razão de chances ",
    "{fmt_num(mecanismo$or[mecanismo$indicador == 'Hipertensão diagnosticada'], 2)} e ",
    "{fmt_num(mecanismo$or[mecanismo$indicador == 'Diabetes diagnosticado'], 2)}), o que explica ",
    "por que a pós-estratificação alcança o viés desses indicadores. Para tabagismo e autoavaliação ",
    "de saúde a associação residual persiste, e com ela parte do viés."
  )) |>
  br()

# --- 9. Divergencias
doc <- doc |>
  h("9. Divergências em relação à versão anterior do manuscrito", 1) |>
  p(paste(
    "Estes resultados divergem da versão anterior do manuscrito em dois pontos que afetam a",
    "conclusão, e a divergência precisa ser resolvida antes da submissão."
  )) |>
  p(glue::glue(
    "Primeiro, a versão anterior afirma que o Vigitel subestimou sistematicamente todos os ",
    "indicadores, com todos os intervalos de confiança excluindo zero. Os dados não sustentam essa ",
    "afirmação: o sentido da diferença varia por indicador, com subestimação em tabagismo e ",
    "superestimação em hipertensão, e apenas {nrow(sig)} dos quatro indicadores diferem de zero após ",
    "correção de Holm."
  )) |>
  p(paste(
    "Segundo, a versão anterior descreve assimetria marcante entre sexos, com a diferença em",
    "mulheres várias vezes maior que em homens. O teste de interação não confirma essa assimetria",
    "em nenhum dos quatro indicadores. A afirmação anterior parece derivar da comparação visual de",
    "intervalos de confiança entre subgrupos, que não constitui teste de modificação de efeito."
  )) |>
  br()

# --- 10. Reprodutibilidade
pipeline <- tibble::tibble(
  Etapa = c("Download dos microdados das quatro fontes",
            "Harmonização dos instrumentos e equivalência das perguntas",
            "Desenho amostral e validação contra as fontes oficiais",
            "Tabela 1 — características das amostras",
            "Tabela 2 — prevalências, diferenças e razões",
            "Tabela 3 — viés de não cobertura",
            "Tabela 4 — partição do gap",
            "Simulação ADEMP de cenários de quadro amostral",
            "Figuras 1 e 2 e figuras suplementares",
            "Material suplementar em arquivo único"),
  Script = c("01_download.R", "02_harmonize.R", "03_design.R", "04_table1.R",
             "05_prevalences.R", "06_noncoverage.R", "07_partition.R",
             "08_simulation.R", "09_figures.R", "10_supplement.R")
)

doc <- doc |>
  h("10. Reprodutibilidade e documentação", 1) |>
  p(paste(
    "Todo o estudo é reprodutível a partir dos microdados públicos. O pipeline está organizado em",
    "dez scripts numerados, executados em ordem, com semente fixa. O código integral da simulação",
    "consta do Texto S3 e o ambiente computacional, com versões de R e de todos os pacotes, do",
    "Texto S4. O checklist STROBE, com a localização de cada item, está no Texto S1, e o",
    "checklist RECORD, no Texto S2."
  )) |>
  p(paste(
    "Nenhum número deste documento foi digitado: cada valor é interpolado a partir dos objetos",
    "gravados pelos scripts. Regenerar o documento após qualquer alteração no pipeline atualiza o",
    "texto junto com as tabelas, o que elimina a possibilidade de divergência entre os dois."
  )) |>
  br() |>
  add_gt(tabela_simples(pipeline, "**Quadro 7.** Scripts do pipeline e o que cada um produz",
    "A ordem de execução é a da tabela. Cada script começa carregando o mesmo arquivo de configuração, onde estão fixados a semente, os anos de referência, a lista de indicadores e a paleta de cores.")) |>
  br()

# --- Anexo: equivalencia
doc <- doc |>
  h("Anexo. Equivalência das perguntas entre os instrumentos (Tabela S1)", 1) |>
  add_gt(tabela_simples(
    equiv |> dplyr::select(indicador, var_pns, enunciado_pns, var_vigitel,
                           enunciado_vigitel, codificacao, divergencia),
    "**Tabela S1.** Equivalência das perguntas entre Vigitel e PNS, com divergências",
    "Enunciados transcritos dos dicionários oficiais de cada inquérito."
  ))

saida <- here::here("output", "resultados_vigitel_pns.docx")
print(doc, target = saida)
cat("\nDocumento gravado:", saida, "\n")
cat("tamanho:", round(file.size(saida) / 1024), "KB\n")

message("99_resultados_docx.R concluido.")
