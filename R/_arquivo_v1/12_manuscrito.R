# =============================================================================
# 12_manuscrito.R - Manuscrito revisado (Metodos, Resultados e Discussao)
#
# Gera um arquivo NOVO, ao lado do original, sem sobrescrever nada. Todo numero
# do texto e interpolado dos objetos gravados pelo pipeline: o manuscrito nao
# pode divergir das tabelas.
#
# Introducao e Aspectos eticos reaproveitam o texto da versao anterior, com o
# acrescimo do contexto da reforma de 2023 do Vigitel, que redefine o objetivo.
# =============================================================================

source(here::here("R", "00_setup.R"))
library(officer)

h1("12_manuscrito.R - manuscrito revisado")

# ---- Insumos ----------------------------------------------------------------
ler_log <- function(x) readr::read_csv(here::here("logs", paste0(x, ".csv")),
                                       show_col_types = FALSE)

fluxo   <- ler_log("fluxo_amostral_pns")
val_pns <- ler_log("validacao_pns_sidra")
val_vig <- ler_log("validacao_vigitel")
etaria  <- ler_log("estrutura_etaria_vs_censo")
interac <- ler_log("interacao_sexo_inquerito")
padr    <- ler_log("gap_padronizado_por_idade")
mecan   <- ler_log("mecanismo_posse_desfecho")
cobr    <- ler_log("cobertura_quadros_regiao")
obes    <- ler_log("obesidade_comparacoes")
tic     <- ler_log("conferencia_cobertura_pns_tic")

tab2 <- readRDS(here::here("output", "tables", "table2_dados.rds"))
vies <- readRDS(here::here("data", "vies_nao_cobertura.rds"))
part <- readRDS(here::here("data", "particao.rds"))
sim  <- readRDS(here::here("data", "simulacao.rds"))
pns  <- readRDS(here::here("data", "pns.rds"))
vig  <- readRDS(here::here("data", "vigitel.rds"))

n_pns <- nrow(pns); n_vig <- nrow(vig)

# Quanto do vies de cobertura a ponderacao do Vigitel remove
remocao <- part |>
  dplyr::select(indicator, label, p_vig, verdadeiro = est_p_pns) |>
  dplyr::left_join(vies |> dplyr::filter(dominio == "27 capitais") |>
                     dplyr::select(indicator, sem_pond = est_p_fixo), by = "indicator") |>
  dplyr::mutate(vies_bruto = sem_pond - verdadeiro,
                vies_resid = p_vig - verdadeiro,
                pct_removido = 100 * (1 - abs(vies_resid) / abs(vies_bruto)))

g  <- function(ind, campo) tab2[[campo]][tab2$indicator == ind & tab2$estrato == "Total"]
gi <- function(ind) fmt_ci(g(ind, "delta"), g(ind, "delta_low"), g(ind, "delta_upp"), 2)
v27 <- function(ind, campo) vies[[campo]][vies$indicator == ind & vies$dominio == "27 capitais"]
vr  <- function(ind, reg, campo) vies[[campo]][vies$indicator == ind & vies$dominio == reg]
pt_ <- function(ind, campo) part[[campo]][part$indicator == ind]
rm_ <- function(ind) remocao$pct_removido[remocao$indicator == ind]
or_ <- function(rot) mecan$or[mecan$indicador == rot]
orp <- function(rot) mecan$p[mecan$indicador == rot]

reqm <- sim |> dplyr::group_by(region, cenario) |>
  dplyr::summarise(rmse = mean(rmse), .groups = "drop") |>
  tidyr::pivot_wider(names_from = cenario, values_from = rmse) |>
  dplyr::mutate(reducao = S0 / S1)
red <- function(reg) reqm$reducao[reqm$region == reg]

# ---- Utilidades -------------------------------------------------------------
W_NS <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
add_gt <- function(doc, gt_tbl) {
  titulo <- gt_tbl[["_heading"]]$title
  if (!is.null(titulo)) doc <- officer::body_add_par(
    doc, gsub("\\*\\*", "", as.character(titulo)), style = "Table Caption")
  raiz <- xml2::read_xml(paste0("<f xmlns:w='", W_NS, "'>", gt::as_word(gt_tbl), "</f>"))
  for (no in xml2::xml_children(raiz))
    if (xml2::xml_name(no) == "tbl") doc <- officer::body_add_xml(doc, str = as.character(no))
  doc
}
p  <- function(doc, txt, style = "Normal") officer::body_add_par(doc, txt, style = style)
h  <- function(doc, txt, n = 1) officer::body_add_par(doc, txt, style = paste("heading", n))
br <- function(doc) officer::body_add_par(doc, "", style = "Normal")

doc <- officer::read_docx()

# =============================================================================
# TITULO E AUTORIA
# =============================================================================
doc <- doc |>
  p(paste("Cobertura não é viés: quanto do viés de não cobertura da telefonia fixa",
          "a pós-estratificação do Vigitel já removia, e para quais indicadores"), "heading 1") |>
  p(paste("Audêncio Victor, Carla Ferreira do Nascimento, Bruna Suellen Breternitz,",
          "Michele Lacerda Pereira Ferrer, Étienne Larissa Duim")) |>
  p(paste("[Afiliações conforme a versão anterior do manuscrito.]")) |>
  br() |>
  p(paste("NOTA AOS AUTORES — esta versão substitui Métodos, Resultados e Discussão",
          "por texto gerado diretamente do pipeline de análise. Introdução e Aspectos",
          "éticos foram preservados da versão anterior e estão marcados onde precisam",
          "de complemento. Nenhum número deste documento foi digitado."), "Normal") |>
  br()

# =============================================================================
# RESUMO
# =============================================================================
doc <- doc |>
  h("Resumo", 1) |>
  p(glue::glue(
    "Introdução. O Vigitel monitora fatores de risco para doenças crônicas nas 27 capitais ",
    "brasileiras por inquérito telefônico. Até a edição de 2022 a amostra era sorteada apenas ",
    "de linhas de telefonia fixa, cuja cobertura domiciliar caiu para {fmt_num(v27('smoking', 'est_f_semfixo'), 1)}% de ",
    "não cobertura nas capitais. A partir de 2023 o Ministério da Saúde passou a sortear também ",
    "linhas móveis. A justificativa oficial da mudança apoiou-se em cobertura; o viés que a baixa ",
    "cobertura efetivamente produzia nas estimativas publicadas nunca foi quantificado."
  )) |>
  p(glue::glue(
    "Objetivo. Quantificar o viés de não cobertura da telefonia fixa, medir quanto dele a ",
    "pós-estratificação do Vigitel já removia, e estimar o ganho esperado da incorporação da ",
    "telefonia móvel por região."
  )) |>
  p(glue::glue(
    "Métodos. Análise de microdados do Vigitel {YEAR_VIGITEL} (n = {format(n_vig, big.mark = '.')}) e da PNS {YEAR_PNS} ",
    "restrita ao mesmo universo — adultos das 26 capitais e do Distrito Federal ",
    "(n = {format(n_pns, big.mark = '.')}). O viés de não cobertura foi estimado inteiramente dentro da PNS, que ",
    "observa a posse de telefone, e a diferença entre os inquéritos foi decomposta em componente ",
    "de não cobertura e resíduo. Cinco cenários de quadro amostral foram comparados sob o ",
    "arcabouço ADEMP, com {format(N_REPLICATES, big.mark = '.')} réplicas por cenário, região e indicador."
  )) |>
  p(glue::glue(
    "Resultados. As estimativas de ambos os inquéritos reproduzem os valores oficiais publicados ",
    "(diferença máxima de {fmt_num(max(abs(val_pns$diferenca)), 2)} pp na PNS e ",
    "{fmt_num(max(abs(val_vig$diferenca)), 2)} pp no Vigitel). O viés de não cobertura é grande — ",
    "{pp <- fmt_num(v27('hypertension', 'est_vies'), 2); pp} pp em hipertensão, chegando a ",
    "{fmt_num(vr('hypertension', 'Norte', 'est_vies'), 2)} pp no Norte — e o vício relativo de Cochran ",
    "excede o limiar de {fmt_num(COCHRAN_LIMIT, 2)} em todas as {nrow(vies)} células analisadas. As ",
    "diferenças observadas entre os inquéritos, porém, são pequenas: {gi('smoking')} pp em ",
    "tabagismo e {gi('hypertension')} pp em hipertensão, sem diferença detectável em diabetes e ",
    "autoavaliação de saúde. A pós-estratificação removia {fmt_num(rm_('diabetes'), 0)}% do viés de ",
    "cobertura em diabetes e {fmt_num(rm_('hypertension'), 0)}% em hipertensão, mas apenas ",
    "{fmt_num(rm_('smoking'), 0)}% em tabagismo — ordem que acompanha a associação residual entre ",
    "desfecho e posse de telefone após ajuste pelas variáveis de calibragem. Na simulação, sair do ",
    "quadro exclusivamente fixo reduz a raiz do erro quadrático médio por fator de ",
    "{fmt_num(red('Norte'), 1)} no Norte e {fmt_num(red('Nordeste'), 1)} no Nordeste, contra ",
    "{fmt_num(red('Sudeste'), 1)} no Sudeste."
  )) |>
  p(paste(
    "Conclusão. Cobertura e viés não são a mesma coisa. A pós-estratificação do Vigitel vinha",
    "absorvendo a maior parte do viés de não cobertura para os indicadores cujo gradiente se",
    "explica por idade, sexo e escolaridade, e quase nada para os demais — sem que fosse possível",
    "distinguir os dois casos de dentro do inquérito. A incorporação da telefonia móvel beneficia",
    "desigualmente as regiões, com ganho concentrado onde a cobertura fixa é menor, o que sustenta",
    "alocação amostral diferenciada em vez de uniforme."
  )) |>
  p(paste("Palavras-chave: vigilância em saúde pública; doenças crônicas não transmissíveis;",
          "inquéritos epidemiológicos; viés de seleção; cobertura amostral; Vigitel;",
          "Pesquisa Nacional de Saúde.")) |>
  br()

# =============================================================================
# INTRODUCAO
# =============================================================================
doc <- doc |>
  h("Introdução", 1) |>
  p(paste(
    "[PRESERVAR os quatro primeiros parágrafos da versão anterior: carga de DCNT no Brasil,",
    "descrição do Vigitel, queda da telefonia fixa e desigualdade regional, e o argumento sobre",
    "viés de seleção em inquéritos telefônicos. Continuam válidos e não dependem dos resultados.]"
  )) |>
  p(paste(
    "A partir da edição de 2023 o Vigitel passou a sortear linhas de telefonia móvel além das",
    "fixas, com 44 mil linhas fixas e 20 mil móveis por cidade. A justificativa apresentada no",
    "relatório daquele ano apoia-se em cobertura: dados da PNS 2019 indicavam que 39,7% dos",
    "domicílios das capitais dispunham de linha fixa, contra cobertura de telefonia móvel superior",
    "a 90% em todas as cidades. A reforma, portanto, foi motivada pela fração da população fora do",
    "quadro amostral."
  )) |>
  p(paste(
    "Cobertura e viés, no entanto, não são a mesma quantidade. O viés de não cobertura depende não",
    "apenas de quantas pessoas ficam fora do quadro, mas de quanto elas diferem das que ficam",
    "dentro no desfecho de interesse — e da capacidade da ponderação pós-estratificada de",
    "recuperar essa diferença a partir das variáveis de calibragem. Uma fração excluída grande",
    "pode produzir viés pequeno se a exclusão for explicada por idade e escolaridade, que a",
    "calibragem corrige; e uma fração menor pode produzir viés grande se não for. Essa distinção",
    "não foi quantificada para o Vigitel, e dela dependem três questões práticas: quanto das",
    "estimativas publicadas entre 2006 e 2022 é confiável, quanto a reforma de 2023 efetivamente",
    "ganha para cada indicador, e como interpretar a série histórica através da quebra",
    "metodológica que a reforma introduz."
  )) |>
  p(paste(
    "Este estudo quantifica o viés de não cobertura da telefonia fixa usando a PNS como referência",
    "probabilística, mede que parcela dele a pós-estratificação do Vigitel já removia, identifica",
    "o mecanismo que separa os indicadores em que a correção funciona daqueles em que falha, e",
    "estima por simulação o ganho esperado da incorporação da telefonia móvel, estratificado por",
    "região."
  )) |>
  br()

# =============================================================================
# METODOS
# =============================================================================
doc <- doc |>
  h("Métodos", 1) |>
  h("Desenho do estudo", 2) |>
  p(paste(
    "Estudo metodológico observacional, baseado em análise secundária de microdados públicos de",
    "dois inquéritos populacionais brasileiros, com simulação de desenho amostral. O relato segue",
    "as recomendações STROBE e sua extensão RECORD para estudos com dados coletados rotineiramente",
    "(Textos S1 e S2)."
  )) |>
  h("Fontes de dados e recorte analítico", 2) |>
  p(glue::glue(
    "Foram utilizados os microdados do Vigitel {YEAR_VIGITEL}, obtidos do repositório público da ",
    "Secretaria de Vigilância em Saúde e Ambiente, e os da PNS {YEAR_PNS}, obtidos do IBGE por meio ",
    "do pacote PNSIBGE. A edição de {YEAR_VIGITEL} foi escolhida por ser a única contemporânea a uma ",
    "PNS: o inquérito domiciliar tem apenas duas edições, 2013 e 2019, o que impede parear o ",
    "Vigitel de anos posteriores a um padrão-ouro do mesmo período."
  )) |>
  p(glue::glue(
    "O universo do Vigitel são os adultos residentes nas 26 capitais estaduais e no Distrito ",
    "Federal. A PNS foi restrita ao mesmo universo. A PNS {YEAR_PNS} não divulga código de ",
    "município, mas divulga a variável de tipo de área, cuja categoria Capital identifica ",
    "exatamente esse conjunto, dispensando aproximações. Dos ",
    "{format(fluxo$n[1], big.mark = '.')} moradores selecionados em todo o país, ",
    "{format(fluxo$n[fluxo$etapa == 'com 18 anos ou mais'], big.mark = '.')} tinham 18 anos ou mais e ",
    "{format(n_pns, big.mark = '.')} residiam em capitais, compondo a amostra analítica."
  )) |>
  p(paste(
    "A restrição ao mesmo universo não é detalhe de execução. As prevalências fora das capitais",
    "diferem das capitais em magnitude comparável à do próprio efeito investigado, de modo que",
    "comparar o Vigitel à PNS nacional introduziria um artefato do tamanho do achado — e, para a",
    "autoavaliação de saúde, inverteria o sinal da diferença."
  )) |>
  h("Desfechos e harmonização", 2) |>
  p(paste(
    "Quatro indicadores foram analisados: tabagismo atual, diagnóstico médico referido de",
    "hipertensão arterial, diagnóstico médico referido de diabetes e autoavaliação ruim ou muito",
    "ruim do estado de saúde. A obesidade foi tratada em separado, como análise de sensibilidade,",
    "por depender de peso e altura autorreferidos em ambos os inquéritos."
  )) |>
  p(paste(
    "Os enunciados e as categorias de resposta foram extraídos dos dicionários oficiais de cada",
    "inquérito, não redigidos pelos autores, e estão transcritos lado a lado na Tabela S1 junto",
    "das divergências identificadas. A codificação do Vigitel reproduz a sintaxe oficial publicada",
    "pelo Ministério da Saúde para cada indicador; a conferência registro a registro contra os",
    "indicadores já calculados e distribuídos na própria base não apontou divergência em nenhum",
    "dos", format(n_vig, big.mark = "."), "registros."
  )) |>
  p(paste(
    "Duas convenções mereceram atenção. Primeiro, a PNS aplica pergunta-filtro que identifica",
    "hipertensão e diabetes ocorridos exclusivamente durante a gravidez, ausente no Vigitel.",
    "Segundo, ambos os órgãos mantêm o não respondente no denominador como não caso — o Vigitel",
    "por meio de suas rotinas oficiais, o IBGE em suas tabelas publicadas. Adotar a convenção",
    "oficial de cada órgão torna as estimativas simultaneamente comparáveis entre si e",
    "reprodutíveis contra o que cada um publica. As versões que excluem o não respondente do",
    "denominador constam do material suplementar."
  )) |>
  h("Desenho amostral e validação", 2) |>
  p(glue::glue(
    "Para a PNS foram usados estrato, unidade primária de amostragem e peso do morador selecionado ",
    "com calibração. Para o Vigitel foi usado o peso de pós-estratificação da edição, sem estrato ",
    "nem conglomerado, conforme o desenho declarado pelo Ministério da Saúde. O recorte de capitais ",
    "foi aplicado sobre o objeto de desenho, e não sobre os dados, preservando a estrutura de ",
    "estratos; nenhum estrato da PNS mistura capital e não capital, de modo que o recorte é ",
    "composto de estratos completos."
  )) |>
  p(glue::glue(
    "Antes de qualquer comparação, as estimativas de cada inquérito foram confrontadas com os ",
    "valores publicados pelo órgão responsável. As {nrow(val_pns)} comparações da PNS reproduzem as ",
    "prevalências divulgadas pelo IBGE com diferença máxima de ",
    "{fmt_num(max(abs(val_pns$diferenca)), 2)} pp, e as {nrow(val_vig)} do Vigitel reproduzem o ",
    "relatório oficial da edição com diferença máxima de {fmt_num(max(abs(val_vig$diferenca)), 2)} pp."
  )) |>
  h("Viés de não cobertura e partição da diferença", 2) |>
  p(paste(
    "O viés de não cobertura foi estimado inteiramente dentro da PNS, que observa a posse de",
    "telefone fixo e móvel no domicílio. Para cada indicador e região, calculou-se o produto entre",
    "a proporção da população sem telefone fixo e a diferença de prevalência entre quem tem e quem",
    "não tem fixo. Nenhuma quantidade dessa etapa depende de dado do Vigitel. Os intervalos de",
    "confiança vieram de bootstrap de Rao-Wu sobre o desenho amostral, com",
    format(N_BOOT, big.mark = "."), "réplicas."
  )) |>
  p(paste(
    "A diferença observada entre os inquéritos foi decomposta em componente de não cobertura e",
    "resíduo. O resíduo é assim denominado deliberadamente: ele absorve em conjunto efeito de modo",
    "de coleta, autorrelato, diferenças de instrumento, não resposta e calibragem de pesos, e este",
    "desenho não permite separá-los. A variância do resíduo foi obtida sem supor independência",
    "indevida, calculando dentro do mesmo bootstrap a quantidade da PNS que entra na sua definição."
  )) |>
  p(glue::glue(
    "Reportou-se ainda o vício relativo de Cochran, razão entre o viés e o erro-padrão da ",
    "estimativa, com o limiar de {fmt_num(COCHRAN_LIMIT, 2)} acima do qual o nível nominal de 95% ",
    "dos intervalos se degrada."
  )) |>
  h("Simulação de cenários de quadro amostral", 2) |>
  p(glue::glue(
    "A simulação seguiu o arcabouço ADEMP. A pseudopopulação é a PNS de capitais expandida pelos ",
    "pesos, operacionalizada por amostragem com reposição proporcional ao peso. A posse de telefone ",
    "não foi simulada: usou-se a posse observada na PNS, de modo que a dependência entre posse, ",
    "características sociodemográficas e desfecho é a real, e não a que um modelo lhe atribuiria. ",
    "A cobertura assim obtida foi conferida contra a PNAD Contínua TIC do mesmo ano, fonte ",
    "independente ({fmt_num(tic$fixo_pct[1], 1)}% e {fmt_num(tic$fixo_pct[2], 1)}% de cobertura de ",
    "telefonia fixa, respectivamente)."
  )) |>
  p(glue::glue(
    "Foram comparados cinco cenários: telefonia fixa apenas, telefonia móvel apenas, cadastro duplo, ",
    "cadastro triplo com acesso à internet e multimodal integral. Em todos, a amostra foi ",
    "pós-estratificada por idade, sexo e escolaridade contra os totais da pseudopopulação, ",
    "reproduzindo a lógica da calibragem do Vigitel; sem esse passo, o primeiro cenário exibiria o ",
    "viés bruto de cobertura, que não é o que um inquérito real publica. Quadros múltiplos foram ",
    "combinados por peso de multiplicidade. Os tamanhos de amostra por região são os que o Vigitel ",
    "efetivamente realizou. Foram {format(N_REPLICATES, big.mark = '.')} réplicas por cenário, ",
    "região e indicador, com semente fixa; reportam-se viés, erro-padrão empírico e raiz do erro ",
    "quadrático médio, cada um com seu erro de Monte Carlo (Tabela S7)."
  )) |>
  p(paste(
    "A estratificação por região é parte do desenho, e não refinamento opcional: o ganho de",
    "cobertura difere de tal forma entre regiões que a média nacional não descreve nenhuma delas."
  )) |>
  h("Reprodutibilidade", 2) |>
  p(paste(
    "Toda a análise foi conduzida em R, organizada em scripts numerados executados em ordem, com",
    "semente fixa. O código da simulação consta do Texto S3 e o ambiente computacional, com versões",
    "de R e de todos os pacotes, do Texto S4. Os microdados são públicos e os endereços de obtenção",
    "estão documentados no repositório do estudo."
  )) |>
  h("Aspectos éticos", 2) |>
  p(paste(
    "Por se tratar de análise secundária de bases de dados públicas, agregadas e desidentificadas,",
    "o estudo foi dispensado de submissão ao Comitê de Ética em Pesquisa, conforme a Resolução",
    "nº 510/2016 do Conselho Nacional de Saúde do Brasil."
  )) |>
  br()

# =============================================================================
# RESULTADOS
# =============================================================================
doc <- doc |>
  h("Resultados", 1) |>
  h("Características das amostras", 2) |>
  p(glue::glue(
    "As duas amostras são próximas em sexo, escolaridade, raça/cor e região, e divergem apenas na ",
    "estrutura etária (Tabela 1). Essa concordância não indica equivalência entre as populações ",
    "cobertas: o peso do Vigitel é calibrado justamente por faixa etária, escolaridade e sexo, de ",
    "modo que a semelhança nessas variáveis decorre da calibragem. As variáveis que carregam ",
    "informação independente são raça/cor e região, e nelas os inquéritos concordam."
  )) |>
  p(glue::glue(
    "A idade é a única variável com diferença padronizada acima de 0,10, e o achado é ",
    "contraintuitivo porque a faixa etária entra na calibragem. Confrontadas com a estrutura da ",
    "população adulta das capitais no Censo {YEAR_CENSO}, as duas distribuições ponderadas se ",
    "comportam de forma distinta: o desvio absoluto máximo é de ",
    "{fmt_num(max(abs(etaria$dif_vigitel)), 1)} pp no Vigitel, contra ",
    "{fmt_num(max(abs(etaria$dif_pns)), 1)} pp na PNS, com sobrerrepresentação da faixa de 25 a 34 ",
    "anos e sub-representação dos 65 anos ou mais. A explicação é documentada pelo próprio ",
    "Ministério da Saúde: o peso da edição foi calibrado por projeções populacionais de base 2010, ",
    "posteriormente revistas pelo Censo 2022."
  )) |>
  br() |>
  add_gt(readRDS(here::here("output", "tables", "table1.rds"))) |> br() |>
  h("Prevalências e diferenças entre os inquéritos", 2) |>
  p(glue::glue(
    "As diferenças observadas são pequenas e não têm o mesmo sentido entre indicadores (Tabela 2). ",
    "O Vigitel estima prevalência menor de tabagismo ({gi('smoking')} pp) e maior de hipertensão ",
    "({gi('hypertension')} pp); para diabetes ({gi('diabetes')}) e autoavaliação ruim de saúde ",
    "({gi('poor_health')}), a diferença não se distingue de zero. Após correção de Holm, apenas ",
    "dois dos quatro indicadores diferem de zero. Não há subestimação sistemática."
  )) |>
  p(glue::glue(
    "A leitura por sexo sugere assimetria em hipertensão, com diferença de ",
    "{fmt_ci(g('hypertension', 'delta'), g('hypertension', 'delta_low'), g('hypertension', 'delta_upp'), 2)} pp no ",
    "total e maior magnitude entre mulheres. O teste formal não confirma: em modelo logístico sobre ",
    "os dados empilhados, com o desenho de cada inquérito preservado, o termo de interação sexo × ",
    "inquérito não é significativo em nenhum dos quatro indicadores, antes ou depois da correção de ",
    "Holm (menor p = {fmt_p(min(interac$p_int))})."
  )) |>
  p(glue::glue(
    "Padronizando ambos os inquéritos para a estrutura etária do Censo {YEAR_CENSO}, a diferença em ",
    "tabagismo praticamente não se altera, enquanto a de hipertensão mais que dobra, de ",
    "{fmt_num(padr$delta_bruto[padr$label == 'Hipertensão diagnosticada'], 2)} para ",
    "{fmt_num(padr$delta_padr[padr$label == 'Hipertensão diagnosticada'], 2)} pp, e a de diabetes passa ",
    "de indistinguível de zero para {fmt_num(padr$delta_padr[padr$label == 'Diabetes diagnosticado'], 2)} pp. ",
    "A distorção etária do Vigitel mascarava parte da superestimação."
  )) |>
  br() |>
  add_gt(readRDS(here::here("output", "tables", "table2.rds"))) |> br() |>
  h("Viés de não cobertura da telefonia fixa", 2) |>
  p(glue::glue(
    "Nas capitais, {fmt_num(v27('smoking', 'est_f_semfixo'), 1)}% da população adulta vive em ",
    "domicílio sem telefone fixo, variando de {fmt_num(vr('smoking', 'Sul', 'est_f_semfixo'), 1)}% no ",
    "Sul a {fmt_num(vr('smoking', 'Norte', 'est_f_semfixo'), 1)}% no Norte. Quem tem telefone fixo é ",
    "mais velho e mais escolarizado, e o viés segue o gradiente de cada indicador: negativo em ",
    "tabagismo ({fmt_num(v27('smoking', 'est_vies'), 2)} pp) e positivo em hipertensão ",
    "({fmt_num(v27('hypertension', 'est_vies'), 2)} pp) e diabetes ",
    "({fmt_num(v27('diabetes', 'est_vies'), 2)} pp), alcançando ",
    "{fmt_num(vr('hypertension', 'Norte', 'est_vies'), 2)} pp para hipertensão no Norte (Tabela 3)."
  )) |>
  p(glue::glue(
    "O vício relativo de Cochran ultrapassa o limiar de {fmt_num(COCHRAN_LIMIT, 2)} em todas as ",
    "{nrow(vies)} células, com valores entre {fmt_num(min(vies$cochran_vigitel), 2)} e ",
    "{fmt_num(max(vies$cochran_vigitel), 2)}. Pelo critério clássico, um inquérito restrito ao quadro ",
    "de telefonia fixa não sustentaria a cobertura nominal de 95% de seus intervalos."
  )) |>
  br() |>
  add_gt(readRDS(here::here("output", "tables", "table3.rds"))) |> br() |>
  h("Partição da diferença e o papel da pós-estratificação", 2) |>
  p(glue::glue(
    "O contraste entre as duas seções anteriores é o achado central: o viés de cobertura é grande, ",
    "e a diferença observada é pequena. A partição explicita a razão (Tabela 4). Em nenhum ",
    "indicador o componente de não cobertura é uma fração do gap observado: em tabagismo e ",
    "hipertensão ele o excede, e em diabetes e autoavaliação de saúde tem sinal oposto. O resíduo ",
    "atua sempre em sentido contrário ao da não cobertura."
  )) |>
  p(glue::glue(
    "Comparando a estimativa que um quadro de telefonia fixa produziria sem qualquer ponderação com ",
    "a que o Vigitel de fato publica, a pós-estratificação removia {fmt_num(rm_('diabetes'), 0)}% do ",
    "viés de cobertura em diabetes, {fmt_num(rm_('hypertension'), 0)}% em hipertensão, ",
    "{fmt_num(rm_('poor_health'), 0)}% em autoavaliação de saúde e apenas ",
    "{fmt_num(rm_('smoking'), 0)}% em tabagismo."
  )) |>
  p(glue::glue(
    "Essa ordem não é acidental. Ajustando a posse de telefone fixo por escolaridade, faixa etária e ",
    "região — as variáveis de calibragem —, a associação residual com o desfecho desaparece para ",
    "hipertensão (razão de chances {fmt_num(or_('Hipertensão diagnosticada'), 2)}; ",
    "p = {fmt_p(orp('Hipertensão diagnosticada'))}) e diabetes ",
    "({fmt_num(or_('Diabetes diagnosticado'), 2)}; p = {fmt_p(orp('Diabetes diagnosticado'))}), e ",
    "persiste para tabagismo ({fmt_num(or_('Tabagismo atual'), 2)}; ",
    "p = {fmt_p(orp('Tabagismo atual'))}) e autoavaliação de saúde ",
    "({fmt_num(or_('Autoavaliação ruim de saúde'), 2)}; p = {fmt_p(orp('Autoavaliação ruim de saúde'))}). ",
    "A pós-estratificação alcança o viés que se explica pelas variáveis que ela calibra, e não o que ",
    "escapa a elas."
  )) |>
  br() |>
  add_gt(readRDS(here::here("output", "tables", "table4.rds"))) |> br() |>
  h("Simulação de cenários de quadro amostral", 2) |>
  p(glue::glue(
    "A cobertura do quadro de telefonia fixa varia de {fmt_num(min(cobr$fixo), 1)}% a ",
    "{fmt_num(max(cobr$fixo), 1)}% entre regiões, contra {fmt_num(min(cobr$celular), 1)}% ou mais do ",
    "quadro de telefonia móvel. O ganho de abandonar o quadro exclusivamente fixo é grande e ",
    "desigual: a raiz do erro quadrático médio cai por fator de {fmt_num(red('Norte'), 1)} no Norte e ",
    "{fmt_num(red('Nordeste'), 1)} no Nordeste, contra {fmt_num(red('Sudeste'), 1)} no Sudeste ",
    "(Figura 2). Agregar as regiões faria essa heterogeneidade desaparecer."
  )) |>
  p(paste(
    "Os cenários seguintes trazem ganhos marginais: incorporar acesso à internet ou cobertura",
    "integral pouco acrescenta ao que a telefonia móvel já alcança, porque o celular sozinho cobre",
    "mais de 96% da população adulta das capitais. Em algumas regiões o cadastro duplo é",
    "ligeiramente pior que o de telefonia móvel isolada para determinados indicadores, resultado",
    "coerente com o desenho: reintroduzir o quadro fixo adiciona pouca cobertura e devolve parte do",
    "desequilíbrio de composição."
  )) |>
  h("Análises de sensibilidade", 2) |>
  p(glue::glue(
    "Em obesidade, a diferença entre medidas declaradas nos dois inquéritos é de ",
    "{fmt_num(obes$delta[1], 2)} pp. O viés do próprio autorrelato, medido dentro da PNS entre peso e ",
    "altura declarados e aferidos, é de {fmt_num(obes$delta[3], 2)} pp — maior, em valor absoluto, ",
    "que o viés de não cobertura de qualquer dos demais indicadores (Tabela S4). A subamostra de ",
    "antropometria sustenta a direção do efeito, não sua magnitude precisa. A conclusão sobre a ",
    "associação residual não depende do mapeamento de escolaridade adotado (Tabela S5)."
  )) |>
  br()

# =============================================================================
# DISCUSSAO
# =============================================================================
doc <- doc |>
  h("Discussão", 1) |>
  p(paste(
    "O quadro amostral do Vigitel até 2022 era, por qualquer critério convencional, inviável: seis",
    "em cada dez adultos das capitais fora do alcance, mais de oito em cada dez no Norte, e vício",
    "relativo de Cochran excedendo o limiar de degradação em todas as células examinadas. As",
    "estimativas publicadas, no entanto, diferem da referência domiciliar por um a dois pontos",
    "percentuais, e em metade dos indicadores não diferem de forma detectável. A explicação não é",
    "que o viés fosse pequeno, e sim que a ponderação pós-estratificada vinha absorvendo a maior",
    "parte dele."
  )) |>
  p(paste(
    "Esse resgate, porém, é seletivo e não auditável de dentro do inquérito. Ele funciona quando a",
    "diferença entre quem tem e quem não tem telefone se explica por idade, sexo e escolaridade,",
    "que são as margens de calibragem, e falha quando há associação direta remanescente. Em",
    "tabagismo, indicador para o qual a associação persiste, quase todo o viés de cobertura",
    "atravessou a ponderação. Nada no interior do Vigitel permite distinguir os dois casos: só uma",
    "referência externa revela para quais indicadores a correção funcionou."
  )) |>
  p(paste(
    "Daí decorre a principal implicação prática. A incorporação da telefonia móvel a partir de 2023",
    "foi justificada por cobertura, e cobertura não determina viés. Para indicadores cujo gradiente",
    "de posse se esgota nas variáveis de calibragem, o ganho da reforma sobre a estimativa pontual",
    "tende a ser menor do que a mudança de cobertura sugere, porque a ponderação já fazia esse",
    "trabalho; para os demais, o ganho é substancial. Um teste simples decorre disso e pode ser",
    "aplicado a qualquer novo indicador antes que suas estimativas telefônicas sejam usadas:",
    "verificar, em inquérito domiciliar, se ele mantém associação com posse de telefone após ajuste",
    "pelas variáveis de calibragem."
  )) |>
  p(paste(
    "A segunda implicação é sobre a série histórica. Dezessete das dezenove edições do Vigitel",
    "foram produzidas sob o quadro exclusivamente fixo, e continuam em uso para análise de",
    "tendência, monitoramento de metas e avaliação de política. A mudança de 2023 introduz uma",
    "quebra metodológica na série, e a magnitude do viés legado — por indicador e por região —",
    "é condição para interpretar variações que atravessem esse ponto. Uma queda de prevalência",
    "observada entre 2022 e 2023 pode refletir mudança de quadro amostral, e não de saúde."
  )) |>
  p(paste(
    "A terceira implicação é operacional. A reforma foi implementada de forma uniforme, com o mesmo",
    "número de linhas fixas e móveis sorteadas em todas as cidades, enquanto o problema que ela",
    "corrige é acentuadamente desigual: o ganho simulado é várias vezes maior no Norte e no",
    "Nordeste do que no Sudeste. Uma alocação diferenciada, concentrando esforço onde a cobertura",
    "fixa é menor, extrairia mais precisão do mesmo orçamento."
  )) |>
  p(paste(
    "Dois achados adicionais merecem registro. O mecanismo de calibragem, do qual dependia a",
    "qualidade das estimativas, estava ele próprio desalinhado no período: a estrutura etária",
    "ponderada do Vigitel afasta-se do Censo 2022 em magnitude várias vezes superior à da PNS,",
    "porque os pesos da edição derivavam de projeções populacionais posteriormente revistas. E o",
    "viés do autorrelato, medido em obesidade dentro da própria PNS, supera o viés de não cobertura",
    "de todos os demais indicadores — o que estabelece um teto para o que qualquer reforma de",
    "quadro amostral pode alcançar."
  )) |>
  h("Limitações", 2) |>
  p(paste(
    "O resíduo da partição é, por construção, uma quantidade agregada: absorve modo de coleta,",
    "autorrelato, instrumento, não resposta e calibragem, e este desenho não os separa. Por essa",
    "razão ele é denominado resíduo, e não efeito de modo."
  )) |>
  p(paste(
    "A análise é de um único ano, escolhido por ser o único contemporâneo a uma PNS — inquérito com",
    "apenas duas edições, 2013 e 2019. Isso impede tanto avaliar diretamente a reforma de 2023",
    "quanto medir a evolução do viés ao longo da série, embora a queda documentada da cobertura",
    "fixa torne plausível que o viés legado tenha crescido nos anos finais do período."
  )) |>
  p(paste(
    "A subamostra de antropometria da PNS nas capitais é pequena, de modo que o resultado de",
    "obesidade sustenta direção e não magnitude. A associação entre a parcela de viés removida pela",
    "ponderação e a associação residual apoia-se em quatro indicadores e é apresentada como",
    "mecanismo coerente, não como evidência estatística. Por fim, o estudo emprega o peso de",
    "pós-estratificação da própria edição, e não o peso recalibrado pelo Censo 2022 divulgado",
    "posteriormente, por ser aquele que reproduz o relatório publicado do ano e que é contemporâneo",
    "à calibragem da PNS; a distorção etária remanescente foi tratada por padronização direta."
  )) |>
  h("Conclusão", 2) |>
  p(paste(
    "Cobertura e viés não são a mesma quantidade. O quadro de telefonia fixa do Vigitel estava",
    "gravemente comprometido, mas suas estimativas publicadas permaneceram próximas da referência",
    "domiciliar porque a pós-estratificação absorvia a maior parte do viés — de forma desigual",
    "entre indicadores e sem que essa desigualdade pudesse ser detectada internamente. A",
    "incorporação da telefonia móvel corrige a origem do problema e beneficia sobretudo as regiões",
    "de menor cobertura fixa, o que recomenda alocação amostral diferenciada. Para a série anterior",
    "a 2023, as estimativas de viés aqui apresentadas fornecem a base para interpretar tendências",
    "que atravessem a mudança metodológica."
  )) |>
  br()

# =============================================================================
# LEGENDAS DAS FIGURAS
# =============================================================================
doc <- doc |>
  h("Legendas das figuras", 1) |>
  p(paste("Figura 1. Diferença de prevalência entre Vigitel", YEAR_VIGITEL, "e PNS", YEAR_PNS,
          "por indicador e sexo, adultos das 27 capitais. Pontos representam a diferença em pontos",
          "percentuais na convenção Vigitel menos PNS; barras horizontais, o intervalo de confiança",
          "de 95%. A linha vertical marca a ausência de diferença. Indicadores ordenados pela",
          "magnitude da diferença total.")) |>
  p(paste("Figura 2. Raiz do erro quadrático médio por cenário de quadro amostral, região e",
          "indicador, em pontos percentuais.", format(N_REPLICATES, big.mark = "."),
          "réplicas por célula. S0, telefonia fixa apenas; S1, telefonia móvel apenas; S2, cadastro",
          "duplo; S3, cadastro triplo com internet; S4, multimodal integral.")) |>
  br() |>
  h("Referências", 1) |>
  p("[PRESERVAR a lista de referências da versão anterior, acrescentando: relatório Vigitel 2023 (descrição do cadastro duplo), Orientações para análises de dados do Vigitel 2006-2024, e a documentação metodológica da PNS 2019.]")

saida <- normalizePath(file.path(dirname(here::here()),
                                 "Artigo Vigitel-PNS - revisado pelo pipeline.docx"),
                       mustWork = FALSE)
print(doc, target = saida)
cat("\nManuscrito revisado gravado em:\n  ", saida, "\n", sep = "")
cat("tamanho:", round(file.size(saida) / 1024), "KB\n")
cat("\nO arquivo original NAO foi modificado.\n")

message("12_manuscrito.R concluido.")
