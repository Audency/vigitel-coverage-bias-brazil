# =============================================================================
# 13_manuscrito.R
# O QUE FAZ : Gera o texto de titulo, resumo, objetivos, metodos, resultados,
#             discussao, limitacoes e conclusao com TODOS os numeros
#             interpolados dos objetos do pipeline, e chama o injetor que grava
#             no .docx preservando Introducao, referencias, afiliacoes, aspectos
#             eticos e formatacao.
# ENTRADAS  : data/derivado/*.rds, output/logs/*.csv
# SAIDAS    : output/manuscrito/secoes.csv, e (via injetor) o .docx atualizado
#             com realce amarelo e output/logs/mudancas.md
# =============================================================================

source(here::here("R", "00_setup.R"))
dir.create(here::here("output", "manuscrito"), showWarnings = FALSE, recursive = TRUE)

h1("13_manuscrito.R")

desc  <- readRDS(here::here("data", "derivado", "descritivas.rds"))
comp  <- readRDS(here::here("data", "derivado", "comparacoes.rds"))
vies  <- readRDS(here::here("data", "derivado", "vies_ncob.rds"))
part  <- readRDS(here::here("data", "derivado", "particao.rds"))
sim   <- readRDS(here::here("data", "derivado", "simulacao.rds"))
val23 <- readRDS(here::here("data", "derivado", "validacao_2023.rds"))
mec   <- le_log("mecanismo_posse_desfecho")
cob   <- le_log("cobertura_quadros_regiao")
val_pns <- le_log("validacao_pns_sidra"); val_vig <- le_log("validacao_vigitel")
fluxo <- le_log("fluxo_amostral_pns")
est_et <- desc$estrutura_etaria

n_vig <- desc$n$n[desc$n$inquerito == "Vigitel"]
n_pns <- desc$n$n[desc$n$inquerito == "PNS"]

# ---- atalhos de leitura -----------------------------------------------------
cp <- comp$comparacoes |> dplyr::filter(estrato == "Total")
g  <- function(i, c) cp[[c]][cp$indicador == i]
gi <- function(i) fmt_ic(g(i, "delta"), g(i, "delta_low"), g(i, "delta_upp"), 2)
v27 <- function(i, c) vies[[c]][vies$indicador == i & vies$dominio == "27 capitais"]
vr  <- function(i, r, c) vies[[c]][vies$indicador == i & vies$dominio == r]
pt  <- function(i, c) part$particao[[c]][part$particao$indicador == i]
or_ <- function(r) mec$or[mec$indicador == r]; orp <- function(r) mec$p[mec$indicador == r]
pd  <- function(i, c) comp$padronizadas[[c]][comp$padronizadas$indicador == i]
v23 <- function(i, c) val23$confronto[[c]][val23$confronto$indicador == i]

reqm <- sim |> dplyr::group_by(region, cenario) |>
  dplyr::summarise(r = mean(rmse), .groups = "drop") |>
  tidyr::pivot_wider(names_from = cenario, values_from = r) |>
  dplyr::mutate(red = S0 / S1)
red <- function(r) reqm$red[reqm$region == r]
perfil23 <- le_log("vigitel2023_perfil_por_quadro")
pf <- function(q, c) perfil23[[c]][perfil23$quadro == q]

S <- tibble::tibble(chave = character(), texto = character())
add <- function(k, t) S <<- dplyr::add_row(S, chave = k, texto = as.character(t))

# =============================================================================
add("titulo", paste(
  "Cobertura não é viés: quanto do viés de não cobertura da telefonia fixa a",
  "pós-estratificação do Vigitel já removia, e para quais indicadores"))

add("resumo_objetivo", glue::glue(
  "Objetivo: Quantificar o viés de não cobertura da telefonia fixa nas estimativas do Vigitel, ",
  "medir que parcela dele a pós-estratificação já removia em cada indicador, e validar a previsão ",
  "assim obtida contra a adoção real do cadastro duplo pelo sistema em {ANO_VIG_DUAL}."))

add("resumo_metodos", glue::glue(
  "Métodos: Análise de microdados públicos do Vigitel {ANO_VIGITEL} (n = {format(n_vig, big.mark='.')}) e ",
  "da Pesquisa Nacional de Saúde {ANO_PNS} restrita ao mesmo universo, as 26 capitais estaduais e o ",
  "Distrito Federal (n = {format(n_pns, big.mark='.')}). O recorte de capitais na PNS é exato, obtido pela ",
  "variável de tipo de área, e não uma aproximação. Quatro indicadores foram harmonizados a partir dos ",
  "dicionários oficiais. O viés de não cobertura foi estimado inteiramente dentro da PNS, que registra a ",
  "posse de telefone, e a diferença entre os inquéritos foi partida em componente de não cobertura e ",
  "resíduo, com intervalos por bootstrap de Rao-Wu sobre o desenho amostral ({format(N_BOOT, big.mark='.')} ",
  "réplicas). Cinco cenários de quadro amostral foram comparados sob o arcabouço ADEMP, com a posse de ",
  "telefone observada e não modelada. A previsão foi confrontada com a edição de {ANO_VIG_DUAL}, ",
  "reconstruindo o desenho legado dentro dela."))

add("resumo_resultados", glue::glue(
  "Resultados: As estimativas de ambos os inquéritos reproduzem os valores oficiais publicados, com ",
  "diferença máxima de {fmt_num(max(abs(val_pns$diferenca)),2)} pp na PNS e ",
  "{fmt_num(max(abs(val_vig$diferenca)),2)} pp no Vigitel. Nas capitais, ",
  "{fmt_num(v27('smoking','est_f_semfixo'),1)}% dos adultos viviam em domicílio sem telefone fixo ",
  "({fmt_num(vr('smoking','Norte','est_f_semfixo'),1)}% no Norte). O viés de não cobertura foi de ",
  "{fmt_num(v27('hypertension','est_vies'),2)} pp para hipertensão e ",
  "{fmt_num(v27('smoking','est_vies'),2)} pp para tabagismo, com vício relativo de Cochran acima do ",
  "limiar de {fmt_num(LIMIAR_COCHRAN,2)} nas {nrow(vies)} combinações de indicador e região. As ",
  "diferenças observadas, porém, foram pequenas e de sentidos opostos: {gi('smoking')} pp para ",
  "tabagismo e {gi('hypertension')} pp para hipertensão, sem diferença detectável para diabetes e ",
  "autoavaliação de saúde. A pós-estratificação removia {fmt_num(pt('diabetes','pct_removido'),0)}% do ",
  "viés de cobertura em diabetes e {fmt_num(pt('hypertension','pct_removido'),0)}% em hipertensão, mas ",
  "apenas {fmt_num(pt('smoking','pct_removido'),0)}% em tabagismo — ordem que acompanha a associação ",
  "residual entre desfecho e posse de telefone após ajuste pelas variáveis de calibragem. Na transição ",
  "real de {ANO_VIG_DUAL}, a diferença entre o desenho legado reconstruído e o cadastro duplo foi de ",
  "{fmt_num(v23('smoking','obs_2023'),2)} pp para tabagismo, único indicador com diferença ",
  "distinguível de zero, reproduzindo o padrão previsto."))

add("resumo_conclusao", paste(
  "Conclusão: Cobertura e viés não são a mesma quantidade. O quadro de telefonia fixa do Vigitel estava",
  "gravemente comprometido, mas suas estimativas publicadas permaneceram próximas da referência",
  "domiciliar porque a pós-estratificação absorvia a maior parte do viés — de forma desigual entre",
  "indicadores e sem que essa desigualdade pudesse ser detectada de dentro do inquérito. A adoção do",
  "cadastro duplo corrige a origem do problema, e a magnitude do ganho é indicador-específica. Para as",
  "dezessete edições anteriores, ainda em uso para série histórica, as estimativas de viés aqui",
  "apresentadas fornecem a base para interpretar tendências que atravessem a mudança metodológica."))

add("palavras_chave", paste(
  "Palavras-chave: vigilância em saúde pública; doenças crônicas não transmissíveis; inquéritos",
  "epidemiológicos; viés de seleção; cobertura amostral; pós-estratificação; Vigitel; Pesquisa",
  "Nacional de Saúde."))

# ---- Metodos ---------------------------------------------------------------
add("met_recorte", glue::glue(
  "A análise restringiu a PNS ao universo do Vigitel, formado pelas 26 capitais estaduais e pelo ",
  "Distrito Federal. Embora a base pública da PNS não divulgue o código municipal, ela divulga a ",
  "variável de tipo de área, cuja categoria Capital identifica exatamente esse conjunto de municípios; ",
  "o recorte é portanto exato no nível de agregação liberado pelo IBGE, e não uma aproximação. Dos ",
  "{format(fluxo$n[1], big.mark='.')} moradores selecionados em todo o país, ",
  "{format(n_pns, big.mark='.')} compunham a amostra analítica. A restrição não é detalhe de execução: ",
  "as prevalências fora das capitais diferem das capitais em magnitude comparável à do próprio efeito ",
  "investigado, de modo que comparar o Vigitel à PNS nacional introduziria um artefato do tamanho do ",
  "achado e, para a autoavaliação de saúde, inverteria o sinal da diferença."))

add("met_validacao", glue::glue(
  "Antes de qualquer comparação, as estimativas de cada inquérito foram confrontadas com os valores ",
  "publicados pelo órgão responsável. As {nrow(val_pns)} comparações da PNS reproduzem as prevalências ",
  "divulgadas pelo IBGE no SIDRA com diferença máxima de {fmt_num(max(abs(val_pns$diferenca)),2)} pp, e ",
  "as {nrow(val_vig)} do Vigitel reproduzem o relatório oficial da edição com diferença máxima de ",
  "{fmt_num(max(abs(val_vig$diferenca)),2)} pp. Nenhuma divergência atinge 0,5 pp."))

add("met_bootstrap", glue::glue(
  "O viés de não cobertura é um produto de estimativas correlacionadas dentro do mesmo desenho ",
  "complexo, de modo que a incerteza veio de bootstrap de Rao-Wu sobre o desenho amostral, com ",
  "{format(N_BOOT, big.mark='.')} réplicas e semente fixa, e não de linearização. O recorte de capitais ",
  "é composto de estratos completos — nenhum estrato da PNS mistura capital e não-capital —, de modo ",
  "que construir as réplicas sobre o subconjunto é exato. A variância do resíduo foi obtida sem supor ",
  "independência indevida: a quantidade da PNS que entra em sua definição é calculada dentro do mesmo ",
  "bootstrap, incorporando a correlação entre as duas parcelas."))

add("met_validacao_2023", glue::glue(
  "A previsão foi confrontada com a transição real do sistema. A base pública do Vigitel ",
  "{ANO_VIG_DUAL} divulga, para cada entrevista, o quadro amostral de origem e dois pesos de ",
  "pós-estratificação adicionais, um para o quadro de telefonia fixa isolado e outro para o de ",
  "telefonia móvel isolado, além do peso do cadastro duplo efetivamente publicado. Como os três pesos ",
  "expandem para a mesma população-alvo, reconstruiu-se dentro de {ANO_VIG_DUAL} a estimativa que o ",
  "desenho legado teria produzido, comparando-a à do desenho vigente com o tempo mantido constante — o ",
  "que a comparação entre edições de anos distintos não permitiria, por confundir mudança de desenho ",
  "com mudança epidemiológica."))

# ---- Resultados -------------------------------------------------------------
add("res_amostras", glue::glue(
  "As duas amostras são próximas em sexo, escolaridade, raça/cor e região, e divergem apenas na ",
  "estrutura etária (Tabela 1). Essa concordância não indica equivalência entre as populações cobertas: ",
  "o peso do Vigitel é calibrado justamente por faixa etária, escolaridade e sexo. As variáveis que ",
  "carregam informação independente são raça/cor e região, e nelas os inquéritos concordam. Confrontadas ",
  "com a estrutura da população adulta das capitais no Censo {ANO_CENSO}, o desvio absoluto máximo é de ",
  "{fmt_num(max(abs(est_et$dif_vigitel)),1)} pp no Vigitel contra {fmt_num(max(abs(est_et$dif_pns)),1)} pp ",
  "na PNS, com sobrerrepresentação da faixa de 25 a 34 anos e sub-representação dos 65 anos ou mais."))

add("res_prevalencias", glue::glue(
  "As diferenças observadas são pequenas e não têm o mesmo sentido entre indicadores (Tabela 2). O ",
  "Vigitel estima prevalência menor de tabagismo ({gi('smoking')} pp) e maior de hipertensão ",
  "({gi('hypertension')} pp); para diabetes ({gi('diabetes')}) e autoavaliação ruim de saúde ",
  "({gi('poor_health')}) a diferença não se distingue de zero. Apenas dois dos quatro indicadores ",
  "diferem de zero após correção de Holm, e o teste de interação sexo × inquérito não é significativo ",
  "em nenhum deles (menor p = {fmt_p(min(comp$interacao$p_int))}). Padronizando ambos os inquéritos ",
  "para a estrutura etária do Censo {ANO_CENSO}, a diferença em tabagismo praticamente não se altera, ",
  "enquanto a de hipertensão passa de {fmt_num(pd('hypertension','delta_bruto'),2)} para ",
  "{fmt_num(pd('hypertension','delta_padr'),2)} pp e a de diabetes, de indistinguível de zero para ",
  "{fmt_num(pd('diabetes','delta_padr'),2)} pp."))

add("res_ncob", glue::glue(
  "Nas capitais, {fmt_num(v27('smoking','est_f_semfixo'),1)}% da população adulta vivia em domicílio ",
  "sem telefone fixo, de {fmt_num(vr('smoking','Sul','est_f_semfixo'),1)}% no Sul a ",
  "{fmt_num(vr('smoking','Norte','est_f_semfixo'),1)}% no Norte. O viés segue o gradiente de cada ",
  "indicador: negativo em tabagismo ({fmt_num(v27('smoking','est_vies'),2)} pp) e positivo em ",
  "hipertensão ({fmt_num(v27('hypertension','est_vies'),2)} pp) e diabetes ",
  "({fmt_num(v27('diabetes','est_vies'),2)} pp), alcançando ",
  "{fmt_num(vr('hypertension','Norte','est_vies'),2)} pp para hipertensão no Norte (Tabela 3). O vício ",
  "relativo de Cochran ultrapassa o limiar de {fmt_num(LIMIAR_COCHRAN,2)} nas {nrow(vies)} células, ",
  "entre {fmt_num(min(vies$cochran_vigitel),2)} e {fmt_num(max(vies$cochran_vigitel),2)}."))

add("res_particao", glue::glue(
  "O contraste entre as duas seções anteriores é o achado central: o viés de cobertura é grande e a ",
  "diferença observada é pequena. Em nenhum indicador o componente de não cobertura é uma fração do ",
  "gap: em tabagismo e hipertensão ele o excede, e em diabetes e autoavaliação de saúde tem sinal ",
  "oposto (Tabela 4). Comparando a estimativa que um quadro de telefonia fixa produziria sem ponderação ",
  "com a efetivamente publicada, a pós-estratificação removia {fmt_num(pt('diabetes','pct_removido'),0)}% ",
  "do viés em diabetes, {fmt_num(pt('hypertension','pct_removido'),0)}% em hipertensão, ",
  "{fmt_num(pt('poor_health','pct_removido'),0)}% em autoavaliação de saúde e ",
  "{fmt_num(pt('smoking','pct_removido'),0)}% em tabagismo. A ordem não é acidental: ajustando a posse ",
  "de telefone pelas variáveis de calibragem, a associação residual com o desfecho desaparece para ",
  "hipertensão (razão de chances {fmt_num(or_('Hipertensão diagnosticada'),2)}; ",
  "p = {fmt_p(orp('Hipertensão diagnosticada'))}) e diabetes ({fmt_num(or_('Diabetes diagnosticado'),2)}; ",
  "p = {fmt_p(orp('Diabetes diagnosticado'))}) e persiste para tabagismo ",
  "({fmt_num(or_('Tabagismo atual'),2)}; p = {fmt_p(orp('Tabagismo atual'))}) e autoavaliação de saúde ",
  "({fmt_num(or_('Autoavaliação ruim de saúde'),2)}; p = {fmt_p(orp('Autoavaliação ruim de saúde'))})."))

add("res_simulacao", glue::glue(
  "A cobertura do quadro de telefonia fixa varia de {fmt_num(min(cob$fixo),1)}% a ",
  "{fmt_num(max(cob$fixo),1)}% entre regiões, contra {fmt_num(min(cob$celular),1)}% ou mais do quadro ",
  "móvel. Abandonar o quadro exclusivamente fixo reduz a raiz do erro quadrático médio por fator de ",
  "{fmt_num(red('Norte'),1)} no Norte e {fmt_num(red('Nordeste'),1)} no Nordeste, contra ",
  "{fmt_num(red('Sudeste'),1)} no Sudeste (Tabela 5, Figura 2). Os cenários seguintes trazem ganhos ",
  "marginais, porque a telefonia móvel sozinha já cobre mais de 96% da população adulta das capitais."))

add("res_validacao23", glue::glue(
  "A edição de {ANO_VIG_DUAL} reuniu {format(sum(perfil23$n), big.mark='.')} entrevistas, ",
  "{format(pf('Fixo','n'), big.mark='.')} do quadro de telefonia fixa e ",
  "{format(pf('Celular','n'), big.mark='.')} do de telefonia móvel. O contraste documenta, dentro do ",
  "próprio inquérito, quem o desenho legado alcançava: idade média de {fmt_num(pf('Fixo','idade_media'),1)} ",
  "anos no quadro fixo contra {fmt_num(pf('Celular','idade_media'),1)} no móvel, e ",
  "{fmt_num(pf('Fixo','pct_65mais'),1)}% de pessoas com 65 anos ou mais contra ",
  "{fmt_num(pf('Celular','pct_65mais'),1)}%. Reconstruído o desenho legado, a estimativa que ele teria ",
  "produzido difere da publicada em {fmt_ic(v23('smoking','obs_2023'), v23('smoking','fd_low'), v23('smoking','fd_upp'), 2)} pp ",
  "para tabagismo, único indicador cuja diferença exclui o zero; as demais são compatíveis com zero ",
  "(Tabela 6, Figura 3). A correlação entre o resíduo previsto a partir de {ANO_VIGITEL} e a diferença ",
  "observada na transição real foi de ",
  "{fmt_num(stats::cor(val23$confronto$vies_residual_2019, val23$confronto$obs_2023),2)}, com ",
  "concordância de sinal em três dos quatro indicadores; com quatro indicadores, a correlação é ",
  "descritiva e não constitui teste. A estratificação regional, contudo, não se confirmou: a diferença ",
  "observada foi maior no Sul e no Centro-Oeste do que no Norte e no Nordeste, invertendo a ordenação ",
  "prevista pela simulação (Tabela S8)."))

# ---- Discussao --------------------------------------------------------------
add("disc_1", paste(
  "O quadro amostral do Vigitel até 2022 era, por qualquer critério convencional, inviável: seis em",
  "cada dez adultos das capitais fora do alcance, mais de oito em cada dez no Norte, e vício relativo",
  "de Cochran excedendo o limiar de degradação em todas as células examinadas. As estimativas",
  "publicadas, no entanto, diferem da referência domiciliar por um a dois pontos percentuais, e em",
  "metade dos indicadores não diferem de forma detectável. A explicação não é que o viés fosse pequeno,",
  "e sim que a ponderação pós-estratificada vinha absorvendo a maior parte dele."))

add("disc_2", paste(
  "Esse resgate é seletivo e não auditável de dentro do inquérito. Funciona quando a diferença entre",
  "quem tem e quem não tem telefone se explica por idade, sexo e escolaridade, que são as margens de",
  "calibragem, e falha quando há associação direta remanescente. Em tabagismo, indicador para o qual a",
  "associação persiste, quase todo o viés de cobertura atravessou a ponderação. Nada no interior do",
  "Vigitel permite distinguir os dois casos: apenas uma referência externa revela para quais indicadores",
  "a correção funcionou. Daí decorre um teste simples, aplicável a qualquer novo indicador antes que",
  "suas estimativas telefônicas sejam usadas: verificar, em inquérito domiciliar, se ele mantém",
  "associação com posse de telefone após ajuste pelas variáveis de calibragem."))

add("disc_3", glue::glue(
  "A adoção do cadastro duplo em {ANO_VIG_DUAL} foi justificada por cobertura, e cobertura não determina ",
  "viés. Para indicadores cujo gradiente de posse se esgota nas variáveis de calibragem, o ganho da ",
  "reforma sobre a estimativa pontual tende a ser menor do que a mudança de cobertura sugere; para os ",
  "demais, é substancial. A transição real confirma essa leitura: entre os quatro indicadores, apenas o ",
  "tabagismo apresentou diferença detectável entre o desenho legado reconstruído e o cadastro duplo."))

add("disc_4", paste(
  "A segunda implicação é sobre a série histórica. Dezessete das dezenove edições do Vigitel foram",
  "produzidas sob o quadro exclusivamente fixo e continuam em uso para análise de tendência,",
  "monitoramento de metas e avaliação de política. A mudança introduz uma quebra metodológica na série,",
  "e a magnitude do viés legado, por indicador, é condição para interpretar variações que atravessem",
  "esse ponto: uma queda observada entre 2022 e 2023 pode refletir mudança de quadro amostral, e não de",
  "saúde."))

add("disc_5", glue::glue(
  "Um achado adicional merece registro: o mecanismo de calibragem, do qual dependia a qualidade das ",
  "estimativas, estava ele próprio desalinhado no período. A estrutura etária ponderada do Vigitel ",
  "afasta-se do Censo {ANO_CENSO} em {fmt_num(max(abs(est_et$dif_vigitel)),1)} pp, contra ",
  "{fmt_num(max(abs(est_et$dif_pns)),1)} pp da PNS, porque os pesos da edição derivavam de projeções ",
  "populacionais posteriormente revistas pelo próprio Censo. O ajuste que sustentava as estimativas ",
  "operava, portanto, sobre margens desatualizadas, o que reforça a fragilidade de depender dele."))

add("limitacoes", glue::glue(
  "O resíduo da partição é, por construção, uma quantidade agregada: absorve modo de coleta, ",
  "autorrelato, instrumento, não resposta e calibragem, e este desenho não os separa. Por essa razão é ",
  "denominado resíduo, e não efeito de modo. A comparação principal usa um único ano, escolhido por ser ",
  "o único contemporâneo a uma PNS, inquérito com apenas duas edições, 2013 e {ANO_PNS}; isso impede ",
  "medir a evolução do viés ao longo da série, embora a queda documentada da cobertura fixa torne ",
  "plausível que ele tenha crescido nos anos finais do período. A associação entre a parcela de viés ",
  "removida pela ponderação e a associação residual apoia-se em quatro indicadores, e a comparação ",
  "entre previsto e observado, em quatro indicadores e cinco regiões: ambas são descritivas e não ",
  "constituem teste de hipótese. A previsão regional não se confirmou na transição real, resultado ",
  "negativo que reportamos como tal e cuja explicação mais provável é que a população que ainda mantinha ",
  "telefonia fixa em {ANO_VIG_DUAL} difere daquela de {ANO_PNS}, sobre a qual a simulação foi calibrada. ",
  "Por fim, o estudo emprega o peso de pós-estratificação da própria edição, e não o recalibrado pelo ",
  "Censo {ANO_CENSO} divulgado posteriormente, por ser aquele que reproduz o relatório publicado do ano; ",
  "a distorção etária remanescente foi tratada por padronização direta."))

add("conclusao", paste(
  "Cobertura e viés não são a mesma quantidade. O quadro de telefonia fixa do Vigitel estava gravemente",
  "comprometido, mas suas estimativas publicadas permaneceram próximas da referência domiciliar porque a",
  "pós-estratificação absorvia a maior parte do viés — de forma desigual entre indicadores e sem que essa",
  "desigualdade pudesse ser detectada internamente. A incorporação da telefonia móvel corrige a origem do",
  "problema, com ganho indicador-específico. Para a série anterior à mudança, as estimativas de viés aqui",
  "apresentadas fornecem a base para interpretar tendências que atravessem a transição metodológica."))

readr::write_csv(S, here::here("output", "manuscrito", "secoes.csv"))
cat("secoes geradas:", nrow(S), "\n")
print(as.data.frame(S |> dplyr::mutate(texto = stringr::str_trunc(texto, 70))))

# ---- Injecao no .docx -------------------------------------------------------
h2("Injecao no manuscrito")
res <- system2("python3", c(shQuote(here::here("injeta_manuscrito.py"))),
               stdout = TRUE, stderr = TRUE)
cat(paste(res, collapse = "\n"), "\n")
if (any(grepl("BLOQUEADO|ERRO", res))) {
  stop("Injecao nao concluida - ver mensagem acima.", call. = FALSE)
}

message("13_manuscrito.R concluido.")
