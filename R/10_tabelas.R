# =============================================================================
# 10_tabelas.R
# O QUE FAZ : Monta as seis tabelas do manuscrito a partir dos objetos ja
#             calculados. Nao recalcula nada. Padrao epidemiologico: cabecalho
#             com n e unidade, virgula decimal, IC95% entre parenteses, notas de
#             rodape com fonte, pesos e definicao de cada indicador.
# ENTRADAS  : data/derivado/*.rds
# SAIDAS    : output/tables/tabela{1..6}.{rds,docx,html} + *_dados.rds
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("10_tabelas.R")

desc  <- readRDS(here::here("data", "derivado", "descritivas.rds"))
comp  <- readRDS(here::here("data", "derivado", "comparacoes.rds"))
prev  <- readRDS(here::here("data", "derivado", "prevalencias.rds"))
vies  <- readRDS(here::here("data", "derivado", "vies_ncob.rds"))
part  <- readRDS(here::here("data", "derivado", "particao.rds"))
sim   <- readRDS(here::here("data", "derivado", "simulacao.rds"))
val23 <- readRDS(here::here("data", "derivado", "validacao_2023.rds"))

FONTE_BASE <- glue::glue(
  "Fontes: Vigitel {ANO_VIGITEL} (Ministério da Saúde) e Pesquisa Nacional de Saúde {ANO_PNS} ",
  "(IBGE), restrita às 26 capitais estaduais e ao Distrito Federal. Estimativas ponderadas pelo ",
  "desenho amostral de cada inquérito."
)

# =============================================================================
# TABELA 1 - caracteristicas das amostras
# =============================================================================
h2("Tabela 1")

n_vig <- desc$n$n[desc$n$inquerito == "Vigitel"]
n_pns <- desc$n$n[desc$n$inquerito == "PNS"]

rot_var <- c(age = "Idade, anos", sex = "Sexo", age_grp = "Faixa etária, anos",
             education = "Escolaridade, anos de estudo", race = "Raça/cor", region = "Região")

corpo_cat <- desc$categoricas |>
  dplyr::mutate(celula = fmt_ic(est, low, upp, 1)) |>
  dplyr::select(variavel, nivel, inquerito, celula, smd) |>
  tidyr::pivot_wider(names_from = inquerito, values_from = celula)

corpo_idade <- desc$idade |>
  dplyr::mutate(celula = paste0(fmt_num(est, 1), " (", fmt_num(dp, 1), ")")) |>
  dplyr::select(variavel, nivel, inquerito, celula, smd) |>
  tidyr::pivot_wider(names_from = inquerito, values_from = celula)

t1_dados <- dplyr::bind_rows(corpo_idade, corpo_cat) |>
  # A base grava "Indigena" sem acento; o rotulo da tabela leva acento.
  dplyr::mutate(nivel = dplyr::recode(nivel, "Indigena" = "Indígena"),
                grupo = factor(rot_var[variavel], levels = unname(rot_var)),
                nivel = factor(nivel, levels = c("Média (DP)", "Masculino", "Feminino",
                                                 FAIXAS_IDADE, FAIXAS_ESC, "Branca", "Preta",
                                                 "Parda", "Amarela", "Indígena", REGIOES))) |>
  dplyr::arrange(grupo, nivel) |>
  dplyr::select(grupo, nivel, Vigitel, PNS, smd)

tab1 <- t1_dados |>
  gt::gt(groupname_col = "grupo", rowname_col = "nivel") |>
  gt::cols_label(
    Vigitel = gt::md(glue::glue("**Vigitel {ANO_VIGITEL}**<br>27 capitais<br>n = {format(n_vig, big.mark='.')}<br>% (IC 95%)")),
    PNS = gt::md(glue::glue("**PNS {ANO_PNS}**<br>27 capitais<br>n = {format(n_pns, big.mark='.')}<br>% (IC 95%)")),
    smd = gt::md("**DPE**")) |>
  gt::fmt_number(columns = smd, decimals = 2, dec_mark = ",") |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = c(Vigitel, PNS, smd)) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 1.** Características das amostras de adultos (≥18 anos) das 27 capitais, ",
    "Vigitel {ANO_VIGITEL} e PNS {ANO_PNS}"))) |>
  gt::tab_footnote("A linha de idade traz média e desvio-padrão ponderados; as demais, percentual com intervalo de confiança de 95%.",
                   locations = gt::cells_column_labels(columns = Vigitel)) |>
  gt::tab_footnote("DPE = diferença padronizada entre os dois inquéritos, calculada linha a linha para proporções e sobre média e desvio-padrão para a idade. Valores com módulo igual ou superior a 0,10 indicam desequilíbrio relevante.",
                   locations = gt::cells_column_labels(columns = smd)) |>
  gt::tab_footnote(paste(
    "O peso de pós-estratificação do Vigitel é calibrado por faixa etária, escolaridade e sexo",
    "(Ministério da Saúde, Orientações para análises de dados do Vigitel, item 4.5). Diferença",
    "padronizada próxima de zero nessas três variáveis decorre da calibragem e não indica que as",
    "populações cobertas sejam equivalentes. Raça/cor e região não entram na calibragem."),
    locations = gt::cells_row_groups(groups = "Escolaridade, anos de estudo")) |>
  gt::tab_source_note(gt::md(paste(FONTE_BASE, "Casos sem resposta foram excluídos do denominador da respectiva variável."))) |>
  estilo_gt() |>
  gt::tab_style(gt::cell_text(weight = "bold"),
                gt::cells_body(columns = smd, rows = abs(smd) >= 0.10))
salva_tabela(tab1, "tabela1", t1_dados)

# =============================================================================
# TABELA 2 - prevalencias brutas e padronizadas
# =============================================================================
h2("Tabela 2")

t2_dados <- comp$comparacoes |>
  dplyr::left_join(prev |> dplyr::select(indicador, estrato, inquerito, low, upp) |>
                     tidyr::pivot_wider(names_from = inquerito, values_from = c(low, upp)),
                   by = c("indicador", "estrato")) |>
  dplyr::left_join(comp$padronizadas |> dplyr::select(indicador, vig_padr, pns_padr,
                                                      delta_padr, delta_padr_low, delta_padr_upp),
                   by = "indicador") |>
  dplyr::mutate(
    estrato = factor(estrato, levels = c("Total", "Masculino", "Feminino")),
    rotulo = factor(rotulo, levels = INDICADORES$rotulo),
    c_vig = fmt_ic(est_Vigitel, low_Vigitel, upp_Vigitel, 1),
    c_pns = fmt_ic(est_PNS, low_PNS, upp_PNS, 1),
    c_dif = fmt_ic(delta, delta_low, delta_upp, 2),
    c_rp  = fmt_ic(rp, rp_low, rp_upp, 2),
    c_padr = dplyr::if_else(estrato == "Total",
                            fmt_ic(delta_padr, delta_padr_low, delta_padr_upp, 2), NA_character_),
    c_p = dplyr::if_else(estrato == "Total", fmt_p(p_holm), NA_character_)) |>
  dplyr::arrange(rotulo, estrato)

tab2 <- t2_dados |>
  dplyr::select(rotulo, estrato, c_vig, c_pns, c_dif, c_padr, c_rp, c_p) |>
  gt::gt(groupname_col = "rotulo", rowname_col = "estrato") |>
  gt::cols_label(
    c_vig = gt::md(glue::glue("**Vigitel {ANO_VIGITEL}**<br>% (IC 95%)")),
    c_pns = gt::md(glue::glue("**PNS {ANO_PNS}**<br>% (IC 95%)")),
    c_dif = gt::md("**Δ bruta**<br>pp (IC 95%)"),
    c_padr = gt::md("**Δ padronizada**<br>pp (IC 95%)"),
    c_rp = gt::md("**RP**<br>(IC 95%)"),
    c_p = gt::md("**p**<br>(Holm)")) |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = dplyr::starts_with("c_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 2.** Prevalência dos indicadores em adultos (≥18 anos) das 27 capitais, ",
    "com diferença absoluta bruta e padronizada por idade e razão de prevalências"))) |>
  gt::tab_footnote(gt::md(glue::glue("Δ = Vigitel − PNS, em pontos percentuais (pp). Valor negativo indica prevalência menor no Vigitel.")),
                   locations = gt::cells_column_labels(columns = c_dif)) |>
  gt::tab_footnote(glue::glue("Padronização direta para a estrutura etária da população adulta das 27 capitais no Censo {ANO_CENSO}, aplicada aos dois inquéritos."),
                   locations = gt::cells_column_labels(columns = c_padr)) |>
  gt::tab_footnote("RP = razão de prevalências (Vigitel dividido por PNS); intervalo pelo método delta na escala logarítmica.",
                   locations = gt::cells_column_labels(columns = c_rp)) |>
  gt::tab_footnote("Teste bilateral da hipótese de diferença nula, tratando os inquéritos como amostras independentes, com correção de Holm sobre os quatro testes principais (linhas Total). As linhas por sexo não entram na correção; a assimetria entre sexos é avaliada pelo teste de interação, em nota de cada indicador.",
                   locations = gt::cells_column_labels(columns = c_p))
for (i in seq_len(nrow(comp$interacao))) {
  tab2 <- tab2 |> gt::tab_footnote(
    glue::glue("Interação sexo × inquérito: p = {fmt_p(comp$interacao$p_int[i])} ",
               "(p de Holm = {fmt_p(comp$interacao$p_int_holm[i])}), de modelo logístico sobre os ",
               "dados empilhados com o desenho de cada inquérito preservado."),
    locations = gt::cells_row_groups(groups = comp$interacao$rotulo[i]))
}
tab2 <- tab2 |>
  gt::tab_source_note(gt::md(paste(FONTE_BASE, "Definições dos indicadores e equivalência das perguntas na Tabela S1."))) |>
  estilo_gt()
salva_tabela(tab2, "tabela2", t2_dados)

# =============================================================================
# TABELA 3 - vies de nao cobertura
# =============================================================================
h2("Tabela 3")

t3_dados <- vies |>
  dplyr::mutate(c_fixo = fmt_ic(est_p_fixo, p_fixo_low, p_fixo_upp, 1),
                c_sem = fmt_ic(est_p_semfixo, p_sem_low, p_sem_upp, 1),
                c_f = fmt_ic(est_f_semfixo, f_low, f_upp, 1),
                c_vies = fmt_ic(est_vies, vies_low, vies_upp, 2),
                c_coch = paste0(fmt_num(cochran_vigitel, 2),
                                dplyr::if_else(acima_limiar, " *", "")))

tab3 <- t3_dados |>
  dplyr::select(rotulo, dominio, c_fixo, c_sem, c_f, c_vies, c_coch) |>
  gt::gt(groupname_col = "rotulo", rowname_col = "dominio") |>
  gt::cols_label(c_fixo = gt::md("**Com telefone fixo**<br>% (IC 95%)"),
                 c_sem = gt::md("**Sem telefone fixo**<br>% (IC 95%)"),
                 c_f = gt::md("**População sem fixo**<br>% (IC 95%)"),
                 c_vies = gt::md("**Viés de não cobertura**<br>pp (IC 95%)"),
                 c_coch = gt::md("**Vício relativo**<br>de Cochran")) |>
  gt::cols_align("center", columns = dplyr::starts_with("c_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 3.** Viés de não cobertura da telefonia fixa, estimado dentro da PNS {ANO_PNS}, ",
    "por indicador e região — adultos (≥18 anos) das 27 capitais"))) |>
  gt::tab_footnote("Viés de não cobertura = proporção da população sem telefone fixo multiplicada pela diferença de prevalência entre quem tem e quem não tem telefone fixo. Valor positivo indica que um inquérito restrito ao quadro de telefonia fixa superestimaria a prevalência populacional; negativo, que subestimaria.",
                   locations = gt::cells_column_labels(columns = c_vies)) |>
  gt::tab_footnote(glue::glue(
    "Vício relativo de Cochran = módulo do viés dividido pelo erro-padrão da estimativa. Acima de ",
    "{fmt_num(LIMIAR_COCHRAN, 2)} o nível nominal de 95% dos intervalos se degrada; células acima do ",
    "limiar marcadas com asterisco ({sum(vies$acima_limiar)} de {nrow(vies)}). A razão apresentada usa ",
    "o erro-padrão que o Vigitel efetivamente tem para o indicador, leitura operacional do critério; ",
    "a razão com o erro-padrão interno da PNS consta da Tabela S4."),
    locations = gt::cells_column_labels(columns = c_coch)) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Fonte: PNS {ANO_PNS} (IBGE), adultos das 26 capitais e do Distrito Federal. Nenhuma quantidade ",
    "desta tabela depende do Vigitel: a estimação é inteiramente interna à PNS. Intervalos por ",
    "bootstrap de Rao-Wu sobre o desenho amostral, com {format(N_BOOT, big.mark='.')} réplicas e ",
    "semente fixa."))) |>
  estilo_gt()
salva_tabela(tab3, "tabela3", t3_dados)

# =============================================================================
# TABELA 4 - particao
# =============================================================================
h2("Tabela 4")

t4_dados <- part$particao |>
  dplyr::mutate(rotulo = factor(rotulo, levels = INDICADORES$rotulo)) |>
  dplyr::arrange(rotulo) |>
  dplyr::mutate(c_gap = fmt_num(gap_total, 2),
                c_comp = fmt_ic(componente, comp_low, comp_upp, 2),
                c_res = fmt_ic(residuo, residuo_low, residuo_upp, 2),
                c_pct = pct_txt,
                c_rem = paste0(fmt_num(pct_removido, 0), "%"))

tab4 <- t4_dados |>
  dplyr::select(rotulo, c_gap, c_comp, c_res, c_pct, c_rem) |>
  gt::gt(rowname_col = "rotulo") |>
  gt::cols_label(c_gap = gt::md("**Gap total**<br>pp"),
                 c_comp = gt::md("**Componente de<br>não cobertura**<br>pp (IC 95%)"),
                 c_res = gt::md("**Resíduo**<br>pp (IC 95%)"),
                 c_pct = gt::md("**Parcela do gap<br>atribuível à<br>não cobertura**"),
                 c_rem = gt::md("**Viés de cobertura<br>removido pela<br>pós-estratificação**")) |>
  gt::cols_align("center", columns = dplyr::starts_with("c_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 4.** Partição da diferença entre Vigitel {ANO_VIGITEL} e PNS {ANO_PNS} em componente ",
    "de não cobertura e resíduo — adultos (≥18 anos) das 27 capitais"))) |>
  gt::tab_footnote("Gap total = prevalência no Vigitel menos prevalência na PNS, em pontos percentuais. Reproduz a coluna de diferença bruta da Tabela 2.",
                   locations = gt::cells_column_labels(columns = c_gap)) |>
  gt::tab_footnote("Componente estimado inteiramente dentro da PNS (Tabela 3): é o viés que um inquérito restrito ao quadro de telefonia fixa sofreria por excluir a população sem telefone fixo.",
                   locations = gt::cells_column_labels(columns = c_comp)) |>
  gt::tab_footnote("Resíduo = gap total menos componente de não cobertura, isto é, a parcela da diferença não atribuível à não cobertura telefônica. Absorve conjuntamente efeito de modo de coleta, autorrelato, diferenças de instrumento, não resposta e calibragem de pesos; este desenho não permite separá-los. Intervalo pela soma das variâncias do Vigitel e da quantidade correspondente na PNS, esta obtida no mesmo bootstrap que gerou o componente, de modo a respeitar a correlação entre as parcelas.",
                   locations = gt::cells_column_labels(columns = c_res)) |>
  gt::tab_footnote("Quando o componente tem sinal oposto ao gap observado, ou o excede em magnitude, a razão entre os dois deixa de ser interpretável como fração e a situação é descrita em palavras.",
                   locations = gt::cells_column_labels(columns = c_pct)) |>
  gt::tab_footnote("Parcela do viés de cobertura que a pós-estratificação do Vigitel remove, comparando a estimativa que um quadro de telefonia fixa produziria sem ponderação com a efetivamente publicada, tendo a PNS como referência.",
                   locations = gt::cells_column_labels(columns = c_rem)) |>
  gt::tab_source_note(gt::md(paste(FONTE_BASE, "Convenção de sinal: Δ = Vigitel − PNS."))) |>
  estilo_gt()
salva_tabela(tab4, "tabela4", t4_dados)

# =============================================================================
# TABELA 5 - desempenho da simulacao
# =============================================================================
h2("Tabela 5")

t5_dados <- sim |>
  dplyr::group_by(region, cenario, rotulo) |>
  dplyr::summarise(cobertura = mean(cobertura), vies = mean(vies), empse = mean(empse),
                   rmse = mean(rmse), mcse = mean(mcse_rmse), .groups = "drop") |>
  dplyr::mutate(region = factor(region, levels = REGIOES),
                rotulo = factor(rotulo, levels = INDICADORES$rotulo)) |>
  dplyr::arrange(rotulo, region, cenario)

tab5 <- t5_dados |>
  dplyr::transmute(rotulo, region, cenario,
                   Cobertura = fmt_num(cobertura, 1),
                   `Viés` = fmt_num(vies, 3),
                   `EP empírico` = fmt_num(empse, 3),
                   REQM = paste0(fmt_num(rmse, 3), " (", fmt_num(mcse, 3), ")")) |>
  gt::gt(groupname_col = "rotulo") |>
  gt::cols_label(region = gt::md("**Região**"), cenario = gt::md("**Cenário**"),
                 Cobertura = gt::md("**Cobertura**<br>%")) |>
  gt::cols_align("center", columns = -c(rotulo, region)) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 5.** Desempenho dos cenários de quadro amostral na simulação, por região e indicador"))) |>
  gt::tab_footnote(glue::glue(
    "Simulação sob o arcabouço ADEMP com {format(N_REPLICAS, big.mark='.')} réplicas por cenário, ",
    "região e indicador, semente fixa. Valores em pontos percentuais. REQM = raiz do erro quadrático ",
    "médio, desfecho principal; entre parênteses, o erro de Monte Carlo, que mede a incerteza devida ",
    "ao número finito de réplicas e não a incerteza amostral."),
    locations = gt::cells_column_labels(columns = REQM)) |>
  gt::tab_footnote("S0, telefonia fixa apenas; S1, telefonia móvel apenas; S2, cadastro duplo; S3, cadastro triplo com acesso à internet; S4, multimodal integral. O cenário S1 não é subconjunto do S0: tem perfil de não cobertura próprio, e viés de sinal oposto entre eles é resultado legítimo.",
                   locations = gt::cells_column_labels(columns = cenario)) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Pseudopopulação: PNS {ANO_PNS} restrita às 27 capitais, expandida pelos pesos amostrais. A posse ",
    "de telefone não é simulada: usa-se a observada na PNS. Parâmetros do mecanismo gerador na Tabela S6."))) |>
  estilo_gt()
salva_tabela(tab5, "tabela5", t5_dados)

# =============================================================================
# TABELA 6 - validacao 2023
# =============================================================================
h2("Tabela 6")

t6_dados <- val23$confronto |>
  dplyr::mutate(rotulo = factor(rotulo, levels = INDICADORES$rotulo)) |>
  dplyr::arrange(rotulo)

tab6 <- t6_dados |>
  dplyr::transmute(rotulo,
                   previsto = fmt_num(vies_residual_2019, 2),
                   observado = fmt_ic(obs_2023, fd_low, fd_upp, 2),
                   concorda = dplyr::if_else(sign(vies_residual_2019) == sign(obs_2023), "sim", "não"),
                   removido = paste0(fmt_num(pct_removido_2019, 0), "%")) |>
  gt::gt(rowname_col = "rotulo") |>
  gt::cols_label(previsto = gt::md(glue::glue("**Previsto**<br>a partir de {ANO_VIGITEL}<br>pp")),
                 observado = gt::md(glue::glue("**Observado**<br>na transição de {ANO_VIG_DUAL}<br>pp (IC 95%)")),
                 concorda = gt::md("**Concordância<br>de sinal**"),
                 removido = gt::md("**Viés removido pela<br>pós-estratificação<br>em 2019**")) |>
  gt::cols_align("center", columns = -rotulo) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 6.** Validação da previsão contra a transição real do sistema: diferença prevista a ",
    "partir de {ANO_VIGITEL} e observada na adoção do cadastro duplo em {ANO_VIG_DUAL}"))) |>
  gt::tab_footnote(glue::glue(
    "Previsto: resíduo estimado em {ANO_VIGITEL}, isto é, a diferença entre a estimativa publicada ",
    "pelo Vigitel e a prevalência populacional medida na PNS."),
    locations = gt::cells_column_labels(columns = previsto)) |>
  gt::tab_footnote(glue::glue(
    "Observado: diferença entre a estimativa reconstruída do desenho legado dentro da edição de ",
    "{ANO_VIG_DUAL} (entrevistas do quadro de telefonia fixa com o peso pesorake_fixo) e a publicada ",
    "pelo cadastro duplo (peso pesorake), com o tempo mantido constante. Intervalos por bootstrap com ",
    "reamostragem dentro de cada quadro, {format(N_BOOT, big.mark='.')} réplicas, respeitando a ",
    "correlação entre os dois braços, que compartilham entrevistas."),
    locations = gt::cells_column_labels(columns = observado)) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Correlação de Pearson entre previsto e observado: ",
    "{fmt_num(stats::cor(t6_dados$vies_residual_2019, t6_dados$obs_2023), 2)}. Com quatro ",
    "indicadores, a correlação é descritiva e não constitui teste de hipótese."))) |>
  estilo_gt()
salva_tabela(tab6, "tabela6", t6_dados)

cat("\ntabelas em output/tables/:\n"); print(list.files(here::here("output", "tables"), pattern = "^tabela.*\\.docx$"))
message("10_tabelas.R concluido.")
