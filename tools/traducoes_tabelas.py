# -*- coding: utf-8 -*-
"""
Dicionario PT -> EN dos segmentos de texto das tabelas do manuscrito
'Artigo versao.  boaoooooo.docx'. Chave = texto exato do segmento no documento
(marcadores de nota sobrescritos preservados). Numeros nao entram aqui: sao
convertidos por regra (virgula decimal -> ponto, milhar -> virgula, ' a ' -> ' to ').
"""

TABELAS = {

# ---- Tabela 1 - caracteristicas das amostras --------------------------------
"Características": "Characteristics",
"Vigitel 2019": "Vigitel 2019",
"PNS 2019": "PNS 2019",
"n = 52.443": "n = 52,443",
"n = 32.111": "n = 32,111",
"SMD²": "SMD²",
"Idade, anos": "Age, years",
"Média (DP)": "Mean (SD)",
"Sexo": "Sex",
"Masculino": "Men",
"Feminino": "Women",
"Faixa etária, anos": "Age group, years",
"65+": "65+",
"Escolaridade, anos de estudo³": "Education, years of schooling³",
"Escolaridade, anos de estudo": "Education, years of schooling",
"12+": "12+",
"Raça/cor": "Race/skin colour",
"Branca": "White",
"Preta": "Black",
"Parda": "Brown (parda)",
"Amarela": "Asian",
"Indigena": "Indigenous",
"Região": "Region",
"Norte": "North",
"Nordeste": "Northeast",
"Sudeste": "Southeast",
"Sul": "South",
"Centro-Oeste": "Central-West",
"¹Estimativas ponderadas pelo desenho de cada inquérito. A linha de idade traz média e desvio-padrão ponderados; as demais, percentual com intervalo de confiança de 95%.":
  "¹Estimates weighted by the design of each survey. The age row reports the weighted mean and standard deviation; all other rows report percentages with 95% confidence intervals.",
"²SMD = diferença padronizada entre os dois inquéritos, calculada linha a linha (proporções) ou sobre média e desvio-padrão (idade). Valores com |SMD| ≥ 0,10 estão destacados.":
  "²SMD = standardised difference between the two surveys, computed row by row (proportions) or from the mean and standard deviation (age). Values with |SMD| ≥ 0.10 are highlighted.",
"³O peso de pós-estratificação do Vigitel é calibrado por faixa etária, escolaridade e sexo. SMD próxima de zero nessas três variáveis decorre da calibragem e não indica que as populações cobertas sejam equivalentes. Raça/cor e região não entram na calibragem.":
  "³The Vigitel post-stratification weight is calibrated by age group, education and sex. An SMD close to zero for these three variables follows from the calibration and does not indicate that the covered populations are equivalent. Race/skin colour and region do not enter the calibration.",

# ---- Tabela 2 - prevalencias ------------------------------------------------
"Indicador": "Indicator",
"% (IC 95%)": "% (95% CI)",
"Δ, pp (IC 95%)¹": "Δ, pp (95% CI)¹",
"RP (IC 95%)²": "PR (95% CI)²",
"p³": "p³",
"Total": "Overall",
"—": "—",
"Tabagismo atual⁴": "Current smoking⁴",
"Hipertensão diagnosticada⁵": "Diagnosed hypertension⁵",
"Diabetes diagnosticado⁶": "Diagnosed diabetes⁶",
"Autoavaliação ruim de saúde⁷": "Poor self-rated health⁷",
"¹Δ = Vigitel − PNS, em pontos percentuais. Valor negativo indica prevalência menor no Vigitel.":
  "¹Δ = Vigitel − PNS, in percentage points. A negative value indicates a lower prevalence in Vigitel.",
"²RP = razão de prevalências (Vigitel / PNS). Intervalo de confiança pelo método delta na escala logarítmica.":
  "²PR = prevalence ratio (Vigitel / PNS). Confidence interval by the delta method on the logarithmic scale.",
"³Teste bilateral de H0: Δ = 0, tratando os dois inquéritos como amostras independentes, com correção de Holm.":
  "³Two-sided test of H0: Δ = 0, treating the two surveys as independent samples, with Holm correction.",
"⁴Interação sexo × inquérito: p = 0,882 (p de Holm = 1,000), de modelo logístico sobre os dados empilhados com o desenho de cada inquérito preservado.":
  "⁴Sex × survey interaction: p = 0.882 (Holm-adjusted p = 1.000), from a logistic model on the stacked data with the design of each survey preserved.",
"⁵Interação sexo × inquérito: p = 0,142 (p de Holm = 0,568), de modelo logístico sobre os dados empilhados com o desenho de cada inquérito preservado.":
  "⁵Sex × survey interaction: p = 0.142 (Holm-adjusted p = 0.568), from a logistic model on the stacked data with the design of each survey preserved.",
"⁶Interação sexo × inquérito: p = 0,809 (p de Holm = 1,000), de modelo logístico sobre os dados empilhados com o desenho de cada inquérito preservado.":
  "⁶Sex × survey interaction: p = 0.809 (Holm-adjusted p = 1.000), from a logistic model on the stacked data with the design of each survey preserved.",
"⁷Interação sexo × inquérito: p = 0,250 (p de Holm = 0,751), de modelo logístico sobre os dados empilhados com o desenho de cada inquérito preservado.":
  "⁷Sex × survey interaction: p = 0.250 (Holm-adjusted p = 0.751), from a logistic model on the stacked data with the design of each survey preserved.",

# ---- Tabela 3 - vies de nao cobertura ---------------------------------------
"Indicador / estrato": "Indicator / stratum",
"Com telefone fixo": "With a landline",
"Sem telefone fixo": "Without a landline",
"Sem fixo": "Without a landline",
"% da população": "% of the population",
"Viés de não cobertura, pp (IC 95%)¹": "Non-coverage bias, pp (95% CI)¹",
"Vício relativo de Cochran²": "Cochran's relative bias²",
"Tabagismo atual": "Current smoking",
"Hipertensão diagnosticada": "Diagnosed hypertension",
"Diabetes diagnosticado": "Diagnosed diabetes",
"Autoavaliação ruim de saúde": "Poor self-rated health",
"27 capitais": "27 capitals",
"¹Viés de não cobertura = (proporção sem telefone fixo) × (prevalência entre quem tem fixo − prevalência entre quem não tem). Valor positivo indica que um inquérito restrito ao quadro de telefonia fixa superestimaria a prevalência populacional; negativo, que subestima.":
  "¹Non-coverage bias = (proportion without a landline) × (prevalence among those with a landline − prevalence among those without). A positive value indicates that a survey restricted to the landline frame would overestimate the population prevalence; a negative value, that it would underestimate it.",
"²Vício relativo de Cochran = |viés| dividido pelo erro-padrão da estimativa. Acima de 0,40 o nível nominal de 95% dos intervalos se degrada de forma perceptível; os valores acima desse limiar estão marcados com asterisco.":
  "²Cochran's relative bias = |bias| divided by the standard error of the estimate. Above 0.40 the nominal 95% level of the intervals degrades perceptibly; values above that threshold are marked with an asterisk.",

# ---- Tabela 4 - particao ----------------------------------------------------
"Indicadores": "Indicators",
"Gap total (pp)¹": "Total gap (pp)¹",
"Componente de não cobertura (pp, IC 95%)²": "Non-coverage component (pp, 95% CI)²",
"Resíduo (pp, IC 95%)³": "Residual (pp, 95% CI)³",
"¹Gap total = prevalência no Vigitel − prevalência na PNS, em pontos percentuais.":
  "¹Total gap = prevalence in Vigitel − prevalence in the PNS, in percentage points.",
"²Componente de não cobertura estimado inteiramente dentro da PNS":
  "²Non-coverage component estimated entirely within the PNS",
"³Resíduo = gap total − componente de não cobertura, isto é, a parcela da diferença não atribuível à não cobertura telefônica.":
  "³Residual = total gap − non-coverage component, that is, the share of the difference not attributable to telephone non-coverage.",
"⁴Quando o componente de não cobertura tem sinal oposto ao gap observado, ou o excede em magnitude, a razão entre os dois deixa de ser interpretável.":
  "⁴When the non-coverage component has the opposite sign to the observed gap, or exceeds it in magnitude, the ratio between the two is no longer interpretable.",

# ---- Tabela S1 - equivalencia das perguntas ---------------------------------
"PNS": "PNS",
"Vigitel¹": "Vigitel¹",
"Codificação adotada": "Coding adopted",
"Divergência observada": "Observed divergence",
"P050: Atualmente, o(a) Sr(a) fuma algum produto do tabaco? [1 = Sim, diariamente; 2 = Sim, menos que diariamente; 3 = Não fumo atualmente; 9 = Ignorado]":
  "P050: Do you currently smoke any tobacco product? [1 = Yes, daily; 2 = Yes, less than daily; 3 = I do not currently smoke; 9 = Not reported]",
"q60: fumante [1 = sim, diariamente; 2 = sim, mas não diariamente; 3 = não] Rotina oficial: gen fumante = cond(q60<3 & q60!=., 1, 0)":
  "q60: smoker [1 = yes, daily; 2 = yes, but not daily; 3 = no] Official routine: gen fumante = cond(q60<3 & q60!=., 1, 0)",
"1 = fuma atualmente (diariamente ou menos que diariamente); 0 = não fuma":
  "1 = currently smokes (daily or less than daily); 0 = does not smoke",
"A PNS pergunta por 'algum produto do tabaco'; o Vigitel pergunta por cigarros. Diferença de escopo do produto, não de estrutura da resposta.":
  "The PNS asks about 'any tobacco product'; Vigitel asks about cigarettes. A difference in the scope of the product, not in the structure of the response.",
"Q00201: Algum médico já lhe deu o diagnóstico de hipertensão arterial (pressão alta)? [1 = Sim; 2 = Não; 9 = Ignorado]":
  "Q00201: Has a doctor ever given you a diagnosis of arterial hypertension (high blood pressure)? [1 = Yes; 2 = No; 9 = Not reported]",
"q75: pressão alta [1 = sim; 2 = não; 777 = não sabe] Rotina oficial: gen hart = cond((q75 == 1), 1, 0)":
  "q75: high blood pressure [1 = yes; 2 = no; 777 = does not know] Official routine: gen hart = cond((q75 == 1), 1, 0)",
"1 = diagnóstico médico referido; 0 = não": "1 = reported medical diagnosis; 0 = no",
"A PNS tem filtro para hipertensao exclusivamente gestacional (Q00202), que o Vigitel não tem. O Vigitel tem 777 'não sabe' (44 casos), que a rotina oficial conta como não-caso; na versao harmonizada eles saem do denominador, como o 'Ignorado' da PNS.":
  "The PNS has a filter for hypertension occurring exclusively during pregnancy (Q00202), which Vigitel does not have. Vigitel has code 777 'does not know' (44 cases), which the official routine counts as non-cases; in the harmonised version they are removed from the denominator, as with the PNS 'Not reported'.",
"Q03001: Algum médico já lhe deu o diagnóstico de diabetes? [1 = Sim; 2 = Não; 9 = Ignorado]":
  "Q03001: Has a doctor ever given you a diagnosis of diabetes? [1 = Yes; 2 = No; 9 = Not reported]",
"q76: diabetes [1 = sim; 2 = não; 777 = não sabe] Rotina oficial: gen diab = cond((q76 == 1), 1, 0)":
  "q76: diabetes [1 = yes; 2 = no; 777 = does not know] Official routine: gen diab = cond((q76 == 1), 1, 0)",
"Mesma questão gestacional (Q03002) e mesmo tratamento de 777 (58 casos) descrito para a hipertensao.":
  "The same pregnancy-related question (Q03002) and the same treatment of code 777 (58 cases) described for hypertension.",
"Autoavaliação ruim de saude": "Poor self-rated health",
"N001: Em geral, como o(a) Sr(a) avalia a sua saúde [1 = Muito boa; 2 = Boa; 3 = Regular; 4 = Ruim; 5 = Muito ruim; 9 = Ignorado]":
  "N001: In general, how do you rate your health? [1 = Very good; 2 = Good; 3 = Fair; 4 = Poor; 5 = Very poor; 9 = Not reported]",
"q74: estado de saúde [1 = muito bom; 2 = bom; 3 = regular; 4 = ruim; 5 = muito ruim; 777 = não sabe; 888 = não quis informar] Rotina oficial: gen saruim = cond(q74 == 4 | q74 == 5, 1, 0)":
  "q74: health status [1 = very good; 2 = good; 3 = fair; 4 = poor; 5 = very poor; 777 = does not know; 888 = declined to answer] Official routine: gen saruim = cond(q74 == 4 | q74 == 5, 1, 0)",
"1 = 'Ruim' ou 'Muito ruim'; 0 = 'Muito boa', 'Boa' ou 'Regular'":
  "1 = 'Poor' or 'Very poor'; 0 = 'Very good', 'Good' or 'Fair'",
"Escala de 5 pontos idêntica nos dois inquéritos. O Vigitel acrescenta 777/888 (562 casos), tratados como acima.":
  "A five-point scale identical in the two surveys. Vigitel adds codes 777/888 (562 cases), handled as above.",
"Escolaridade": "Education",
"VDD004A: Nível de instrução mais elevado alcançado (pessoas de 5 anos ou mais de idade) padronizado para o Ensino Fundamental - SISTEMA DE 9 ANOS [1 = Sem instrução; 2 = Fundamental incompleto ou equivalente; 3 = Fundamental completo ou equivalente; 4 = Médio incompleto ou equivalente; 5 = Médio completo ou equivalente; 6 = Superior incompleto ou equivalente; 7 = Superior completo]":
  "VDD004A: Highest level of education attained (persons aged 5 years or over), standardised to the nine-year primary school system [1 = No schooling; 2 = Incomplete primary or equivalent; 3 = Complete primary or equivalent; 4 = Incomplete secondary or equivalent; 5 = Complete secondary or equivalent; 6 = Incomplete tertiary or equivalent; 7 = Complete tertiary]",
"fesc: escolaridade (faixas de anos de estudo) [1 = 0 a 8 anos; 2 = 9 a 11 anos; 3 = 12 anos e mais] Rotina oficial: (variavel derivada da base)":
  "fesc: education (bands of years of schooling) [1 = 0 to 8 years; 2 = 9 to 11 years; 3 = 12 years or more] Official routine: (variable derived from the data set)",
"0-8 = até fundamental completo; 9-11 = ensino médio; 12+ = ensino superior":
  "0-8 = up to complete primary; 9-11 = secondary; 12+ = tertiary",
"A PNS coleta nível de instrução; o Vigitel coleta anos de estudo declarados. As faixas do Vigitel (0 a 8 / 9 a 11 / 12 anos e mais) fixam a fronteira em 11 anos para o médio completo, e o mapeamento da PNS segue essa fronteira. A alternativa pelo sistema de 9 anos e testada na Tabela S5.":
  "The PNS collects level of education; Vigitel collects self-reported years of schooling. The Vigitel bands (0 to 8 / 9 to 11 / 12 or more) place the boundary for complete secondary schooling at 11 years, and the PNS mapping follows that boundary. The alternative under the nine-year system is tested in Table S5.",
"Posse de telefone": "Telephone ownership",
"A018017 / A018019: Neste domicílio existe telefone fixo convencional | Neste domicílio existe telefone móvel celular [1 = Sim; 2 = Não; 9 = Ignorado | 1 = Sim; 2 = Não; 9 = Ignorado]":
  "A018017 / A018019: Does this household have a conventional landline telephone | Does this household have a mobile telephone [1 = Yes; 2 = No; 9 = Not reported | 1 = Yes; 2 = No; 9 = Not reported]",
"(não aplicavel): (não aplicavel) [(não aplicavel)]": "(not applicable): (not applicable) [(not applicable)]",
"Fixo (com ou sem celular) / Somente celular / Nenhum":
  "Landline (with or without a mobile) / Mobile only / None",
"Variável existe apenas na PNS. É a base de toda a estimacao de não cobertura: o Vigitel, por construcao, só entrevista quem tem telefone.":
  "The variable exists only in the PNS. It is the basis of the whole non-coverage estimation: Vigitel, by construction, interviews only those who have a telephone.",
"¹Enunciados e categorias transcritos dos dicionários oficiais de cada inquérito, não redigidos pelos autores. A rotina oficial é a sintaxe publicada pelo Ministério da Saúde para gerar cada indicador do Vigitel; a codificação deste estudo a reproduz sem divergência em nenhum dos registros da base.":
  "¹Item wording and response categories transcribed from the official data dictionaries of each survey and translated by the authors, not drafted by them. The official routine is the syntax published by the Ministry of Health to generate each Vigitel indicator; the coding used in this study reproduces it with no discrepancy in any record of the data set.",

# ---- Tabela S2 - perfil por posse de telefone -------------------------------
"Característica": "Characteristic",
"Fixo": "Landline",
"Somente celular¹": "Mobile only¹",
"Nenhum": "No telephone",
"¹Distribuição percentual dentro de cada grupo de posse, com intervalo de confiança de 95%. As colunas somam 100% dentro de cada bloco de variável. O grupo 'Somente celular' é o que um quadro amostral dual-frame passaria a alcançar, e por isso o seu perfil é o parâmetro central da simulação.":
  "¹Percentage distribution within each ownership group, with 95% confidence intervals. Columns sum to 100% within each variable block. The 'Mobile only' group is the one a dual-frame sampling design would newly reach, which is why its profile is the central parameter of the simulation.",
"Fonte: PNS 2019 (IBGE). Posse de telefone das variáveis A018017 (fixo) e A018019 (celular) do módulo de características do domicílio.":
  "Source: PNS 2019 (IBGE). Telephone ownership from variables A018017 (landline) and A018019 (mobile) of the household characteristics module.",

# ---- Tabela S3 - vies por regiao --------------------------------------------
"Indicador / região": "Indicator / region",
"População": "Population",
"%": "%",
"Com fixo": "With a landline",
"% da pop.": "% of the pop.",
"Viés": "Bias",
"pp (IC 95%)": "pp (95% CI)",
"Cochran": "Cochran",
"(EP da PNS)": "(PNS SE)",
"Cochran¹": "Cochran¹",
"¹As duas últimas colunas trazem o vício relativo de Cochran sob os dois denominadores possíveis: o erro-padrão interno da PNS e o erro-padrão que o Vigitel efetivamente tem. O limiar de degradação é 0,40 em ambos.":
  "¹The last two columns give Cochran's relative bias under the two possible denominators: the internal standard error of the PNS and the standard error that Vigitel actually has. The degradation threshold is 0.40 in both.",

# ---- Tabela S4 - obesidade (sensibilidade) ----------------------------------
"Estimativa": "Estimate",
"Estimativa (IC 95%)": "Estimate (95% CI)",
"n": "n",
"Prevalência de obesidade (IMC ≥ 30)": "Prevalence of obesity (BMI ≥ 30)",
"Vigitel — peso e altura declarados": "Vigitel — self-reported weight and height",
"PNS — peso e altura declarados": "PNS — self-reported weight and height",
"PNS — peso e altura aferidos": "PNS — measured weight and height",
"Diferenças, pontos percentuais": "Differences, percentage points",
"Δ sem correção (Vigitel declarado − PNS declarado)": "Δ without correction (Vigitel self-reported − PNS self-reported)",
"Δ com correção (Vigitel declarado − PNS aferido)": "Δ with correction (Vigitel self-reported − PNS measured)",
"Viés do autorrelato na PNS (declarado − aferido)": "Self-report bias within the PNS (self-reported − measured)",
"¹A comparação 'sem correção' confronta duas medidas declaradas e é a comparação equivalente à das Tabelas 2 e 4.":
  "¹The 'without correction' comparison contrasts two self-reported measures and is the comparison equivalent to those in Tables 2 and 4.",

# ---- Tabela S5 - sensibilidade ao ajuste ------------------------------------
"Conjunto de covariáveis de ajuste¹": "Set of adjustment covariates¹",
"Sem ajuste": "Unadjusted",
"Idade e sexo": "Age and sex",
"Idade, sexo, escolaridade e região": "Age, sex, education and region",
"Acima + escolaridade alternativa (9 anos)": "Above + alternative education coding (nine-year system)",
"¹Razão de chances de ter telefone fixo no domicílio associada a cada desfecho, com intervalo de confiança de 95%, em modelo logístico com desenho complexo. A razão sem ajuste é a associação bruta; a ajustada por idade, sexo, escolaridade e região é a associação residual às variáveis de calibragem do Vigitel. Quanto mais a razão se aproxima de 1 com o ajuste, mais o viés de não cobertura é de composição e mais a pós-estratificação o alcança.":
  "¹Odds ratio of having a landline in the household associated with each outcome, with 95% confidence intervals, from a logistic model accounting for the complex design. The unadjusted ratio is the crude association; the one adjusted for age, sex, education and region is the association residual to the Vigitel calibration variables. The closer the ratio moves to 1 with adjustment, the more the non-coverage bias is compositional and the more post-stratification reaches it.",

# ---- Tabela S6 - mecanismo gerador ------------------------------------------
"Parâmetro": "Parameter",
"Valor": "Value",
"Cobertura de cada quadro amostral, por região (%)": "Coverage of each sampling frame, by region (%)",
"Associação residual desfecho ~ posse de telefone fixo (OR)":
  "Residual association outcome ~ landline ownership (OR)",
"Conferência da cobertura entre fontes independentes (%)":
  "Cross-check of coverage between independent sources (%)",
"PNS (capitais)": "PNS (capitals)",
"PNAD-C TIC (capitais)": "PNAD-C ICT (capitals)",
"fixo 39,7; celular 97,3": "landline 39.7; mobile 97.3",
"fixo 38,3; celular 98,1": "landline 38.3; mobile 98.1",
"¹A posse de telefone não é simulada: a pseudopopulação usa a posse observada na PNS, de modo que a dependência entre posse, características sociodemográficas e desfecho é a que existe nos dados.":
  "¹Telephone ownership is not simulated: the pseudo-population uses the ownership observed in the PNS, so that the dependence between ownership, sociodemographic characteristics and outcome is the one present in the data.",

# ---- Tabela S7 - simulacao completa -----------------------------------------
"Cenário": "Scenario",
"Cobertura": "Coverage",
"Verdadeiro": "True value",
"EP empírico": "Empirical SE",
"REQM¹": "RMSE¹",
"S0": "S0", "S1": "S1", "S2": "S2", "S3": "S3", "S4": "S4",
"¹1.000 réplicas por cenário × região × indicador, semente fixa. Valores em pontos percentuais. O erro de Monte Carlo entre parênteses mede a incerteza devida ao número finito de réplicas, e não a incerteza amostral.":
  "¹1,000 replicates per scenario × region × indicator, with a fixed seed. Values in percentage points. The Monte Carlo standard error in parentheses measures the uncertainty due to the finite number of replicates, not sampling uncertainty.",

# ---- Tabela S8 - cenarios ---------------------------------------------------
"Frame amostral": "Sampling frame",
"telefonia fixa apenas": "landline only",
"telefonia móvel apenas": "mobile only",
"cadastro duplo (fixo + móvel)": "dual frame (landline + mobile)",
"cadastro triplo (fixo + móvel + web)": "triple frame (landline + mobile + web)",
"multimodal integral": "full multimodal",

# ---- Tabela S9 - previsto x observado por regiao ----------------------------
"REQM S0 (simulação)": "RMSE S0 (simulation)",
"REQM S2 (simulação)": "RMSE S2 (simulation)",
"Redução prevista": "Predicted reduction",
"Diferença média observada em 2023 (pp)": "Mean difference observed in 2023 (pp)",
}

# Itens "quadro — regiao" da Tabela S6, gerados para nao repetir 20 linhas iguais.
for _pt, _en in (("fixo", "landline"), ("celular", "mobile"),
                 ("web", "web"), ("presencial", "in person")):
    for _rpt, _ren in (("Norte", "North"), ("Nordeste", "Northeast"),
                       ("Sudeste", "Southeast"), ("Sul", "South"),
                       ("Centro-Oeste", "Central-West")):
        TABELAS[f"{_pt} — {_rpt}"] = f"{_en} — {_ren}"


LEGENDAS = {
"Tabela 1. Características das amostras de adultos (≥18 anos) das 27 capitais, Vigitel 2019 e PNS 2019":
  "Table 1. Characteristics of the adult samples (aged 18 years or over) of the 27 capitals, Vigitel 2019 and PNS 2019",
"Tabela 2. Prevalência dos indicadores em adultos (≥18 anos) das 27 capitais, Vigitel 2019 e PNS 2019, com diferença absoluta e razão de prevalências.":
  "Table 2. Prevalence of the indicators among adults (aged 18 years or over) of the 27 capitals, Vigitel 2019 and PNS 2019, with absolute differences and prevalence ratios.",
"Tabela 3. Viés de não cobertura da telefonia fixa, estimado dentro da PNS 2019, por indicador e região adultos (≥18 anos) das 27 capitais":
  "Table 3. Non-coverage bias of the landline frame, estimated within the PNS 2019, by indicator and region — adults (aged 18 years or over) of the 27 capitals",
"Tabela 4. Partição da diferença entre Vigitel 2019 e PNS 2019 em componente de não cobertura e resíduo adultos (≥18 anos) das 27 capitais":
  "Table 4. Partition of the difference between Vigitel 2019 and PNS 2019 into a non-coverage component and a residual — adults (aged 18 years or over) of the 27 capitals",
"Figura 1. Diferença de prevalência entre Vigitel 2019 e PNS 2019 (Δ = Vigitel − PNS), em pontos percentuais, no total e por sexo, adultos (≥18 anos) das 27 capitais.":
  "Figure 1. Prevalence difference between Vigitel 2019 and PNS 2019 (Δ = Vigitel − PNS), in percentage points, overall and by sex, adults (aged 18 years or over) of the 27 capitals.",
"Figura 2. Raiz do erro quadrático médio (REQM) por cenário de quadro amostral (S0–S4) e macrorregião, para os quatro indicadores, simulação de Monte Carlo.":
  "Figure 2. Root mean squared error (RMSE) by sampling-frame scenario (S0–S4) and macro-region, for the four indicators, Monte Carlo simulation.",
"Tabela S1. Equivalência das perguntas e da codificação dos indicadores entre Vigitel 2019 e PNS 2019, com as divergências observadas.":
  "Table S1. Equivalence of the items and of the indicator coding between Vigitel 2019 and PNS 2019, with the divergences observed.",
"Tabela S2. Perfil sociodemográfico segundo a posse de telefone no domicílio, adultos (≥18 anos) das 27 capitais, PNS 2019.":
  "Table S2. Sociodemographic profile according to household telephone ownership, adults (aged 18 years or over) of the 27 capitals, PNS 2019.",
"Tabela S3. Prevalências segundo a posse de telefone fixo e viés de não cobertura, por macrorregião, adultos das 27 capitais, PNS 2019.":
  "Table S3. Prevalences according to landline ownership and non-coverage bias, by macro-region, adults of the 27 capitals, PNS 2019.",
"Tabela S4. Obesidade como análise de sensibilidade, com e sem correção do autorrelato, Vigitel 2019 e PNS 2019.":
  "Table S4. Obesity as a sensitivity analysis, with and without correction for self-report, Vigitel 2019 and PNS 2019.",
"Tabela S5. Sensibilidade da associação entre desfecho e posse de telefone fixo ao conjunto de covariáveis de ajuste, PNS 2019.":
  "Table S5. Sensitivity of the association between outcome and landline ownership to the set of adjustment covariates, PNS 2019.",
"Tabela S6. Parâmetros do mecanismo gerador da simulação e cobertura de cada quadro amostral, por macrorregião.":
  "Table S6. Parameters of the data-generating mechanism of the simulation and coverage of each sampling frame, by macro-region.",
"Tabela S7. Resultados completos da simulação por cenário, macrorregião e indicador, com erro de Monte Carlo entre parênteses.":
  "Table S7. Full simulation results by scenario, macro-region and indicator, with the Monte Carlo standard error in parentheses.",
"Tabela S8. Cenários de frame amostral avaliados na simulação.":
  "Table S8. Sampling-frame scenarios evaluated in the simulation.",
"Tabela S9. Previsto e observado por região o resultado negativo da estratificação regional":
  "Table S9. Predicted and observed by region: the negative result of the regional stratification",
"Figura S1. Perfil sociodemográfico dos grupos de posse de telefone no domicílio, adultos (≥18 anos) das 27 capitais, PNS 2019.":
  "Figure S1. Sociodemographic profile of the household telephone ownership groups, adults (aged 18 years or over) of the 27 capitals, PNS 2019.",
"Figura S2. Viés das estimativas por cenário de quadro amostral (S0–S4) e macrorregião, simulação de Monte Carlo.":
  "Figure S2. Bias of the estimates by sampling-frame scenario (S0–S4) and macro-region, Monte Carlo simulation.",
"Figura S3. Partição da diferença entre Vigitel 2019 e PNS 2019 em componente de não cobertura e resíduo, por indicador.":
  "Figure S3. Partition of the difference between Vigitel 2019 and PNS 2019 into a non-coverage component and a residual, by indicator.",
}
