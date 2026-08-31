# =============================================================================
# 07_partition.R - Tabela 4: particao do gap
#
#   gap_total       = prevalencia_vigitel - prevalencia_pns
#   componente_ncob = vies de nao cobertura (script 06, estimado dentro da PNS)
#   residuo         = gap_total - componente_ncob
#
# NOMENCLATURA OBRIGATORIA: o residuo chama-se "residuo" ou "componente nao
# atribuivel a nao cobertura". NUNCA "efeito de modo". Modo de coleta e uma
# hipotese discutida no texto, nao um rotulo de coluna: o residuo tambem absorve
# efeito de autorrelato, diferenca de instrumento, nao resposta, calibragem e
# qualquer outra fonte que este desenho nao separa.
#
# Variancia do residuo, sem supor independencia indevida:
#   residuo = p_vigitel - (p_pns + vies) = p_vigitel - alvo
# O termo "alvo" e uma quantidade inteiramente da PNS, calculada dentro do mesmo
# bootstrap do script 06, de modo que sua variancia ja incorpora a correlacao
# entre p_pns e vies. O Vigitel e amostra independente, entao
#   Var(residuo) = Var(p_vigitel) + Var(alvo).
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("07_partition.R - particao do gap (Tabela 4)")

vies  <- readRDS(here::here("data", "vies_nao_cobertura.rds"))
prev  <- readRDS(here::here("data", "prevalencias.rds"))
tab2  <- readRDS(here::here("output", "tables", "table2_dados.rds"))

# ---- Particao no conjunto das 27 capitais -----------------------------------

vig_total <- prev |>
  dplyr::filter(survey == "Vigitel", estrato == "Total") |>
  dplyr::select(indicator, label, p_vig = est, se_vig = se)

alvo <- vies |>
  dplyr::filter(dominio == "27 capitais") |>
  dplyr::select(indicator, est_p_pns, se_p_pns, est_vies, se_vies, est_alvo, se_alvo)

particao <- vig_total |>
  dplyr::left_join(alvo, by = "indicator") |>
  dplyr::mutate(
    gap_total   = p_vig - est_p_pns,
    componente  = est_vies,
    residuo     = p_vig - est_alvo,
    se_residuo  = sqrt(se_vig^2 + se_alvo^2),
    residuo_low = residuo - stats::qnorm(0.975) * se_residuo,
    residuo_upp = residuo + stats::qnorm(0.975) * se_residuo,
    comp_low    = componente - stats::qnorm(0.975) * se_vies,
    comp_upp    = componente + stats::qnorm(0.975) * se_vies,
    pct_explicado = 100 * componente / gap_total,
    # A soma tem de fechar por construcao; conferido no QA
    checagem    = componente + residuo - gap_total
  )

h2("Particao do gap, 27 capitais")
print(as.data.frame(particao |> dplyr::select(label, gap_total, componente, residuo,
                                              pct_explicado, checagem)), digits = 4)
cat("\nmaior residuo da soma (deve ser ~0):", format(max(abs(particao$checagem)), digits = 3), "\n")
stopifnot(max(abs(particao$checagem)) < 1e-8)

# ---- Interpretacao do percentual explicado ----------------------------------
# Quando o componente de nao cobertura excede o gap observado, ou tem sinal
# oposto, o "percentual explicado" deixa de ser interpretavel como fracao. Em vez
# de imprimir um numero sem sentido, classificamos a situacao.

particao <- particao |>
  dplyr::mutate(
    situacao = dplyr::case_when(
      sign(componente) != sign(gap_total) ~ "sinal oposto ao gap",
      abs(componente) > abs(gap_total)    ~ "excede o gap observado",
      TRUE                                 ~ "fracao do gap"
    ),
    pct_txt = dplyr::if_else(situacao == "fracao do gap",
                             paste0(fmt_num(pct_explicado, 0), "%"),
                             situacao)
  )
print(as.data.frame(particao |> dplyr::select(label, gap_total, componente, situacao)), digits = 3)

# ---- Particao por regiao ----------------------------------------------------
# Alimenta o material suplementar e sustenta a leitura regional da discussao.

h2("Particao por regiao")

des_vig <- readRDS(here::here("data", "design_vigitel.rds"))
OUTCOMES <- INDICATORS |> dplyr::mutate(var = paste0(indicator, "_official"))

vig_regiao <- purrr::pmap_dfr(
  list(OUTCOMES$var, OUTCOMES$indicator),
  function(var, ind) {
    v <- rlang::sym(var)
    des_vig |>
      srvyr::filter(!is.na(!!v)) |>
      srvyr::group_by(region) |>
      srvyr::summarise(p = srvyr::survey_mean(!!v, vartype = "se",
                                              proportion = TRUE, na.rm = TRUE)) |>
      dplyr::transmute(indicator = ind, dominio = as.character(region),
                       p_vig = p * 100, se_vig = p_se * 100)
  }
)

particao_regiao <- vies |>
  dplyr::filter(dominio != "27 capitais") |>
  dplyr::select(indicator, label, dominio, est_p_pns, est_vies, se_vies, est_alvo, se_alvo) |>
  dplyr::left_join(vig_regiao, by = c("indicator", "dominio")) |>
  dplyr::mutate(
    gap_total  = p_vig - est_p_pns,
    componente = est_vies,
    residuo    = p_vig - est_alvo,
    se_residuo = sqrt(se_vig^2 + se_alvo^2),
    residuo_low = residuo - stats::qnorm(0.975) * se_residuo,
    residuo_upp = residuo + stats::qnorm(0.975) * se_residuo,
    comp_low   = componente - stats::qnorm(0.975) * se_vies,
    comp_upp   = componente + stats::qnorm(0.975) * se_vies,
    dominio    = factor(dominio, levels = REGION_LEVELS),
    label      = factor(label, levels = INDICATORS$label_pt)
  ) |>
  dplyr::arrange(label, dominio)

print(as.data.frame(particao_regiao |> dplyr::select(label, dominio, gap_total,
                                                     componente, residuo)), digits = 3)
write_log(particao |> dplyr::select(-dplyr::any_of("checagem")), "particao_gap")
write_log(particao_regiao, "particao_gap_regiao")

# ---- Tabela 4 ---------------------------------------------------------------

corpo4 <- particao |>
  dplyr::mutate(
    label = factor(label, levels = INDICATORS$label_pt),
    col_gap  = fmt_num(gap_total, 2),
    col_comp = fmt_ci(componente, comp_low, comp_upp, 2),
    col_res  = fmt_ci(residuo, residuo_low, residuo_upp, 2),
    col_pct  = pct_txt
  ) |>
  dplyr::arrange(label) |>
  dplyr::select(label, col_gap, col_comp, col_res, col_pct)

gt4 <- corpo4 |>
  gt::gt(rowname_col = "label") |>
  gt::cols_label(
    col_gap  = gt::md("**Gap total**<br>pp"),
    col_comp = gt::md("**Componente de<br>não cobertura**<br>pp (IC 95%)"),
    col_res  = gt::md("**Resíduo**<br>pp (IC 95%)"),
    col_pct  = gt::md("**Parcela do gap<br>atribuível à<br>não cobertura**")
  ) |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = dplyr::starts_with("col_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 4.** Partição da diferença entre Vigitel {YEAR_VIGITEL} e PNS {YEAR_PNS} ",
    "em componente de não cobertura e resíduo — adultos (≥18 anos) das 27 capitais"
  ))) |>
  gt::tab_footnote(
    footnote = "Gap total = prevalência no Vigitel − prevalência na PNS, em pontos percentuais. Reproduz exatamente a coluna Δ da Tabela 2.",
    locations = gt::cells_column_labels(columns = col_gap)
  ) |>
  gt::tab_footnote(
    footnote = paste(
      "Componente de não cobertura estimado inteiramente dentro da PNS (Tabela 3):",
      "é o viés que um inquérito restrito ao quadro de telefonia fixa sofreria por",
      "excluir a população sem telefone fixo. Não depende de nenhum dado do Vigitel."
    ),
    locations = gt::cells_column_labels(columns = col_comp)
  ) |>
  gt::tab_footnote(
    footnote = paste(
      "Resíduo = gap total − componente de não cobertura, isto é, a parcela da",
      "diferença não atribuível à não cobertura telefônica. O resíduo absorve",
      "conjuntamente efeito de modo de coleta, autorrelato, diferenças de",
      "instrumento, não resposta e calibragem de pesos; este desenho não permite",
      "separá-los. Intervalo de confiança pela soma das variâncias do Vigitel e da",
      "quantidade correspondente na PNS, esta última obtida no mesmo bootstrap que",
      "gerou o componente, de modo a respeitar a correlação entre as duas parcelas."
    ),
    locations = gt::cells_column_labels(columns = col_res)
  ) |>
  gt::tab_footnote(
    footnote = paste(
      "Quando o componente de não cobertura tem sinal oposto ao gap observado, ou o",
      "excede em magnitude, a razão entre os dois deixa de ser interpretável como",
      "fração e a situação é descrita em palavras. Componente maior que o gap",
      "indica que outras fontes atuam em sentido contrário ao da não cobertura,",
      "compensando-a parcialmente."
    ),
    locations = gt::cells_column_labels(columns = col_pct)
  ) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Fontes: Vigitel {YEAR_VIGITEL} (Ministério da Saúde) e PNS {YEAR_PNS} (IBGE), ",
    "adultos das 26 capitais e do Distrito Federal. Convenção de sinal: Δ = Vigitel − PNS."
  ))) |>
  gt_study_style()

save_table(gt4, "table4")
saveRDS(particao, here::here("output", "tables", "table4_dados.rds"))
saveRDS(particao, here::here("data", "particao.rds"))
saveRDS(particao_regiao, here::here("data", "particao_regiao.rds"))

# ---- Conferencia com a Tabela 2 (QA item 2) ---------------------------------

h2("Conferencia: gap da Tabela 4 x diferenca da Tabela 2")

conf <- particao |>
  dplyr::select(indicator, label, gap_tab4 = gap_total) |>
  dplyr::left_join(
    tab2 |> dplyr::filter(estrato == "Total") |> dplyr::select(indicator, gap_tab2 = delta),
    by = "indicator"
  ) |>
  dplyr::mutate(diferenca = gap_tab4 - gap_tab2)
print(as.data.frame(conf), digits = 6)
# Tolerancia de 1e-6 pp, nao zero exato: a Tabela 2 obtem a prevalencia por
# svyciprop (ajuste iterativo na escala logit) e o script 06 pela media ponderada
# direta. Os dois caminhos concordam ate a oitava casa; exigir igualdade binaria
# faria o QA falhar por ruido de ponto flutuante, nao por erro de analise.
TOL_GAP <- 1e-6
if (max(abs(conf$diferenca)) > TOL_GAP) {
  stop("Gap da Tabela 4 nao bate com a Tabela 2. Investigue antes de seguir.", call. = FALSE)
}
cat("\nOK: gap identico ao da Tabela 2 nos quatro indicadores",
    "(maior discrepancia:", format(max(abs(conf$diferenca)), digits = 2), "pp).\n")

message("07_partition.R concluido.")
