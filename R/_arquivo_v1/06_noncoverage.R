# =============================================================================
# 06_noncoverage.R - Tabela 3: vies de nao cobertura telefonica
#
# NUCLEO DO ARTIGO. Toda a estimacao acontece DENTRO da PNS: nenhum numero desta
# tabela depende do Vigitel. A PNS observa quem tem e quem nao tem telefone fixo,
# e permite calcular o vies que um inquerito restrito ao quadro de telefonia fixa
# sofreria por deixar de fora quem nao tem fixo.
#
# Para cada indicador e cada regiao:
#   p_fixo    = prevalencia entre quem tem telefone fixo no domicilio
#   p_semfixo = prevalencia entre quem nao tem fixo (so celular + nenhum)
#   f_semfixo = proporcao sem telefone fixo
#   vies_ncob = f_semfixo * (p_fixo - p_semfixo)
#
# Vies positivo significa que o quadro de telefonia fixa SUPERESTIMA a prevalencia
# populacional; negativo, que subestima.
#
# Intervalos por bootstrap de Rao-Wu sobre o desenho (subbootstrap), declarado
# explicitamente na nota da tabela. O recorte de capitais e composto de estratos
# completos - nenhum estrato mistura capital e nao-capital -, entao construir as
# replicas sobre o subconjunto e exato.
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("06_noncoverage.R - vies de nao cobertura (Tabela 3)")

pns <- readRDS(here::here("data", "pns.rds"))
OUTCOMES <- INDICATORS |> dplyr::mutate(var = paste0(indicator, "_official"))

cat("amostra de capitais:", nrow(pns), "| estratos:", dplyr::n_distinct(pns$strata),
    "| UPAs:", dplyr::n_distinct(pns$psu), "\n")

# ---- Desenho e replicas -----------------------------------------------------

des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~weight,
                         data = pns, nest = TRUE)

cat("gerando", N_BOOT, "replicas bootstrap (Rao-Wu)...\n")
t0 <- Sys.time()
rep_des <- survey::as.svrepdesign(des, type = "subbootstrap", replicates = N_BOOT)
cat("replicas prontas em", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

# ---- Estatistica ------------------------------------------------------------
# Uma unica funcao devolve, de uma vez, todas as quantidades para todos os
# dominios. Assim o bootstrap roda uma vez por indicador e a covariancia entre as
# quantidades e respeitada.

DOMINIOS <- c("27 capitais", REGION_LEVELS)

#' Vetor com p_fixo, p_semfixo, f_semfixo e vies para cada dominio
theta_ncob <- function(w, data, yvar) {
  y <- data[[yvar]]
  L <- data$has_landline
  reg <- as.character(data$region)
  out <- numeric(0)
  for (dom in DOMINIOS) {
    sel <- if (dom == "27 capitais") rep(TRUE, nrow(data)) else reg == dom
    ok  <- sel & !is.na(y) & !is.na(L)
    ww  <- w[ok]; yy <- y[ok]; ll <- L[ok]
    w_fix <- sum(ww * ll)
    w_sem <- sum(ww * (1 - ll))
    p_fix <- sum(ww * yy * ll) / w_fix
    p_sem <- sum(ww * yy * (1 - ll)) / w_sem
    f_sem <- w_sem / (w_fix + w_sem)
    p_pns <- sum(ww * yy) / sum(ww)          # prevalencia populacional na PNS
    vies  <- f_sem * (p_fix - p_sem)
    # "alvo": prevalencia da PNS somada ao vies de nao cobertura. E o valor que o
    # Vigitel produziria se a nao cobertura fosse a UNICA fonte de divergencia.
    # Calculado aqui dentro, na mesma replica, para que o residuo do script 07
    # (residuo = p_vigitel - alvo) tenha variancia exata: o gap e o componente de
    # nao cobertura compartilham a amostra da PNS e sao correlacionados, entao
    # somar variancias como se fossem independentes estaria errado.
    alvo  <- p_pns + vies
    out <- c(out, stats::setNames(
      c(p_fix * 100, p_sem * 100, f_sem * 100, vies * 100, p_pns * 100, alvo * 100),
      paste0(c("p_fixo.", "p_semfixo.", "f_semfixo.", "vies.", "p_pns.", "alvo."), dom)
    ))
  }
  out
}

h2("Estimando por indicador")

resultados <- purrr::pmap_dfr(
  list(OUTCOMES$var, OUTCOMES$indicator, OUTCOMES$label_pt),
  function(var, ind, rot) {
    cat("  ", rot, "... ")
    r <- survey::withReplicates(rep_des, function(w, data) theta_ncob(w, data, var))
    est <- as.numeric(coef(r))
    se  <- as.numeric(survey::SE(r))
    nm  <- names(coef(r))
    cat("ok\n")
    tibble::tibble(
      indicator = ind, label = rot,
      quantidade = sub("\\..*$", "", nm),
      dominio    = sub("^[^.]*\\.", "", nm),
      est = est, se = se
    )
  }
)

# ---- Vicio relativo de Cochran ----------------------------------------------
# Razao entre o vies e o erro-padrao da estimativa. Acima de 0,40, o nivel
# nominal de 95% se degrada de forma perceptivel.
#
# Reportamos DUAS razoes, porque respondem a perguntas diferentes:
#   cochran_pns     - vies dividido pelo EP de p_fixo estimado na propria PNS.
#                     E a leitura literal do criterio, autocontida nesta tabela.
#   cochran_vigitel - vies dividido pelo EP que o Vigitel efetivamente tem para
#                     aquele indicador (script 05). E a leitura operacional: diz
#                     se o vies de nao cobertura ameaca a cobertura nominal dos
#                     intervalos que o Vigitel de fato publica.
# O Vigitel tem n = 52.443 e EP menor, entao a segunda razao e sempre a maior -
# e e ela que importa para quem usa as estimativas publicadas.

prevalencias <- readRDS(here::here("data", "prevalencias.rds"))
se_vigitel <- prevalencias |>
  dplyr::filter(survey == "Vigitel", estrato == "Total") |>
  dplyr::select(indicator, se_vig = se)

largo <- resultados |>
  dplyr::select(indicator, label, dominio, quantidade, est, se) |>
  tidyr::pivot_wider(names_from = quantidade, values_from = c(est, se)) |>
  dplyr::left_join(se_vigitel, by = "indicator") |>
  dplyr::mutate(
    vies_low = est_vies - stats::qnorm(0.975) * se_vies,
    vies_upp = est_vies + stats::qnorm(0.975) * se_vies,
    p_fixo_low = est_p_fixo - stats::qnorm(0.975) * se_p_fixo,
    p_fixo_upp = est_p_fixo + stats::qnorm(0.975) * se_p_fixo,
    p_sem_low  = est_p_semfixo - stats::qnorm(0.975) * se_p_semfixo,
    p_sem_upp  = est_p_semfixo + stats::qnorm(0.975) * se_p_semfixo,
    f_low = est_f_semfixo - stats::qnorm(0.975) * se_f_semfixo,
    f_upp = est_f_semfixo + stats::qnorm(0.975) * se_f_semfixo,
    cochran_pns     = abs(est_vies) / se_p_fixo,
    cochran_vigitel = abs(est_vies) / se_vig,
    acima_limiar    = cochran_vigitel >= COCHRAN_LIMIT
  )

h2("Vies de nao cobertura (pontos percentuais)")
print(as.data.frame(largo |> dplyr::select(label, dominio, est_p_fixo, est_p_semfixo,
                                           est_f_semfixo, est_vies, vies_low, vies_upp,
                                           cochran_pns, cochran_vigitel)), digits = 3)
write_log(largo, "vies_nao_cobertura")

# ---- Perfil dos tres grupos de posse ----------------------------------------
# O grupo "somente celular" e o que importa para a simulacao (script 08): e ele
# que um quadro dual-frame passaria a alcancar.

h2("Prevalencia por grupo de posse de telefone")

des_srvyr <- pns |>
  srvyr::as_survey_design(strata = strata, ids = psu, weights = weight, nest = TRUE)

por_grupo <- purrr::pmap_dfr(
  list(OUTCOMES$var, OUTCOMES$label_pt),
  function(var, rot) {
    v <- rlang::sym(var)
    des_srvyr |>
      srvyr::filter(!is.na(!!v), !is.na(phone3)) |>
      srvyr::group_by(phone3) |>
      srvyr::summarise(
        p = srvyr::survey_mean(!!v, vartype = "ci", proportion = TRUE, na.rm = TRUE),
        n = srvyr::unweighted(dplyr::n())
      ) |>
      dplyr::transmute(label = rot, phone3 = as.character(phone3),
                       est = p * 100, low = p_low * 100, upp = p_upp * 100, n)
  }
)

# Distribuicao da posse (uma vez, nao por indicador)
dist_posse <- des_srvyr |>
  srvyr::filter(!is.na(phone3)) |>
  srvyr::group_by(phone3) |>
  srvyr::summarise(p = srvyr::survey_mean(vartype = "ci", proportion = TRUE),
                   n = srvyr::unweighted(dplyr::n())) |>
  dplyr::transmute(phone3 = as.character(phone3),
                   est = p * 100, low = p_low * 100, upp = p_upp * 100, n)
print(as.data.frame(dist_posse), digits = 3)
print(as.data.frame(por_grupo), digits = 3)
write_log(por_grupo, "prevalencia_por_posse_telefone")
write_log(dist_posse, "distribuicao_posse_telefone")

# ---- Tabela 3 ---------------------------------------------------------------

corpo3 <- largo |>
  dplyr::mutate(
    label   = factor(label, levels = INDICATORS$label_pt),
    dominio = factor(dominio, levels = DOMINIOS),
    col_fixo = fmt_ci(est_p_fixo, p_fixo_low, p_fixo_upp, 1),
    col_sem  = fmt_ci(est_p_semfixo, p_sem_low, p_sem_upp, 1),
    col_f    = fmt_ci(est_f_semfixo, f_low, f_upp, 1),
    col_vies = fmt_ci(est_vies, vies_low, vies_upp, 2),
    col_coch = paste0(fmt_num(cochran_vigitel, 2), dplyr::if_else(acima_limiar, " *", ""))
  ) |>
  dplyr::arrange(label, dominio) |>
  dplyr::select(label, dominio, col_fixo, col_sem, col_f, col_vies, col_coch)

n_acima <- sum(largo$acima_limiar)

gt3 <- corpo3 |>
  gt::gt(groupname_col = "label", rowname_col = "dominio") |>
  gt::cols_label(
    col_fixo = gt::md("**Com telefone fixo**<br>% (IC 95%)"),
    col_sem  = gt::md("**Sem telefone fixo**<br>% (IC 95%)"),
    col_f    = gt::md("**Sem fixo**<br>% da população (IC 95%)"),
    col_vies = gt::md("**Viés de não cobertura**<br>pp (IC 95%)"),
    col_coch = gt::md("**Vício relativo**<br>de Cochran")
  ) |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = dplyr::starts_with("col_")) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 3.** Viés de não cobertura da telefonia fixa, estimado dentro da PNS {YEAR_PNS}, ",
    "por indicador e região — adultos (≥18 anos) das 27 capitais"
  ))) |>
  gt::tab_footnote(
    footnote = paste(
      "Viés de não cobertura = (proporção sem telefone fixo) × (prevalência entre",
      "quem tem fixo − prevalência entre quem não tem). Valor positivo indica que",
      "um inquérito restrito ao quadro de telefonia fixa superestimaria a",
      "prevalência populacional; negativo, que subestimaria."
    ),
    locations = gt::cells_column_labels(columns = col_vies)
  ) |>
  gt::tab_footnote(
    footnote = glue::glue(
      "Vício relativo de Cochran = |viés| dividido pelo erro-padrão da estimativa. ",
      "Acima de {fmt_num(COCHRAN_LIMIT, 2)} o nível nominal de 95% dos intervalos se ",
      "degrada de forma perceptível; os valores acima desse limiar estão marcados com asterisco ",
      "({n_acima} de {nrow(largo)} células). A razão apresentada usa o erro-padrão que o Vigitel ",
      "efetivamente tem para o indicador, que é a leitura operacional do critério. A razão ",
      "calculada com o erro-padrão interno da PNS consta do material suplementar."
    ),
    locations = gt::cells_column_labels(columns = col_coch)
  ) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Fonte: PNS {YEAR_PNS} (IBGE), adultos residentes nas 26 capitais e no Distrito Federal. ",
    "Nenhuma quantidade desta tabela depende do Vigitel: a estimação é inteiramente interna à PNS. ",
    "Intervalos de confiança do viés por bootstrap de Rao-Wu sobre o desenho amostral, com ",
    "{format(N_BOOT, big.mark = '.')} réplicas e semente fixa."
  ))) |>
  gt_study_style()

save_table(gt3, "table3")
saveRDS(largo, here::here("output", "tables", "table3_dados.rds"))
saveRDS(largo, here::here("data", "vies_nao_cobertura.rds"))

# ---- Tabela suplementar S2: perfil por posse de telefone --------------------

h2("Tabela S2 - perfil sociodemografico por posse de telefone")

perfil_vars <- c("sex", "age_grp", "education", "race", "region")
perfil <- purrr::map_dfr(perfil_vars, function(v) {
  s <- rlang::sym(v)
  des_srvyr |>
    srvyr::filter(!is.na(phone3), !is.na(!!s)) |>
    srvyr::group_by(phone3, !!s) |>
    srvyr::summarise(p = srvyr::survey_mean(vartype = "ci", proportion = TRUE),
                     .groups = "drop") |>
    dplyr::transmute(variavel = v, nivel = as.character(!!s),
                     phone3 = as.character(phone3),
                     cell = fmt_ci(p * 100, p_low * 100, p_upp * 100, 1))
}) |>
  tidyr::pivot_wider(names_from = phone3, values_from = cell)

rotulos_perfil <- c(sex = "Sexo", age_grp = "Faixa etária, anos",
                    education = "Escolaridade, anos de estudo",
                    race = "Raça/cor", region = "Região")

perfil <- perfil |>
  dplyr::mutate(grupo = factor(rotulos_perfil[variavel], levels = unname(rotulos_perfil))) |>
  dplyr::arrange(grupo) |>
  dplyr::select(grupo, nivel, dplyr::all_of(PHONE_LEVELS))
print(as.data.frame(perfil))

gtS2 <- perfil |>
  gt::gt(groupname_col = "grupo", rowname_col = "nivel") |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela S2.** Perfil sociodemográfico segundo a posse de telefone no domicílio, ",
    "PNS {YEAR_PNS}, adultos das 27 capitais"
  ))) |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = dplyr::all_of(PHONE_LEVELS)) |>
  gt::tab_footnote(
    footnote = paste(
      "Distribuição percentual dentro de cada grupo de posse, com intervalo de",
      "confiança de 95%. As colunas somam 100% dentro de cada bloco de variável.",
      "O grupo 'Somente celular' é o que um quadro amostral dual-frame passaria a",
      "alcançar, e por isso o seu perfil é o parâmetro central da simulação."
    ),
    locations = gt::cells_column_labels(columns = "Somente celular")
  ) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Fonte: PNS {YEAR_PNS} (IBGE). Posse de telefone das variáveis A018017 (fixo) e ",
    "A018019 (celular) do módulo de características do domicílio."
  ))) |>
  gt_study_style()

save_table(gtS2, "tableS2")
saveRDS(perfil, here::here("output", "tables", "tableS2_dados.rds"))

# ---- Tabela suplementar S3: por regiao, com os tres grupos ------------------

gtS3 <- por_grupo |>
  dplyr::mutate(cell = fmt_ci(est, low, upp, 1),
                label = factor(label, levels = INDICATORS$label_pt)) |>
  dplyr::select(label, phone3, cell) |>
  tidyr::pivot_wider(names_from = phone3, values_from = cell) |>
  dplyr::arrange(label) |>
  gt::gt(rowname_col = "label") |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela S3.** Prevalência dos indicadores segundo a posse de telefone no domicílio, ",
    "PNS {YEAR_PNS}"
  ))) |>
  gt::cols_align("center", columns = dplyr::all_of(PHONE_LEVELS)) |>
  gt::tab_footnote(
    footnote = paste(
      "A separação entre 'Somente celular' e 'Nenhum' importa porque os dois grupos",
      "compõem a população sem telefone fixo, mas apenas o primeiro é alcançável por",
      "um quadro amostral que incorpore telefonia móvel."
    ),
    locations = gt::cells_column_labels(columns = "Nenhum")
  ) |>
  gt::tab_source_note(gt::md(glue::glue("Fonte: PNS {YEAR_PNS} (IBGE), adultos das 27 capitais."))) |>
  gt_study_style()

save_table(gtS3, "tableS3")

message("06_noncoverage.R concluido.")
