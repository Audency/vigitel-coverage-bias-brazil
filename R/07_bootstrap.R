# =============================================================================
# 07_bootstrap.R
# O QUE FAZ : Incerteza do vies de nao cobertura e da particao, por bootstrap de
#             Rao-Wu (subbootstrap) sobre o desenho da PNS. Calcula tambem o
#             vicio relativo de Cochran.
# ENTRADAS  : data/derivado/{pns,particao_pontual,prevalencias}.rds
# SAIDAS    : data/derivado/{vies_ncob,particao}.rds,
#             output/logs/vies_nao_cobertura.csv
#
# Por que bootstrap e nao formula fechada: o vies e um produto de estimativas
# correlacionadas (f_semfixo, p_fixo, p_semfixo) dentro do mesmo desenho
# complexo; a linearizacao exigiria a derivada de um produto de razoes sob
# estratificacao e conglomeracao. O bootstrap de Rao-Wu preserva a estrutura.
#
# O recorte de capitais e composto de estratos completos - nenhum estrato da PNS
# mistura capital e nao-capital -, entao construir as replicas sobre o
# subconjunto e exato, e nao uma aproximacao.
# =============================================================================

source(here::here("R", "00_setup.R"))
dir.create(here::here("data", "derivado"), showWarnings = FALSE, recursive = TRUE)

h1("07_bootstrap.R - Rao-Wu, ", N_BOOT, " replicas")

pns <- readRDS(here::here("data", "derivado", "pns.rds"))
pp  <- readRDS(here::here("data", "derivado", "particao_pontual.rds"))
prevalencias <- readRDS(here::here("data", "derivado", "prevalencias.rds"))
DESFECHOS <- INDICADORES |> dplyr::mutate(var = paste0(indicador, "_official"))
DOMINIOS <- pp$dominios

# Conferencia do pressuposto do recorte
mistura <- pns |> dplyr::distinct(strata, area_type) |> dplyr::count(strata) |>
  dplyr::filter(n > 1) |> nrow()
cat("estratos que misturam capital e nao-capital:", mistura, "\n")
stopifnot(mistura == 0)

des <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~weight,
                         data = pns, nest = TRUE)
set.seed(SEED)
t0 <- Sys.time()
rep_des <- survey::as.svrepdesign(des, type = "subbootstrap", replicates = N_BOOT)
cat("replicas geradas em", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

# ---- Estatistica vetorial ---------------------------------------------------
# Uma unica chamada por indicador devolve todas as quantidades em todos os
# dominios, preservando a covariancia entre elas.
theta <- function(w, dados, yvar) {
  r <- pp$funcao(w, dados, yvar)
  out <- c(rbind(r$p_fixo, r$p_semfixo, r$f_semfixo, r$vies, r$p_pns, r$alvo))
  names(out) <- paste0(rep(c("p_fixo", "p_semfixo", "f_semfixo", "vies", "p_pns", "alvo"),
                           times = nrow(r)),
                       ".", rep(r$dominio, each = 6))
  out
}

h2("Estimando por indicador")
resultados <- purrr::pmap_dfr(
  list(DESFECHOS$var, DESFECHOS$indicador, DESFECHOS$rotulo),
  function(var, ind, rot) {
    cat("  ", rot, "... ")
    r <- survey::withReplicates(rep_des, function(w, data) theta(w, data, var))
    nm <- names(coef(r))
    cat("ok\n")
    tibble::tibble(indicador = ind, rotulo = rot,
                   quantidade = sub("\\..*$", "", nm),
                   dominio = sub("^[^.]*\\.", "", nm),
                   est = as.numeric(coef(r)), se = as.numeric(survey::SE(r)))
  })

# ---- Montagem com IC e vicio de Cochran ------------------------------------
se_vig <- prevalencias |>
  dplyr::filter(inquerito == "Vigitel", estrato == "Total") |>
  dplyr::select(indicador, se_vig = se)

ic <- function(e, s) list(low = e - stats::qnorm(.975) * s, upp = e + stats::qnorm(.975) * s)

vies_ncob <- resultados |>
  tidyr::pivot_wider(names_from = quantidade, values_from = c(est, se)) |>
  dplyr::left_join(se_vig, by = "indicador") |>
  dplyr::mutate(
    vies_low = ic(est_vies, se_vies)$low,      vies_upp = ic(est_vies, se_vies)$upp,
    p_fixo_low = ic(est_p_fixo, se_p_fixo)$low, p_fixo_upp = ic(est_p_fixo, se_p_fixo)$upp,
    p_sem_low = ic(est_p_semfixo, se_p_semfixo)$low, p_sem_upp = ic(est_p_semfixo, se_p_semfixo)$upp,
    f_low = ic(est_f_semfixo, se_f_semfixo)$low, f_upp = ic(est_f_semfixo, se_f_semfixo)$upp,
    # Duas leituras do criterio de Cochran, porque respondem a perguntas
    # diferentes: com o erro-padrao interno da PNS (leitura literal, autocontida)
    # e com o que o Vigitel efetivamente tem (leitura operacional - diz se o vies
    # ameaca a cobertura nominal dos intervalos que o Vigitel publica).
    cochran_pns = abs(est_vies) / se_p_fixo,
    cochran_vigitel = abs(est_vies) / se_vig,
    acima_limiar = cochran_vigitel >= LIMIAR_COCHRAN,
    dominio = factor(dominio, levels = DOMINIOS),
    rotulo = factor(rotulo, levels = INDICADORES$rotulo)
  ) |>
  dplyr::arrange(rotulo, dominio)

h2("Vies de nao cobertura com IC bootstrap")
print(as.data.frame(vies_ncob |> dplyr::filter(dominio == "27 capitais") |>
                      dplyr::select(rotulo, est_p_fixo, est_p_semfixo, est_f_semfixo,
                                    est_vies, vies_low, vies_upp, cochran_vigitel)),
      digits = 3)
cat("\ncelulas acima do limiar de Cochran (", fmt_num(LIMIAR_COCHRAN, 2), "): ",
    sum(vies_ncob$acima_limiar), " de ", nrow(vies_ncob), "\n", sep = "")
cat("faixa do vicio relativo: ", fmt_num(min(vies_ncob$cochran_vigitel), 2), " a ",
    fmt_num(max(vies_ncob$cochran_vigitel), 2), " (EP do Vigitel); ",
    fmt_num(min(vies_ncob$cochran_pns), 2), " a ", fmt_num(max(vies_ncob$cochran_pns), 2),
    " (EP da PNS)\n", sep = "")
grava_log(vies_ncob, "vies_nao_cobertura")

# ---- Variancia da particao --------------------------------------------------
# residuo = p_vigitel - alvo. O alvo e quantidade so da PNS, obtida no MESMO
# bootstrap, de modo que sua variancia ja incorpora a correlacao entre p_pns e
# vies. O Vigitel e amostra independente, entao as variancias se somam.
h2("Particao com IC")

alvo27 <- vies_ncob |> dplyr::filter(dominio == "27 capitais") |>
  dplyr::select(indicador, se_alvo, se_vies_b = se_vies, se_p_pns)

particao <- pp$particao |>
  dplyr::left_join(alvo27, by = "indicador") |>
  dplyr::mutate(
    se_residuo = sqrt(se_vig^2 + se_alvo^2),
    residuo_low = ic(residuo, se_residuo)$low, residuo_upp = ic(residuo, se_residuo)$upp,
    comp_low = ic(componente, se_vies_b)$low,  comp_upp = ic(componente, se_vies_b)$upp,
    # IC do gap observado: Vigitel e PNS sao amostras independentes
    se_gap = sqrt(se_vig^2 + se_p_pns^2),
    gap_low = ic(gap_total, se_gap)$low, gap_upp = ic(gap_total, se_gap)$upp
  )
print(as.data.frame(particao |> dplyr::select(rotulo, gap_total, componente, comp_low,
                                              comp_upp, residuo, residuo_low, residuo_upp,
                                              pct_removido)), digits = 3)

particao_regiao <- pp$particao_regiao |>
  dplyr::left_join(vies_ncob |> dplyr::select(indicador, dominio, se_vies, se_alvo),
                   by = c("indicador", "dominio")) |>
  dplyr::mutate(se_residuo = sqrt(se_vig^2 + se_alvo^2),
                residuo_low = ic(residuo, se_residuo)$low,
                residuo_upp = ic(residuo, se_residuo)$upp,
                comp_low = ic(componente, se_vies)$low,
                comp_upp = ic(componente, se_vies)$upp)

saveRDS(vies_ncob, here::here("data", "derivado", "vies_ncob.rds"))
saveRDS(list(particao = particao, particao_regiao = particao_regiao),
        here::here("data", "derivado", "particao.rds"))

message("07_bootstrap.R concluido.")
