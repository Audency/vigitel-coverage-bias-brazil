# =============================================================================
# 08_simulacao.R
# O QUE FAZ : Simulacao de cenarios de quadro amostral sob ADEMP, com posse de
#             telefone OBSERVADA (nao modelada) e estratificacao regional.
# ENTRADAS  : data/derivado/{pns,vigitel}.rds, data/derivado/pnadc_tic.rds
# SAIDAS    : data/derivado/simulacao.rds, output/logs/mecanismo_*.csv
# =============================================================================
#
# AIMS
#   Quantificar como a escolha do quadro amostral afeta o vies, a precisao e o
#   erro quadratico medio das estimativas de prevalencia de quatro indicadores de
#   DCNT em adultos das 27 capitais, e verificar se o ganho de um quadro
#   dual-frame depende da cobertura de telefonia fixa da regiao.
#
# DATA-GENERATING MECHANISM
#   Pseudopopulacao = PNS 2019 de capitais expandida pelos pesos. Operacionalmente
#   a expansao e implicita: amostramos registros da PNS com reposicao e com
#   probabilidade proporcional ao peso do morador selecionado, o que e equivalente
#   a amostrar da populacao expandida e evita materializar 39 milhoes de linhas.
#
#   A posse de telefone NAO e simulada: usamos a posse observada na PNS. Isso e
#   deliberado e mais forte do que um modelo. O protocolo exige que a posse
#   dependa de escolaridade, faixa etaria, regiao e do proprio desfecho - com a
#   posse observada, essa dependencia e a real, com a forca que ela tem nos dados,
#   e nao a que um modelo lhe atribuiria. O script verifica e imprime o
#   coeficiente do desfecho num modelo de posse, como o protocolo determina: se
#   ele fosse nulo, o vies seria zero por construcao e a simulacao nao informaria
#   nada.
#
#   A cobertura dos quadros e conferida contra a PNAD Continua TIC do mesmo ano.
#
# ESTIMANDS
#   Prevalencia populacional de cada indicador, em cada regiao - calculada na
#   propria pseudopopulacao, portanto conhecida sem erro.
#
# METHODS
#   Cinco cenarios de quadro amostral (S0 a S4). Em todos, a amostra e
#   pos-estratificada por idade x sexo x escolaridade contra os totais da
#   pseudopopulacao, reproduzindo a logica da calibragem rake do Vigitel: sem
#   isso o cenario S0 exibiria o vies de nao cobertura bruto, que nao e o que um
#   inquerito real publica. Quadros multiplos sao combinados por peso de
#   multiplicidade (cada unidade dividida pelo numero de quadros a que pertence),
#   com a escolha do estimador confrontada com os estimadores de Frames2.
#
# PERFORMANCE MEASURES
#   Vies, erro-padrao empirico e REQM (desfecho principal), cada um com o seu
#   erro de Monte Carlo. Estratificacao por regiao e obrigatoria, nao opcional.
#
# =============================================================================

source(here::here("R", "00_setup.R"))
dir.create(here::here("data", "derivado"), showWarnings = FALSE, recursive = TRUE)
library(Frames2)

h1("08_simulation.R - simulacao ADEMP")

pop <- readRDS(here::here("data", "derivado", "pns.rds"))
OUTCOMES <- INDICADORES |> dplyr::mutate(var = paste0(indicador, "_official"))

# Registros com covariaveis completas: a pos-estratificacao precisa delas
pop <- pop |>
  dplyr::filter(!is.na(age_grp), !is.na(sex), !is.na(education),
                !is.na(has_landline), !is.na(has_mobile), !is.na(has_internet))
cat("pseudopopulacao (registros da PNS de capitais):", nrow(pop),
    "| populacao representada:", format(round(sum(pop$weight)), big.mark = "."), "\n")

# =============================================================================
# 1. VERIFICACAO DO MECANISMO GERADOR
# =============================================================================
# Ponto critico do protocolo: se a posse de telefone for independente do
# desfecho, o vies e zero por construcao. Ajustamos o modelo de posse com
# escolaridade, faixa etaria, regiao E o desfecho, e imprimimos o coeficiente do
# desfecho. Ele precisa ser diferente de zero para a simulacao ter conteudo.

h2("Mecanismo gerador: a posse de telefone depende do desfecho?")

# O modelo roda sobre o desenho amostral, com svyglm, e nao com glm(weights=).
# Passar peso de expansao como `weights` do glm faz o ajuste tratar cada peso
# como numero de ensaios binomiais: a amostra efetiva vira a populacao inteira,
# o algoritmo diverge e os coeficientes saem em ordem de 10^14, com p = 0
# espurio. O svyglm usa o peso para estimacao e a estrutura do desenho para a
# variancia, que e o correto aqui.
des_pop <- pop |>
  srvyr::as_survey_design(strata = strata, ids = psu, weights = weight, nest = TRUE)

coef_desfecho <- purrr::pmap_dfr(
  list(OUTCOMES$var, OUTCOMES$rotulo),
  function(var, rot) {
    f <- stats::as.formula(paste("has_landline ~ education + age_grp + region +", var))
    m <- survey::svyglm(f, design = des_pop, family = stats::quasibinomial())
    s <- summary(m)$coefficients
    linha <- grep(paste0("^", var), rownames(s))
    tibble::tibble(
      indicador = rot,
      beta_desfecho = s[linha, 1],
      or            = exp(s[linha, 1]),
      ic_low        = exp(s[linha, 1] - stats::qnorm(0.975) * s[linha, 2]),
      ic_upp        = exp(s[linha, 1] + stats::qnorm(0.975) * s[linha, 2]),
      p             = s[linha, 4]
    )
  }
)
print(as.data.frame(coef_desfecho), digits = 3)
grava_log(coef_desfecho, "mecanismo_posse_desfecho")

# Leitura correta deste quadro, que nao e a leitura ingenua:
#
# O coeficiente acima e a associacao RESIDUAL entre desfecho e posse de telefone,
# depois de descontadas escolaridade, faixa etaria e regiao - que sao justamente
# as variaveis de calibragem do Vigitel. Ele NAO e o vies de nao cobertura, que e
# marginal e esta na Tabela 3.
#
#   residual != 0  -> parte do vies sobrevive a pos-estratificacao, porque a
#                     diferenca entre quem tem e quem nao tem fixo nao se explica
#                     pelas variaveis de calibragem;
#   residual ~ 0   -> o vies e inteiramente de composicao, e a pos-estratificacao
#                     tende a remove-lo. O vies marginal continua grande; o que a
#                     calibragem alcanca e que ele nao chega a estimativa final.
#
# A simulacao continua informativa nos dois casos: e exatamente essa diferenca
# que os cenarios S0 a S4 medem.

com_residual <- coef_desfecho |> dplyr::filter(p < 0.05)
sem_residual <- coef_desfecho |> dplyr::filter(p >= 0.05)

cat("\nAssociacao RESIDUAL entre desfecho e posse de telefone fixo,\n",
    "apos ajuste por escolaridade, faixa etaria e regiao:\n\n", sep = "")
if (nrow(com_residual) > 0) {
  cat("  COM associacao residual (o vies nao e removivel so por calibragem):\n")
  for (i in seq_len(nrow(com_residual))) {
    cat("    - ", com_residual$indicador[i], ": OR = ",
        fmt_num(com_residual$or[i], 2), " (p = ", fmt_p(com_residual$p[i]), ")\n", sep = "")
  }
}
if (nrow(sem_residual) > 0) {
  cat("  SEM associacao residual (vies de composicao, alcancavel pela calibragem):\n")
  for (i in seq_len(nrow(sem_residual))) {
    cat("    - ", sem_residual$indicador[i], ": OR = ",
        fmt_num(sem_residual$or[i], 2), " (p = ", fmt_p(sem_residual$p[i]), ")\n", sep = "")
  }
}
if (nrow(com_residual) == 0) {
  warning("Nenhum desfecho tem associacao residual com a posse. Neste caso a ",
          "pos-estratificacao removeria todo o vies e os cenarios tenderiam a ",
          "coincidir - conferir antes de interpretar.", call. = FALSE)
}

# =============================================================================
# 2. COBERTURA DOS QUADROS E CONFERENCIA COM A PNAD-TIC
# =============================================================================

h2("Cobertura dos quadros amostrais")

FRAMES <- list(
  fixo     = pop$has_landline == 1,
  celular  = pop$has_mobile == 1,
  web      = pop$has_internet == 1,
  presencial = rep(TRUE, nrow(pop))          # face a face alcanca todos
)

cobertura <- tibble::tibble(
  quadro = names(FRAMES),
  cobertura_pct = purrr::map_dbl(FRAMES, ~ 100 * sum(pop$weight[.x]) / sum(pop$weight))
)
print(as.data.frame(cobertura), digits = 3)

cobertura_regiao <- purrr::map_dfr(names(FRAMES), function(q) {
  pop |>
    dplyr::mutate(dentro = FRAMES[[q]]) |>
    dplyr::group_by(region) |>
    dplyr::summarise(quadro = q,
                     cobertura_pct = 100 * sum(weight * dentro) / sum(weight),
                     .groups = "drop")
}) |>
  tidyr::pivot_wider(names_from = quadro, values_from = cobertura_pct)
print(as.data.frame(cobertura_regiao), digits = 3)
grava_log(cobertura_regiao, "cobertura_quadros_regiao")

# Conferencia com a PNAD Continua TIC do mesmo ano ------------------------------
# S01022 = "Este domicilio tem telefone fixo convencional?"
# S01021 = numero de moradores com telefone movel celular para uso pessoal
tic_path <- here::here("data/raw", "pnadc", glue::glue("pnadc{ANO_PNADC}_tic.rds"))
if (file.exists(tic_path)) {
  tic <- readRDS(tic_path)
  uf_region <- pop |> dplyr::distinct(uf_name, region)
  tic_cap <- tic |>
    dplyr::mutate(
      peso   = as.numeric(V1028),
      idade  = as.numeric(V2009),
      area   = as.character(V1023),
      fixo   = dplyr::case_when(S01022 == "1" ~ 1, S01022 == "2" ~ 0, TRUE ~ NA_real_),
      cel    = dplyr::case_when(is.na(S01021) ~ NA_real_,
                                as.numeric(S01021) > 0 ~ 1, TRUE ~ 0)
    ) |>
    # V1023 == "1" e a capital, mesmo criterio de recorte usado na PNS
    dplyr::filter(area == "1", !is.na(idade), idade >= 18, !is.na(peso), peso > 0)

  comp_tic <- tibble::tibble(
    fonte = c("PNS (capitais)", "PNAD-C TIC (capitais)"),
    fixo_pct = c(100 * sum(pop$weight * pop$has_landline) / sum(pop$weight),
                 100 * sum(tic_cap$peso * tic_cap$fixo, na.rm = TRUE) /
                   sum(tic_cap$peso[!is.na(tic_cap$fixo)])),
    celular_pct = c(100 * sum(pop$weight * pop$has_mobile) / sum(pop$weight),
                    100 * sum(tic_cap$peso * tic_cap$cel, na.rm = TRUE) /
                      sum(tic_cap$peso[!is.na(tic_cap$cel)]))
  )
  cat("\nConferencia da cobertura entre as duas fontes do mesmo ano:\n")
  print(as.data.frame(comp_tic), digits = 3)
  grava_log(comp_tic, "conferencia_cobertura_pns_tic")
} else {
  message("PNAD-TIC ausente; conferencia de cobertura nao realizada.")
}

# =============================================================================
# 3. CENARIOS
# =============================================================================

CENARIOS <- list(
  S0 = list(rotulo = "S0 fixo apenas",          quadros = "fixo"),
  S1 = list(rotulo = "S1 celular apenas",       quadros = "celular"),
  S2 = list(rotulo = "S2 dual-frame",           quadros = c("fixo", "celular")),
  S3 = list(rotulo = "S3 triplo-frame (+web)",  quadros = c("fixo", "celular", "web")),
  S4 = list(rotulo = "S4 multimodal integral",  quadros = c("fixo", "celular", "web", "presencial"))
)

# S1 nao e subconjunto de S0: tem perfil de nao cobertura proprio, e vies de
# sinal oposto e resultado legitimo, nao erro.

# =============================================================================
# 4. INFRAESTRUTURA DA SIMULACAO
# =============================================================================

# Celulas de pos-estratificacao: idade x sexo x escolaridade
pop$cell <- as.integer(interaction(pop$age_grp, pop$sex, pop$education, drop = TRUE))
N_CELLS <- max(pop$cell)

# Tamanhos de amostra por regiao: os que o Vigitel efetivamente realizou, para
# que o resultado responda "com o n que o inquerito tem hoje", e nao com um n
# hipotetico.
vig <- readRDS(here::here("data", "derivado", "vigitel.rds"))
n_por_regiao <- vig |>
  dplyr::count(region, name = "n_amostra") |>
  dplyr::mutate(region = as.character(region))
print(as.data.frame(n_por_regiao))

y_mat <- as.matrix(pop[, OUTCOMES$var])
storage.mode(y_mat) <- "double"

#' Estimativa pos-estratificada a partir de indices amostrados
#' @param idx indices (na pseudopopulacao) das unidades sorteadas
#' @param base_w peso de multiplicidade de cada unidade sorteada
#' @param cell_tot totais populacionais por celula, na regiao
estimar <- function(idx, base_w, cell_tot) {
  cl <- pop$cell[idx]
  # soma dos pesos-base por celula na amostra
  wsum <- as.vector(tapply(base_w, factor(cl, levels = seq_len(N_CELLS)), sum))
  wsum[is.na(wsum)] <- 0
  fator <- ifelse(wsum > 0, cell_tot / wsum, 0)
  w <- base_w * fator[cl]
  # Celulas sem nenhuma unidade amostrada perdem sua massa populacional; o
  # reescalonamento devolve o total, que e o que a calibragem faria.
  if (sum(w) == 0) return(rep(NA_real_, ncol(y_mat)))
  w <- w * (sum(cell_tot) / sum(w))
  colSums(y_mat[idx, , drop = FALSE] * w) / sum(w) * 100
}

# =============================================================================
# 5. EXECUCAO
# =============================================================================

h2("Rodando ", N_REPLICAS, " replicas por cenario x regiao")

set.seed(20260803)
resultados <- list()
t0 <- Sys.time()

for (reg in REGIOES) {
  in_reg   <- which(pop$region == reg)
  w_reg    <- pop$weight[in_reg]
  cell_tot <- as.vector(tapply(w_reg, factor(pop$cell[in_reg], levels = seq_len(N_CELLS)), sum))
  cell_tot[is.na(cell_tot)] <- 0

  # Valor verdadeiro: prevalencia na pseudopopulacao da regiao
  verdadeiro <- colSums(y_mat[in_reg, , drop = FALSE] * w_reg) / sum(w_reg) * 100

  n_reg <- n_por_regiao$n_amostra[n_por_regiao$region == reg]

  for (cen in names(CENARIOS)) {
    quadros <- CENARIOS[[cen]]$quadros
    # multiplicidade: em quantos quadros do cenario a unidade esta
    mult <- rowSums(vapply(quadros, function(q) as.numeric(FRAMES[[q]][in_reg]),
                           numeric(length(in_reg))))
    elegivel <- which(mult > 0)
    if (length(elegivel) == 0) next

    # Probabilidade de sorteio proporcional ao peso; peso-base corrigido pela
    # multiplicidade, que e o estimador de multiplicidade para quadros multiplos.
    prob_sorteio <- w_reg[elegivel] * mult[elegivel]
    peso_base_un <- w_reg[elegivel] / mult[elegivel]
    idx_pop      <- in_reg[elegivel]

    est <- matrix(NA_real_, nrow = N_REPLICAS, ncol = ncol(y_mat))
    for (r in seq_len(N_REPLICAS)) {
      s <- sample.int(length(elegivel), size = n_reg, replace = TRUE, prob = prob_sorteio)
      est[r, ] <- estimar(idx_pop[s], peso_base_un[s], cell_tot)
    }

    cobertura_cen <- 100 * sum(w_reg[elegivel]) / sum(w_reg)

    for (k in seq_len(ncol(y_mat))) {
      e <- est[, k]
      e <- e[is.finite(e)]
      R <- length(e)
      vies   <- mean(e) - verdadeiro[k]
      empse  <- stats::sd(e)
      reqm   <- sqrt(mean((e - verdadeiro[k])^2))
      resultados[[length(resultados) + 1]] <- tibble::tibble(
        region = reg, cenario = cen, cenario_rotulo = CENARIOS[[cen]]$rotulo,
        indicador = OUTCOMES$indicador[k], rotulo = OUTCOMES$rotulo[k],
        cobertura = cobertura_cen,
        verdadeiro = verdadeiro[k],
        media_est = mean(e),
        vies = vies, empse = empse, rmse = reqm,
        mcse_vies = empse / sqrt(R),
        mcse_empse = empse / sqrt(2 * (R - 1)),
        mcse_rmse  = sqrt(stats::var((e - verdadeiro[k])^2) / (4 * R * reqm^2)),
        n_replicas = R, n_amostra = n_reg
      )
    }
  }
  cat("  ", reg, "ok (", round(difftime(Sys.time(), t0, units = "mins"), 1), "min )\n")
}

sim <- dplyr::bind_rows(resultados) |>
  dplyr::mutate(
    region  = factor(region, levels = REGIOES),
    cenario = factor(cenario, levels = names(CENARIOS)),
    rotulo  = factor(rotulo, levels = INDICADORES$rotulo)
  ) |>
  dplyr::arrange(rotulo, region, cenario)

cat("\ntempo total:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

# Nenhum cenario deve aparecer com erro-padrao zero: isso seria censo, nao
# resultado. Conferimos explicitamente.
if (any(sim$empse <= 0, na.rm = TRUE)) {
  stop("Cenario com erro-padrao empirico zero: e censo da pseudopopulacao, ",
       "nao resultado. Revise o desenho antes de reportar.", call. = FALSE)
}
cat("OK: nenhum cenario com erro-padrao empirico zero.\n")

h2("Resumo: REQM por cenario (media entre indicadores)")
print(as.data.frame(
  sim |> dplyr::group_by(region, cenario) |>
    dplyr::summarise(cobertura = mean(cobertura), rmse = mean(rmse),
                     vies = mean(abs(vies)), .groups = "drop")
), digits = 3)

saveRDS(sim, here::here("data", "derivado", "simulacao.rds"))
grava_log(sim, "simulacao_completa")

# =============================================================================
# 6. JUSTIFICATIVA DO ESTIMADOR DUAL-FRAME (Frames2)
# =============================================================================
# O protocolo pede que a escolha do estimador de quadros multiplos seja
# confrontada com a funcao de comparacao do Frames2. Rodamos uma replica do
# cenario S2 e comparamos os estimadores disponiveis. A comparacao roda uma vez,
# nao dentro do laco: seu papel e justificar a escolha, nao produzir estimativa.

h2("Comparacao de estimadores dual-frame (Frames2::Compare)")

comparacao <- tryCatch({
  reg <- "Sudeste"
  in_reg <- which(pop$region == reg)
  A <- in_reg[pop$has_landline[in_reg] == 1]      # quadro fixo
  B <- in_reg[pop$has_mobile[in_reg] == 1]        # quadro celular
  nA <- 1000; nB <- 1000
  sA <- sample(A, nA); sB <- sample(B, nB)
  yvar <- OUTCOMES$var[1]

  # Probabilidades de inclusao (amostragem proporcional ao peso, com reposicao)
  piA <- rep(nA / length(A), nA)
  piB <- rep(nB / length(B), nB)
  # Dominio de cada unidade: "a"/"ab" no quadro A, "b"/"ba" no quadro B
  domA <- ifelse(pop$has_mobile[sA] == 1, "ab", "a")
  domB <- ifelse(pop$has_landline[sB] == 1, "ba", "b")

  out <- Frames2::Compare(
    ysA = matrix(pop[[yvar]][sA], ncol = 1),
    ysB = matrix(pop[[yvar]][sB], ncol = 1),
    pi_A = piA, pi_B = piB,
    domains_A = domA, domains_B = domB,
    N_A = length(A), N_B = length(B),
    N_ab = length(intersect(A, B))
  )
  print(out)
  out
}, error = function(e) {
  message("Frames2::Compare nao pode ser executado com esta parametrizacao: ",
          conditionMessage(e))
  message("O estimador adotado na simulacao e o de multiplicidade, que atribui a ",
          "cada unidade peso proporcional ao inverso do numero de quadros a que ",
          "pertence. E o estimador de referencia quando a pertinencia a cada ",
          "quadro e observada para toda unidade sorteada, que e o caso aqui: a ",
          "PNS informa posse de fixo, celular e internet para o mesmo registro.")
  NULL
})

message("08_simulacao.R concluido.")
