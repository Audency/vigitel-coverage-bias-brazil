# =============================================================================
# 06_particao.R
# O QUE FAZ : Estimativas pontuais do vies de nao cobertura da telefonia fixa e
#             da particao da diferenca entre os inqueritos. Toda a estimacao do
#             vies acontece DENTRO da PNS: nenhuma quantidade depende do Vigitel.
#             A incerteza vem do bootstrap em 07_bootstrap.R.
# ENTRADAS  : data/derivado/{pns,prevalencias}.rds
# SAIDAS    : data/derivado/particao_pontual.rds
#
#   p_fixo    = prevalencia entre quem tem telefone fixo
#   p_semfixo = prevalencia entre quem nao tem fixo (so celular + nenhum)
#   f_semfixo = proporcao sem telefone fixo
#   vies      = f_semfixo * (p_fixo - p_semfixo)
#   alvo      = p_pns + vies, que e algebricamente igual a p_fixo: e o valor que
#               um quadro restrito a telefonia fixa produziria sem ponderacao
#   gap       = p_vigitel - p_pns
#   residuo   = gap - vies = p_vigitel - alvo
#
# NOMENCLATURA OBRIGATORIA: o residuo chama-se residuo, ou componente nao
# atribuivel a nao cobertura. NUNCA "efeito de modo": ele absorve tambem
# autorrelato, instrumento, nao resposta e calibragem, que este desenho nao
# separa.
# =============================================================================

source(here::here("R", "00_setup.R"))
dir.create(here::here("data", "derivado"), showWarnings = FALSE, recursive = TRUE)

h1("06_particao.R")

pns <- readRDS(here::here("data", "derivado", "pns.rds"))
prevalencias <- readRDS(here::here("data", "derivado", "prevalencias.rds"))
DESFECHOS <- INDICADORES |> dplyr::mutate(var = paste0(indicador, "_official"))

DOMINIOS <- c("27 capitais", REGIOES)

cat("PNS capitais:", nrow(pns), "| estratos:", dplyr::n_distinct(pns$strata),
    "| UPAs:", dplyr::n_distinct(pns$psu), "\n")

# ---- Nucleo do calculo ------------------------------------------------------
# Mesma funcao usada pelo bootstrap em 07, para que ponto e variancia venham da
# mesma definicao. Recebe pesos como argumento justamente para isso.
componentes_ncob <- function(w, dados, yvar) {
  y <- dados[[yvar]]; L <- dados$has_landline; reg <- as.character(dados$region)
  purrr::map_dfr(DOMINIOS, function(dom) {
    sel <- if (dom == "27 capitais") rep(TRUE, nrow(dados)) else reg == dom
    ok <- sel & !is.na(y) & !is.na(L)
    ww <- w[ok]; yy <- y[ok]; ll <- L[ok]
    w_fix <- sum(ww * ll); w_sem <- sum(ww * (1 - ll))
    p_fix <- sum(ww * yy * ll) / w_fix
    p_sem <- sum(ww * yy * (1 - ll)) / w_sem
    f_sem <- w_sem / (w_fix + w_sem)
    p_pop <- sum(ww * yy) / sum(ww)
    vi    <- f_sem * (p_fix - p_sem)
    al    <- p_pop + vi          # identidade: igual a p_fix
    # Nomes distintos dos das colunas: dentro de tibble() as colunas sao
    # avaliadas em sequencia, e reutilizar o nome faria a expressao seguinte
    # usar o valor ja multiplicado por 100.
    tibble::tibble(dominio = dom,
                   p_fixo = p_fix * 100, p_semfixo = p_sem * 100,
                   f_semfixo = f_sem * 100, p_pns = p_pop * 100,
                   vies = vi * 100, alvo = al * 100)
  })
}

h2("Vies de nao cobertura, estimativas pontuais")

pontual <- purrr::pmap_dfr(
  list(DESFECHOS$var, DESFECHOS$indicador, DESFECHOS$rotulo),
  function(var, ind, rot)
    componentes_ncob(pns$weight, pns, var) |>
      dplyr::mutate(indicador = ind, rotulo = rot, .before = 1)
)
print(as.data.frame(pontual |> dplyr::filter(dominio == "27 capitais")), digits = 4)

# Conferencia algebrica: alvo tem de ser identico a p_fixo
dif_alg <- max(abs(pontual$alvo - pontual$p_fixo))
cat("\nConferencia alvo == p_fixo (identidade algebrica): maior desvio",
    format(dif_alg, digits = 3), "\n")
stopifnot(dif_alg < 1e-9)

# ---- Particao ---------------------------------------------------------------
h2("Particao da diferenca")

vig_total <- prevalencias |>
  dplyr::filter(inquerito == "Vigitel", estrato == "Total") |>
  dplyr::select(indicador, p_vig = est, se_vig = se)

particao <- pontual |>
  dplyr::filter(dominio == "27 capitais") |>
  dplyr::left_join(vig_total, by = "indicador") |>
  dplyr::mutate(
    gap_total  = p_vig - p_pns,
    componente = vies,
    residuo    = p_vig - alvo,
    checagem   = componente + residuo - gap_total,
    pct_explicado = 100 * componente / gap_total,
    situacao = dplyr::case_when(
      sign(componente) != sign(gap_total) ~ "sinal oposto ao gap",
      abs(componente) > abs(gap_total)    ~ "excede o gap observado",
      TRUE ~ "fracao do gap"),
    pct_txt = dplyr::if_else(situacao == "fracao do gap",
                             paste0(fmt_num(pct_explicado, 0), "%"), situacao),
    # Quanto do vies de cobertura a pos-estratificacao do Vigitel remove.
    # Sem ponderacao, um quadro restrito a telefonia fixa erraria por `vies`
    # (= alvo - p_pns). O que sobrevive na estimativa publicada e o proprio gap
    # (= p_vig - p_pns). A fracao removida e, portanto, 1 - |gap| / |vies|.
    pct_removido = 100 * (1 - abs(gap_total) / abs(vies))
  )

print(as.data.frame(particao |> dplyr::select(rotulo, gap_total, componente, residuo,
                                              situacao, pct_removido)), digits = 4)
cat("\nMaior residuo da soma (componente + residuo - gap):",
    format(max(abs(particao$checagem)), digits = 3), "\n")
stopifnot(max(abs(particao$checagem)) < 1e-9)

# ---- Particao por regiao ----------------------------------------------------
h2("Particao por regiao")

des_vig <- readRDS(here::here("data", "derivado", "design_vigitel.rds"))
vig_regiao <- purrr::pmap_dfr(list(DESFECHOS$var, DESFECHOS$indicador), function(var, ind) {
  v <- rlang::sym(var)
  des_vig |>
    srvyr::filter(!is.na(!!v)) |>
    srvyr::group_by(region) |>
    srvyr::summarise(p = srvyr::survey_mean(!!v, vartype = "se", proportion = TRUE, na.rm = TRUE)) |>
    dplyr::transmute(indicador = ind, dominio = as.character(region),
                     p_vig = p * 100, se_vig = p_se * 100)
})

particao_regiao <- pontual |>
  dplyr::filter(dominio != "27 capitais") |>
  dplyr::left_join(vig_regiao, by = c("indicador", "dominio")) |>
  dplyr::mutate(gap_total = p_vig - p_pns, componente = vies, residuo = p_vig - alvo,
                dominio = factor(dominio, levels = REGIOES),
                rotulo = factor(rotulo, levels = INDICADORES$rotulo)) |>
  dplyr::arrange(rotulo, dominio)
print(as.data.frame(particao_regiao |> dplyr::select(rotulo, dominio, gap_total,
                                                     componente, residuo)), digits = 3)

saveRDS(list(pontual = pontual, particao = particao, particao_regiao = particao_regiao,
             funcao = componentes_ncob, dominios = DOMINIOS),
        here::here("data", "derivado", "particao_pontual.rds"))

message("06_particao.R concluido.")
