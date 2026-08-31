# =============================================================================
# 04_table1.R - Tabela 1: caracteristicas das amostras, Vigitel x PNS (capitais)
#
# Todas as estimativas sao ponderadas, com IC95%. A coluna SMD e a diferenca
# padronizada entre os dois inqueritos, sinalizada quando |SMD| >= 0,10.
#
# Leitura obrigatoria desta tabela: o peso do Vigitel e calibrado (rake) por
# faixa etaria, escolaridade e sexo - documento oficial de orientacoes, item 4.5.
# Logo, SMD proximo de zero nessas tres variaveis e consequencia da calibragem,
# nao evidencia de que as populacoes cobertas sejam iguais. As linhas que
# carregam informacao independente sao raca/cor e regiao.
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("04_table1.R - Tabela 1")

des_pns <- readRDS(here::here("data", "design_pns.rds"))
des_vig <- readRDS(here::here("data", "design_vigitel.rds"))

cat("PNS capitais: n =", nrow(des_pns$variables),
    "| Vigitel: n =", nrow(des_vig$variables), "\n")

# ---- Estimadores ------------------------------------------------------------

#' Distribuicao ponderada de uma variavel categorica, com IC95%
#' Devolve tambem o n nao ponderado e o total de casos validos.
cat_dist <- function(design, var, survey_label) {
  v <- rlang::sym(var)
  d <- design |> srvyr::filter(!is.na(!!v))
  d |>
    srvyr::group_by(!!v) |>
    srvyr::summarise(
      p = srvyr::survey_mean(vartype = "ci", proportion = TRUE),
      n = srvyr::unweighted(dplyr::n())
    ) |>
    dplyr::transmute(
      variavel = var,
      nivel    = as.character(!!v),
      survey   = survey_label,
      est      = p * 100,
      low      = p_low * 100,
      upp      = p_upp * 100,
      n_amostra = n
    )
}

#' Media ponderada com IC95% e desvio-padrao ponderado
mean_stat <- function(design, var, survey_label) {
  v <- rlang::sym(var)
  d <- design |> srvyr::filter(!is.na(!!v))
  m <- d |> srvyr::summarise(
    est = srvyr::survey_mean(!!v, vartype = "ci", na.rm = TRUE),
    va  = srvyr::survey_var(!!v, na.rm = TRUE),
    n   = srvyr::unweighted(dplyr::n())
  )
  tibble::tibble(
    variavel = var, nivel = "Média (DP)", survey = survey_label,
    est = m$est, low = m$est_low, upp = m$est_upp,
    sd = sqrt(m$va), n_amostra = m$n
  )
}

#' Quantos casos sem resposta em cada inquerito - vai para nota de rodape
n_missing <- function(design, var) sum(is.na(design$variables[[var]]))

# ---- Montagem ---------------------------------------------------------------

vars_cat <- c("sex", "age_grp", "education", "race", "region")

dist_all <- dplyr::bind_rows(
  purrr::map_dfr(vars_cat, ~ cat_dist(des_vig, .x, "Vigitel")),
  purrr::map_dfr(vars_cat, ~ cat_dist(des_pns, .x, "PNS"))
)

idade <- dplyr::bind_rows(
  mean_stat(des_vig, "age", "Vigitel"),
  mean_stat(des_pns, "age", "PNS")
)

# SMD linha a linha. Para categoria, a diferenca padronizada de duas proporcoes;
# para a idade, a diferenca padronizada de duas medias.
smd_cat <- dist_all |>
  dplyr::select(variavel, nivel, survey, est) |>
  tidyr::pivot_wider(names_from = survey, values_from = est) |>
  dplyr::mutate(smd = smd_prop(Vigitel / 100, PNS / 100))

smd_idade <- idade |>
  dplyr::select(variavel, nivel, survey, est, sd) |>
  tidyr::pivot_wider(names_from = survey, values_from = c(est, sd)) |>
  dplyr::mutate(smd = smd_mean(est_Vigitel, est_PNS, sd_Vigitel, sd_PNS)) |>
  dplyr::select(variavel, nivel, smd)

# ---- Corpo da tabela --------------------------------------------------------

fmt_cell <- function(est, low, upp) fmt_ci(est, low, upp, digits = 1, lang = "pt")

corpo_cat <- dist_all |>
  dplyr::mutate(cell = fmt_cell(est, low, upp)) |>
  dplyr::select(variavel, nivel, survey, cell, n_amostra) |>
  tidyr::pivot_wider(names_from = survey, values_from = c(cell, n_amostra)) |>
  dplyr::left_join(smd_cat |> dplyr::select(variavel, nivel, smd),
                   by = c("variavel", "nivel"))

corpo_idade <- idade |>
  dplyr::mutate(cell = paste0(fmt_num(est, 1), " (", fmt_num(sd, 1), ")")) |>
  dplyr::select(variavel, nivel, survey, cell, n_amostra) |>
  tidyr::pivot_wider(names_from = survey, values_from = c(cell, n_amostra)) |>
  dplyr::left_join(smd_idade, by = c("variavel", "nivel"))

rotulos <- c(
  age       = "Idade, anos",
  sex       = "Sexo",
  age_grp   = "Faixa etária, anos",
  education = "Escolaridade, anos de estudo",
  race      = "Raça/cor",
  region    = "Região"
)

tabela1 <- dplyr::bind_rows(corpo_idade, corpo_cat) |>
  dplyr::mutate(
    grupo = factor(rotulos[variavel], levels = unname(rotulos)),
    nivel = factor(nivel, levels = c(
      "Média (DP)", "Masculino", "Feminino", AGE_LEVELS, EDU_LEVELS,
      "Branca", "Preta", "Parda", "Amarela", "Indígena", REGION_LEVELS
    ))
  ) |>
  dplyr::arrange(grupo, nivel) |>
  dplyr::select(grupo, nivel, cell_Vigitel, cell_PNS, smd)

print(as.data.frame(tabela1), digits = 3)

# ---- Nao resposta, para a nota de rodape ------------------------------------
faltantes <- tibble::tibble(
  variavel = vars_cat,
  vigitel  = purrr::map_int(vars_cat, ~ n_missing(des_vig, .x)),
  pns      = purrr::map_int(vars_cat, ~ n_missing(des_pns, .x))
) |> dplyr::filter(vigitel > 0 | pns > 0)
cat("\nRegistros sem resposta (excluidos do denominador de cada variavel):\n")
print(as.data.frame(faltantes))

n_vig <- nrow(des_vig$variables)
n_pns <- nrow(des_pns$variables)

# ---- gt ---------------------------------------------------------------------

gt1 <- tabela1 |>
  gt::gt(groupname_col = "grupo", rowname_col = "nivel") |>
  gt::cols_label(
    cell_Vigitel = gt::md(glue::glue("**Vigitel {YEAR_VIGITEL}**<br>% (IC 95%)<br>n = {format(n_vig, big.mark = '.')}")),
    cell_PNS     = gt::md(glue::glue("**PNS {YEAR_PNS}**<br>% (IC 95%)<br>n = {format(n_pns, big.mark = '.')}")),
    smd          = gt::md("**SMD**")
  ) |>
  gt::fmt_number(columns = smd, decimals = 2, dec_mark = ",") |>
  gt::sub_missing(columns = dplyr::everything(), missing_text = "—") |>
  gt::cols_align("center", columns = c(cell_Vigitel, cell_PNS, smd)) |>
  gt::tab_header(title = gt::md(glue::glue(
    "**Tabela 1.** Características das amostras de adultos (≥18 anos) das 27 capitais, Vigitel {YEAR_VIGITEL} e PNS {YEAR_PNS}"
  ))) |>
  gt::tab_footnote(
    footnote = "Estimativas ponderadas pelo desenho de cada inquérito. A linha de idade traz média e desvio-padrão ponderados; as demais, percentual com intervalo de confiança de 95%.",
    locations = gt::cells_column_labels(columns = cell_Vigitel)
  ) |>
  gt::tab_footnote(
    footnote = "SMD = diferença padronizada entre os dois inquéritos, calculada linha a linha (proporções) ou sobre média e desvio-padrão (idade). Valores com |SMD| ≥ 0,10 estão destacados.",
    locations = gt::cells_column_labels(columns = smd)
  ) |>
  gt::tab_footnote(
    footnote = paste(
      "O peso de pós-estratificação do Vigitel é calibrado por faixa etária,",
      "escolaridade e sexo (Ministério da Saúde, Orientações para análises de",
      "dados do Vigitel, item 4.5). SMD próxima de zero nessas três variáveis",
      "decorre da calibragem e não indica que as populações cobertas sejam",
      "equivalentes. Raça/cor e região não entram na calibragem."
    ),
    locations = gt::cells_row_groups(groups = "Escolaridade, anos de estudo")
  ) |>
  gt::tab_source_note(gt::md(glue::glue(
    "Fontes: Vigitel {YEAR_VIGITEL} (Ministério da Saúde) e PNS {YEAR_PNS} (IBGE), ",
    "restrita às capitais e ao Distrito Federal. Casos sem resposta foram ",
    "excluídos do denominador da respectiva variável."
  ))) |>
  gt_study_style() |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(columns = smd, rows = abs(smd) >= 0.10)
  )

save_table(gt1, "table1")
saveRDS(tabela1, here::here("output", "tables", "table1_dados.rds"))

# =============================================================================
# CONFERENCIA EXTERNA DA ESTRUTURA ETARIA
# =============================================================================
# A Tabela 1 sinaliza |SMD| >= 0,10 na idade, o que nao deveria ocorrer se a
# calibragem do Vigitel (que inclui faixa etaria) reproduzisse a estrutura da
# populacao. Confrontamos as duas distribuicoes ponderadas com o Censo 2022 para
# saber qual dos inqueritos se afasta - a conclusao entra na discussao e no
# material suplementar, e nao pode ficar so no console.
#
# Contexto documentado: o peso da edicao 2019 foi calibrado pelas projecoes de
# base 2010. O proprio Ministerio publicou depois o "pesorake2025", recalibrando
# 2010-2024 pelo Censo 2022 (Orientacoes, item 1.3). Mantemos o peso da edicao
# por decisao do estudo - e essa e a razao de a conferencia abaixo existir.

h2("Conferencia da estrutura etaria contra o Censo ", YEAR_CENSO)

censo <- readRDS(here::here("data-raw", "censo",
                            glue::glue("censo{YEAR_CENSO}_pop_capitais.rds")))

censo_idade <- censo |>
  dplyr::mutate(idade_num = suppressWarnings(
    as.numeric(stringr::str_extract(Idade, "^\\d+(?= anos?$)")))) |>
  dplyr::filter(!is.na(idade_num), idade_num >= 18) |>
  dplyr::bind_rows(
    censo |> dplyr::filter(Idade == "100 anos ou mais") |> dplyr::mutate(idade_num = 100)
  ) |>
  dplyr::mutate(age_grp = cut(idade_num, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                              labels = AGE_LEVELS, right = FALSE)) |>
  dplyr::group_by(age_grp) |>
  dplyr::summarise(pop = sum(Valor), .groups = "drop") |>
  dplyr::mutate(censo = 100 * pop / sum(pop)) |>
  dplyr::select(age_grp, censo)

pond <- function(design, rotulo) {
  design$variables |>
    dplyr::filter(!is.na(age_grp)) |>
    dplyr::group_by(age_grp) |>
    dplyr::summarise(w = sum(weight), .groups = "drop") |>
    dplyr::mutate(!!rotulo := 100 * w / sum(w)) |>
    dplyr::select(age_grp, dplyr::all_of(rotulo))
}

estrutura_etaria <- censo_idade |>
  dplyr::left_join(pond(des_vig, "vigitel"), by = "age_grp") |>
  dplyr::left_join(pond(des_pns, "pns"), by = "age_grp") |>
  dplyr::mutate(
    dif_vigitel = vigitel - censo,
    dif_pns     = pns - censo
  )

print(as.data.frame(estrutura_etaria), digits = 3)
write_log(estrutura_etaria, "estrutura_etaria_vs_censo")

cat(glue::glue(
  "
  Maior desvio absoluto contra o Censo {YEAR_CENSO}:
    Vigitel: {fmt_num(max(abs(estrutura_etaria$dif_vigitel)), 1)} pp
    PNS    : {fmt_num(max(abs(estrutura_etaria$dif_pns)), 1)} pp
  "
), "\n")

message("04_table1.R concluido.")
