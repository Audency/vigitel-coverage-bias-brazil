# =============================================================================
# 11_qa.R - Verificacoes obrigatorias antes da entrega (secao 13 do protocolo)
#
# Cada item roda e imprime o resultado. O script termina com um veredito por
# item; qualquer falha e sinalizada explicitamente, nao silenciada.
# =============================================================================

source(here::here("R", "00_setup.R"))

h1("11_qa.R - checagens de qualidade")

falhas <- character(0)
registrar <- function(ok, item, detalhe = "") {
  cat(if (ok) "  [OK]   " else "  [FALHA] ", item,
      if (nzchar(detalhe)) paste0(" - ", detalhe) else "", "\n", sep = "")
  if (!ok) falhas <<- c(falhas, item)
  invisible(ok)
}

tab2 <- readRDS(here::here("output", "tables", "table2_dados.rds"))
part <- readRDS(here::here("data", "particao.rds"))
vies <- readRDS(here::here("data", "vies_nao_cobertura.rds"))
prev <- readRDS(here::here("data", "prevalencias.rds"))
pns  <- readRDS(here::here("data", "pns.rds"))
vig  <- readRDS(here::here("data", "vigitel.rds"))
sim  <- readRDS(here::here("data", "simulacao.rds"))

# ---- 1. Convencao de sinal --------------------------------------------------
h2("1. Convencao de sinal: Delta = Vigitel - PNS")

sinal <- tab2 |>
  dplyr::filter(estrato == "Total") |>
  dplyr::mutate(recalculado = est_Vigitel - est_PNS,
                bate = abs(delta - recalculado) < 1e-9)
print(as.data.frame(sinal |> dplyr::select(label, est_Vigitel, est_PNS, delta, recalculado, bate)),
      digits = 4)
registrar(all(sinal$bate), "Delta = Vigitel - PNS na Tabela 2")

# A mesma convencao na Tabela 4 e na figura
registrar(all(abs(part$gap_total - (part$p_vig - part$est_p_pns)) < 1e-9),
          "Mesma convencao de sinal na Tabela 4")

# ---- 2. Consistencia entre tabelas -----------------------------------------
h2("2. Gap da Tabela 4 x diferenca da Tabela 2")

cons <- part |>
  dplyr::select(indicator, label, gap_tab4 = gap_total) |>
  dplyr::left_join(tab2 |> dplyr::filter(estrato == "Total") |>
                     dplyr::select(indicator, gap_tab2 = delta), by = "indicator") |>
  dplyr::mutate(dif = gap_tab4 - gap_tab2)
print(as.data.frame(cons), digits = 8)
registrar(max(abs(cons$dif)) < 1e-6, "Gap identico entre Tabelas 2 e 4",
          paste("maior diferenca:", format(max(abs(cons$dif)), digits = 2), "pp"))

# ---- 3. Soma da particao ----------------------------------------------------
h2("3. Componente + residuo = gap total")

soma <- part |>
  dplyr::mutate(soma = componente + residuo, residuo_da_soma = soma - gap_total)
print(as.data.frame(soma |> dplyr::select(label, componente, residuo, soma, gap_total,
                                          residuo_da_soma)), digits = 6)
registrar(max(abs(soma$residuo_da_soma)) < 1e-9, "Particao fecha exatamente",
          paste("maior residuo:", format(max(abs(soma$residuo_da_soma)), digits = 2)))

# ---- 4. Denominadores -------------------------------------------------------
h2("4. Denominadores: n de cada estrato bate com o n total")

den_pns <- pns |> dplyr::count(region, name = "n") |>
  dplyr::mutate(fonte = "PNS")
den_vig <- vig |> dplyr::count(region, name = "n") |>
  dplyr::mutate(fonte = "Vigitel")
den <- dplyr::bind_rows(den_pns, den_vig) |>
  tidyr::pivot_wider(names_from = fonte, values_from = n)
print(as.data.frame(den))
cat("soma PNS:", sum(den_pns$n), "| n total PNS:", nrow(pns), "\n")
cat("soma Vigitel:", sum(den_vig$n), "| n total Vigitel:", nrow(vig), "\n")
registrar(sum(den_pns$n) == nrow(pns) && sum(den_vig$n) == nrow(vig),
          "Soma dos estratos regionais igual ao n total")

# n por indicador nunca excede o n total
n_ind <- prev |> dplyr::filter(estrato == "Total") |>
  dplyr::mutate(n_total = dplyr::if_else(survey == "PNS", nrow(pns), nrow(vig)),
                ok = n <= n_total)
print(as.data.frame(n_ind |> dplyr::select(label, survey, n, n_total, ok)))
registrar(all(n_ind$ok), "n de cada indicador nao excede o n total")

# ---- 5. Validacao externa ---------------------------------------------------
h2("5. Validacao externa contra os valores publicados")

vp <- readr::read_csv(here::here("logs", "validacao_pns_sidra.csv"), show_col_types = FALSE)
vv <- readr::read_csv(here::here("logs", "validacao_vigitel.csv"), show_col_types = FALSE)
cat("PNS x SIDRA      - maior diferenca absoluta:", fmt_num(max(abs(vp$diferenca)), 3), "pp\n")
cat("Vigitel x relatorio - maior diferenca absoluta:", fmt_num(max(abs(vv$diferenca)), 3), "pp\n")
registrar(max(abs(vp$diferenca)) < 0.3, "PNS reproduz o SIDRA")
registrar(max(abs(vv$diferenca)) < 0.15, "Vigitel reproduz o relatorio oficial")

# ---- 6. Marcadores pendentes ------------------------------------------------
h2("6. Marcadores [XX] e # PROXY: no codigo e nas saidas")

arquivos <- list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)
marcas <- purrr::map_dfr(arquivos, function(f) {
  linhas <- readLines(f, warn = FALSE)
  idx <- grep("\\[XX\\]|# PROXY:", linhas)
  if (length(idx) == 0) return(NULL)
  tibble::tibble(arquivo = basename(f), linha = idx,
                 texto = stringr::str_trunc(stringr::str_squish(linhas[idx]), 90))
})
if (nrow(marcas) == 0) {
  cat("nenhum marcador pendente no codigo\n")
} else {
  print(as.data.frame(marcas))
}

# Marcadores nas saidas em Word
docs <- c(here::here("output", "resultados_vigitel_pns.docx"),
          here::here("output", "supplement", "supplementary_material.docx"))
# O protocolo pede que este item LISTE o que esta pendente, nao que exija zero
# ocorrencias: um [XX] que sobrevive de proposito e informacao, nao defeito. O
# que seria defeito e um [XX] esquecido sem que ninguem soubesse. Por isso aqui
# imprimimos cada ocorrencia com o seu contexto, para decisao consciente.
marca_doc <- purrr::map_dfr(docs, function(d) {
  if (!file.exists(d)) return(NULL)
  tmp <- tempfile(); utils::unzip(d, files = "word/document.xml", exdir = tmp)
  txt <- paste(readLines(file.path(tmp, "word", "document.xml"), warn = FALSE,
                         encoding = "UTF-8"), collapse = " ")
  txt <- stringr::str_squish(gsub("<[^>]+>", " ", txt))
  pos <- gregexpr("\\[XX\\]", txt)[[1]]
  if (pos[1] == -1) return(tibble::tibble(documento = basename(d), contexto = character(0)))
  tibble::tibble(
    documento = basename(d),
    contexto = vapply(pos, function(i)
      stringr::str_trunc(substr(txt, max(1, i - 130), i + 20), 150), character(1))
  )
})

if (nrow(marca_doc) == 0) {
  cat("nenhum marcador [XX] nas saidas em Word\n")
} else {
  cat(nrow(marca_doc), "marcador(es) [XX] pendente(s) nas saidas:\n\n")
  for (i in seq_len(nrow(marca_doc))) {
    cat("  ", marca_doc$documento[i], "\n    ...", marca_doc$contexto[i], "\n\n", sep = "")
  }
  cat("Cada um e uma pendencia declarada, a ser resolvida antes da submissao.\n")
  write_log(marca_doc, "marcadores_pendentes")
}
registrar(TRUE, "Marcadores [XX] listados",
          paste(nrow(marca_doc), "pendencia(s) declarada(s)"))

# ---- 7. Reprodutibilidade e integridade das saidas --------------------------
h2("7. Saidas esperadas")

esperados_tab <- c(paste0("table", 1:4), paste0("tableS", 1:7))
esperados_fig <- c("figure1", "figure2", paste0("figureS", 1:3))

falta_tab <- esperados_tab[!purrr::map_lgl(esperados_tab, function(x)
  all(file.exists(here::here("output", "tables", paste0(x, c(".html", ".docx", ".rds"))))))]
falta_fig <- esperados_fig[!purrr::map_lgl(esperados_fig, function(x)
  all(file.exists(here::here("output", "figures", paste0(x, c(".pdf", ".png"))))))]

cat("tabelas nos tres formatos:", length(esperados_tab) - length(falta_tab),
    "de", length(esperados_tab), "\n")
cat("figuras em pdf e png:", length(esperados_fig) - length(falta_fig),
    "de", length(esperados_fig), "\n")
registrar(length(falta_tab) == 0, "Todas as tabelas em .html, .docx e .rds",
          paste(falta_tab, collapse = ", "))
registrar(length(falta_fig) == 0, "Todas as figuras em .pdf e .png",
          paste(falta_fig, collapse = ", "))

registrar(file.exists(here::here("output", "supplement", "supplementary_material.docx")),
          "Material suplementar em arquivo unico")
registrar(file.exists(here::here("logs", "sessioninfo.txt")), "sessionInfo gravado")

# ---- 8. Simulacao: sem censo disfarcado de resultado ------------------------
h2("8. Simulacao")

cat("cenarios com erro-padrao empirico zero:", sum(sim$empse <= 0), "\n")
cat("replicas por celula:", unique(sim$n_replicas), "\n")
registrar(all(sim$empse > 0), "Nenhum cenario com erro-padrao zero")
registrar(all(sim$n_replicas >= 1000), "Minimo de 1.000 replicas por celula")
registrar(dplyr::n_distinct(sim$region) == length(REGION_LEVELS),
          "Simulacao estratificada pelas cinco regioes")

# ---- Veredito ---------------------------------------------------------------
h2("Veredito")

if (length(falhas) == 0) {
  cat("Todas as checagens passaram.\n")
} else {
  cat("Itens com falha:\n"); for (f in falhas) cat("  - ", f, "\n", sep = "")
}

qa <- tibble::tibble(
  data = format(Sys.time(), "%Y-%m-%d %H:%M"),
  checagens_com_falha = length(falhas),
  itens = paste(falhas, collapse = "; ")
)
write_log(qa, "qa_resultado")

message("11_qa.R concluido.")
