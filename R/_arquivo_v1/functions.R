# =============================================================================
# functions.R - Funcoes utilitarias compartilhadas
# Carregado por 00_setup.R. Nao roda nada por conta propria.
# =============================================================================

# ---- Formatacao numerica ----------------------------------------------------

#' Formata numero com virgula decimal (versao PT) ou ponto (versao EN)
#' @param x numerico
#' @param digits casas decimais
#' @param lang "pt" ou "en"
fmt_num <- function(x, digits = 1, lang = "pt") {
  out <- formatC(x, format = "f", digits = digits,
                 decimal.mark = if (lang == "pt") "," else ".")
  out[is.na(x)] <- NA_character_
  out
}

#' Formata estimativa com IC95%: "12,3 (10,1-14,5)"
#' Travessao (en dash) na versao PT, hifen na EN - convencao da secao 10.
#' Quando algum limite e negativo, o travessao gruda no sinal e o intervalo fica
#' ilegivel ("-2,45–-0,71"), entao trocamos o separador por " a " / " to ", que e
#' a convencao usual para diferencas em pontos percentuais.
fmt_ci <- function(est, low, high, digits = 1, lang = "pt") {
  neg <- (!is.na(low) & low < 0) | (!is.na(high) & high < 0)
  sep <- if (lang == "pt") "–" else "-"
  sep_neg <- if (lang == "pt") " a " else " to "
  ifelse(
    is.na(est), NA_character_,
    paste0(fmt_num(est, digits, lang), " (",
           fmt_num(low, digits, lang),
           ifelse(neg, sep_neg, sep),
           fmt_num(high, digits, lang), ")")
  )
}

#' Formata p-valor com corte em <0,001
fmt_p <- function(p, lang = "pt") {
  dm <- if (lang == "pt") "," else "."
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001,
                paste0("<0", dm, "001"),
                formatC(p, format = "f", digits = 3, decimal.mark = dm)))
}

# ---- Estilo unico das tabelas gt --------------------------------------------

#' Aplica o padrao visual definido na secao 10 do protocolo:
#' serifada no corpo, sem linhas verticais, regras horizontais no topo,
#' sob o cabecalho e no rodape.
gt_study_style <- function(gt_tbl) {
  gt_tbl |>
    gt::opt_table_font(font = gt::google_font("Source Serif 4")) |>
    gt::tab_options(
      table.font.size            = gt::px(13),
      table.border.top.style     = "solid",
      table.border.top.width     = gt::px(2),
      table.border.top.color     = "black",
      table.border.bottom.style  = "solid",
      table.border.bottom.width  = gt::px(2),
      table.border.bottom.color  = "black",
      table_body.hlines.style    = "none",
      table_body.border.top.style    = "solid",
      table_body.border.top.width    = gt::px(1),
      table_body.border.top.color    = "black",
      table_body.border.bottom.style = "solid",
      table_body.border.bottom.width = gt::px(1),
      table_body.border.bottom.color = "black",
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      column_labels.font.weight  = "bold",
      row_group.border.top.style    = "none",
      row_group.border.bottom.style = "none",
      row_group.font.weight      = "bold",
      heading.title.font.weight  = "bold",
      heading.border.bottom.style = "none",
      data_row.padding           = gt::px(4),
      source_notes.font.size     = gt::px(10),
      footnotes.font.size        = gt::px(10)
    )
}

#' Exporta uma tabela gt nos tres formatos exigidos (.html, .docx, .rds)
#' @param gt_tbl objeto gt
#' @param name nome sem extensao, ex. "table1"
#' @param dir pasta de destino
save_table <- function(gt_tbl, name, dir = here::here("output", "tables")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  gt::gtsave(gt_tbl, file.path(dir, paste0(name, ".html")))
  gt::gtsave(gt_tbl, file.path(dir, paste0(name, ".docx")))
  saveRDS(gt_tbl, file.path(dir, paste0(name, ".rds")))
  message(glue::glue("Tabela salva: {name}.[html|docx|rds]"))
  invisible(gt_tbl)
}

#' O cairo_pdf e o caminho preferido para PDF vetorial com Unicode, mas em
#' instalacoes de R sem X11 funcional ele reporta capabilities("cairo") == TRUE e
#' mesmo assim falha ao abrir o dispositivo. Testamos uma vez, de verdade, e
#' guardamos o resultado - assim o pipeline nao depende da configuracao grafica
#' da maquina onde roda.
.cairo_ok <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    tmp <- tempfile(fileext = ".pdf")
    ok <- tryCatch({
      suppressWarnings(grDevices::cairo_pdf(tmp, width = 1, height = 1))
      grDevices::dev.off()
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (file.exists(tmp)) unlink(tmp)
    cache <<- isTRUE(ok) && !inherits(ok, "try-error")
    cache
  }
})

#' Exporta figura em .pdf vetorial e .png 300 dpi, largura de revista
#' @param plot objeto ggplot
#' @param name nome sem extensao
#' @param width_mm 180 (pagina inteira) ou 90 (meia pagina)
save_figure <- function(plot, name, width_mm = 180, height_mm = 120,
                        dir = here::here("output", "figures")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  pdf_path <- file.path(dir, paste0(name, ".pdf"))

  if (.cairo_ok()) {
    ggplot2::ggsave(pdf_path, plot, width = width_mm, height = height_mm,
                    units = "mm", device = grDevices::cairo_pdf)
    dispositivo <- "cairo_pdf"
  } else {
    # Dispositivo PDF nativo: tambem vetorial e aceito para submissao. Exige
    # codificacao ISOLatin1 para os acentos do portugues, e nao suporta o sinal
    # de menos tipografico (U+2212) - por isso os rotulos das figuras usam
    # hifen-menos ASCII.
    ggplot2::ggsave(pdf_path, plot, width = width_mm, height = height_mm,
                    units = "mm", device = grDevices::pdf,
                    encoding = "ISOLatin1", useDingbats = FALSE)
    dispositivo <- "pdf nativo (cairo indisponivel)"
  }

  ggplot2::ggsave(file.path(dir, paste0(name, ".png")), plot,
                  width = width_mm, height = height_mm, units = "mm", dpi = 300)

  if (!file.exists(pdf_path)) {
    stop("PDF nao foi gerado para a figura ", name, call. = FALSE)
  }
  message(glue::glue(
    "Figura salva: {name}.[pdf|png] ({width_mm}x{height_mm} mm, {dispositivo})"
  ))
  invisible(plot)
}

# ---- Utilidades de log ------------------------------------------------------

#' Cabecalho de secao no console, para acompanhar a execucao incremental
h1 <- function(...) {
  txt <- paste0(...)
  cat("\n", strrep("=", 78), "\n", txt, "\n", strrep("=", 78), "\n", sep = "")
}

h2 <- function(...) {
  txt <- paste0(...)
  cat("\n", txt, "\n", strrep("-", nchar(txt)), "\n", sep = "")
}

#' Grava um data frame como log legivel em logs/
write_log <- function(x, name) {
  dir.create(here::here("logs"), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(x, here::here("logs", paste0(name, ".csv")))
  invisible(x)
}

# ---- Diferenca padronizada (SMD) --------------------------------------------

#' SMD para proporcao ponderada (duas amostras independentes)
smd_prop <- function(p1, p2) {
  (p1 - p2) / sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
}

#' SMD para media ponderada
smd_mean <- function(m1, m2, s1, s2) {
  (m1 - m2) / sqrt((s1^2 + s2^2) / 2)
}
