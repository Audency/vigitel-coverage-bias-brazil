# =============================================================================
# funcoes.R
# O QUE FAZ : Helpers compartilhados - formatacao numerica em portugues, estilo
#             unico das tabelas gt, exportacao de tabelas e figuras, SMD, logs.
# ENTRADAS  : nenhuma (carregado por 00_setup.R)
# SAIDAS    : nenhuma
# =============================================================================

# ---- Formatacao -------------------------------------------------------------
fmt_num <- function(x, dig = 1, lang = "pt") {
  out <- formatC(x, format = "f", digits = dig,
                 decimal.mark = if (lang == "pt") "," else ".")
  out[is.na(x)] <- NA_character_
  out
}

#' IC95% como "12,3 (10,1–14,5)". Com limite negativo o travessao gruda no sinal
#' e o intervalo fica ilegivel, entao usamos " a " nesse caso.
fmt_ic <- function(est, low, upp, dig = 1, lang = "pt") {
  neg <- (!is.na(low) & low < 0) | (!is.na(upp) & upp < 0)
  sep <- if (lang == "pt") "–" else "-"
  sep_neg <- if (lang == "pt") " a " else " to "
  ifelse(is.na(est), NA_character_,
         paste0(fmt_num(est, dig, lang), " (", fmt_num(low, dig, lang),
                ifelse(neg, sep_neg, sep), fmt_num(upp, dig, lang), ")"))
}

fmt_p <- function(p, lang = "pt") {
  dm <- if (lang == "pt") "," else "."
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, paste0("<0", dm, "001"),
                formatC(p, format = "f", digits = 3, decimal.mark = dm)))
}

# ---- Tabelas ----------------------------------------------------------------
#' Padrao visual: serifada, sem linhas verticais, regras no topo, sob o
#' cabecalho e no rodape.
estilo_gt <- function(g) {
  g |>
    gt::opt_table_font(font = gt::google_font("Source Serif 4")) |>
    gt::tab_options(
      table.font.size = gt::px(13),
      table.border.top.style = "solid", table.border.top.width = gt::px(2),
      table.border.top.color = "black",
      table.border.bottom.style = "solid", table.border.bottom.width = gt::px(2),
      table.border.bottom.color = "black",
      table_body.hlines.style = "none",
      table_body.border.top.style = "solid", table_body.border.top.width = gt::px(1),
      table_body.border.top.color = "black",
      table_body.border.bottom.style = "solid", table_body.border.bottom.width = gt::px(1),
      table_body.border.bottom.color = "black",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      column_labels.font.weight = "bold",
      row_group.border.top.style = "none", row_group.border.bottom.style = "none",
      row_group.font.weight = "bold",
      heading.title.font.weight = "bold", heading.border.bottom.style = "none",
      data_row.padding = gt::px(4),
      source_notes.font.size = gt::px(10), footnotes.font.size = gt::px(10)
    )
}

#' Grava a tabela em .rds (objeto gt), .docx e .html, e os dados em _dados.rds.
#' Nunca sobrescreve em silencio: registra o que foi gravado.
salva_tabela <- function(g, nome, dados = NULL,
                         dir = here::here("output", "tables")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  existia <- file.exists(file.path(dir, paste0(nome, ".rds")))
  gt::gtsave(g, file.path(dir, paste0(nome, ".html")))
  gt::gtsave(g, file.path(dir, paste0(nome, ".docx")))
  saveRDS(g, file.path(dir, paste0(nome, ".rds")))
  if (!is.null(dados)) saveRDS(dados, file.path(dir, paste0(nome, "_dados.rds")))
  message(glue::glue("  tabela {nome} [.rds .docx .html]{ifelse(existia, ' (substituida)', '')}"))
  invisible(g)
}

# ---- Figuras ----------------------------------------------------------------
#' cairo_pdf falha em instalacoes sem X11 mesmo com capabilities('cairo') TRUE.
#' Testamos de verdade uma vez e guardamos o resultado.
.cairo_ok <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    tmp <- tempfile(fileext = ".pdf")
    ok <- tryCatch({ suppressWarnings(grDevices::cairo_pdf(tmp, width = 1, height = 1))
                     grDevices::dev.off(); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (file.exists(tmp)) unlink(tmp)
    cache <<- isTRUE(ok); cache
  }
})

#' Exporta em .pdf vetorial e .png 300 dpi. Largura 180 mm (pagina inteira) ou
#' 90 mm (meia pagina). Rotulos devem usar hifen ASCII: o dispositivo PDF nativo
#' nao suporta o sinal de menos tipografico (U+2212).
salva_figura <- function(p, nome, larg_mm = 180, alt_mm = 120,
                         dir = here::here("output", "figures")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  pdf_path <- file.path(dir, paste0(nome, ".pdf"))
  if (.cairo_ok()) {
    ggplot2::ggsave(pdf_path, p, width = larg_mm, height = alt_mm, units = "mm",
                    device = grDevices::cairo_pdf); disp <- "cairo"
  } else {
    ggplot2::ggsave(pdf_path, p, width = larg_mm, height = alt_mm, units = "mm",
                    device = grDevices::pdf, encoding = "ISOLatin1", useDingbats = FALSE)
    disp <- "pdf nativo"
  }
  ggplot2::ggsave(file.path(dir, paste0(nome, ".png")), p,
                  width = larg_mm, height = alt_mm, units = "mm", dpi = 300)
  if (!file.exists(pdf_path)) stop("PDF nao gerado: ", nome, call. = FALSE)
  message(glue::glue("  figura {nome} [.pdf .png] {larg_mm}x{alt_mm} mm ({disp})"))
  invisible(p)
}

# ---- Logs e diversos --------------------------------------------------------
h1 <- function(...) cat("\n", strrep("=", 78), "\n", paste0(...), "\n",
                        strrep("=", 78), "\n", sep = "")
h2 <- function(...) { t <- paste0(...); cat("\n", t, "\n", strrep("-", nchar(t)), "\n", sep = "") }

grava_log <- function(x, nome) {
  dir.create(here::here("output", "logs"), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(x, here::here("output", "logs", paste0(nome, ".csv")))
  invisible(x)
}

le_log <- function(nome) readr::read_csv(here::here("output", "logs", paste0(nome, ".csv")),
                                         show_col_types = FALSE)

smd_prop <- function(p1, p2) (p1 - p2) / sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
smd_media <- function(m1, m2, s1, s2) (m1 - m2) / sqrt((s1^2 + s2^2) / 2)

num <- function(x) suppressWarnings(as.numeric(as.character(x)))
