# ==============================================================================
# 12_GERAR_TODAS_AS_FIGURAS.R
# ==============================================================================
# OBJETIVO
#   Refazer qualquer figura do projeto usando SOMENTE os resultados .rds que ja
#   foram salvos pelos Scripts 01 a 11.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ==============================================================================
# 1. O QUE GERAR
# ==============================================================================

# CONTROLE UNICO DO SCRIPT:
if (!exists("SOBRESCREVER_FIGURAS", inherits = FALSE)) SOBRESCREVER_FIGURAS <- TRUE
if (!exists("PARAR_SE_FALTAR_RESULTADO", inherits = FALSE)) PARAR_SE_FALTAR_RESULTADO <- FALSE
if (!exists("PARAR_SE_FIGURA_FALHAR", inherits = FALSE)) PARAR_SE_FIGURA_FALHAR <- FALSE

# ==============================================================================
# 2. CONFIGURACAO GERAL DE EXPORTACAO
# ==============================================================================

CONFIGURACAO_GERAL <- list(
  formatos = c("pdf", "png"),
  dpi_padrao = 300,
  fundo = "white",
  fonte = "",
  mostrar_no_painel = interactive(),
  ajustar_ao_A4_por_padrao = TRUE,
  limiar_delta_aicc_hidden = 2
)

# ==============================================================================
# 3. PALETAS COMPARTILHADAS
# ==============================================================================

STATE_LABELS <- c(
  "0" = "Monomorphic",
  "1" = "Hemidimorphic",
  "2" = "Holodimorphic"
)

PALETTE_DIMORPHISM <- c(
  "0" = "#355C7D",
  "1" = "#D98C3F",
  "2" = "#4C956C"
)

HABIT_LABELS <- c(
  "0" = "Terrestrial",
  "1" = "Rupicolous sensu lato",
  "2" = "Epiphyte sensu lato",
  "3" = "Aquatic sensu lato",
  "4" = "Hemiepiphyte",
  "5" = "Climber",
  "6" = "Two or more habit types"
)

ARCHITECTURE_LABELS <- c(
  "0" = "Monomorphic",
  "1" = "Contracted type",
  "2" = "Spiciform type",
  "3" = "Paniculiform type",
  "4" = "Digitiform type",
  "5" = "Sporocarp type",
  "6" = "Radiciform type"
)

VARIATION_LABELS <- c(
  "0" = "Monomorphic",
  "1" = "Variation in frond shape",
  "2" = "Variation in frond size",
  "3" = "Variation in petiole size",
  "4" = "Variation in frond shape and size",
  "5" = "Variation in frond shape and petiole size",
  "6" = "Variation in frond and petiole size",
  "7" = "Total variation"
)

SIZE_LABELS <- c(
  "0" = "Monomorphic",
  "1" = "Larger fertile frond",
  "2" = "Smaller fertile frond",
  "3" = "Similar frond length",
  "4" = "Intra-individual variation"
)

PALETA_ARQUITETURA <- c(
  "0" = "#D9D9D9", "1" = "#DCEAF7", "2" = "#B7D4EC", "3" = "#7EB6DD",
  "4" = "#438FC5", "5" = "#1769A3", "6" = "#083B73"
)

PALETA_VARIACAO <- c(
  "0" = "#D9D9D9", "1" = "#F0E6F6", "2" = "#D9BFDF", "3" = "#BE91C9",
  "4" = "#9D61B2", "5" = "#7B3598", "6" = "#561A78", "7" = "#31004F"
)

PALETA_TAMANHO <- c(
  "0" = "#D9D9D9", "1" = "#FBE3C2", "2" = "#F3B66D",
  "3" = "#DD7B2D", "4" = "#9E3F13"
)

PALETA_HABITO <- c(
  "0" = "#E1F3EF", "1" = "#C1E4DC", "2" = "#91CDBF", "3" = "#5BAF9E",
  "4" = "#318D7D", "5" = "#176B62", "6" = "#0A4E4A"
)

PALETA_SENSIBILIDADE <- c(
  "FTOL consensus" = "#216A5A",
  "FTOL maximum likelihood" = "#D27D2D"
)

# ==============================================================================
# 4. CATALOGO DE IDENTIFICACAO
# ==============================================================================

FIGURAS <- structure(list(id = c("FigS1", "FigS2", "FigS3", "FigS3B", "FigS3C", 
"FigS3D", "FigS3E", "Fig5", "Fig5B", "Fig4_familias", "FigS4_filogenias", 
"FigS4B", "D_dimorfismo", "FigS5_habito", "Fig2_habito", "FigS21", 
"FigS19", "FigS20", "FigS22", "FigS23", "FigS6", "FigS8", "FigS11", 
"FigS9", "FigS12", "FigS10", "FigS13", "FigS7", "FigS24", "FigS17", 
"FigS18", "Fig3", "FigS25", "FigS26", "FigS27", "FigS28", "FigS25_S28", 
"FigS29"), script = c("02", "02", "03", "03", "03", "03", "03", 
"03", "03", "03", "03", "03", "04", "05", "05", "05", "05", "05", 
"05", "05", "07", "07", "07", "07", "07", "07", "07", "07", "07", 
"08", "08", "09", "10", "10", "10", "10", "10", "11"), stem = c("FigS1. Comparacao de modelos do dimorfismo", 
"FigS2. Rede de transicoes do dimorfismo", "FigS3. Reconstrucao filogenetica do dimorfismo", 
"FigS3B. Fertile frond architecture", "FigS3C. Morphological variation fertile", 
"FigS3D. Size ratio", "FigS3E. Habit", "Fig5. Filogenia circular com aneis de caracteres", 
"Fig5B. Esquema de identificacao dos aneis", "Fig4. Proporcao de dimorfismo por familia", 
"FigS4", 
"FigS4B. Distancias filogeneticas por dimorfismo", 
"Sinal filogenetico D do dimorfismo", "FigS5. Prevalence of dimorphism by habit", 
"Fig2. Associacao entre dimorfismo e habito", "FigS21. Dimorphic states versus Monomorphic by habit breadth", 
"FigS19. Dimorphic-state profile by habit", "FigS20. Standardized residuals for habit x dimorphism", 
"FigS22. Fertile-frond architecture by habit", "FigS23. Pagel entre amplitude de habito e dimorfismo", 
"FigS6. Frequencies of morphological states", "FigS8. Rede de transicoes da arquitetura", 
"FigS11. Matriz Q da arquitetura", "FigS9. Rede de transicoes da variacao morfologica", 
"FigS12. Matriz Q da variacao morfologica", "FigS10. Rede de transicoes da relacao de tamanho", 
"FigS13. Matriz Q da relacao de tamanho", "FigS7. Comparison of morphological models", 
"FigS24. Independent origins of morphological solutions", "FigS17. Evolucao correlacionada - teste de Pagel", 
"FigS18. Sinal filogenetico D da morfologia", "Fig3. Morphological syndromes", 
"FigS25. Phylogenetic sensitivity - Mk model support", "FigS26. Phylogenetic sensitivity - D statistic", 
"FigS27. Phylogenetic sensitivity - habit association", "FigS28. Phylogenetic sensitivity - Pagel tests", 
"FigS25-S28. Phylogenetic sensitivity plate", "FigS29. Hiddenrate_sensitivity_of_Pagel_tests"
)), row.names = c(NA, -38L), class = "data.frame")
FIGURAS$width <- NA_real_
FIGURAS$height <- NA_real_
FIGURAS$dpi <- NA_real_
FIGURAS$fit_a4 <- NA

# Versao alternativa da FigS4, com nomes completos das especies.
# A FigS4_filogenias permanece sem nomes.
FIGURAS <- rbind(
  FIGURAS,
  data.frame(
    id = "FigS4_nomes_especies",
    script = "03",
    stem = "FigS4_com_nomes_especies",
    width = NA_real_,
    height = NA_real_,
    dpi = NA_real_,
    fit_a4 = NA,
    stringsAsFactors = FALSE
  )
)

# Figura composta com todas as seis figuras S3.
FIGURAS <- rbind(
  FIGURAS,
  data.frame(
    id = "FigS3_composta",
    script = "03",
    stem = "FigS3. Painel composto das filogenias",
    width = NA_real_,
    height = NA_real_,
    dpi = NA_real_,
    fit_a4 = NA,
    stringsAsFactors = FALSE
  )
)

# Exportacoes extras em alta resolucao
FIGURAS <- rbind(
  FIGURAS,
  data.frame(
    id = "FigS4A_alta_res",
    script = "03",
    stem = "FigS4A. Painel A filogenia meia-lua alta resolucao v4",
    width = NA_real_,
    height = NA_real_,
    dpi = NA_real_,
    fit_a4 = NA,
    stringsAsFactors = FALSE
  ),
  data.frame(
    id = "FigS4A_nomes_alta_res",
    script = "03",
    stem = "FigS4A. Painel A com nomes das especies alta resolucao v4",
    width = NA_real_,
    height = NA_real_,
    dpi = NA_real_,
    fit_a4 = NA,
    stringsAsFactors = FALSE
  ),
  data.frame(
    id = "Fig5_filogenia_alta_res",
    script = "03",
    stem = "Fig5A. Filogenia circular alta resolucao v3",
    width = NA_real_,
    height = NA_real_,
    dpi = NA_real_,
    fit_a4 = NA,
    stringsAsFactors = FALSE
  ),
  data.frame(
    id = "Fig5_legendas_A4",
    script = "03",
    stem = "Fig5_legendas_apenas_A4",
    width = NA_real_,
    height = NA_real_,
    dpi = NA_real_,
    fit_a4 = NA,
    stringsAsFactors = FALSE
  )
)

# Prancha composta com as figuras S21 e S23.
FIGURAS <- rbind(
  FIGURAS,
  data.frame(
    id = "FigS21_S23",
    script = "05",
    stem = "FigS21-S23. Habit breadth and dimorphism plate",
    width = NA_real_,
    height = NA_real_,
    dpi = NA_real_,
    fit_a4 = NA,
    stringsAsFactors = FALSE
  )
)

if (anyDuplicated(tolower(FIGURAS$id))) {
  stop("Ha identificadores duplicados no catalogo FIGURAS.", call. = FALSE)
}

# ==============================================================================
# 6. CAMINHOS PORTATEIS
# ==============================================================================

get_scripts_dir_figures <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(active_path)) {
      return(dirname(normalizePath(active_path, winslash = "/", mustWork = TRUE)))
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

SCRIPTS_DIR <- get_scripts_dir_figures()
PROJECT_DIR <- dirname(SCRIPTS_DIR)

PATHS <- list(
  output_root = PROJECT_DIR,
  data = file.path(PROJECT_DIR, "Tabelas_", "_Dados_processados"),
  logs = file.path(SCRIPTS_DIR, "_Logs"),
  models = file.path(SCRIPTS_DIR, "_Modelos"),
  objects = file.path(SCRIPTS_DIR, "_Objetos_R"),
  tables = file.path(PROJECT_DIR, "Tabelas_"),
  tables_main = file.path(PROJECT_DIR, "Tabelas_"),
  tables_supp = file.path(PROJECT_DIR, "Tabelas_"),
  figures = file.path(PROJECT_DIR, "Imagens_"),
  figures_main = file.path(PROJECT_DIR, "Imagens_"),
  figures_supp = file.path(PROJECT_DIR, "Imagens_")
)

for (folder in c(PATHS$logs, PATHS$objects, PATHS$figures)) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

LOG_FILE_FIGURES <- file.path(PATHS$logs, "execucao_figuras.log")
REPORT_FILE_FIGURES <- file.path(PATHS$logs, "relatorio_ultima_geracao_figuras.csv")

# ==============================================================================
# 7. FUNCOES GERAIS DE CONTROLE
# ==============================================================================

windows_safe_path <- function(path) {
  if (.Platform$OS.type != "windows") return(path)
  destination_folder <- dirname(path)
  if (!dir.exists(destination_folder)) return(path)
  short_folder <- shortPathName(destination_folder)
  if (!nzchar(short_folder)) return(path)
  file.path(short_folder, basename(path))
}

figure_log <- function(..., level = "INFO") {
  text <- paste(..., collapse = "")
  line <- sprintf("[%s] [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, text)
  cat(line, "\n")
  cat(line, "\n", file = windows_safe_path(LOG_FILE_FIGURES), append = TRUE)
  invisible(line)
}

console_banner <- function(script, title) {
  cat("\n============================================================\n")
  cat(script, " - ", title, "\n", sep = "")
  cat("============================================================\n")
}

console_step <- function(step, total, text) cat(sprintf("\n>>> PASSO %d/%d - %s\n", step, total, text))
console_note <- function(text) cat("    NOTA: ", text, "\n", sep = "")
console_output <- function(text) cat("    SALVO: ", text, "\n", sep = "")

is_true <- function(x) tolower(trimws(x)) %in% c("1", "true", "t", "yes", "y", "sim", "s")

check_packages <- function(required_packages) {
  missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop("Pacotes necessarios para as figuras: ", paste(missing, collapse = ", "),
         "\nInstale-os antes de executar este arquivo.", call. = FALSE)
  }
  invisible(TRUE)
}

load_packages <- function(required_packages) {
  suppressPackageStartupMessages({
    for (package in required_packages) library(package, character.only = TRUE)
  })
  invisible(TRUE)
}

REQUIRED_PACKAGES <- c("ape", "dplyr", "ggplot2", "patchwork", "scales", "stringr", "tidyr", "viridisLite")
check_packages(REQUIRED_PACKAGES)
load_packages(REQUIRED_PACKAGES)

requested_ids <- tolower(as.character(FIGURAS_PARA_GERAR))
generate_all <- length(requested_ids) == 1L && requested_ids == "todas"

if (!generate_all) {
  unknown_ids <- setdiff(requested_ids, tolower(FIGURAS$id))
  if (length(unknown_ids) > 0L) {
    stop("Identificadores desconhecidos em FIGURAS_PARA_GERAR: ",
         paste(unknown_ids, collapse = ", "), call. = FALSE)
  }
}

figure_selected <- function(id) generate_all || tolower(id) %in% requested_ids

any_figure_selected <- function(ids) any(vapply(ids, figure_selected, logical(1)))

figure_config <- function(id) {
  row <- FIGURAS[tolower(FIGURAS$id) == tolower(id), , drop = FALSE]
  if (nrow(row) != 1L) stop("Figura nao encontrada no catalogo: ", id, call. = FALSE)
  row
}

figure_config_from_stem <- function(stem) {
  row <- FIGURAS[FIGURAS$stem == stem, , drop = FALSE]
  if (nrow(row) == 1L) row else NULL
}

FIGURE_REPORT <- data.frame(
  horario = character(), id = character(), script = character(),
  status = character(), detalhe = character(), stringsAsFactors = FALSE
)

register_figure_status <- function(id, status, detail = "") {
  config_row <- FIGURAS[tolower(FIGURAS$id) == tolower(id), , drop = FALSE]
  source_script <- if (nrow(config_row) == 1L) config_row$script else NA_character_
  FIGURE_REPORT <<- rbind(
    FIGURE_REPORT,
    data.frame(
      horario = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      id = id, script = source_script,
      status = status, detalhe = detail,
      stringsAsFactors = FALSE
    )
  )
  invisible(NULL)
}

fit_a4_dimensions <- function(width, height) {
  if (width >= height) { max_width <- 11.69; max_height <- 8.27 }
  else { max_width <- 8.27; max_height <- 11.69 }
  scale_factor <- min(1, max_width / width, max_height / height)
  c(width = width * scale_factor, height = height * scale_factor)
}

resolve_export_config <- function(stem, width, height, dpi, fit_a4) {
  row <- figure_config_from_stem(stem)
  if (!is.null(row)) {
    width <- row$width; height <- row$height; dpi <- row$dpi; fit_a4 <- row$fit_a4
    id <- row$id
  } else {
    id <- stem
  }
  if (isTRUE(fit_a4)) {
    dimensions <- fit_a4_dimensions(width, height)
    width <- unname(dimensions["width"]); height <- unname(dimensions["height"])
  }
  list(id = id, stem = stem, width = width, height = height, dpi = dpi, fit_a4 = fit_a4)
}

preview_ggplot <- function(plot) {
  if (isTRUE(CONFIGURACAO_GERAL$mostrar_no_painel)) print(plot)
  invisible(plot)
}

apply_global_font <- function(plot) {
  font <- CONFIGURACAO_GERAL$fonte
  if (!is.character(font) || length(font) != 1L || !nzchar(font)) return(plot)
  font_theme <- ggplot2::theme(text = ggplot2::element_text(family = font))
  if (inherits(plot, "patchwork")) plot & font_theme else plot + font_theme
}

save_plot_dual <- function(plot, stem, width, height, directory = PATHS$figures) {
  export <- resolve_export_config(stem, width, height,
    dpi = CONFIGURACAO_GERAL$dpi_padrao,
    fit_a4 = CONFIGURACAO_GERAL$ajustar_ao_A4_por_padrao)
  if (!figure_selected(export$id)) return(invisible(stem))

  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  plot <- apply_global_font(plot)
  preview_ggplot(plot)
  output_files <- character()

  copy_safely <- function(from, to) {
    suppressWarnings(
      tryCatch(
        isTRUE(file.copy(from, to, overwrite = TRUE)),
        error = function(e) FALSE
      )
    )
  }

  for (format in tolower(CONFIGURACAO_GERAL$formatos)) {
    output_path <- file.path(directory, paste0(export$stem, ".", format))

    if (file.exists(output_path) && !isTRUE(SOBRESCREVER_FIGURAS)) {
      figure_log("Mantido arquivo existente: ", output_path)
      output_files <- c(output_files, output_path)
      next
    }

    temp_path <- file.path(
      tempdir(),
      paste0("gg_", format(Sys.time(), "%H%M%S"), "_",
             sample.int(1e6, 1), ".", format)
    )

    if (format == "pdf") {
      pdf_ok <- FALSE

      # 1) tenta cairo_pdf
      if (capabilities("cairo")) {
        pdf_ok <- isTRUE(tryCatch({
          ggplot2::ggsave(
            filename = temp_path, plot = plot,
            width = export$width, height = export$height, units = "in",
            device = grDevices::cairo_pdf, bg = CONFIGURACAO_GERAL$fundo
          )
          TRUE
        }, error = function(e) FALSE, warning = function(w) FALSE))
      }

      # 2) fallback para pdf() tradicional
      if (!isTRUE(pdf_ok)) {
        pdf_ok <- isTRUE(tryCatch({
          ggplot2::ggsave(
            filename = temp_path, plot = plot,
            width = export$width, height = export$height, units = "in",
            device = grDevices::pdf, bg = CONFIGURACAO_GERAL$fundo,
            useDingbats = FALSE
          )
          TRUE
        }, error = function(e) FALSE, warning = function(w) FALSE))
      }

      if (!isTRUE(pdf_ok) || !file.exists(temp_path)) {
        stop("Nao foi possivel gerar o PDF temporario da figura: ", export$id,
             call. = FALSE)
      }

    } else if (format == "png") {
      ggplot2::ggsave(
        filename = temp_path, plot = plot,
        width = export$width, height = export$height, units = "in",
        dpi = export$dpi, bg = CONFIGURACAO_GERAL$fundo
      )
    } else {
      stop("Formato nao suportado: ", format)
    }

    final_output_path <- output_path
    ok <- copy_safely(temp_path, output_path)

    # caminho curto da pasta no Windows
    if (!isTRUE(ok) && .Platform$OS.type == "windows") {
      folder_short <- suppressWarnings(shortPathName(dirname(output_path)))
      if (length(folder_short) == 1L && nzchar(folder_short)) {
        alt_short <- file.path(folder_short, basename(output_path))
        ok <- copy_safely(temp_path, alt_short)
        if (isTRUE(ok)) final_output_path <- alt_short
      }
    }

    # nome curto baseado no ID
    if (!isTRUE(ok)) {
      alt_id <- file.path(directory, paste0(export$id, ".", format))
      ok <- copy_safely(temp_path, alt_id)
      if (isTRUE(ok)) final_output_path <- alt_id
    }

    # nome novo com timestamp
    if (!isTRUE(ok)) {
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      alt_new <- file.path(directory, paste0(export$id, "_", stamp, ".", format))
      ok <- copy_safely(temp_path, alt_new)
      if (isTRUE(ok)) final_output_path <- alt_new
    }

    if (!isTRUE(ok)) {
      stop(
        "Nao foi possivel gravar a figura na pasta: ", directory,
        "\nFeche qualquer PDF/PNG antigo da mesma figura que esteja aberto ",
        "e verifique se a pasta do OneDrive esta disponivel para escrita.",
        call. = FALSE
      )
    }

    unlink(temp_path)
    output_files <- c(output_files, final_output_path)
  }

  register_figure_status(export$id, "gerada", paste(output_files, collapse = " | "))
  figure_log("Figura gerada: ", export$id)
  invisible(stem)
}

save_base_plot_dual <- function(stem, width, height, plot_function,
                                directory = PATHS$figures, fit_to_a4 = TRUE) {
  
  export <- resolve_export_config(
    stem, width, height,
    dpi = CONFIGURACAO_GERAL$dpi_padrao,
    fit_a4 = fit_to_a4
  )
  if (!figure_selected(export$id)) return(invisible(stem))
  
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  output_files <- character()
  
  if (isTRUE(CONFIGURACAO_GERAL$mostrar_no_painel)) plot_function()
  
  for (format in tolower(CONFIGURACAO_GERAL$formatos)) {
    
    output_path <- file.path(directory, paste0(export$stem, ".", format))
    
    if (file.exists(output_path) && !isTRUE(SOBRESCREVER_FIGURAS)) {
      figure_log("Mantido arquivo existente: ", output_path)
      output_files <- c(output_files, output_path)
      next
    }
    
    temp_path <- file.path(
      tempdir(),
      paste0("fig_", format(Sys.time(), "%H%M%S"),
             "_", sample.int(1e6, 1), ".", format)
    )
    
    device_open <- FALSE
    tryCatch({
      if (format == "pdf") {
        if (capabilities("cairo")) {
          grDevices::cairo_pdf(temp_path,
                               width = export$width, height = export$height,
                               bg = CONFIGURACAO_GERAL$fundo)
        } else {
          grDevices::pdf(temp_path,
                         width = export$width, height = export$height,
                         useDingbats = FALSE, bg = CONFIGURACAO_GERAL$fundo)
        }
      } else if (format == "png") {
        grDevices::png(temp_path,
                       width = export$width, height = export$height,
                       units = "in", res = export$dpi,
                       bg = CONFIGURACAO_GERAL$fundo)
      } else {
        stop("Formato nao suportado: ", format)
      }
      device_open <- TRUE
      plot_function()
    }, finally = {
      if (device_open) grDevices::dev.off()
    })
    
    # ------------------------------------------------------------------------
    # GRAVACAO ROBUSTA PARA WINDOWS / ONEDRIVE
    # ------------------------------------------------------------------------
    final_output_path <- output_path

    copy_safely <- function(from, to) {
      suppressWarnings(
        tryCatch(
          isTRUE(file.copy(from, to, overwrite = TRUE)),
          error = function(e) FALSE
        )
      )
    }

    # 1) Caminho normal.
    ok <- copy_safely(temp_path, output_path)

    # 2) Caminho curto da pasta no Windows.
    if (!isTRUE(ok) && .Platform$OS.type == "windows") {
      folder_short <- suppressWarnings(shortPathName(dirname(output_path)))
      if (length(folder_short) == 1L && nzchar(folder_short)) {
        alt_short <- file.path(folder_short, basename(output_path))
        ok <- copy_safely(temp_path, alt_short)
        if (isTRUE(ok)) final_output_path <- alt_short
      }
    }

    # 3) Nome curto baseado apenas no ID da figura.
    if (!isTRUE(ok)) {
      alt_id <- file.path(directory, paste0(export$id, ".", format))
      ok <- copy_safely(temp_path, alt_id)
      if (isTRUE(ok)) final_output_path <- alt_id
    }

    # 4) Se um PDF/PNG antigo estiver aberto e bloqueado,
    # cria automaticamente um novo arquivo com data e hora.
    if (!isTRUE(ok)) {
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      alt_new <- file.path(
        directory,
        paste0(export$id, "_", stamp, ".", format)
      )
      ok <- copy_safely(temp_path, alt_new)
      if (isTRUE(ok)) final_output_path <- alt_new
    }

    if (!isTRUE(ok)) {
      stop(
        "Nao foi possivel gravar a figura na pasta: ", directory,
        "\nFeche qualquer PDF/PNG antigo da mesma figura que esteja aberto ",
        "e verifique se a pasta do OneDrive esta disponivel para escrita.",
        call. = FALSE
      )
    }

    unlink(temp_path)
    output_files <- c(output_files, final_output_path)
  }
  
  register_figure_status(export$id, "gerada", paste(output_files, collapse = " | "))
  figure_log("Figura gerada: ", export$id)
  invisible(stem)
}

# ==============================================================================
# 7b. FUNCOES AUXILIARES DE LEITURA E CONTROLE DE FIGURAS
# ==============================================================================

read_saved_result <- function(filename, paths = PATHS) {
  candidates <- c(
    file.path(paths$objects, filename),
    file.path(paths$models,  filename),
    file.path(paths$data,    filename),
    file.path(paths$tables,  filename),
    file.path(paths$logs,    filename)
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) {
    stop("Arquivo .rds nao encontrado: ", filename,
         "\nProcurei em:\n  ", paste(candidates, collapse = "\n  "),
         call. = FALSE)
  }
  readRDS(hit[1])
}

required_results_available <- function(files, ids = NULL, scripts = NULL,
                                       parar = PARAR_SE_FALTAR_RESULTADO) {

  faltando <- files[!vapply(files, function(f) {
    any(file.exists(c(
      file.path(PATHS$objects, f),
      file.path(PATHS$models,  f),
      file.path(PATHS$data,    f),
      file.path(PATHS$tables,  f)
    )))
  }, logical(1))]

  if (length(faltando) == 0L) return(invisible(TRUE))

  msg <- paste0(
    "Resultados ausentes para gerar: ",
    if (!is.null(ids)) paste(ids, collapse = ", ") else "(ids nao informados)",
    "\nArquivos faltando: ", paste(faltando, collapse = ", "),
    if (!is.null(scripts)) paste0("\nRode antes: ", paste(scripts, collapse = ", ")) else ""
  )

  if (isTRUE(parar)) stop(msg, call. = FALSE)

  if (!is.null(ids)) {
    for (id in ids) register_figure_status(id, "nao_gerada", msg)
  }
  figure_log(msg, level = "AVISO")
  invisible(FALSE)
}

safe_figure <- function(id, expr, parar = PARAR_SE_FIGURA_FALHAR) {
  if (!figure_selected(id)) return(invisible(NULL))
  tryCatch(
    {
      force(expr)
      if (!any(FIGURE_REPORT$id == id & FIGURE_REPORT$status == "gerada")) {
        register_figure_status(id, "gerada", "")
      }
      invisible(TRUE)
    },
    error = function(e) {
      msg <- conditionMessage(e)
      register_figure_status(id, "erro", msg)
      figure_log("Erro ao gerar ", id, ": ", msg, level = "ERRO")
      if (isTRUE(parar)) stop(e)
      invisible(FALSE)
    }
  )
}

# ==============================================================================
# 8. CARREGAMENTO SEGURO DAS FUNCOES GRAFICAS
# ==============================================================================

load_function_definitions <- function(filename, extra_assignments = character(),
                                      parent_env = globalenv()) {
  source_path <- file.path(SCRIPTS_DIR, filename)
  if (!file.exists(source_path)) {
    stop("Script de origem nao encontrado: ", source_path, call. = FALSE)
  }
  target <- new.env(parent = parent_env)
  expressions <- parse(source_path, keep.source = FALSE, encoding = "UTF-8")
  for (expression in expressions) {
    if (!is.call(expression) || !identical(expression[[1]], as.name("<-"))) next
    if (!is.symbol(expression[[2]])) next
    object_name <- as.character(expression[[2]])
    right_side <- expression[[3]]
    is_function <- is.call(right_side) && identical(right_side[[1]], as.name("function"))
    if (is_function || object_name %in% extra_assignments) {
      eval(expression, envir = target)
    }
  }
  target
}

CORE_ENV <- load_function_definitions(
  "00_CONFIGURACAO_E_FUNCOES.R",
  extra_assignments = c("MK_POLICY_VERSION", "POSTPROCESS_VERSION"),
  parent_env = globalenv()
)

SOURCE_ENV <- list(
  `02` = load_function_definitions("02_EVOLUCAO_DIMORFISMO.R",
    extra_assignments = c("quick_mode", "CONFIG"), parent_env = CORE_ENV),
  `03` = load_function_definitions("03_SIMMAP_DIMORFISMO.R", parent_env = CORE_ENV),
  `04` = load_function_definitions("04_SINAL_FILOGENETICO_DIMORFISMO.R", parent_env = CORE_ENV),
  `05` = load_function_definitions("05_HABITO_E_DIMORFISMO.R",
    extra_assignments = c("HABIT_LABELS_EN", "STATE_LABELS_EN", "ARCHITECTURE_LABELS_EN",
                          "S21_OUTCOME_LEVELS", "S21_PREDICTOR_LEVELS"),
    parent_env = CORE_ENV),
  `07` = load_function_definitions("07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R", parent_env = CORE_ENV),
  `08` = load_function_definitions("08_CORRELACOES_E_SINAL_MORFOLOGICO.R", parent_env = CORE_ENV),
  `09` = load_function_definitions("09_ANALISES_DESCRITIVAS_E_EXPLORATORIAS.R", parent_env = CORE_ENV),
  `10` = load_function_definitions("10_SENSIBILIDADE_FILOGENETICA.R", parent_env = CORE_ENV),
  `11` = load_function_definitions("11_SENSIBILIDADE_PAGEL_HIDDEN_RATES.R", parent_env = CORE_ENV)
)


# ------------------------------------------------------------------------------
# AJUSTE VISUAL EXCLUSIVO DA FIGURA S29
# Mantem um espaco horizontal fixo entre cada ponto/quadrado e seu valor numerico.
# A funcao original vem do Script 11; aqui alteramos apenas o deslocamento visual.
# ------------------------------------------------------------------------------
ajustar_espacamento_rotulos_FigS29 <- function(fun) {
  corpo <- paste(
    deparse(body(fun), width.cutoff = 500L, control = c("keepInteger", "keepNA")),
    collapse = "\n"
  )

  alvo <- "label_offset <- pmax(1.6, 0.02 * abs(d$Delta_AICc_hidden_favor_correlated))"
  novo <- "label_offset <- rep(8.0, nrow(d))"

  if (!grepl(alvo, corpo, fixed = TRUE)) {
    stop("Nao foi encontrado o trecho de espacamento dos rotulos da FigS29.", call. = FALSE)
  }

  corpo <- sub(alvo, novo, corpo, fixed = TRUE)
  corpo_novo <- parse(text = corpo, keep.source = FALSE)
  if (length(corpo_novo) != 1L) {
    stop("Falha ao reconstruir a funcao da FigS29.", call. = FALSE)
  }

  body(fun) <- corpo_novo[[1L]]
  fun
}

SOURCE_ENV$`11`$plot_hidden_rate_summary11 <- ajustar_espacamento_rotulos_FigS29(
  SOURCE_ENV$`11`$plot_hidden_rate_summary11
)

SOURCE_ENV$`03`$PALETA_ARQUITETURA <- PALETA_ARQUITETURA
SOURCE_ENV$`03`$PALETA_VARIACAO <- PALETA_VARIACAO
SOURCE_ENV$`03`$PALETA_TAMANHO <- PALETA_TAMANHO
SOURCE_ENV$`03`$PALETA_HABITO <- PALETA_HABITO
SOURCE_ENV$`03`$STATE_LABELS_FIG <- STATE_LABELS
SOURCE_ENV$`03`$ARCHITECTURE_LABELS_FIG <- ARCHITECTURE_LABELS
SOURCE_ENV$`03`$HABIT_LABELS_FIG <- HABIT_LABELS

MAPA_PALETAS_03 <- c(
  architecture_palette = "PALETA_ARQUITETURA",
  variation_palette = "PALETA_VARIACAO",
  size_palette = "PALETA_TAMANHO",
  habit_palette = "PALETA_HABITO"
)

corrigir_paletas_funcao_03 <- function(fun, mapa = MAPA_PALETAS_03) {
  corpo_texto <- paste(deparse(body(fun), width.cutoff = 500L,
    control = c("keepInteger", "keepNA")), collapse = "\n")
  nomes_antigos <- names(mapa)
  usa_nome_antigo <- any(vapply(nomes_antigos,
    function(nome) grepl(nome, corpo_texto, fixed = TRUE), logical(1)))
  if (!usa_nome_antigo) return(fun)
  for (nome_antigo in nomes_antigos) {
    corpo_texto <- gsub(nome_antigo, unname(mapa[nome_antigo]),
      corpo_texto, fixed = TRUE)
  }
  corpo_novo <- parse(text = corpo_texto, keep.source = FALSE)
  if (length(corpo_novo) != 1L) stop("Falha ao reconstruir funcao do Script 03.", call. = FALSE)
  body(fun) <- corpo_novo[[1L]]
  fun
}

for (nome_objeto in ls(SOURCE_ENV$`03`, all.names = TRUE)) {
  objeto <- get(nome_objeto, envir = SOURCE_ENV$`03`, inherits = FALSE)
  if (is.function(objeto)) {
    assign(nome_objeto, corrigir_paletas_funcao_03(objeto), envir = SOURCE_ENV$`03`)
  }
}
rm(nome_objeto, objeto)

# ==============================================================================
# Fig5 - FILOGENIA CIRCULAR COM ESPACAMENTO RADIAL GRADUAL + AJUSTES FINOS
# ==============================================================================

SOURCE_ENV$`03`$plot_circular_phylogeny_rings <- function(tree, edge_probabilities,
                                                          analysis_data, stem,
                                                          width = 16, height = 14,
                                                          ring_gap_fraction = 0.004,
                                                          ring_width_fraction = 0.028,
                                                          ring_start_multiplier = 1.028,
                                                          branch_width = 0.95,
                                                          family_connector_gap_fraction = 0.012,
                                                          family_connector_width = 0.78,
                                                          family_label_cex = 0.98,
                                                          show_ring_guides = TRUE,
                                                          ring_guide_cex = 0.92,
                                                          show_family_panel = TRUE,
                                                          family_panel_cex = 0.92) {
  
  STATE_LABELS_FIG        <- STATE_LABELS
  ARCHITECTURE_LABELS_FIG <- ARCHITECTURE_LABELS
  HABIT_LABELS_FIG        <- HABIT_LABELS
  
  required_columns <- c(
    "species", "family", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho", "Habito"
  )
  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop("Colunas ausentes para a Fig5: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (nrow(edge_probabilities) != nrow(tree$edge)) {
    stop("As probabilidades dos ramos nao correspondem a arvore da Fig5.", call. = FALSE)
  }

  dimorphism_palette_fig1 <- PALETTE_DIMORPHISM

  state_codes <- colnames(edge_probabilities)
  dominant_index <- max.col(edge_probabilities, ties.method = "first")
  dominant_state <- state_codes[dominant_index]
  confidence <- edge_probabilities[cbind(seq_len(nrow(edge_probabilities)), dominant_index)]
  confidence[!is.finite(confidence)] <- 0

  branch_colors <- mapply(
    function(color, certainty) {
      grDevices::adjustcolor(color, alpha.f = 0.52 + 0.43 * certainty)
    },
    unname(dimorphism_palette_fig1[dominant_state]),
    confidence, USE.NAMES = FALSE
  )

  plot_data <- analysis_data[match(tree$tip.label, analysis_data$species), , drop = FALSE]
  if (anyNA(plot_data$species) || anyNA(plot_data$family)) {
    stop("Falha ao alinhar dados e arvore na Fig5.", call. = FALSE)
  }

  terminal_rows_fig1 <- match(seq_len(ape::Ntip(tree)), tree$edge[, 2])
  terminal_states_fig1 <- as.character(plot_data$Padrao_Dimorfismo)
  branch_colors[terminal_rows_fig1] <- unname(dimorphism_palette_fig1[terminal_states_fig1])

  tracks <- list(
    list(title = "1 Pattern type",
         values = as.character(plot_data$Padrao_Dimorfismo),
         labels = STATE_LABELS_FIG, palette = dimorphism_palette_fig1),
    list(title = "2 Fertile frond Architecture",
         values = as.character(plot_data$Arquitetura_Lamina),
         labels = ARCHITECTURE_LABELS_FIG, palette = PALETA_ARQUITETURA),
    list(title = "3 Morphological variation fertile",
         values = as.character(plot_data$Variacao_Morfologica),
         labels = VARIATION_LABELS, palette = PALETA_VARIACAO),
    list(title = "4 Size ratio",
         values = as.character(plot_data$Relacao_Tamanho),
         labels = SIZE_LABELS, palette = PALETA_TAMANHO),
    list(title = "5 Habit",
         values = as.character(plot_data$Habito),
         labels = HABIT_LABELS_FIG, palette = PALETA_HABITO)
  )

  make_family_codes <- function(family_names) {
    stems <- toupper(sub("ACEAE$", "", family_names, ignore.case = TRUE))
    codes <- substr(stems, 1L, 3L)
    repeat {
      dup <- duplicated(codes) | duplicated(codes, fromLast = TRUE)
      if (!any(dup)) break
      previous <- codes
      codes[dup] <- vapply(which(dup), function(i) {
        width_i <- min(nchar(stems[i]), nchar(codes[i]) + 1L)
        substr(stems[i], 1L, width_i)
      }, character(1))
      if (identical(previous, codes)) { codes <- make.unique(codes, sep = ""); break }
    }
    setNames(codes, family_names)
  }

  draw_compact_legend <- function(title, labels, palette, observed = NULL,
                                  cex = 0.76, ncol = 1L) {
    codes <- names(labels)
    if (!is.null(observed)) codes <- codes[codes %in% unique(as.character(observed))]
    graphics::par(mar = c(0.1, 0.1, 0.1, 0.1), xpd = NA)
    graphics::plot.new()
    graphics::legend("center", title = title, legend = unname(labels[codes]),
      pch = 15, col = unname(palette[codes]), pt.cex = 1.15, cex = cex,
      y.intersp = 0.92, x.intersp = 0.54, ncol = ncol, bty = "n")
  }

  draw_atlas <- function() {
    if (isTRUE(show_family_panel)) {
      graphics::layout(
        matrix(c(rep(1, 8), rep(2, 4),
                 3,3,4,4,5,5,6,6,7,7,8,8,
                 rep(9, 12)), nrow = 3, byrow = TRUE),
        heights = c(10.0, 2.20, 2.10),
        widths = c(rep(1, 8), rep(1.06, 4))
      )
    } else {
      graphics::layout(
        matrix(c(rep(1, 12),
                 2,2,3,3,4,4,5,5,6,6,7,7,
                 rep(8, 12)), nrow = 3, byrow = TRUE),
        heights = c(13.2, 1.95, 1.85),
        widths = rep(1, 12)
      )
    }

    tree_height <- max(ape::node.depth.edgelength(tree)[seq_len(ape::Ntip(tree))])

    ring_gap   <- tree_height * ring_gap_fraction
    ring_width <- tree_height * ring_width_fraction
    ring_start <- tree_height * ring_start_multiplier

    family_width <- tree_height * 0.030
    family_inner <- ring_start + length(tracks) * ring_width + length(tracks) * ring_gap
    family_outer <- family_inner + family_width
    outer_limit <- tree_height * 2.38

    graphics::par(mar = c(0.05, 0.05, 0.05, 0.05), xpd = NA)

    ape::plot.phylo(tree, type = "fan", show.tip.label = FALSE,
      edge.color = branch_colors, edge.width = branch_width,
      no.margin = FALSE,
      x.lim = c(-outer_limit, outer_limit),
      y.lim = c(-outer_limit, outer_limit), main = "")

    last_plot <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    tip_x <- last_plot$xx[seq_len(ape::Ntip(tree))]
    tip_y <- last_plot$yy[seq_len(ape::Ntip(tree))]
    tip_radius <- sqrt(tip_x^2 + tip_y^2)
    unit_x <- tip_x / tip_radius
    unit_y <- tip_y / tip_radius
    tip_angle <- atan2(unit_y, unit_x)
    tip_angle_0_2pi <- (tip_angle + 2 * pi) %% (2 * pi)

    draw_continuous_annulus <- function(inner_radius, outer_radius, colors) {
      angle_order_local <- order(tip_angle_0_2pi)
      ordered_angles_local <- tip_angle_0_2pi[angle_order_local]
      n_tips_local <- length(ordered_angles_local)
      previous_angles <- c(ordered_angles_local[n_tips_local] - 2 * pi,
                           ordered_angles_local[-n_tips_local])
      next_angles <- c(ordered_angles_local[-1L], ordered_angles_local[1L] + 2 * pi)
      left_boundaries <- (previous_angles + ordered_angles_local) / 2
      right_boundaries <- (ordered_angles_local + next_angles) / 2
      overlap <- 1e-5
      for (j in seq_len(n_tips_local)) {
        theta <- seq(left_boundaries[j] - overlap, right_boundaries[j] + overlap,
                     length.out = 10L)
        color_j <- colors[angle_order_local[j]]
        if (is.na(color_j) || !nzchar(color_j)) color_j <- "#F3F4F6"
        graphics::polygon(
          x = c(cos(theta) * outer_radius, rev(cos(theta) * inner_radius)),
          y = c(sin(theta) * outer_radius, rev(sin(theta) * inner_radius)),
          col = color_j, border = NA)
      }
    }

    for (track_index in seq_along(tracks)) {
      track <- tracks[[track_index]]
      inner_radius <- ring_start + (track_index - 1L) * (ring_width + ring_gap)
      outer_radius <- inner_radius + ring_width
      colors <- unname(track$palette[track$values])
      colors[is.na(colors)] <- "#F3F4F6"
      draw_continuous_annulus(inner_radius, outer_radius, colors)
    }

    family_by_tip <- as.character(plot_data$family)
    angle_order <- order(tip_angle_0_2pi)
    family_order <- unique(family_by_tip[angle_order])
    n_families <- length(family_order)

    family_colors <- rep(c("#344955", "#657D89", "#9EADB4"), length.out = n_families)
    family_palette <- setNames(family_colors, family_order)
    family_codes <- make_family_codes(family_order)

    draw_continuous_annulus(family_inner, family_outer,
                            unname(family_palette[family_by_tip]))

    ordered_angles <- tip_angle_0_2pi[angle_order]
    ordered_families <- family_by_tip[angle_order]
    next_index <- c(seq.int(2L, length(angle_order)), 1L)
    family_changes <- which(ordered_families != ordered_families[next_index])

    for (i in family_changes) {
      next_i <- next_index[i]
      angle_1 <- ordered_angles[i]
      angle_2 <- ordered_angles[next_i]
      if (next_i == 1L) angle_2 <- angle_2 + 2 * pi
      boundary_angle <- ((angle_1 + angle_2) / 2) %% (2 * pi)
      graphics::segments(cos(boundary_angle) * family_inner,
        sin(boundary_angle) * family_inner,
        cos(boundary_angle) * family_outer,
        sin(boundary_angle) * family_outer,
        col = "white", lwd = 0.9, lend = "butt")
    }

    family_mid_angle <- vapply(family_order, function(family_name) {
      indices <- which(family_by_tip == family_name)
      atan2(mean(sin(tip_angle[indices])), mean(cos(tip_angle[indices])))
    }, numeric(1))

    # ------------------------------------------------------------------------
    # NUMEROS DAS FAMILIAS AO REDOR DO CIRCULO
    #
    # - sem colunas laterais;
    # - sem anel numerico extra;
    # - mais afastados do anel para evitar emaranhamento;
    # - linhas encostam no numero, em vez de ficarem "debaixo" dele.
    # ------------------------------------------------------------------------
    # ------------------------------------------------------------------------
    # ESPACAMENTO ADAPTATIVO DOS NUMEROS DAS FAMILIAS
    #
    # Mantemos no maximo 9 niveis, mas o afastamento radial deixa de ser fixo.
    # A distancia aumenta gradualmente conforme os angulos das familias ficam
    # mais proximos. Assim:
    #   - mesmo dois numeros proximos ja recebem uma pequena separacao;
    #   - grupos muito densos recebem afastamento progressivamente maior;
    #   - nenhum nivel adicional alem dos 9 e criado.
    # ------------------------------------------------------------------------

    label_level <- calcular_niveis_rotulos_circulares(
      family_mid_angle,
      separacao_min = 11 * pi / 180,
      max_niveis = 9L
    )

    # Distancia angular para o vizinho mais proximo de cada familia.
    angles_norm <- (family_mid_angle + 2 * pi) %% (2 * pi)
    angle_ord <- order(angles_norm)
    angles_sorted <- angles_norm[angle_ord]
    n_angle <- length(angles_sorted)

    if (n_angle > 1L) {
      prev_angles <- c(
        angles_sorted[n_angle] - 2 * pi,
        angles_sorted[-n_angle]
      )
      next_angles <- c(
        angles_sorted[-1L],
        angles_sorted[1L] + 2 * pi
      )

      nearest_sorted <- pmin(
        angles_sorted - prev_angles,
        next_angles - angles_sorted
      )

      nearest_gap <- numeric(n_angle)
      nearest_gap[angle_ord] <- nearest_sorted
    } else {
      nearest_gap <- rep(pi, n_angle)
    }

    # Intensidade de congestionamento:
    # 0 = bem separado; 1 = extremamente proximo.
    crowding_target <- 12 * pi / 180
    crowding <- pmax(
      0,
      pmin(
        1,
        (crowding_target - nearest_gap) / crowding_target
      )
    )

    # Distancia radial progressiva.
    #
    # O termo label_level separa os rotulos em niveis.
    # O termo crowding acrescenta distancia extra nos trechos congestionados.
    # O termo exponencial aumenta suavemente a separacao nos niveis mais altos.
    level_progression <- 0.0065 * (label_level ^ 1.35)

    label_radius <- family_outer + tree_height * (
      0.045 +
      0.048 * label_level +
      level_progression +
      0.040 * crowding
    )

    # ------------------------------------------------------------------------
    # AJUSTES FINOS EM GRUPOS QUE AINDA FICARAM MUITO PROXIMOS
    #
    # Os valores abaixo sao incrementos RADIAIS adicionais, aplicados somente
    # aos numeros indicados. A progressao evita que os numeros se encostem.
    #
    # 04-05
    # 10-11
    # 18-19-20-21
    # 25-26
    # 32-33
    # ------------------------------------------------------------------------
    extra_radial <- rep(0, length(family_order))

    manual_offsets <- c(
      "4"  = 0.000,
      "5"  = 0.085,

      "10" = 0.000,
      "11" = 0.085,

      "18" = 0.000,
      "19" = 0.060,
      "20" = 0.120,
      "21" = 0.180,

      "25" = 0.000,
      "26" = 0.085,

      "32" = 0.000,
      "33" = 0.085
    )

    ids_manual <- intersect(
      names(manual_offsets),
      as.character(seq_along(family_order))
    )

    extra_radial[as.integer(ids_manual)] <- unname(
      manual_offsets[ids_manual]
    )

    label_radius <- label_radius + tree_height * extra_radial

    connector_gap <- tree_height * family_connector_gap_fraction

    for (i in seq_along(family_order)) {
      angle <- family_mid_angle[i]
      angle_degrees <- (angle * 180 / pi + 360) %% 360

      text_rotation <- angle_degrees
      text_adj <- c(0, 0.5)

      if (angle_degrees > 90 && angle_degrees < 270) {
        text_rotation <- angle_degrees + 180
        text_adj <- c(1, 0.5)
      }

      text_x <- cos(angle) * label_radius[i]
      text_y <- sin(angle) * label_radius[i]

      connector_start <- family_outer + connector_gap

      # A linha vai ate o proprio ponto de ancoragem do texto.
      graphics::segments(
        x0 = cos(angle) * connector_start,
        y0 = sin(angle) * connector_start,
        x1 = text_x,
        y1 = text_y,
        col = "#546874",
        lwd = family_connector_width,
        lend = "round"
      )

      graphics::text(
        x = text_x,
        y = text_y,
        labels = sprintf("%02d", i),
        srt = text_rotation,
        adj = text_adj,
        cex = family_label_cex,
        font = 2,
        col = "#1F2D35",
        xpd = NA
      )
    }

    if (isTRUE(show_ring_guides)) {
      guide_angle <- 18 * pi / 180
      guide_anchor_x <- tree_height * 1.26
      guide_text_x <- tree_height * 1.56
      guide_labels <- c("1. Pattern type", "2. Fertile frond Architecture",
        "3. Morphological variation fertile", "4. Size ratio", "5. Habit", "F. Families")
      guide_target_radii <- c(
        vapply(seq_along(tracks), function(track_index) {
          inner_radius <- ring_start + (track_index - 1L) * (ring_width + ring_gap)
          outer_radius <- inner_radius + ring_width
          (inner_radius + outer_radius) / 2
        }, numeric(1)),
        (family_inner + family_outer) / 2)
      guide_target_x <- cos(guide_angle) * guide_target_radii
      guide_target_y <- sin(guide_angle) * guide_target_radii
      base_y <- seq(tree_height * 0.66, tree_height * 0.25, length.out = length(guide_labels))
      for (i in seq_along(guide_labels)) {
        graphics::segments(x0 = guide_anchor_x, y0 = base_y[i],
          x1 = guide_target_x[i], y1 = guide_target_y[i],
          col = "#5A6B78", lwd = 1.15, xpd = NA)
        graphics::arrows(x0 = guide_anchor_x, y0 = base_y[i],
          x1 = guide_target_x[i], y1 = guide_target_y[i],
          length = 0.07, angle = 18, code = 2, col = "#5A6B78", lwd = 1.15, xpd = NA)
        graphics::text(x = guide_text_x, y = base_y[i], labels = guide_labels[i],
          adj = c(0, 0.5), cex = ring_guide_cex, font = 2, col = "#23313B", xpd = NA)
      }
    }

    if (isTRUE(show_family_panel)) {
      dimorphism_by_family <- table(
        factor(family_by_tip, levels = family_order),
        factor(as.character(plot_data$Padrao_Dimorfismo), levels = c("0", "1", "2"))
      )
      family_totals <- rowSums(dimorphism_by_family)
      family_prop <- sweep(dimorphism_by_family, 1, pmax(family_totals, 1), "/")
      graphics::par(mar = c(7.0, 5.4, 1.2, 0.9), xpd = NA, las = 1)
      graphics::barplot(t(family_prop), horiz = FALSE, beside = FALSE,
        col = unname(dimorphism_palette_fig1[c("0", "1", "2")]), border = NA,
        names.arg = unname(family_codes[rownames(family_prop)]),
        cex.names = family_panel_cex * 0.92, las = 2,
        ylim = c(0, 1.02), axes = FALSE, space = 0.10)
      graphics::axis(2, at = c(0, 0.25, 0.50, 0.75, 1.00),
        labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
        cex.axis = family_panel_cex * 1.04, tck = -0.012, las = 1)
      graphics::mtext("Proporcao", side = 2, line = 3.0, cex = family_panel_cex * 1.14, font = 2)
      graphics::mtext("Familia", side = 1, line = 5.0, cex = family_panel_cex * 1.14, font = 2)
      graphics::legend("top",
        legend = c("Monomorphic (Mon)", "Hemidimorphic (Hem)", "Holodimorphic (Hol)"),
        fill = unname(dimorphism_palette_fig1[c("0", "1", "2")]),
        border = NA, bty = "n", horiz = TRUE,
        cex = family_panel_cex * 0.88, x.intersp = 0.55, y.intersp = 0.9,
        inset = c(0, -0.02))
    }

    draw_compact_legend("Ramos do SIMMAP", STATE_LABELS, dimorphism_palette_fig1, cex = 1.16)
    draw_compact_legend(tracks[[1]]$title, tracks[[1]]$labels, tracks[[1]]$palette,
                        observed = tracks[[1]]$values, cex = 1.12)
    draw_compact_legend(tracks[[2]]$title, tracks[[2]]$labels, tracks[[2]]$palette,
                        observed = tracks[[2]]$values, cex = 1.05)
    draw_compact_legend(tracks[[3]]$title, tracks[[3]]$labels, tracks[[3]]$palette,
                        observed = tracks[[3]]$values, cex = 1.00)
    draw_compact_legend(tracks[[4]]$title, tracks[[4]]$labels, tracks[[4]]$palette,
                        observed = tracks[[4]]$values, cex = 1.06)
    draw_compact_legend(tracks[[5]]$title, tracks[[5]]$labels, tracks[[5]]$palette,
                        observed = tracks[[5]]$values, cex = 0.98, ncol = 2L)

    graphics::par(mar = c(0.05, 0.05, 0.05, 0.05), xpd = NA)
    graphics::plot.new()
    graphics::par(mar = c(0.25, 0.25, 0.25, 0.25), xpd = NA)
    graphics::legend("center",
      title = "F  Families - numbers refer to the outer ring",
      legend = paste0(sprintf("%02d", seq_along(family_order)), "  ", family_order),
      cex = 0.98, y.intersp = 0.98, x.intersp = 0.18, ncol = 5, bty = "n")
  }

  save_base_plot_dual(stem, width = width, height = height,
    plot_function = draw_atlas,
    directory = PATHS$figures_main, fit_to_a4 = FALSE)
}

environment(SOURCE_ENV$`03`$plot_circular_phylogeny_rings) <- SOURCE_ENV$`03`

# ==============================================================================
# FigS4 - FILOGENIA EM MEIA-LUA + 3 PAINEIS (A4 RETRATO)
# ==============================================================================
#
# PAINEL A
#   - filogenia em semicirculo (180 graus), com a raiz no centro inferior;
#   - faixas 1-5 de caracteres acompanhando o arco;
#   - faixa 6 = familias;
#   - nomes COMPLETOS das especies fora dos aneis;
#   - nomes distribuidos em niveis radiais alternados para reduzir sobreposicao;
#   - margem externa propositalmente livre para futura inclusao de fotografias.
#
# PAINEL B
#   - perfis comparativos Hem/Hol e legenda dos estados observados.
#
# PAINEL C
#   - legenda numerada das familias.
#
# IMPORTANTE
#   - a figura e salva em A4 retrato (8.27 x 11.69 pol.);
#   - PDF permanece vetorial, preservando nitidez dos nomes;
#   - PNG da FigS4 e exportado a 600 dpi pelo catalogo de figuras.
# ==============================================================================

SOURCE_ENV$`03`$plot_upward_character_phylogenies <- function(
    tree,
    analysis_data,
    stem,
    width = 8.27,
    height = 11.69,
    directory = PATHS$figures_supp,
    show_species_names = FALSE) {

  STATE_LABELS_FIG        <- STATE_LABELS
  ARCHITECTURE_LABELS_FIG <- ARCHITECTURE_LABELS
  HABIT_LABELS_FIG        <- HABIT_LABELS

  required_columns <- c(
    "species", "family", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho", "Habito"
  )

  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop(
      "Colunas ausentes para a FigS4: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Somente especies hemi- e holodimorficas.
  # ---------------------------------------------------------------------------
  plot_data <- analysis_data[
    as.character(analysis_data$Padrao_Dimorfismo) %in% c("1", "2"),
    , drop = FALSE
  ]

  plot_data$species <- as.character(plot_data$species)
  plot_data$family  <- as.character(plot_data$family)

  common_species <- intersect(tree$tip.label, plot_data$species)
  if (length(common_species) < 3L) {
    stop("Poucas especies hemi/holo em comum com a arvore.", call. = FALSE)
  }

  tree_sub <- ape::drop.tip(
    tree,
    setdiff(tree$tip.label, common_species)
  )

  tree_sub <- ape::ladderize(tree_sub, right = FALSE)

  plot_data <- plot_data[
    match(tree_sub$tip.label, plot_data$species),
    , drop = FALSE
  ]

  if (anyNA(plot_data$species) || anyNA(plot_data$family)) {
    stop(
      "Falha ao alinhar a arvore e a matriz de dados da FigS4.",
      call. = FALSE
    )
  }

  n_tip <- ape::Ntip(tree_sub)

  # ---------------------------------------------------------------------------
  # Cores dos ramos da filogenia.
  # Ramos internos ficam em cinza; ramos terminais recebem cor de acordo com
  # o tipo de dimorfismo da especie (hemi ou holo).
  # ---------------------------------------------------------------------------
  tip_states <- as.character(plot_data$Padrao_Dimorfismo)
  terminal_rows <- match(seq_len(n_tip), tree_sub$edge[, 2])

  branch_colors <- rep("#8A97A3", nrow(tree_sub$edge))
  branch_colors[terminal_rows] <- unname(PALETTE_DIMORPHISM[tip_states])
  branch_colors[is.na(branch_colors)] <- "#8A97A3"

  # ---------------------------------------------------------------------------
  # Caracteres.
  # ---------------------------------------------------------------------------
  tracks <- list(
    list(
      number = 1L,
      title = "Pattern type",
      values = as.character(plot_data$Padrao_Dimorfismo),
      labels = STATE_LABELS_FIG,
      palette = PALETTE_DIMORPHISM
    ),
    list(
      number = 2L,
      title = "Fertile frond architecture",
      values = as.character(plot_data$Arquitetura_Lamina),
      labels = ARCHITECTURE_LABELS_FIG,
      palette = PALETA_ARQUITETURA
    ),
    list(
      number = 3L,
      title = "Morphological variation fertile",
      values = as.character(plot_data$Variacao_Morfologica),
      labels = VARIATION_LABELS,
      palette = PALETA_VARIACAO
    ),
    list(
      number = 4L,
      title = "Size ratio",
      values = as.character(plot_data$Relacao_Tamanho),
      labels = SIZE_LABELS,
      palette = PALETA_TAMANHO
    ),
    list(
      number = 5L,
      title = "Habit",
      values = as.character(plot_data$Habito),
      labels = HABIT_LABELS_FIG,
      palette = PALETA_HABITO
    )
  )

  # ---------------------------------------------------------------------------
  # Configuracao dos nomes.
  #
  # Em uma meia-lua ha muito mais espaco angular que na arvore retangular.
  # Os nomes permanecem completos e sao escalonados em niveis radiais.
  # ---------------------------------------------------------------------------
  if (n_tip >= 280L) {
    species_cex <- 0.50
    label_tiers <- 5L
  } else if (n_tip >= 230L) {
    species_cex <- 0.53
    label_tiers <- 4L
  } else if (n_tip >= 170L) {
    species_cex <- 0.57
    label_tiers <- 4L
  } else if (n_tip >= 110L) {
    species_cex <- 0.62
    label_tiers <- 3L
  } else {
    species_cex <- 0.70
    label_tiers <- 2L
  }

  family_number_cex <- if (n_tip >= 230L) 0.45 else 0.52

  # ---------------------------------------------------------------------------
  # Familias.
  # ---------------------------------------------------------------------------
  family_order <- unique(plot_data$family)

  family_numbers <- setNames(
    seq_along(family_order),
    family_order
  )

  # Tons neutros alternados para a faixa das familias.
  family_fill <- setNames(
    rep(
      c("#D7E0E6", "#B8C6CF", "#E3E8EC"),
      length.out = length(family_order)
    ),
    family_order
  )

  # ---------------------------------------------------------------------------
  # Funcoes auxiliares do painel B.
  # ---------------------------------------------------------------------------
  draw_stacked_bar <- function(x0, x1, y, h, proportions, colors) {

    proportions <- as.numeric(proportions)
    proportions[!is.finite(proportions)] <- 0

    total <- sum(proportions)

    if (total <= 0) {
      graphics::rect(
        x0, y - h / 2,
        x1, y + h / 2,
        col = "#F2F3F4",
        border = NA
      )
      return(invisible(NULL))
    }

    proportions <- proportions / total
    starts <- c(0, head(cumsum(proportions), -1L))
    ends   <- cumsum(proportions)

    for (i in seq_along(proportions)) {
      graphics::rect(
        x0 + starts[i] * (x1 - x0),
        y - h / 2,
        x0 + ends[i] * (x1 - x0),
        y + h / 2,
        col = colors[i],
        border = NA
      )
    }

    invisible(NULL)
  }

  trait_proportions <- function(track, dimorphism_state) {

    idx <- as.character(plot_data$Padrao_Dimorfismo) == dimorphism_state
    codes <- names(track$labels)

    observed <- as.character(track$values[idx])

    counts <- table(
      factor(observed, levels = codes)
    )

    counts <- as.numeric(counts)

    if (sum(counts) == 0) {
      props <- rep(0, length(codes))
    } else {
      props <- counts / sum(counts)
    }

    names(props) <- codes
    props
  }

  draw_profile_block <- function(
      x0, x1, y0, y1,
      track,
      title_cex = 0.78,
      legend_cex = 0.58) {

    region_w <- x1 - x0
    region_h <- y1 - y0

    graphics::text(
      x0, y1,
      labels = paste0(track$number, "  ", track$title),
      adj = c(0, 1),
      font = 2,
      cex = title_cex,
      col = "#1F2D3A"
    )

    codes <- names(track$labels)
    codes <- codes[
      codes %in% unique(as.character(track$values))
    ]

    hemi <- trait_proportions(track, "1")[codes]
    holo <- trait_proportions(track, "2")[codes]

    cols <- unname(track$palette[codes])
    cols[is.na(cols)] <- "#D9D9D9"

    bar_x0 <- x0 + 0.13 * region_w
    bar_x1 <- x1 - 0.02 * region_w
    bar_h  <- 0.072 * region_h

    y_hemi <- y1 - 0.20 * region_h
    y_holo <- y1 - 0.34 * region_h

    graphics::text(
      x0, y_hemi,
      "Hem",
      adj = c(0, 0.5),
      cex = 0.64
    )

    graphics::text(
      x0, y_holo,
      "Hol",
      adj = c(0, 0.5),
      cex = 0.64
    )

    draw_stacked_bar(
      bar_x0, bar_x1,
      y_hemi, bar_h,
      hemi, cols
    )

    draw_stacked_bar(
      bar_x0, bar_x1,
      y_holo, bar_h,
      holo, cols
    )

    n_codes <- length(codes)
    ncol_leg <- if (n_codes >= 6L) 2L else 1L
    nrow_leg <- ceiling(n_codes / ncol_leg)

    col_w <- region_w / ncol_leg
    legend_top <- y1 - 0.44 * region_h

    line_gap <- min(
      0.088 * region_h,
      (legend_top - y0) / max(nrow_leg, 1L)
    )

    for (j in seq_along(codes)) {

      col_j <- (j - 1L) %/% nrow_leg
      row_j <- (j - 1L) %% nrow_leg

      lx <- x0 + col_j * col_w
      ly <- legend_top - row_j * line_gap

      graphics::rect(
        lx,
        ly - 0.012,
        lx + 0.012,
        ly + 0.012,
        col = cols[j],
        border = NA
      )

      label_j <- paste0(
        unname(track$labels[codes[j]]),
        "  Hem: ",
        round(100 * hemi[j]),
        "%  |  Hol: ",
        round(100 * holo[j]),
        "%"
      )

      graphics::text(
        lx + 0.018,
        ly,
        labels = label_j,
        adj = c(0, 0.5),
        cex = legend_cex,
        col = "#263238"
      )
    }

    invisible(NULL)
  }

  # ---------------------------------------------------------------------------
  # Desenha uma faixa semicircular continua dividida pelas pontas.
  # ---------------------------------------------------------------------------
  draw_semicircular_track <- function(
      angles,
      inner_radius,
      outer_radius,
      colors) {

    angle_order <- order(angles)
    ordered_angles <- angles[angle_order]
    n_local <- length(ordered_angles)

    if (n_local == 1L) {
      left_boundaries <- 0
      right_boundaries <- pi
    } else {
      middle_boundaries <- (
        ordered_angles[-1L] +
        ordered_angles[-n_local]
      ) / 2

      first_gap <- (
        ordered_angles[2L] -
        ordered_angles[1L]
      ) / 2

      last_gap <- (
        ordered_angles[n_local] -
        ordered_angles[n_local - 1L]
      ) / 2

      left_boundaries <- c(
        max(0, ordered_angles[1L] - first_gap),
        middle_boundaries
      )

      right_boundaries <- c(
        middle_boundaries,
        min(pi, ordered_angles[n_local] + last_gap)
      )
    }

    overlap <- 1e-5

    for (j in seq_len(n_local)) {

      theta <- seq(
        left_boundaries[j] - overlap,
        right_boundaries[j] + overlap,
        length.out = 12L
      )

      theta <- pmax(0, pmin(pi, theta))

      color_j <- colors[angle_order[j]]

      if (is.na(color_j) || !nzchar(color_j)) {
        color_j <- "#F3F4F6"
      }

      graphics::polygon(
        x = c(
          cos(theta) * outer_radius,
          rev(cos(theta) * inner_radius)
        ),
        y = c(
          sin(theta) * outer_radius,
          rev(sin(theta) * inner_radius)
        ),
        col = color_j,
        border = NA
      )
    }

    invisible(NULL)
  }

  # ---------------------------------------------------------------------------
  # PAINEL PRINCIPAL.
  # ---------------------------------------------------------------------------
  draw_atlas <- function() {

    # ========================================================================
    # A4 RETRATO
    # ========================================================================
    graphics::layout(
      matrix(c(1, 2, 3), ncol = 1),
      heights = c(6.45, 3.35, 1.89)
    )

    # ========================================================================
    # PAINEL A - FILOGENIA EM MEIA-LUA
    # ========================================================================
    graphics::par(
      mar = c(0.10, 0.18, 1.00, 0.18),
      xpd = NA
    )

    tree_height <- max(
      ape::node.depth.edgelength(tree_sub)[
        seq_len(ape::Ntip(tree_sub))
      ],
      na.rm = TRUE
    )

    if (!is.finite(tree_height) || tree_height <= 0) {
      tree_height <- 1
    }

    # O limite externo inclui:
    # arvore + aneis + nomes completos + uma faixa branca para futuras fotos.
    if (isTRUE(show_species_names)) {
      horizontal_limit <- tree_height * 2.12
      vertical_top     <- tree_height * 2.22
    } else {
      horizontal_limit <- tree_height * 1.82
      vertical_top     <- tree_height * 1.92
    }

    ape::plot.phylo(
      tree_sub,
      type = "fan",
      open.angle = 180,
      rotate.tree = 0,
      show.tip.label = FALSE,
      use.edge.length = TRUE,
      edge.color = branch_colors,
      edge.width = 0.64,
      no.margin = FALSE,
      x.lim = c(-horizontal_limit, horizontal_limit),
      y.lim = c(-tree_height * 0.10, vertical_top),
      main = ""
    )

    last_plot <- get(
      "last_plot.phylo",
      envir = .PlotPhyloEnv
    )

    n_tip_local <- ape::Ntip(tree_sub)

    tip_x <- last_plot$xx[
      seq_len(n_tip_local)
    ]

    tip_y <- last_plot$yy[
      seq_len(n_tip_local)
    ]

    tip_radius <- sqrt(
      tip_x^2 + tip_y^2
    )

    tip_angle <- atan2(
      tip_y,
      tip_x
    )

    # Garante angulos entre 0 e pi.
    tip_angle[tip_angle < 0] <- (
      tip_angle[tip_angle < 0] +
      2 * pi
    )

    tip_angle <- pmax(
      0,
      pmin(pi, tip_angle)
    )

    max_tip_radius <- max(
      tip_radius,
      na.rm = TRUE
    )

    if (!is.finite(max_tip_radius) ||
        max_tip_radius <= 0) {
      max_tip_radius <- tree_height
    }

    # ------------------------------------------------------------------------
    # Conectores das pontas ate a primeira faixa.
    # ------------------------------------------------------------------------
    ring_gap   <- max_tip_radius * 0.0045
    ring_width <- max_tip_radius * 0.0260
    ring_start <- max_tip_radius * 1.025

    graphics::segments(
      x0 = tip_x,
      y0 = tip_y,
      x1 = cos(tip_angle) * (ring_start - max_tip_radius * 0.006),
      y1 = sin(tip_angle) * (ring_start - max_tip_radius * 0.006),
      col = "#CBD2D8",
      lwd = 0.32
    )

    # ------------------------------------------------------------------------
    # Faixas 1-5.
    # ------------------------------------------------------------------------
    for (track_index in seq_along(tracks)) {

      track <- tracks[[track_index]]

      inner_radius <- ring_start +
        (track_index - 1L) *
        (ring_width + ring_gap)

      outer_radius <- inner_radius +
        ring_width

      track_colors <- unname(
        track$palette[
          as.character(track$values)
        ]
      )

      track_colors[is.na(track_colors)] <- "#F3F4F6"

      draw_semicircular_track(
        angles = tip_angle,
        inner_radius = inner_radius,
        outer_radius = outer_radius,
        colors = track_colors
      )
    }

    # ------------------------------------------------------------------------
    # Faixa 6 - familias.
    # ------------------------------------------------------------------------
    family_inner <- ring_start +
      length(tracks) *
      (ring_width + ring_gap)

    family_outer <- family_inner +
      ring_width * 1.10

    family_colors_tip <- unname(
      family_fill[
        as.character(plot_data$family)
      ]
    )

    family_colors_tip[is.na(family_colors_tip)] <- "#D7E0E6"

    draw_semicircular_track(
      angles = tip_angle,
      inner_radius = family_inner,
      outer_radius = family_outer,
      colors = family_colors_tip
    )

    # ------------------------------------------------------------------------
    # Numeros das faixas 1-6 fora dos aneis, abaixo da meia-lua.
    # ------------------------------------------------------------------------
    ring_label_x <- rep(-horizontal_limit * 0.92, 6L)
    ring_label_y <- seq(
      from = tree_height * 0.18,
      to   = -tree_height * 0.04,
      length.out = 6L
    )

    for (i in seq_len(5L)) {
      graphics::text(
        x = ring_label_x[i],
        y = ring_label_y[i],
        labels = i,
        cex = 0.70,
        font = 2,
        col = "#263238",
        adj = c(0.5, 0.5)
      )
    }

    graphics::text(
      x = ring_label_x[6L],
      y = ring_label_y[6L],
      labels = "6",
      cex = 0.70,
      font = 2,
      col = "#263238",
      adj = c(0.5, 0.5)
    )

    # ------------------------------------------------------------------------
    # Numero de cada familia dentro da faixa 6.
    # Usa o angulo medio das especies daquela familia.
    # ------------------------------------------------------------------------
    # ------------------------------------------------------------------------
    # ORDEM FILOGENETICA DAS FAMILIAS
    # ------------------------------------------------------------------------
    # A numeracao segue EXPLICITAMENTE a disposicao visual da meia-lua:
    #
    #   direita  ->  esquerda
    #   basal/antiga  ->  derivada/recente
    #
    # Em coordenadas polares da meia-lua, o lado direito possui os menores
    # angulos e o lado esquerdo os maiores. Portanto, ordenamos as pontas por
    # tip_angle crescente e atribuímos os numeros conforme a PRIMEIRA
    # ocorrencia de cada familia nessa trajetoria.
    tip_order_right_to_left <- order(
      tip_angle,
      decreasing = FALSE,
      na.last = NA
    )

    family_order_local <- unique(
      as.character(
        plot_data$family[
          tip_order_right_to_left
        ]
      )
    )

    family_order_local <- family_order_local[
      !is.na(family_order_local) &
      nzchar(family_order_local)
    ]

    family_numbers_local <- setNames(
      seq_along(family_order_local),
      family_order_local
    )

    # Atualiza os objetos usados no painel C.
    family_order <<- family_order_local
    family_numbers <<- family_numbers_local

    for (fam in family_order_local) {

      idx <- which(
        as.character(plot_data$family) == fam
      )

      if (length(idx) == 0L) next

      fam_angles <- tip_angle[idx]

      # Media circular adequada ao semicirculo.
      fam_angle <- atan2(
        mean(sin(fam_angles)),
        mean(cos(fam_angles))
      )

      if (fam_angle < 0) {
        fam_angle <- fam_angle + 2 * pi
      }

      fam_angle <- pmax(
        0,
        pmin(pi, fam_angle)
      )

      angle_deg <- fam_angle * 180 / pi
      text_rotation <- angle_deg - 90

      graphics::text(
        x = cos(fam_angle) *
          ((family_inner + family_outer) / 2),
        y = sin(fam_angle) *
          ((family_inner + family_outer) / 2),
        labels = family_numbers_local[fam],
        srt = text_rotation,
        cex = family_number_cex,
        font = 2,
        col = "#263238"
      )
    }

    # ------------------------------------------------------------------------
    # NOMES DAS ESPECIES
    # ------------------------------------------------------------------------
    # FigS4_filogenias: show_species_names = FALSE
    # FigS4_nomes_especies: show_species_names = TRUE
    if (isTRUE(show_species_names)) {

      species_names <- gsub(
        "_",
        " ",
        tree_sub$tip.label,
        fixed = TRUE
      )

      # Escalonamento radial: especies vizinhas ocupam niveis diferentes.
      # Isso aumenta a distancia entre nomes e reduz sobreposicoes.
      if (n_tip_local >= 280L) {
        species_cex_names <- 0.44
        label_tiers_names <- 7L
        tier_gap_names <- max_tip_radius * 0.068
      } else if (n_tip_local >= 230L) {
        species_cex_names <- 0.48
        label_tiers_names <- 6L
        tier_gap_names <- max_tip_radius * 0.073
      } else if (n_tip_local >= 180L) {
        species_cex_names <- 0.52
        label_tiers_names <- 5L
        tier_gap_names <- max_tip_radius * 0.078
      } else if (n_tip_local >= 120L) {
        species_cex_names <- 0.58
        label_tiers_names <- 4L
        tier_gap_names <- max_tip_radius * 0.084
      } else {
        species_cex_names <- 0.66
        label_tiers_names <- 3L
        tier_gap_names <- max_tip_radius * 0.090
      }

      label_base_radius <- family_outer + max_tip_radius * 0.045

      tip_angle_order_names <- order(tip_angle)
      tier_by_tip_names <- integer(n_tip_local)

      tier_by_tip_names[tip_angle_order_names] <- (
        (seq_len(n_tip_local) - 1L) %% label_tiers_names
      )

      label_radius_names <- label_base_radius +
        tier_by_tip_names * tier_gap_names

      for (i in seq_len(n_tip_local)) {

        angle <- tip_angle[i]
        angle_deg <- angle * 180 / pi

        connector_start <- family_outer + max_tip_radius * 0.006
        connector_end <- label_radius_names[i] - max_tip_radius * 0.012

        graphics::segments(
          x0 = cos(angle) * connector_start,
          y0 = sin(angle) * connector_start,
          x1 = cos(angle) * connector_end,
          y1 = sin(angle) * connector_end,
          col = "#BFC7CD",
          lwd = 0.28,
          lty = 3
        )

        if (angle_deg > 90) {
          text_rotation <- angle_deg + 180
          horizontal_adj <- 1
        } else {
          text_rotation <- angle_deg
          horizontal_adj <- 0
        }

        graphics::text(
          x = cos(angle) * label_radius_names[i],
          y = sin(angle) * label_radius_names[i],
          labels = species_names[i],
          srt = text_rotation,
          adj = c(horizontal_adj, 0.5),
          cex = species_cex_names,
          font = 3,
          col = "#15191D",
          xpd = NA
        )
      }
    }
    graphics::mtext(
      "A  Phylogeny and character rings",
      side = 3,
      line = 0.16,
      adj = 0,
      cex = 1.02,
      font = 2,
      col = "#1F2D3A"
    )

    # ========================================================================
    # PAINEL B - PERFIS COMPARATIVOS
    # ========================================================================
    graphics::par(
      mar = c(0.10, 0.42, 0.15, 0.30),
      xpd = NA
    )

    graphics::plot.new()

    graphics::plot.window(
      xlim = c(0, 1),
      ylim = c(0, 1),
      xaxs = "i",
      yaxs = "i"
    )

    graphics::text(
      0.005, 0.985,
      "B  Comparative profiles and legend of observed states",
      adj = c(0, 1),
      cex = 0.98,
      font = 2,
      col = "#1F2D3A"
    )

    graphics::text(
      0.995, 0.985,
      "Legend values shown separately: Hem = hemidimorphic; Hol = holodimorphic",
      adj = c(1, 1),
      cex = 0.52,
      col = "#59656F"
    )

    n_hemi <- sum(
      as.character(plot_data$Padrao_Dimorfismo) == "1",
      na.rm = TRUE
    )

    n_holo <- sum(
      as.character(plot_data$Padrao_Dimorfismo) == "2",
      na.rm = TRUE
    )

    graphics::text(
      0.23, 0.890,
      "1  Dimorphism",
      cex = 0.76,
      font = 2
    )

    graphics::rect(
      0.40, 0.872,
      0.412, 0.894,
      col = PALETTE_DIMORPHISM["1"],
      border = NA
    )

    graphics::text(
      0.420, 0.883,
      paste0(
        "Hemidimorphic (n = ",
        n_hemi,
        ")"
      ),
      adj = c(0, 0.5),
      cex = 0.60
    )

    graphics::rect(
      0.65, 0.872,
      0.662, 0.894,
      col = PALETTE_DIMORPHISM["2"],
      border = NA
    )

    graphics::text(
      0.670, 0.883,
      paste0(
        "Holodimorphic (n = ",
        n_holo,
        ")"
      ),
      adj = c(0, 0.5),
      cex = 0.60
    )

    draw_profile_block(
      0.01, 0.48,
      0.47, 0.80,
      tracks[[2]],
      title_cex = 0.72,
      legend_cex = 0.50
    )

    draw_profile_block(
      0.52, 0.99,
      0.47, 0.80,
      tracks[[3]],
      title_cex = 0.72,
      legend_cex = 0.50
    )

    draw_profile_block(
      0.01, 0.48,
      0.03, 0.41,
      tracks[[4]],
      title_cex = 0.72,
      legend_cex = 0.51
    )

    draw_profile_block(
      0.52, 0.99,
      0.03, 0.41,
      tracks[[5]],
      title_cex = 0.72,
      legend_cex = 0.47
    )

    # ========================================================================
    # PAINEL C - FAMILIAS
    # ========================================================================
    graphics::par(
      mar = c(0.08, 0.42, 0.10, 0.30),
      xpd = NA
    )

    graphics::plot.new()

    graphics::plot.window(
      xlim = c(0, 1),
      ylim = c(0, 1),
      xaxs = "i",
      yaxs = "i"
    )

    graphics::text(
      0.005, 0.985,
      "C  Band 6: family legend (1 = rightmost/basal family)",
      adj = c(0, 1),
      cex = 0.86,
      font = 2,
      col = "#1F2D3A"
    )

    n_fam <- length(family_order)
    ncol_fam <- 4L
    nrow_fam <- ceiling(
      n_fam / ncol_fam
    )

    x_cols <- seq(
      0.02,
      0.72,
      length.out = ncol_fam
    )

    y_start <- 0.81

    y_gap <- if (nrow_fam > 1L) {
      0.58 / (nrow_fam - 1L)
    } else {
      0
    }

    for (i in seq_along(family_order)) {

      col_i <- (i - 1L) %/% nrow_fam + 1L
      row_i <- (i - 1L) %% nrow_fam

      x <- x_cols[col_i]
      y <- y_start - row_i * y_gap

      fam <- family_order[i]

      graphics::rect(
        x,
        y - 0.021,
        x + 0.013,
        y + 0.021,
        col = unname(family_fill[fam]),
        border = NA
      )

      graphics::text(
        x + 0.020,
        y,
        labels = paste0(
          family_numbers[fam],
          "  ",
          fam
        ),
        adj = c(0, 0.5),
        cex = 0.58,
        col = "#263238"
      )
    }
  }

  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    directory = directory,
    fit_to_a4 = FALSE,
    plot_function = draw_atlas
  )
}

environment(
  SOURCE_ENV$`03`$plot_upward_character_phylogenies
) <- SOURCE_ENV$`03`

# IMPORTANTE:
# A funcao acima vive dentro de SOURCE_ENV$`03`. O Script 03 original pode
# possuir uma funcao save_base_plot_dual() antiga, que tenta abrir diretamente
# o arquivo final no OneDrive. Para a FigS4, substituimos explicitamente essa
# versao pela rotina robusta deste Script 12, que salva primeiro em tempdir().
assign(
  "save_base_plot_dual",
  save_base_plot_dual,
  envir = SOURCE_ENV$`03`
)

# Mantemos tambem a rotina ggplot atualizada no mesmo ambiente para evitar que
# uma versao antiga do Script 03 seja usada por chamadas auxiliares.
assign(
  "save_plot_dual",
  save_plot_dual,
  envir = SOURCE_ENV$`03`
)

# ==============================================================================
# FigS4B - DISTANCIAS FILOGENETICAS (com folga nas bordas)
# ==============================================================================

SOURCE_ENV$`03`$plot_within_state_phylogenetic_distances <- function(
    tree, analysis_data, stem, width = 11.69, height = 8.27,
    directory = PATHS$figures_supp) {

  required_columns <- c("species", "Estado")
  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop("Colunas ausentes para a figura de distancias filogeneticas: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  state_by_species <- setNames(as.character(analysis_data$Estado),
                               as.character(analysis_data$species))
  tip_states <- state_by_species[tree$tip.label]
  valid_states <- c("0", "1", "2")
  keep <- !is.na(tip_states) & tip_states %in% valid_states
  if (sum(keep) < 3L) stop("Ha poucas especies com estado de dimorfismo valido para gerar a figura.")

  tree_plot <- if (all(keep)) tree else ape::drop.tip(tree, tree$tip.label[!keep])
  tip_states <- tip_states[tree_plot$tip.label]

  distance_matrix <- ape::cophenetic.phylo(tree_plot)
  max_distance <- max(distance_matrix, na.rm = TRUE)
  if (!is.finite(max_distance) || max_distance <= 0) stop("Nao foi possivel normalizar as distancias.")
  distance_matrix <- distance_matrix / max_distance

  distance_rows <- lapply(valid_states, function(state) {
    species_state <- tree_plot$tip.label[tip_states == state]
    if (length(species_state) < 2L) return(NULL)
    local_matrix <- distance_matrix[species_state, species_state, drop = FALSE]
    values <- local_matrix[upper.tri(local_matrix)]
    data.frame(Estado = state, Distancia = as.numeric(values), stringsAsFactors = FALSE)
  })

  distance_data <- dplyr::bind_rows(distance_rows)
  if (nrow(distance_data) == 0L) stop("Nenhum par de especies do mesmo estado foi encontrado.")

  state_names <- STATE_LABELS
  distance_palette <- PALETTE_DIMORPHISM

  summary_rows <- lapply(valid_states, function(state) {
    values <- distance_data$Distancia[distance_data$Estado == state]
    n_species <- sum(tip_states == state, na.rm = TRUE)
    if (length(values) == 0L) return(NULL)
    data.frame(Estado = state, n = n_species, pares = length(values),
               mediana = stats::median(values, na.rm = TRUE), stringsAsFactors = FALSE)
  })
  summary_data <- dplyr::bind_rows(summary_rows)

  legend_labels <- setNames(vapply(valid_states, function(state) {
    row <- summary_data[summary_data$Estado == state, , drop = FALSE]
    if (nrow(row) == 0L) return(unname(state_names[state]))
    paste0(unname(state_names[state]), "  |  n = ", row$n,
           "  |  pairs = ", row$pares, "  |  median = ", sprintf("%.2f", row$mediana))
  }, character(1)), valid_states)

  distance_data$Estado <- factor(distance_data$Estado, levels = valid_states)
  summary_data$Estado <- factor(summary_data$Estado, levels = valid_states)

  plot <- ggplot2::ggplot(distance_data,
    ggplot2::aes(x = Distancia, colour = Estado, fill = Estado)) +
    ggplot2::geom_density(alpha = 0.16, linewidth = 1.05, adjust = 0.85, na.rm = TRUE) +
    ggplot2::geom_vline(data = summary_data[summary_data$Estado == "2", , drop = FALSE],
      ggplot2::aes(xintercept = mediana, colour = Estado),
      linewidth = 1.20, linetype = "longdash", alpha = 0.82, show.legend = FALSE) +
    ggplot2::geom_vline(data = summary_data[summary_data$Estado == "1", , drop = FALSE],
      ggplot2::aes(xintercept = mediana, colour = Estado),
      linewidth = 0.92, linetype = "dotdash", alpha = 0.92, show.legend = FALSE) +
    ggplot2::geom_vline(data = summary_data[summary_data$Estado == "0", , drop = FALSE],
      ggplot2::aes(xintercept = mediana, colour = Estado),
      linewidth = 0.62, linetype = "dotted", alpha = 1.00, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = distance_palette, breaks = valid_states,
      labels = legend_labels, drop = FALSE, name = NULL) +
    ggplot2::scale_fill_manual(values = distance_palette, breaks = valid_states,
      labels = legend_labels, drop = FALSE, guide = "none") +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(
          linetype = c("dotted", "dotdash", "longdash"),
          linewidth = c(0.62, 0.92, 1.20),
          alpha = 1,
          fill = NA
        )
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.12),
      breaks = c(seq(0, 1, by = 0.2), 1.1),
      labels = c(sprintf("%.1f", seq(0, 1, by = 0.2)), "1.1"),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "Normalized phylogenetic distance", y = "Density",
      caption = paste0("Each curve has area = 1 and compares distribution shape, ",
                       "not the number of species. Descriptive analysis: species pairs are not independent.")) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 16),
      axis.text = ggplot2::element_text(size = 12),
      legend.position = c(0.03, 0.95),
      legend.justification = c(0, 1),
      legend.text = ggplot2::element_text(size = 12.5),
      legend.key.width = grid::unit(1.25, "cm"),
      legend.background = ggplot2::element_rect(
        fill = grDevices::adjustcolor("white", alpha.f = 0.82), colour = NA),
      plot.caption = ggplot2::element_text(size = 10.5, hjust = 0.5,
        colour = "#65727E", margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(12, 14, 10, 12))

  save_plot_dual(plot, stem, width, height, directory = directory)

  invisible(list(distances = distance_data, summary = summary_data, plot = plot))
}

environment(SOURCE_ENV$`03`$plot_within_state_phylogenetic_distances) <- SOURCE_ENV$`03`


# ==============================================================================
# Fig4_familias - MODELO ORIGINAL COM NOMES COMPLETOS
# ==============================================================================
SOURCE_ENV$`03`$plot_family_dimorphism_summary <- function(
    tree,
    analysis_data,
    stem,
    width = 10.8,
    height = 7.0,
    directory = PATHS$figures) {

  required_columns <- c("species", "family", "Padrao_Dimorfismo")
  missing_columns <- setdiff(required_columns, names(analysis_data))

  if (length(missing_columns) > 0L) {
    stop(
      "Colunas ausentes para Fig4_familias: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  tree_plot <- ape::ladderize(tree, right = FALSE)
  n_tip <- ape::Ntip(tree_plot)

  get_tip_order_from_tree <- function(tr) {
    n_tip_local <- ape::Ntip(tr)
    edge_local <- tr$edge

    roots <- setdiff(unique(edge_local[, 1]), unique(edge_local[, 2]))
    if (length(roots) == 0L) {
      stop("Nao foi possivel identificar a raiz da arvore.", call. = FALSE)
    }
    root <- roots[1L]

    walk_node <- function(node) {
      children <- edge_local[edge_local[, 1] == node, 2]
      if (length(children) == 0L) return(integer(0))
      unlist(
        lapply(children, function(child) {
          if (child <= n_tip_local) child else walk_node(child)
        }),
        use.names = FALSE
      )
    }

    out <- walk_node(root)
    if (length(out) != n_tip_local) out <- seq_len(n_tip_local)
    out
  }

  tip_order <- get_tip_order_from_tree(tree_plot)
  tree_species <- tree_plot$tip.label[tip_order]

  plot_data <- analysis_data[
    match(tree_species, as.character(analysis_data$species)),
    , drop = FALSE
  ]

  valid <- (
    !is.na(plot_data$species) &
    !is.na(plot_data$family) &
    nzchar(as.character(plot_data$family)) &
    as.character(plot_data$Padrao_Dimorfismo) %in% c("0", "1", "2")
  )
  plot_data <- plot_data[valid, , drop = FALSE]

  if (nrow(plot_data) == 0L) {
    stop("Nenhum dado valido para gerar Fig4_familias.", call. = FALSE)
  }

  plot_data$family <- as.character(plot_data$family)
  plot_data$Padrao_Dimorfismo <- as.character(plot_data$Padrao_Dimorfismo)

  # Ordem filogenética: da mais basal à mais derivada, pela primeira
  # ocorrência de cada família ao percorrer a árvore ladderizada.
  family_order <- unique(plot_data$family)
  family_labels_full <- setNames(
    paste0(seq_along(family_order), ". ", family_order),
    family_order
  )
  valid_states <- c("0", "1", "2")

  count_df <- as.data.frame(
    table(
      family = factor(plot_data$family, levels = family_order),
      state = factor(plot_data$Padrao_Dimorfismo, levels = valid_states)
    ),
    stringsAsFactors = FALSE
  )
  names(count_df) <- c("family", "state", "n")

  count_df <- count_df[count_df$family %in% family_order, , drop = FALSE]
  count_df$family <- factor(as.character(count_df$family), levels = family_order)
  count_df$state <- factor(as.character(count_df$state), levels = valid_states)

  totals <- stats::aggregate(n ~ family, data = count_df, FUN = sum)
  names(totals)[2] <- "total_n"
  count_df <- merge(count_df, totals, by = "family", all.x = TRUE, sort = FALSE)
  count_df <- count_df[match(
    paste(as.character(count_df$family), as.character(count_df$state)),
    paste(rep(family_order, each = length(valid_states)), rep(valid_states, times = length(family_order)))
  ), , drop = FALSE]

  count_df$proportion <- ifelse(count_df$total_n > 0, count_df$n / count_df$total_n, 0)
  count_df$family <- factor(as.character(count_df$family), levels = family_order)
  count_df$state <- factor(as.character(count_df$state), levels = valid_states)

  state_labels_fig4 <- c(
    "0" = "Monomorphic (Mon)",
    "1" = "Hemidimorphic (Hem)",
    "2" = "Holodimorphic (Hol)"
  )

  plot <- ggplot2::ggplot(
    count_df,
    ggplot2::aes(x = family, y = proportion, fill = state)
  ) +
    ggplot2::geom_col(width = 0.88, colour = NA, position = "stack") +
    ggplot2::scale_fill_manual(
      values = unname(PALETTE_DIMORPHISM[c("0", "1", "2")]),
      breaks = valid_states,
      labels = unname(state_labels_fig4[valid_states]),
      drop = FALSE,
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.0),
      breaks = c(0, 0.25, 0.50, 0.75, 1.00),
      labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_x_discrete(labels = family_labels_full) +
    ggplot2::labs(x = NULL, y = "Proportion") +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 17, face = "bold"),
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 0.5, hjust = 1, size = 10.3, colour = "#303A44"
      ),
      axis.text.y = ggplot2::element_text(size = 12, colour = "#303A44"),
      legend.position = "top",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 12.5),
      legend.key.size = grid::unit(0.8, "cm"),
      plot.margin = ggplot2::margin(12, 10, 10, 10)
    )

  save_plot_dual(plot, stem, width, height, directory = directory)

  invisible(list(
    data = count_df,
    plot = plot,
    family_order = family_order,
    family_labels_full = family_labels_full
  ))
}

environment(
  SOURCE_ENV$`03`$plot_family_dimorphism_summary
) <- SOURCE_ENV$`03`

# ==============================================================================
# Objetos globais dos Scripts 07 e 08
# ==============================================================================

MORPH_LABELS_SHARED <- list(
  Arquitetura_Lamina = ARCHITECTURE_LABELS,
  Variacao_Morfologica = VARIATION_LABELS,
  Relacao_Tamanho = SIZE_LABELS
)

SOURCE_ENV$`07`$MORPH_LABELS <- MORPH_LABELS_SHARED
SOURCE_ENV$`07`$MORPH_TRAITS <- names(MORPH_LABELS_SHARED)
SOURCE_ENV$`07`$MORPH_PATHS <- list(logs = PATHS$logs, data = PATHS$data,
  tables = PATHS$tables_supp, figures = PATHS$figures_supp, objects = PATHS$objects)
SOURCE_ENV$`07`$MORPH_LOG <- LOG_FILE_FIGURES

SOURCE_ENV$`07`$morph_save_plot <- function(plot, stem, width = 10, height = 7,
    directory = PATHS$figures_supp) {
  save_plot_dual(plot, stem, width, height, directory = directory)
}
SOURCE_ENV$`07`$morph_save_base_plot <- function(stem, width = 10, height = 10,
    plot_fun, directory = PATHS$figures_supp) {
  save_base_plot_dual(stem, width, height, plot_fun, directory = directory)
}

SOURCE_ENV$`08`$MORPH_LABELS <- MORPH_LABELS_SHARED
SOURCE_ENV$`08`$MORPH_LABELS_FIG08 <- MORPH_LABELS_SHARED
SOURCE_ENV$`08`$ARCHITECTURE_LABELS_FIG08 <- ARCHITECTURE_LABELS
SOURCE_ENV$`08`$VARIATION_LABELS_FIG08 <- VARIATION_LABELS
SOURCE_ENV$`08`$SIZE_LABELS_FIG08 <- SIZE_LABELS
SOURCE_ENV$`08`$MORPH_PATHS <- list(logs = PATHS$logs, data = PATHS$data,
  tables = PATHS$tables_supp, figures = PATHS$figures_supp, objects = PATHS$objects)
SOURCE_ENV$`08`$MORPH_LOG <- LOG_FILE_FIGURES

SOURCE_ENV$`10`$SENS_TREE_LEVELS <- c("Main_consensus", "Alternative_ML")
SOURCE_ENV$`10`$SENS_TREE_LABELS <- c("Main_consensus" = "FTOL consensus",
                                      "Alternative_ML" = "FTOL maximum likelihood")
SOURCE_ENV$`10`$SENS_TREE_COLORS <- PALETA_SENSIBILIDADE
SOURCE_ENV$`10`$SENS_TREE_SHAPES <- c("FTOL consensus" = 16, "FTOL maximum likelihood" = 17)
SOURCE_ENV$`10`$SENS_PATHS <- list(data = PATHS$data, tables = PATHS$tables_supp,
  figures = PATHS$figures_supp, objects = PATHS$objects, logs = PATHS$logs)
SOURCE_ENV$`10`$SENS_LOG <- LOG_FILE_FIGURES

SOURCE_ENV$`11`$CONFIG11 <- list(delta_threshold = CONFIGURACAO_GERAL$limiar_delta_aicc_hidden)
SOURCE_ENV$`11`$LOG11 <- LOG_FILE_FIGURES

# A funcao grafica do Script 11 foi escrita para usar save_plot_dual() do
# pipeline original. Aqui apontamos explicitamente para a rotina de exportacao
# robusta deste Script 12. Assim, a FigS29 e salva primeiro em arquivo temporario
# e depois copiada para Imagens_, evitando falhas do cairo_pdf em caminhos longos
# ou sincronizados pelo OneDrive. Isso NAO executa nenhuma analise novamente.
SOURCE_ENV$`11`$PATHS <- PATHS
SOURCE_ENV$`11`$save_plot_dual <- save_plot_dual

# ==============================================================================
# Auxiliares para rotulos circulares (compatibilidade)
# ==============================================================================

calcular_niveis_rotulos_circulares <- function(angulos, separacao_min = 5 * pi / 180,
                                                max_niveis = 6L) {
  if (length(angulos) <= 1L) return(rep.int(0L, length(angulos)))
  angulos_02pi <- (angulos + 2 * pi) %% (2 * pi)
  ordem <- order(angulos_02pi); ordenados <- angulos_02pi[ordem]
  niveis_ord <- integer(length(ordenados))
  distancia_circular <- function(a, b) { d <- abs(a - b); pmin(d, 2 * pi - d) }
  for (i in seq_along(ordenados)) {
    if (i == 1L) { niveis_ord[i] <- 0L; next }
    anteriores <- seq_len(i - 1L)
    proximos <- anteriores[distancia_circular(ordenados[i], ordenados[anteriores]) < separacao_min]
    ocupados <- if (length(proximos) > 0L) unique(niveis_ord[proximos]) else integer(0)
    disponiveis <- setdiff(0:max_niveis, ocupados)
    niveis_ord[i] <- if (length(disponiveis) > 0L) min(disponiveis) else max_niveis
  }
  niveis <- integer(length(angulos)); niveis[ordem] <- niveis_ord
  niveis
}

SOURCE_ENV$`03`$calcular_niveis_rotulos_circulares <- calcular_niveis_rotulos_circulares

# ==============================================================================
# 9. GERACAO DAS FIGURAS DO SCRIPT 02
# ==============================================================================

FIGURAS[FIGURAS$id == "FigS1", c("width", "height", "dpi", "fit_a4")] <- list(
  width = 8.6, height = 4.9, dpi = 300, fit_a4 = TRUE
)
CONFIG_S1 <- list(largura_barras = 0.66, cor_barras = "#2A9D8F",
  texto_valores = 4.5, texto_base = 15, x_divisoria = 1.22, x_aicc = 1.34,
  margens = c(22, 80, 12, 10))

plot_model_comparison_final12 <- function(comparison_table) {
  plot_data <- comparison_table[is.finite(comparison_table$AICc), , drop = FALSE]
  plot_data <- plot_data[order(plot_data$AICc), , drop = FALSE]
  model_labels <- c("SYM" = "SYM (symmetrical)", "ARD" = "ARD (asymmetric)",
    "ER" = "ER (single-rate)", "Ordenado_ARD" = "Ordered ARD",
    "Ordenado_SYM" = "Ordered SYM", "Ordered_ARD" = "Ordered ARD",
    "Ordered_SYM" = "Ordered SYM")
  plot_data$Modelo_label <- unname(model_labels[as.character(plot_data$Modelo)])
  missing_lab <- is.na(plot_data$Modelo_label)
  plot_data$Modelo_label[missing_lab] <- as.character(plot_data$Modelo[missing_lab])
  plot_data$Modelo_label <- factor(plot_data$Modelo_label, levels = rev(plot_data$Modelo_label))
  plot_data$Peso_label <- ifelse(plot_data$Peso_Akaike < 0.001, "< 0.001",
                                 sprintf("%.3f", plot_data$Peso_Akaike))
  plot_data$AICc_label <- sprintf("%.1f", plot_data$AICc)
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Peso_Akaike, y = Modelo_label)) +
    ggplot2::geom_col(width = CONFIG_S1$largura_barras, fill = CONFIG_S1$cor_barras) +
    ggplot2::geom_text(ggplot2::aes(label = Peso_label), hjust = -0.06,
      size = CONFIG_S1$texto_valores, color = "#20262C") +
    ggplot2::geom_vline(xintercept = CONFIG_S1$x_divisoria, linewidth = 0.35, color = "#D1D5DB") +
    ggplot2::geom_text(ggplot2::aes(x = CONFIG_S1$x_aicc, label = AICc_label),
      hjust = 0, size = CONFIG_S1$texto_valores, color = "#20262C") +
    ggplot2::annotate("text", x = CONFIG_S1$x_aicc,
      y = length(levels(plot_data$Modelo_label)) + 0.42, label = "AICc",
      hjust = 0, fontface = "bold", size = 4.8) +
    ggplot2::scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.00),
      labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
      limits = c(0, 1.55), expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::coord_cartesian(xlim = c(0, 1.55), clip = "off") +
    ggplot2::labs(x = "Akaike weight (support)", y = NULL) +
    ggplot2::theme_minimal(base_size = CONFIG_S1$texto_base) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.35, color = "#E5E7EB"),
      axis.title.x = ggplot2::element_text(size = 15.5, face = "bold",
        margin = ggplot2::margin(t = 9)),
      axis.text.x = ggplot2::element_text(size = 12.5, color = "#20262C"),
      axis.text.y = ggplot2::element_text(size = 14.0, color = "#20262C", face = "bold"),
      plot.margin = do.call(ggplot2::margin, as.list(CONFIG_S1$margens)))
  save_plot_dual(p, "FigS1. Comparacao de modelos do dimorfismo",
    figure_config("FigS1")$width, figure_config("FigS1")$height)
  invisible(p)
}
SOURCE_ENV$`02`$plot_model_comparison <- plot_model_comparison_final12

FIGURAS[FIGURAS$id == "FigS2", c("width", "height", "dpi", "fit_a4")] <- list(
  width = 6.2, height = 4.8, dpi = 300, fit_a4 = TRUE
)

# ==============================================================================
# PROTECAO CONTRA EXECUCAO PARCIAL DO BLOCO DE FIGURAS
# ------------------------------------------------------------------------------
# Se este trecho for executado isoladamente no RStudio e a funcao auxiliar
# required_results_available() ainda nao estiver carregada na sessao, recria
# apenas essa funcao sem alterar nenhum ajuste visual das figuras.
# ==============================================================================

if (!exists("required_results_available", mode = "function", inherits = TRUE)) {
  required_results_available <- function(files, ids = NULL, scripts = NULL,
                                         parar = if (exists("PARAR_SE_FALTAR_RESULTADO",
                                                           inherits = TRUE)) {
                                           PARAR_SE_FALTAR_RESULTADO
                                         } else {
                                           FALSE
                                         }) {

    if (!exists("PATHS", inherits = TRUE)) {
      stop(
        "O objeto PATHS ainda nao existe. Execute o script 12 completo pelo botao Source.",
        call. = FALSE
      )
    }

    faltando <- files[!vapply(files, function(f) {
      any(file.exists(c(
        file.path(PATHS$objects, f),
        file.path(PATHS$models,  f),
        file.path(PATHS$data,    f),
        file.path(PATHS$tables,  f)
      )))
    }, logical(1))]

    if (length(faltando) == 0L) {
      return(invisible(TRUE))
    }

    msg <- paste0(
      "Resultados ausentes para gerar: ",
      if (!is.null(ids)) paste(ids, collapse = ", ") else "(ids nao informados)",
      "\nArquivos faltando: ", paste(faltando, collapse = ", "),
      if (!is.null(scripts)) {
        paste0("\nRode antes: ", paste(scripts, collapse = ", "))
      } else {
        ""
      }
    )

    if (isTRUE(parar)) {
      stop(msg, call. = FALSE)
    }

    if (!is.null(ids) &&
        exists("register_figure_status", mode = "function", inherits = TRUE)) {
      for (id in ids) {
        register_figure_status(id, "nao_gerada", msg)
      }
    }

    if (exists("figure_log", mode = "function", inherits = TRUE)) {
      figure_log(msg, level = "AVISO")
    } else {
      warning(msg, call. = FALSE)
    }

    invisible(FALSE)
  }
}

IDS_02 <- c("FigS1", "FigS2")
if (any_figure_selected(IDS_02) &&
    required_results_available(
      c("01_dados_e_filogenia.rds", "02_evolucao_dimorfismo.rds"),
      IDS_02, c("01_PREPARAR_DADOS_E_FILOGENIA.R", "02_EVOLUCAO_DIMORFISMO.R"))) {
  console_banner("FIGURAS DO SCRIPT 02", "Modelos Mk e matriz Q")
  prepared01 <- read_saved_result("01_dados_e_filogenia.rds")
  result02 <- read_saved_result("02_evolucao_dimorfismo.rds")
  safe_figure("FigS1", SOURCE_ENV$`02`$plot_model_comparison(result02$models$comparison))
  cfg <- figure_config("FigS2")
  safe_figure("FigS2", SOURCE_ENV$`02`$plot_transition_network(
    q_matrix = result02$Q$matrix, state_labels = STATE_LABELS,
    palette = PALETTE_DIMORPHISM, state_counts = prepared01$state_counts,
    scheme_name = "Tres estados", model_name = result02$models$best_model, stem = cfg$stem))
  rm(prepared01, result02, cfg); invisible(gc())
}

# ==============================================================================
# 10. GERACAO DAS FIGURAS DO SCRIPT 03
# ==============================================================================

# ---- S3 ----
FIGURAS[FIGURAS$id == "FigS3",  c("width","height","dpi","fit_a4")] <- list(width = 28, height = 28, dpi = 300, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS3B", c("width","height","dpi","fit_a4")] <- list(width = 28, height = 28, dpi = 300, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS3C", c("width","height","dpi","fit_a4")] <- list(width = 28, height = 28, dpi = 300, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS3D", c("width","height","dpi","fit_a4")] <- list(width = 28, height = 28, dpi = 300, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS3E", c("width","height","dpi","fit_a4")] <- list(width = 28, height = 28, dpi = 300, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS3_composta", c("width","height","dpi","fit_a4")] <- list(width = 16.5, height = 20.5, dpi = 320, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS4A_alta_res", c("width","height","dpi","fit_a4")] <- list(width = 8.27, height = 11.69, dpi = 600, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS4A_nomes_alta_res", c("width","height","dpi","fit_a4")] <- list(width = 8.27, height = 11.69, dpi = 600, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "Fig5_filogenia_alta_res", c("width","height","dpi","fit_a4")] <- list(width = 8.27, height = 11.69, dpi = 600, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "Fig5_legendas_A4", c("width","height","dpi","fit_a4")] <- list(width = 8.27, height = 11.69, dpi = 400, fit_a4 = FALSE)

IDS_S3 <- c("FigS3", "FigS3B", "FigS3C", "FigS3D", "FigS3E", "FigS3_composta", "FigS4A_alta_res", "FigS4A_nomes_alta_res", "Fig5_filogenia_alta_res", "Fig5_legendas_A4")
CONFIG_S3 <- list(espessura_ramos = 1.20)

CONFIG_S3F <- list(
  largura = 30, altura = 30,
  espessura_ramos = 1.45, cor_ramos = "#2B3A45",
  espessura_conectores = 0.20, cor_conectores = "grey45", opacidade_conectores = 0.45,
  espessura_anel = 5, meia_largura_anel = 0.52,
  paleta_familias = "Dynamic",
  raio_conectores = 1.025, raio_anel = 1.050,
  raio_especies = 1.020,
  raio_familias = 1.55, incremento_raio_familias = 0.095,
  raio_inicio_guias_familias = 1.34, recuo_texto_guias_familias = 0.036,
  separacao_angular_familias = 6.5 * pi / 180, max_niveis_familias = 8L,
  espessura_guias_familias = 0.70, cor_guias_familias = "grey40",
  opacidade_guias_familias = 0.72,
  limite_externo = 1.35,
  texto_especies_1000 = 0.24, texto_especies_700 = 0.28,
  texto_especies_400 = 0.36, texto_especies_menor = 0.50,
  cor_especies = "grey15", texto_familias = 0.78,
  margens = c(0.35, 0.35, 0.35, 0.35)
)

# ---- Fig5 / Fig5B ----
# OBS.: A ultima atualizacao da FigS4 foi preservada integralmente.
FIGURAS[FIGURAS$id == "Fig5", c("width","height","dpi","fit_a4")] <- list(
  width = 23.2, height = 15.2, dpi = 220, fit_a4 = FALSE
)
FIGURAS[FIGURAS$id == "Fig5B", c("width","height","dpi","fit_a4")] <- list(
  width = 9.2, height = 5.2, dpi = 300, fit_a4 = FALSE
)

CONFIG_FIG1 <- list(
  espaco_entre_aneis = 0.004,
  espessura_aneis = 0.022,
  distancia_da_arvore = 1.005,
  espessura_ramos = 0.85,
  folga_linhas_familias = 0.012,
  espessura_linhas_familias = 0.95,
  tamanho_rotulos_familias = 1.30,
  mostrar_guia_aneis = FALSE, tamanho_texto_guia_aneis = 0.92,
  mostrar_painel_familias = FALSE, tamanho_texto_painel_familias = 1.02
)

CONFIG_FIG1B <- list(espessura_linhas = 2.5, tamanho_numeros = 1.38, tamanho_rotulos = 1.18)

# ---- Fig4_familias ----
FIGURAS[FIGURAS$id == "Fig4_familias", c("width","height","dpi","fit_a4")] <- list(
  width = 10.8, height = 6.6, dpi = 300, fit_a4 = TRUE
)

# ---- FigS4_filogenias / FigS4B ----
FIGURAS[FIGURAS$id == "FigS4_filogenias", c("width","height","dpi","fit_a4")] <- list(
  width = 8.27, height = 11.69, dpi = 600, fit_a4 = FALSE
)
FIGURAS[FIGURAS$id == "FigS4_nomes_especies", c("width","height","dpi","fit_a4")] <- list(
  width = 8.27, height = 11.69, dpi = 600, fit_a4 = FALSE
)
FIGURAS[FIGURAS$id == "FigS4B", c("width","height","dpi","fit_a4")] <- list(
  width = 11.69, height = 8.27, dpi = 300, fit_a4 = FALSE
)

IDS_03 <- c("FigS3", "FigS3B", "FigS3C", "FigS3D", "FigS3E", "FigS3_composta", "FigS4A_alta_res", "FigS4A_nomes_alta_res", "Fig5_filogenia_alta_res", "Fig5_legendas_A4",
            "Fig5", "Fig5B", "Fig4_familias", "FigS4_filogenias",
            "FigS4_nomes_especies", "FigS4B")

padronizar_arvore_s3 <- function(tree, edge_probabilities = NULL) {
  tree_original <- tree
  tree_padronizada <- ape::ladderize(tree, right = FALSE)
  probs_padronizadas <- edge_probabilities
  if (!is.null(edge_probabilities)) {
    if (nrow(edge_probabilities) != nrow(tree_original$edge)) {
      stop("Numero de probabilidades diferente do numero de ramos.", call. = FALSE)
    }
    chave_original <- paste(tree_original$edge[, 1], tree_original$edge[, 2], sep = ">")
    chave_padronizada <- paste(tree_padronizada$edge[, 1], tree_padronizada$edge[, 2], sep = ">")
    ordem_probabilidades <- match(chave_padronizada, chave_original)
    if (anyNA(ordem_probabilidades)) stop("Falha ao realinhar probabilidades.", call. = FALSE)
    probs_padronizadas <- edge_probabilities[ordem_probabilidades, , drop = FALSE]
  }
  list(tree = tree_padronizada, edge_probabilities = probs_padronizadas)
}

afinar_funcao_filogenia <- function(fun, largura = 0.70, reforcar_cores = TRUE) {
  codigo <- paste(deparse(body(fun), width.cutoff = 500L), collapse = "\n")

  # Aumenta a espessura dos ramos
  codigo_novo <- gsub(
    "edge\\.width\\s*=\\s*[0-9.]+",
    paste0("edge.width = ", largura),
    codigo,
    perl = TRUE
  )

  # Reforca a visibilidade das cores dos ramos,
  # reduzindo a transparência excessiva ligada à incerteza.
  if (isTRUE(reforcar_cores)) {
    codigo_novo <- gsub(
      "alpha\\.f\\s*=\\s*0\\.52\\s*\\+\\s*0\\.43\\s*\\*\\s*certainty",
      "alpha.f = 0.94 + 0.06 * certainty",
      codigo_novo,
      perl = TRUE
    )
    codigo_novo <- gsub(
      "alpha\\.f\\s*=\\s*0\\.52\\s*\\+\\s*0\\.43\\s*\\*\\s*confidence",
      "alpha.f = 0.94 + 0.06 * confidence",
      codigo_novo,
      perl = TRUE
    )
  }

  if (identical(codigo, codigo_novo)) {
    warning("Nenhuma alteracao encontrada na funcao de filogenia.")
  }

  body(fun) <- parse(text = codigo_novo)[[1]]
  fun
}

SOURCE_ENV$`03`$plot_phylogeny_summary <- afinar_funcao_filogenia(
  SOURCE_ENV$`03`$plot_phylogeny_summary,
  largura = CONFIG_S3$espessura_ramos,
  reforcar_cores = TRUE
)
SOURCE_ENV$`03`$plot_phylogeny_trait_colored <- afinar_funcao_filogenia(
  SOURCE_ENV$`03`$plot_phylogeny_trait_colored,
  largura = CONFIG_S3$espessura_ramos,
  reforcar_cores = TRUE
)

# ==============================================================================
# FigS3F - apenas arvore + nomes das especies (sem anel de familias)
# ==============================================================================
plot_s3_referencia <- function(tree, analysis_data) {
  if (!"species" %in% names(analysis_data)) {
    stop("A coluna 'species' nao foi encontrada em analysis_data.", call. = FALSE)
  }
  tree_ref <- ape::ladderize(tree, right = FALSE)
  n_tip <- ape::Ntip(tree_ref)

  cex_species <- if (n_tip >= 1000) CONFIG_S3F$texto_especies_1000
  else if (n_tip >= 700) CONFIG_S3F$texto_especies_700
  else if (n_tip >= 400) CONFIG_S3F$texto_especies_400
  else CONFIG_S3F$texto_especies_menor

  profundidade <- ape::node.depth.edgelength(tree_ref)
  raio_arvore <- max(profundidade[seq_len(n_tip)], na.rm = TRUE)
  limite <- raio_arvore * CONFIG_S3F$limite_externo

  old_par <- graphics::par(mar = CONFIG_S3F$margens, xpd = NA)
  on.exit(graphics::par(old_par), add = TRUE)

  ape::plot.phylo(tree_ref, type = "fan", show.tip.label = FALSE,
    edge.width = CONFIG_S3F$espessura_ramos,
    edge.color = CONFIG_S3F$cor_ramos,
    no.margin = TRUE,
    x.lim = c(-limite, limite), y.lim = c(-limite, limite))

  ultimo_plot <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  x_tip <- ultimo_plot$xx[seq_len(n_tip)]
  y_tip <- ultimo_plot$yy[seq_len(n_tip)]
  angulo <- atan2(y_tip, x_tip)
  angulo[angulo < 0] <- angulo[angulo < 0] + 2 * pi
  raio_tip <- sqrt(x_tip^2 + y_tip^2)
  raio_max <- max(raio_tip, na.rm = TRUE)
  raio_species <- raio_max * CONFIG_S3F$raio_especies

  nomes_especies <- gsub("_", " ", tree_ref$tip.label, fixed = TRUE)
  for (i in seq_len(n_tip)) {
    ang_deg <- angulo[i] * 180 / pi
    x_text <- raio_species * cos(angulo[i])
    y_text <- raio_species * sin(angulo[i])
    if (ang_deg > 90 && ang_deg < 270) {
      rotacao <- ang_deg + 180; alinhamento <- c(1, 0.5)
    } else {
      rotacao <- ang_deg; alinhamento <- c(0, 0.5)
    }
    graphics::text(x = x_text, y = y_text, labels = nomes_especies[i],
      srt = rotacao, adj = alinhamento, cex = cex_species, font = 3,
      col = CONFIG_S3F$cor_especies)
  }
  invisible(NULL)
}

save_s3_reference <- function(plot_function,
    stem = "FigS3F. Reference phylogeny with species and families",
    width = CONFIG_S3F$largura, height = CONFIG_S3F$altura) {
  dir.create(PATHS$figures_supp, recursive = TRUE, showWarnings = FALSE)
  output_files <- character()
  for (formato in tolower(CONFIGURACAO_GERAL$formatos)) {
    output_path <- file.path(PATHS$figures_supp, paste0(stem, ".", formato))
    if (file.exists(output_path) && !isTRUE(SOBRESCREVER_FIGURAS)) {
      output_files <- c(output_files, output_path); next
    }
    device_open <- FALSE
    tryCatch({
      if (formato == "pdf") {
        if (capabilities("cairo")) {
          grDevices::cairo_pdf(windows_safe_path(output_path), width = width,
            height = height, bg = CONFIGURACAO_GERAL$fundo)
        } else {
          grDevices::pdf(windows_safe_path(output_path), width = width,
            height = height, useDingbats = FALSE, bg = CONFIGURACAO_GERAL$fundo)
        }
      } else if (formato == "png") {
        grDevices::png(windows_safe_path(output_path), width = width,
          height = height, units = "in", res = CONFIGURACAO_GERAL$dpi_padrao,
          bg = CONFIGURACAO_GERAL$fundo)
      } else stop("Formato nao suportado: ", formato)
      device_open <- TRUE; plot_function()
    }, finally = { if (device_open) grDevices::dev.off() })
    output_files <- c(output_files, output_path)
  }
  register_figure_status("FigS3F", "gerada", paste(output_files, collapse = " | "))
  figure_log("Figura gerada: FigS3F")
  invisible(output_files)
}


# ==============================================================================
# FigS3_composta - placa unica com as 6 figuras S3
# ==============================================================================
crop_png_array <- function(img, top = 0, bottom = 0, left = 0, right = 0) {
  nr <- dim(img)[1]
  nc <- dim(img)[2]

  r1 <- max(1L, floor(nr * top) + 1L)
  r2 <- min(nr, ceiling(nr * (1 - bottom)))
  c1 <- max(1L, floor(nc * left) + 1L)
  c2 <- min(nc, ceiling(nc * (1 - right)))

  img[r1:r2, c1:c2, , drop = FALSE]
}

draw_discrete_legend_grid <- function(title, labels, colors,
                                      title_size = 10.5,
                                      text_size = 7.8) {
  grid::grid.rect(gp = grid::gpar(col = NA, fill = NA))

  grid::grid.text(
    title,
    x = grid::unit(0, "npc"),
    y = grid::unit(1, "npc") - grid::unit(1.2, "mm"),
    just = c("left", "top"),
    gp = grid::gpar(fontface = "bold", fontsize = title_size, col = "#1F2D35")
  )

  labels <- unname(labels)
  colors <- unname(colors)

  n <- length(labels)
  y_top <- 0.82
  y_bottom <- 0.10

  y_pos <- if (n == 1L) {
    0.46
  } else {
    seq(y_top, y_bottom, length.out = n)
  }

  for (i in seq_len(n)) {
    grid::grid.rect(
      x = grid::unit(0.03, "npc"),
      y = grid::unit(y_pos[i], "npc"),
      width = grid::unit(0.050, "npc"),
      height = grid::unit(0.050, "npc"),
      just = c("left", "center"),
      gp = grid::gpar(fill = colors[i], col = NA)
    )

    grid::grid.text(
      labels[i],
      x = grid::unit(0.10, "npc"),
      y = grid::unit(y_pos[i], "npc"),
      just = c("left", "center"),
      gp = grid::gpar(fontsize = text_size, col = "#263238")
    )
  }

  invisible(NULL)
}

compose_s3_panel <- function(stem,
                             width = 16.5,
                             height = 20.5,
                             directory = PATHS$figures_supp) {

  if (!requireNamespace("png", quietly = TRUE)) {
    stop(
      "O pacote 'png' e necessario para gerar FigS3_composta. ",
      "Instale com install.packages('png').",
      call. = FALSE
    )
  }

  panel_specs <- list(
    list(id = "FigS3F",
         path = file.path(directory,
           "FigS3F. Reference phylogeny with species and families.png"),
         top = 0.02, bottom = 0.02, left = 0.02, right = 0.02),
    list(id = "FigS3",
         path = file.path(directory,
           paste0(figure_config("FigS3")$stem, ".png")),
         top = 0.03, bottom = 0.17, left = 0.03, right = 0.03),
    list(id = "FigS3B",
         path = file.path(directory,
           paste0(figure_config("FigS3B")$stem, ".png")),
         top = 0.03, bottom = 0.17, left = 0.03, right = 0.03),
    list(id = "FigS3C",
         path = file.path(directory,
           paste0(figure_config("FigS3C")$stem, ".png")),
         top = 0.03, bottom = 0.17, left = 0.03, right = 0.03),
    list(id = "FigS3D",
         path = file.path(directory,
           paste0(figure_config("FigS3D")$stem, ".png")),
         top = 0.03, bottom = 0.17, left = 0.03, right = 0.03),
    list(id = "FigS3E",
         path = file.path(directory,
           paste0(figure_config("FigS3E")$stem, ".png")),
         top = 0.03, bottom = 0.17, left = 0.03, right = 0.03)
  )

  missing_paths <- vapply(panel_specs, function(x) !file.exists(x$path), logical(1))
  if (any(missing_paths)) {
    stop(
      "As seguintes imagens nao foram encontradas para montar FigS3_composta:\n",
      paste(vapply(panel_specs[missing_paths], `[[`, "", "path"), collapse = "\n"),
      "\nGere primeiro FigS3, FigS3B, FigS3C, FigS3D, FigS3E e FigS3F.",
      call. = FALSE
    )
  }

  imgs <- lapply(panel_specs, function(spec) {
    img <- png::readPNG(spec$path)
    crop_png_array(
      img,
      top = spec$top,
      bottom = spec$bottom,
      left = spec$left,
      right = spec$right
    )
  })

  draw_plate <- function() {
    grid::grid.newpage()

    lay <- grid::grid.layout(
      nrow = 4,
      ncol = 3,
      heights = grid::unit(c(1.35, 1.35, 0.82, 0.82), "null"),
      widths = grid::unit(c(1, 1, 1), "null")
    )

    grid::pushViewport(grid::viewport(layout = lay))

    panel_positions <- list(
      c(1, 1), c(1, 2), c(1, 3),
      c(2, 1), c(2, 2), c(2, 3)
    )

    for (i in seq_along(imgs)) {
      rc <- panel_positions[[i]]
      grid::pushViewport(
        grid::viewport(layout.pos.row = rc[1], layout.pos.col = rc[2])
      )
      grid::grid.rect(gp = grid::gpar(fill = NA, col = "#D7DDE2", lwd = 0.6))
      grid::grid.raster(
        imgs[[i]],
        x = 0.5, y = 0.5,
        width = grid::unit(0.98, "npc"),
        height = grid::unit(0.98, "npc"),
        interpolate = TRUE
      )
      grid::upViewport()
    }

    # Linha 3 - primeiras tres legendas
    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 1))
    draw_discrete_legend_grid(
      title = "SIMMAP branches",
      labels = c("Monomorphic", "Hemidimorphic", "Holodimorphic"),
      colors = c("#8A97A3", PALETTE_DIMORPHISM["1"], PALETTE_DIMORPHISM["2"]),
      title_size = 10.0, text_size = 7.7
    )
    grid::upViewport()

    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 2))
    draw_discrete_legend_grid(
      title = "Pattern type",
      labels = unname(STATE_LABELS),
      colors = unname(PALETTE_DIMORPHISM[names(STATE_LABELS)]),
      title_size = 10.0, text_size = 7.7
    )
    grid::upViewport()

    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 3))
    draw_discrete_legend_grid(
      title = "Fertile frond architecture",
      labels = unname(ARCHITECTURE_LABELS),
      colors = unname(PALETA_ARQUITETURA[names(ARCHITECTURE_LABELS)]),
      title_size = 10.0, text_size = 7.2
    )
    grid::upViewport()

    # Linha 4 - ultimas tres legendas
    grid::pushViewport(grid::viewport(layout.pos.row = 4, layout.pos.col = 1))
    draw_discrete_legend_grid(
      title = "Morphological variation fertile",
      labels = unname(VARIATION_LABELS),
      colors = unname(PALETA_VARIACAO[names(VARIATION_LABELS)]),
      title_size = 10.0, text_size = 7.2
    )
    grid::upViewport()

    grid::pushViewport(grid::viewport(layout.pos.row = 4, layout.pos.col = 2))
    draw_discrete_legend_grid(
      title = "Size ratio",
      labels = unname(SIZE_LABELS),
      colors = unname(PALETA_TAMANHO[names(SIZE_LABELS)]),
      title_size = 10.0, text_size = 7.7
    )
    grid::upViewport()

    grid::pushViewport(grid::viewport(layout.pos.row = 4, layout.pos.col = 3))
    draw_discrete_legend_grid(
      title = "Habit",
      labels = unname(HABIT_LABELS),
      colors = unname(PALETA_HABITO[names(HABIT_LABELS)]),
      title_size = 10.0, text_size = 7.2
    )
    grid::upViewport()

    grid::upViewport()
  }

  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    directory = directory,
    fit_to_a4 = FALSE,
    plot_function = draw_plate
  )
}




# ==============================================================================
# EXPORTACAO EXTRA 1 - FIGS4 PAINEL A EM ALTA RESOLUCAO
# ==============================================================================
plot_s4_panel_a_highres <- function(
    tree,
    analysis_data,
    stem,
    width = 8.27,
    height = 11.69,
    directory = PATHS$figures_supp) {

  required_columns <- c(
    "species", "family", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho", "Habito"
  )

  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop(
      "Colunas ausentes para FigS4A_alta_res: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  plot_data <- analysis_data[
    as.character(analysis_data$Padrao_Dimorfismo) %in% c("1", "2"),
    , drop = FALSE
  ]

  plot_data$species <- as.character(plot_data$species)
  plot_data$family  <- as.character(plot_data$family)

  common_species <- intersect(tree$tip.label, plot_data$species)
  if (length(common_species) < 3L) {
    stop("Poucas especies hemi/holo em comum com a arvore.", call. = FALSE)
  }

  tree_sub <- ape::drop.tip(tree, setdiff(tree$tip.label, common_species))
  tree_sub <- ape::ladderize(tree_sub, right = FALSE)

  plot_data <- plot_data[
    match(tree_sub$tip.label, plot_data$species),
    , drop = FALSE
  ]

  if (anyNA(plot_data$species) || anyNA(plot_data$family)) {
    stop("Falha ao alinhar a arvore e a matriz de dados da FigS4A_alta_res.",
         call. = FALSE)
  }

  n_tip <- ape::Ntip(tree_sub)
  tip_states <- as.character(plot_data$Padrao_Dimorfismo)
  terminal_rows <- match(seq_len(n_tip), tree_sub$edge[, 2])

  branch_colors <- rep("#8A97A3", nrow(tree_sub$edge))
  branch_colors[terminal_rows] <- unname(PALETTE_DIMORPHISM[tip_states])
  branch_colors[is.na(branch_colors)] <- "#8A97A3"

  tracks <- list(
    list(values = as.character(plot_data$Padrao_Dimorfismo), palette = PALETTE_DIMORPHISM),
    list(values = as.character(plot_data$Arquitetura_Lamina), palette = PALETA_ARQUITETURA),
    list(values = as.character(plot_data$Variacao_Morfologica), palette = PALETA_VARIACAO),
    list(values = as.character(plot_data$Relacao_Tamanho), palette = PALETA_TAMANHO),
    list(values = as.character(plot_data$Habito), palette = PALETA_HABITO)
  )

  draw_semicircular_track <- function(angles, inner_radius, outer_radius, colors) {
    angle_order <- order(angles)
    ordered_angles <- angles[angle_order]
    n_local <- length(ordered_angles)

    if (n_local == 1L) {
      left_boundaries <- 0
      right_boundaries <- pi
    } else {
      middle_boundaries <- (ordered_angles[-1L] + ordered_angles[-n_local]) / 2
      first_gap <- (ordered_angles[2L] - ordered_angles[1L]) / 2
      last_gap <- (ordered_angles[n_local] - ordered_angles[n_local - 1L]) / 2
      left_boundaries <- c(max(0, ordered_angles[1L] - first_gap), middle_boundaries)
      right_boundaries <- c(middle_boundaries, min(pi, ordered_angles[n_local] + last_gap))
    }

    overlap <- 1e-5

    for (j in seq_len(n_local)) {
      theta <- seq(left_boundaries[j] - overlap,
                   right_boundaries[j] + overlap,
                   length.out = 12L)
      theta <- pmax(0, pmin(pi, theta))
      color_j <- colors[angle_order[j]]
      if (is.na(color_j) || !nzchar(color_j)) color_j <- "#F3F4F6"

      graphics::polygon(
        x = c(cos(theta) * outer_radius, rev(cos(theta) * inner_radius)),
        y = c(sin(theta) * outer_radius, rev(sin(theta) * inner_radius)),
        col = color_j, border = NA
      )
    }

    invisible(NULL)
  }

  draw_panel <- function() {
    # Margens praticamente zeradas e eixos sem expansao automatica.
    # Isso faz a meia-lua usar quase toda a largura util da pagina.
    graphics::par(
      mar = c(0.02, 0.02, 0.02, 0.02),
      xpd = NA,
      xaxs = "i",
      yaxs = "i"
    )

    tree_height <- max(ape::node.depth.edgelength(tree_sub)[seq_len(ape::Ntip(tree_sub))], na.rm = TRUE)
    if (!is.finite(tree_height) || tree_height <= 0) tree_height <- 1

    # Limite lateral bem mais justo: o anel de familias fica proximo das
    # margens esquerda e direita, preservando uma pequena folga de seguranca
    # para os numeros externos das familias.
    horizontal_limit <- tree_height * 1.32
    vertical_top <- tree_height * 1.98

    ape::plot.phylo(
      tree_sub,
      type = "fan",
      open.angle = 180,
      rotate.tree = 0,
      show.tip.label = FALSE,
      use.edge.length = TRUE,
      edge.color = branch_colors,
      edge.width = 0.76,
      no.margin = FALSE,
      x.lim = c(-horizontal_limit, horizontal_limit),
      y.lim = c(-tree_height * 0.12, vertical_top),
      main = ""
    )

    last_plot <- get("last_plot.phylo", envir = .PlotPhyloEnv)

    tip_x <- last_plot$xx[seq_len(n_tip)]
    tip_y <- last_plot$yy[seq_len(n_tip)]

    tip_radius <- sqrt(tip_x^2 + tip_y^2)
    tip_angle <- atan2(tip_y, tip_x)
    tip_angle[tip_angle < 0] <- tip_angle[tip_angle < 0] + 2 * pi
    tip_angle <- pmax(0, pmin(pi, tip_angle))

    max_tip_radius <- max(tip_radius, na.rm = TRUE)
    if (!is.finite(max_tip_radius) || max_tip_radius <= 0) max_tip_radius <- tree_height

    ring_gap   <- max_tip_radius * 0.0060
    ring_width <- max_tip_radius * 0.0340
    ring_start <- max_tip_radius * 1.028

    graphics::segments(
      x0 = tip_x,
      y0 = tip_y,
      x1 = cos(tip_angle) * (ring_start - max_tip_radius * 0.008),
      y1 = sin(tip_angle) * (ring_start - max_tip_radius * 0.008),
      col = "#CBD2D8",
      lwd = 0.38
    )

    for (track_index in seq_along(tracks)) {
      inner_radius <- ring_start + (track_index - 1L) * (ring_width + ring_gap)
      outer_radius <- inner_radius + ring_width
      track_colors <- unname(tracks[[track_index]]$palette[as.character(tracks[[track_index]]$values)])
      track_colors[is.na(track_colors)] <- "#F3F4F6"

      draw_semicircular_track(
        angles = tip_angle,
        inner_radius = inner_radius,
        outer_radius = outer_radius,
        colors = track_colors
      )
    }

    family_inner <- ring_start + length(tracks) * (ring_width + ring_gap)
    family_outer <- family_inner + ring_width * 1.14

    tip_order_right_to_left <- order(tip_angle, decreasing = FALSE, na.last = NA)
    family_order_local <- unique(as.character(plot_data$family[tip_order_right_to_left]))
    family_order_local <- family_order_local[!is.na(family_order_local) & nzchar(family_order_local)]
    family_numbers_local <- setNames(seq_along(family_order_local), family_order_local)

    family_fill <- setNames(
      rep(c("#D7E0E6", "#B8C6CF", "#E3E8EC"), length.out = length(family_order_local)),
      family_order_local
    )

    family_colors_tip <- unname(family_fill[as.character(plot_data$family)])
    family_colors_tip[is.na(family_colors_tip)] <- "#D7E0E6"

    draw_semicircular_track(
      angles = tip_angle,
      inner_radius = family_inner,
      outer_radius = family_outer,
      colors = family_colors_tip
    )

    family_number_cex <- if (n_tip >= 230L) 0.48 else 0.56

    # Numeros das familias fora do anel, com linhas curtas ate o rotulo.
    family_mid_angles <- vapply(family_order_local, function(fam) {
      idx <- which(as.character(plot_data$family) == fam)
      fam_angles <- tip_angle[idx]
      ang <- atan2(mean(sin(fam_angles)), mean(cos(fam_angles)))
      if (ang < 0) ang <- ang + 2 * pi
      pmax(0, pmin(pi, ang))
    }, numeric(1))

    nearest_gap <- rep(pi, length(family_order_local))
    if (length(family_mid_angles) > 1L) {
      for (i in seq_along(family_mid_angles)) {
        d <- abs(family_mid_angles[i] - family_mid_angles)
        d[i] <- Inf
        nearest_gap[i] <- min(d, na.rm = TRUE)
      }
    }

    family_level <- integer(length(family_order_local))
    crowded_idx <- which(nearest_gap < (7.0 * pi / 180))
    if (length(crowded_idx) > 0L) {
      ord <- crowded_idx[order(family_mid_angles[crowded_idx])]
      family_level[ord] <- (seq_along(ord) - 1L) %% 4L
    }

    manual_extra <- rep(0, length(family_order_local))
    if (length(family_order_local) >= 24L) {
      manual_extra[10] <- 0.000
      manual_extra[11] <- 0.060
      manual_extra[21] <- 0.000
      manual_extra[22] <- 0.060
      manual_extra[23] <- 0.120
      manual_extra[24] <- 0.180
    }

    family_label_radius <- family_outer + max_tip_radius * (
      0.020 +
      0.016 * family_level +
      manual_extra
    )

    connector_start <- family_outer + max_tip_radius * 0.004

    for (k in seq_along(family_order_local)) {
      fam <- family_order_local[k]
      fam_angle <- family_mid_angles[k]
      angle_deg <- fam_angle * 180 / pi

      text_rotation <- angle_deg - 90
      text_x <- cos(fam_angle) * family_label_radius[k]
      text_y <- sin(fam_angle) * family_label_radius[k]

      graphics::segments(
        x0 = cos(fam_angle) * connector_start,
        y0 = sin(fam_angle) * connector_start,
        x1 = text_x,
        y1 = text_y,
        col = "#546874",
        lwd = 0.55,
        lend = "round"
      )

      graphics::text(
        x = text_x,
        y = text_y,
        labels = family_numbers_local[fam],
        srt = text_rotation,
        adj = c(0.5, 0.5),
        cex = family_number_cex,
        font = 2,
        col = "#263238"
      )
    }

    # Numeracao lateral dos aneis removida a pedido da usuaria.

    invisible(NULL)
  }

  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    directory = directory,
    fit_to_a4 = FALSE,
    plot_function = draw_panel
  )
}

# ==============================================================================
# EXPORTACAO EXTRA 2 - FIG5 FILOGENIA ISOLADA EM ALTA RESOLUCAO
# ==============================================================================

# ==============================================================================
# EXPORTACAO EXTRA 1B - FIGS4 PAINEL A COM NOMES DAS ESPECIES
# ==============================================================================
plot_s4_panel_a_highres_species <- function(
    tree,
    analysis_data,
    stem,
    width = 8.27,
    height = 11.69,
    directory = PATHS$figures_supp) {

  required_columns <- c(
    "species", "family", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho", "Habito"
  )

  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop(
      "Colunas ausentes para FigS4A_nomes_alta_res: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  plot_data <- analysis_data[
    as.character(analysis_data$Padrao_Dimorfismo) %in% c("1", "2"),
    , drop = FALSE
  ]

  plot_data$species <- as.character(plot_data$species)
  common_species <- intersect(tree$tip.label, plot_data$species)
  if (length(common_species) < 3L) {
    stop("Poucas especies hemi/holo em comum com a arvore.", call. = FALSE)
  }

  tree_sub <- ape::drop.tip(tree, setdiff(tree$tip.label, common_species))
  tree_sub <- ape::ladderize(tree_sub, right = FALSE)

  plot_data <- plot_data[
    match(tree_sub$tip.label, plot_data$species),
    , drop = FALSE
  ]

  if (anyNA(plot_data$species)) {
    stop("Falha ao alinhar a arvore e a matriz de dados da FigS4A_nomes_alta_res.",
         call. = FALSE)
  }

  n_tip <- ape::Ntip(tree_sub)
  tip_states <- as.character(plot_data$Padrao_Dimorfismo)
  terminal_rows <- match(seq_len(n_tip), tree_sub$edge[, 2])

  branch_colors <- rep("#8A97A3", nrow(tree_sub$edge))
  branch_colors[terminal_rows] <- unname(PALETTE_DIMORPHISM[tip_states])
  branch_colors[is.na(branch_colors)] <- "#8A97A3"

  tracks <- list(
    list(values = as.character(plot_data$Padrao_Dimorfismo), palette = PALETTE_DIMORPHISM),
    list(values = as.character(plot_data$Arquitetura_Lamina), palette = PALETA_ARQUITETURA),
    list(values = as.character(plot_data$Variacao_Morfologica), palette = PALETA_VARIACAO),
    list(values = as.character(plot_data$Relacao_Tamanho), palette = PALETA_TAMANHO),
    list(values = as.character(plot_data$Habito), palette = PALETA_HABITO)
  )

  draw_semicircular_track <- function(angles, inner_radius, outer_radius, colors) {
    angle_order <- order(angles)
    ordered_angles <- angles[angle_order]
    n_local <- length(ordered_angles)

    if (n_local == 1L) {
      left_boundaries <- 0
      right_boundaries <- pi
    } else {
      middle_boundaries <- (ordered_angles[-1L] + ordered_angles[-n_local]) / 2
      first_gap <- (ordered_angles[2L] - ordered_angles[1L]) / 2
      last_gap <- (ordered_angles[n_local] - ordered_angles[n_local - 1L]) / 2
      left_boundaries <- c(max(0, ordered_angles[1L] - first_gap), middle_boundaries)
      right_boundaries <- c(middle_boundaries, min(pi, ordered_angles[n_local] + last_gap))
    }

    overlap <- 1e-5

    for (j in seq_len(n_local)) {
      theta <- seq(left_boundaries[j] - overlap,
                   right_boundaries[j] + overlap,
                   length.out = 12L)
      theta <- pmax(0, pmin(pi, theta))
      color_j <- colors[angle_order[j]]
      if (is.na(color_j) || !nzchar(color_j)) color_j <- "#F3F4F6"

      graphics::polygon(
        x = c(cos(theta) * outer_radius, rev(cos(theta) * inner_radius)),
        y = c(sin(theta) * outer_radius, rev(sin(theta) * inner_radius)),
        col = color_j, border = NA
      )
    }

    invisible(NULL)
  }

  draw_panel <- function() {
    # Mesma logica compacta do painel sem nomes, mas com folga extra para os nomes.
    graphics::par(
      mar = c(0.02, 0.02, 0.02, 0.02),
      xpd = NA,
      xaxs = "i",
      yaxs = "i"
    )

    tree_height <- max(ape::node.depth.edgelength(tree_sub)[seq_len(ape::Ntip(tree_sub))], na.rm = TRUE)
    if (!is.finite(tree_height) || tree_height <= 0) tree_height <- 1

    # Versao com nomes: mantem a meia-lua bem aberta, mas com mais espaco lateral
    # do que o painel sem nomes para acomodar os rotulos das especies.
    horizontal_limit <- tree_height * 1.92
    vertical_top <- tree_height * 1.90

    ape::plot.phylo(
      tree_sub,
      type = "fan",
      open.angle = 180,
      rotate.tree = 0,
      show.tip.label = FALSE,
      use.edge.length = TRUE,
      edge.color = branch_colors,
      edge.width = 0.76,
      no.margin = FALSE,
      x.lim = c(-horizontal_limit, horizontal_limit),
      y.lim = c(-tree_height * 0.08, vertical_top),
      main = ""
    )

    last_plot <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    tip_x <- last_plot$xx[seq_len(n_tip)]
    tip_y <- last_plot$yy[seq_len(n_tip)]

    tip_radius <- sqrt(tip_x^2 + tip_y^2)
    tip_angle <- atan2(tip_y, tip_x)
    tip_angle[tip_angle < 0] <- tip_angle[tip_angle < 0] + 2 * pi
    tip_angle <- pmax(0, pmin(pi, tip_angle))

    max_tip_radius <- max(tip_radius, na.rm = TRUE)
    if (!is.finite(max_tip_radius) || max_tip_radius <= 0) max_tip_radius <- tree_height

    ring_gap   <- max_tip_radius * 0.0060
    ring_width <- max_tip_radius * 0.0340
    ring_start <- max_tip_radius * 1.028

    graphics::segments(
      x0 = tip_x,
      y0 = tip_y,
      x1 = cos(tip_angle) * (ring_start - max_tip_radius * 0.008),
      y1 = sin(tip_angle) * (ring_start - max_tip_radius * 0.008),
      col = "#CBD2D8",
      lwd = 0.38
    )

    for (track_index in seq_along(tracks)) {
      inner_radius <- ring_start + (track_index - 1L) * (ring_width + ring_gap)
      outer_radius <- inner_radius + ring_width
      track_colors <- unname(tracks[[track_index]]$palette[as.character(tracks[[track_index]]$values)])
      track_colors[is.na(track_colors)] <- "#F3F4F6"

      draw_semicircular_track(
        angles = tip_angle,
        inner_radius = inner_radius,
        outer_radius = outer_radius,
        colors = track_colors
      )
    }

    species_names <- gsub("_", " ", tree_sub$tip.label, fixed = TRUE)

    # Solucao mais limpa:
    # duas faixas externas alinhadas (nao varios niveis irregulares e
    # nao repel angular exagerado). Isso reduz a sobreposicao e evita
    # aquela distorcao nas extremidades.
    if (n_tip >= 250L) {
      species_cex <- 0.28
      n_tiers <- 2L
      tier_gap <- max_tip_radius * 0.052
    } else if (n_tip >= 200L) {
      species_cex <- 0.31
      n_tiers <- 2L
      tier_gap <- max_tip_radius * 0.055
    } else if (n_tip >= 160L) {
      species_cex <- 0.35
      n_tiers <- 2L
      tier_gap <- max_tip_radius * 0.058
    } else {
      species_cex <- 0.40
      n_tiers <- 2L
      tier_gap <- max_tip_radius * 0.062
    }

    label_base_radius <- ring_start + length(tracks) * (ring_width + ring_gap) + max_tip_radius * 0.10

    tip_angle_order <- order(tip_angle)
    tier_by_tip <- integer(n_tip)
    tier_by_tip[tip_angle_order] <- (seq_len(n_tip) - 1L) %% n_tiers

    label_radius <- label_base_radius + tier_by_tip * tier_gap
    connector_start <- ring_start + length(tracks) * (ring_width + ring_gap) + max_tip_radius * 0.003

    for (i in seq_len(n_tip)) {
      angle <- tip_angle[i]
      angle_deg <- angle * 180 / pi

      text_x <- cos(angle) * label_radius[i]
      text_y <- sin(angle) * label_radius[i]

      graphics::segments(
        x0 = cos(angle) * connector_start,
        y0 = sin(angle) * connector_start,
        x1 = text_x,
        y1 = text_y,
        col = "#C1C8CE",
        lwd = 0.22,
        lty = 3
      )

      if (angle_deg > 90) {
        text_rotation <- angle_deg + 180
        horizontal_adj <- 1
      } else {
        text_rotation <- angle_deg
        horizontal_adj <- 0
      }

      graphics::text(
        x = text_x,
        y = text_y,
        labels = species_names[i],
        srt = text_rotation,
        adj = c(horizontal_adj, 0.5),
        cex = species_cex,
        font = 3,
        col = "#15191D",
        xpd = NA
      )
    }

    # Numeracao lateral dos aneis removida a pedido da usuaria.

    invisible(NULL)
  }

  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    directory = directory,
    fit_to_a4 = FALSE,
    plot_function = draw_panel
  )
}

plot_fig5_phylogeny_only <- function(
    tree,
    edge_probabilities,
    analysis_data,
    stem,
    width = 8.27,
    height = 11.69,
    directory = PATHS$figures_main) {

  required_columns <- c(
    "species", "family", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho", "Habito"
  )
  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop("Colunas ausentes para Fig5_filogenia_alta_res: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (nrow(edge_probabilities) != nrow(tree$edge)) {
    stop("As probabilidades dos ramos nao correspondem a arvore da Fig5.", call. = FALSE)
  }

  state_codes <- colnames(edge_probabilities)
  dominant_index <- max.col(edge_probabilities, ties.method = "first")
  dominant_state <- state_codes[dominant_index]
  confidence <- edge_probabilities[cbind(seq_len(nrow(edge_probabilities)), dominant_index)]
  confidence[!is.finite(confidence)] <- 0

  branch_colors <- mapply(
    function(color, certainty) {
      grDevices::adjustcolor(color, alpha.f = 0.52 + 0.43 * certainty)
    },
    unname(PALETTE_DIMORPHISM[dominant_state]),
    confidence,
    USE.NAMES = FALSE
  )

  plot_data <- analysis_data[match(tree$tip.label, analysis_data$species), , drop = FALSE]
  if (anyNA(plot_data$species) || anyNA(plot_data$family)) {
    stop("Falha ao alinhar dados e arvore na Fig5_filogenia_alta_res.", call. = FALSE)
  }

  terminal_rows_fig1 <- match(seq_len(ape::Ntip(tree)), tree$edge[, 2])
  terminal_states_fig1 <- as.character(plot_data$Padrao_Dimorfismo)
  branch_colors[terminal_rows_fig1] <- unname(PALETTE_DIMORPHISM[terminal_states_fig1])

  tracks <- list(
    list(values = as.character(plot_data$Padrao_Dimorfismo), palette = PALETTE_DIMORPHISM),
    list(values = as.character(plot_data$Arquitetura_Lamina), palette = PALETA_ARQUITETURA),
    list(values = as.character(plot_data$Variacao_Morfologica), palette = PALETA_VARIACAO),
    list(values = as.character(plot_data$Relacao_Tamanho), palette = PALETA_TAMANHO),
    list(values = as.character(plot_data$Habito), palette = PALETA_HABITO)
  )

  draw_continuous_annulus <- function(tip_angle_0_2pi, inner_radius, outer_radius, colors) {
    angle_order_local <- order(tip_angle_0_2pi)
    ordered_angles_local <- tip_angle_0_2pi[angle_order_local]
    n_tips_local <- length(ordered_angles_local)
    previous_angles <- c(ordered_angles_local[n_tips_local] - 2 * pi,
                         ordered_angles_local[-n_tips_local])
    next_angles <- c(ordered_angles_local[-1L], ordered_angles_local[1L] + 2 * pi)
    left_boundaries <- (previous_angles + ordered_angles_local) / 2
    right_boundaries <- (ordered_angles_local + next_angles) / 2
    overlap <- 1e-5

    for (j in seq_len(n_tips_local)) {
      theta <- seq(left_boundaries[j] - overlap, right_boundaries[j] + overlap, length.out = 10L)
      color_j <- colors[angle_order_local[j]]
      if (is.na(color_j) || !nzchar(color_j)) color_j <- "#F3F4F6"

      graphics::polygon(
        x = c(cos(theta) * outer_radius, rev(cos(theta) * inner_radius)),
        y = c(sin(theta) * outer_radius, rev(sin(theta) * inner_radius)),
        col = color_j,
        border = NA
      )
    }

    invisible(NULL)
  }

  draw_panel <- function() {
    graphics::par(mar = c(0.14, 0.14, 0.14, 0.14), xpd = NA)

    tree_height <- max(ape::node.depth.edgelength(tree)[seq_len(ape::Ntip(tree))])

    ring_gap   <- tree_height * 0.005
    ring_width <- tree_height * 0.034
    ring_start <- tree_height * 1.032

    family_width <- tree_height * 0.035
    family_inner <- ring_start + length(tracks) * ring_width + length(tracks) * ring_gap
    family_outer <- family_inner + family_width

    outer_limit <- tree_height * 2.18

    ape::plot.phylo(
      tree,
      type = "fan",
      show.tip.label = FALSE,
      edge.color = branch_colors,
      edge.width = 0.86,
      no.margin = FALSE,
      x.lim = c(-outer_limit, outer_limit),
      y.lim = c(-outer_limit, outer_limit),
      main = ""
    )

    last_plot <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    tip_x <- last_plot$xx[seq_len(ape::Ntip(tree))]
    tip_y <- last_plot$yy[seq_len(ape::Ntip(tree))]
    tip_radius <- sqrt(tip_x^2 + tip_y^2)
    unit_x <- tip_x / tip_radius
    unit_y <- tip_y / tip_radius
    tip_angle <- atan2(unit_y, unit_x)
    tip_angle_0_2pi <- (tip_angle + 2 * pi) %% (2 * pi)

    for (track_index in seq_along(tracks)) {
      inner_radius <- ring_start + (track_index - 1L) * (ring_width + ring_gap)
      outer_radius <- inner_radius + ring_width

      colors <- unname(tracks[[track_index]]$palette[tracks[[track_index]]$values])
      colors[is.na(colors)] <- "#F3F4F6"

      draw_continuous_annulus(
        tip_angle_0_2pi = tip_angle_0_2pi,
        inner_radius = inner_radius,
        outer_radius = outer_radius,
        colors = colors
      )
    }

    family_by_tip <- as.character(plot_data$family)
    angle_order <- order(tip_angle_0_2pi)
    family_order <- unique(family_by_tip[angle_order])
    n_families <- length(family_order)

    family_colors <- rep(c("#344955", "#657D89", "#9EADB4"), length.out = n_families)
    family_palette <- setNames(family_colors, family_order)

    draw_continuous_annulus(
      tip_angle_0_2pi = tip_angle_0_2pi,
      inner_radius = family_inner,
      outer_radius = family_outer,
      colors = unname(family_palette[family_by_tip])
    )

    ordered_angles <- tip_angle_0_2pi[angle_order]
    ordered_families <- family_by_tip[angle_order]
    next_index <- c(seq.int(2L, length(angle_order)), 1L)
    family_changes <- which(ordered_families != ordered_families[next_index])

    for (i in family_changes) {
      next_i <- next_index[i]
      angle_1 <- ordered_angles[i]
      angle_2 <- ordered_angles[next_i]
      if (next_i == 1L) angle_2 <- angle_2 + 2 * pi
      boundary_angle <- ((angle_1 + angle_2) / 2) %% (2 * pi)

      graphics::segments(
        cos(boundary_angle) * family_inner,
        sin(boundary_angle) * family_inner,
        cos(boundary_angle) * family_outer,
        sin(boundary_angle) * family_outer,
        col = "white",
        lwd = 0.9,
        lend = "butt"
      )
    }

    family_mid_angle <- vapply(family_order, function(family_name) {
      indices <- which(family_by_tip == family_name)
      atan2(mean(sin(tip_angle[indices])), mean(cos(tip_angle[indices])))
    }, numeric(1))

    label_level <- calcular_niveis_rotulos_circulares(
      family_mid_angle,
      separacao_min = 11 * pi / 180,
      max_niveis = 9L
    )

    angles_norm <- (family_mid_angle + 2 * pi) %% (2 * pi)
    angle_ord <- order(angles_norm)
    angles_sorted <- angles_norm[angle_ord]
    n_angle <- length(angles_sorted)

    if (n_angle > 1L) {
      prev_angles <- c(angles_sorted[n_angle] - 2 * pi, angles_sorted[-n_angle])
      next_angles <- c(angles_sorted[-1L], angles_sorted[1L] + 2 * pi)
      nearest_sorted <- pmin(angles_sorted - prev_angles, next_angles - angles_sorted)
      nearest_gap <- numeric(n_angle)
      nearest_gap[angle_ord] <- nearest_sorted
    } else {
      nearest_gap <- rep(pi, n_angle)
    }

    crowding_target <- 12 * pi / 180
    crowding <- pmax(0, pmin(1, (crowding_target - nearest_gap) / crowding_target))
    level_progression <- 0.0065 * (label_level ^ 1.35)

    label_radius <- family_outer + tree_height * (
      0.045 + 0.048 * label_level + level_progression + 0.040 * crowding
    )

    extra_radial <- rep(0, length(family_order))
    manual_offsets <- c(
      "4"  = 0.000, "5"  = 0.085,
      "10" = 0.000, "11" = 0.085,
      "18" = 0.000, "19" = 0.060, "20" = 0.120, "21" = 0.180,
      "25" = 0.000, "26" = 0.085,
      "32" = 0.000, "33" = 0.085
    )
    ids_manual <- intersect(names(manual_offsets), as.character(seq_along(family_order)))
    extra_radial[as.integer(ids_manual)] <- unname(manual_offsets[ids_manual])
    label_radius <- label_radius + tree_height * extra_radial

    connector_gap <- tree_height * 0.012

    for (i in seq_along(family_order)) {
      angle <- family_mid_angle[i]
      angle_degrees <- (angle * 180 / pi + 360) %% 360

      text_rotation <- angle_degrees
      text_adj <- c(0, 0.5)
      if (angle_degrees > 90 && angle_degrees < 270) {
        text_rotation <- angle_degrees + 180
        text_adj <- c(1, 0.5)
      }

      text_x <- cos(angle) * label_radius[i]
      text_y <- sin(angle) * label_radius[i]

      connector_start <- family_outer + connector_gap

      graphics::segments(
        x0 = cos(angle) * connector_start,
        y0 = sin(angle) * connector_start,
        x1 = text_x,
        y1 = text_y,
        col = "#546874",
        lwd = 0.70,
        lend = "round"
      )

      graphics::text(
        x = text_x,
        y = text_y,
        labels = sprintf("%02d", i),
        srt = text_rotation,
        adj = text_adj,
        cex = 0.90,
        font = 2,
        col = "#1F2D35",
        xpd = NA
      )
    }

    invisible(NULL)
  }

  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    directory = directory,
    fit_to_a4 = FALSE,
    plot_function = draw_panel
  )
}

plot_fig5_legends_only <- function(
    tree,
    analysis_data,
    stem,
    width = 8.27,
    height = 11.69,
    directory = PATHS$figures_main) {

  required_columns <- c(
    "species", "family", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho", "Habito"
  )
  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop("Colunas ausentes para Fig5_legendas_A4: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  plot_data <- analysis_data[match(tree$tip.label, analysis_data$species), , drop = FALSE]
  if (anyNA(plot_data$species) || anyNA(plot_data$family)) {
    stop("Falha ao alinhar dados e arvore na Fig5_legendas_A4.", call. = FALSE)
  }

  make_family_codes <- function(family_names) {
    stems <- toupper(sub("ACEAE$", "", family_names, ignore.case = TRUE))
    codes <- substr(stems, 1L, 3L)
    repeat {
      dup <- duplicated(codes) | duplicated(codes, fromLast = TRUE)
      if (!any(dup)) break
      previous <- codes
      codes[dup] <- vapply(which(dup), function(i) {
        width_i <- min(nchar(stems[i]), nchar(codes[i]) + 1L)
        substr(stems[i], 1L, width_i)
      }, character(1))
      if (identical(previous, codes)) { codes <- make.unique(codes, sep = ""); break }
    }
    setNames(codes, family_names)
  }

  get_family_order_fig5 <- function(tr, family_by_tip) {
    tmp_pdf <- tempfile(fileext = ".pdf")
    grDevices::pdf(tmp_pdf, width = 4, height = 4)
    on.exit({
      grDevices::dev.off()
      unlink(tmp_pdf)
    }, add = TRUE)

    ape::plot.phylo(
      tr,
      type = "fan",
      show.tip.label = FALSE,
      edge.color = "#FFFFFF",
      edge.width = 0.01,
      no.margin = TRUE
    )

    last_plot <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    tip_x <- last_plot$xx[seq_len(ape::Ntip(tr))]
    tip_y <- last_plot$yy[seq_len(ape::Ntip(tr))]
    tip_angle <- atan2(tip_y, tip_x)
    tip_angle_0_2pi <- (tip_angle + 2 * pi) %% (2 * pi)

    family_order <- unique(as.character(family_by_tip)[order(tip_angle_0_2pi)])
    family_order[!is.na(family_order) & nzchar(family_order)]
  }

  family_order <- get_family_order_fig5(tree, plot_data$family)
  family_codes <- make_family_codes(family_order)

  dim_labels <- c(
    "Monomorphic",
    "Hemidimorphic",
    "Holodimorphic"
  )
  dim_colors <- unname(PALETTE_DIMORPHISM[c("0", "1", "2")])

  arch_labels <- unname(ARCHITECTURE_LABELS)
  arch_colors <- unname(PALETA_ARQUITETURA[names(ARCHITECTURE_LABELS)])

  var_labels <- unname(VARIATION_LABELS)
  var_colors <- unname(PALETA_VARIACAO[names(VARIATION_LABELS)])

  size_labels <- unname(SIZE_LABELS)
  size_colors <- unname(PALETA_TAMANHO[names(SIZE_LABELS)])

  habit_labels <- unname(HABIT_LABELS)
  habit_colors <- unname(PALETA_HABITO[names(HABIT_LABELS)])

  draw_items <- function(x_left, x_box, y_top, title, labels, colors,
                         row_h = 0.022, title_gap = 0.015, box_w = 0.018,
                         cex_title = 1.00, cex_text = 0.90) {
    graphics::text(
      x_left, y_top, labels = title, adj = c(0, 1),
      cex = cex_title, font = 2, col = "#1F2D35"
    )
    y <- y_top - title_gap
    for (i in seq_along(labels)) {
      graphics::rect(
        x_box, y - row_h * 0.72, x_box + box_w, y - row_h * 0.20,
        col = colors[i], border = NA
      )
      graphics::text(
        x_box + box_w + 0.010, y - row_h * 0.46,
        labels = labels[i], adj = c(0, 0.5),
        cex = cex_text, col = "#20262C"
      )
      y <- y - row_h
    }
    y
  }

  draw_family_block <- function(x_left, y_top, codes, family_names,
                                ncol = 2L, row_h = 0.018, cex_title = 1.00,
                                cex_code = 0.88, cex_name = 0.84) {
    graphics::text(
      x_left, y_top,
      labels = "Families - abbreviations in the outer ring",
      adj = c(0, 1), cex = cex_title, font = 2, col = "#1F2D35"
    )

    y_start <- y_top - 0.018
    n <- length(family_names)
    ncol <- max(1L, ncol)
    nrow_col <- ceiling(n / ncol)
    col_width <- 0.46

    for (col_i in seq_len(ncol)) {
      idx_start <- (col_i - 1L) * nrow_col + 1L
      idx_end <- min(col_i * nrow_col, n)
      if (idx_start > idx_end) next

      y <- y_start
      x_code <- x_left + (col_i - 1L) * col_width
      x_name <- x_code + 0.060

      for (j in idx_start:idx_end) {
        graphics::text(
          x_code, y, labels = codes[j],
          adj = c(0, 1), cex = cex_code, font = 2, col = "#253844"
        )
        graphics::text(
          x_name, y, labels = family_names[j],
          adj = c(0, 1), cex = cex_name, col = "#20262C"
        )
        y <- y - row_h
      }
    }
  }

  draw_panel <- function() {
    graphics::par(mar = c(0.55, 0.55, 0.55, 0.55), xpd = NA)
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))

    graphics::text(
      0.5, 0.982,
      labels = "Fig. 5 - Legends only",
      cex = 1.20, font = 2, col = "#1B2830"
    )

    # Coluna esquerda
    y_left <- 0.955
    y_left <- draw_items(
      x_left = 0.06, x_box = 0.06, y_top = y_left,
      title = "Branches of SIMMAP",
      labels = dim_labels, colors = dim_colors,
      row_h = 0.021, cex_title = 0.98, cex_text = 0.90
    ) - 0.014

    y_left <- draw_items(
      x_left = 0.06, x_box = 0.06, y_top = y_left,
      title = "1 Pattern type",
      labels = dim_labels, colors = dim_colors,
      row_h = 0.021, cex_title = 0.98, cex_text = 0.90
    ) - 0.014

    y_left <- draw_items(
      x_left = 0.06, x_box = 0.06, y_top = y_left,
      title = "2 Fertile frond architecture",
      labels = arch_labels, colors = arch_colors,
      row_h = 0.0185, cex_title = 0.98, cex_text = 0.82
    ) - 0.014

    # Coluna direita
    y_right <- 0.955
    y_right <- draw_items(
      x_left = 0.54, x_box = 0.54, y_top = y_right,
      title = "3 Morphological variation fertile",
      labels = var_labels, colors = var_colors,
      row_h = 0.0182, cex_title = 0.98, cex_text = 0.80
    ) - 0.014

    y_right <- draw_items(
      x_left = 0.54, x_box = 0.54, y_top = y_right,
      title = "4 Size ratio",
      labels = size_labels, colors = size_colors,
      row_h = 0.0200, cex_title = 0.98, cex_text = 0.84
    ) - 0.014

    y_right <- draw_items(
      x_left = 0.54, x_box = 0.54, y_top = y_right,
      title = "5 Habit",
      labels = habit_labels, colors = habit_colors,
      row_h = 0.0188, cex_title = 0.98, cex_text = 0.82
    ) - 0.014

    draw_family_block(
      x_left = 0.06,
      y_top = min(y_left, y_right) - 0.006,
      codes = unname(family_codes),
      family_names = names(family_codes),
      ncol = 2L,
      row_h = 0.019,
      cex_title = 0.98,
      cex_code = 0.83,
      cex_name = 0.80
    )

    invisible(NULL)
  }

  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    directory = directory,
    fit_to_a4 = FALSE,
    plot_function = draw_panel
  )

  invisible(list(family_order = family_order, family_codes = family_codes))
}

# ==============================================================================
# GERACAO DAS FIGURAS DO SCRIPT 03
# ==============================================================================
if (any_figure_selected(IDS_03) &&
    required_results_available(
      c("01_dados_e_filogenia.rds", "03_simmap_dimorfismo.rds"),
      IDS_03, c("01_PREPARAR_DADOS_E_FILOGENIA.R", "03_SIMMAP_DIMORFISMO.R"))) {
  console_banner("FIGURAS DO SCRIPT 03", "SIMMAP e filogenias")

  prepared01 <- read_saved_result("01_dados_e_filogenia.rds")
  result03 <- read_saved_result("03_simmap_dimorfismo.rds")

  analysis_data <- prepared01$data
  analysis_tree <- prepared01$tree
  edge_probabilities <- result03$simmap$summary$edge_probabilities

  s3_padronizada <- padronizar_arvore_s3(tree = analysis_tree,
    edge_probabilities = edge_probabilities)
  analysis_tree_s3 <- s3_padronizada$tree
  edge_probabilities_s3 <- s3_padronizada$edge_probabilities

  cfg <- figure_config("FigS3")
  safe_figure("FigS3", SOURCE_ENV$`03`$plot_phylogeny_summary(
    tree = analysis_tree_s3, edge_probabilities = edge_probabilities_s3,
    trait_vector = result03$trait_vector, state_labels = STATE_LABELS,
    palette = PALETTE_DIMORPHISM, scheme_name = "Tres estados",
    stem = cfg$stem, width = cfg$width, height = cfg$height))

  trait_architecture <- setNames(as.character(analysis_data$Arquitetura_Lamina), analysis_data$species)
  cfg <- figure_config("FigS3B")
  safe_figure("FigS3B", SOURCE_ENV$`03`$plot_phylogeny_trait_colored(
    tree = analysis_tree_s3, trait_vector = trait_architecture,
    state_labels = ARCHITECTURE_LABELS, palette = PALETA_ARQUITETURA,
    scheme_name = "Fertile frond architecture", stem = cfg$stem,
    width = cfg$width, height = cfg$height))

  trait_variation <- setNames(as.character(analysis_data$Variacao_Morfologica), analysis_data$species)
  cfg <- figure_config("FigS3C")
  safe_figure("FigS3C", SOURCE_ENV$`03`$plot_phylogeny_trait_colored(
    tree = analysis_tree_s3, trait_vector = trait_variation,
    state_labels = VARIATION_LABELS, palette = PALETA_VARIACAO,
    scheme_name = "Morphological variation fertile", stem = cfg$stem,
    width = cfg$width, height = cfg$height))

  trait_size <- setNames(as.character(analysis_data$Relacao_Tamanho), analysis_data$species)
  cfg <- figure_config("FigS3D")
  safe_figure("FigS3D", SOURCE_ENV$`03`$plot_phylogeny_trait_colored(
    tree = analysis_tree_s3, trait_vector = trait_size,
    state_labels = SIZE_LABELS, palette = PALETA_TAMANHO,
    scheme_name = "Size ratio", stem = cfg$stem,
    width = cfg$width, height = cfg$height))

  trait_habit <- setNames(as.character(analysis_data$Habito), analysis_data$species)
  cfg <- figure_config("FigS3E")
  safe_figure("FigS3E", SOURCE_ENV$`03`$plot_phylogeny_trait_colored(
    tree = analysis_tree_s3, trait_vector = trait_habit,
    state_labels = HABIT_LABELS, palette = PALETA_HABITO,
    scheme_name = "Habit", stem = cfg$stem,
    width = cfg$width, height = cfg$height))

  if (generate_all || any_figure_selected(IDS_S3)) {
    save_s3_reference(plot_function = function() {
      plot_s3_referencia(tree = analysis_tree_s3, analysis_data = analysis_data)
    })
  }

  if (generate_all || any_figure_selected("FigS4A_alta_res")) {
    cfg <- figure_config("FigS4A_alta_res")
    safe_figure("FigS4A_alta_res", plot_s4_panel_a_highres(
      tree = analysis_tree,
      analysis_data = analysis_data,
      stem = cfg$stem,
      width = cfg$width,
      height = cfg$height,
      directory = PATHS$figures_supp
    ))
  }

  if (generate_all || any_figure_selected("FigS4A_nomes_alta_res")) {
    cfg <- figure_config("FigS4A_nomes_alta_res")
    safe_figure("FigS4A_nomes_alta_res", plot_s4_panel_a_highres_species(
      tree = analysis_tree,
      analysis_data = analysis_data,
      stem = cfg$stem,
      width = cfg$width,
      height = cfg$height,
      directory = PATHS$figures_supp
    ))
  }

  if (generate_all || any_figure_selected("FigS3_composta")) {
    cfg <- figure_config("FigS3_composta")
    safe_figure("FigS3_composta", compose_s3_panel(
      stem = cfg$stem,
      width = cfg$width,
      height = cfg$height,
      directory = PATHS$figures_supp
    ))
  }

  if (generate_all || any_figure_selected("Fig5_filogenia_alta_res")) {
    cfg <- figure_config("Fig5_filogenia_alta_res")
    safe_figure("Fig5_filogenia_alta_res", plot_fig5_phylogeny_only(
      tree = analysis_tree,
      edge_probabilities = edge_probabilities,
      analysis_data = analysis_data,
      stem = cfg$stem,
      width = cfg$width,
      height = cfg$height,
      directory = PATHS$figures_main
    ))
  }

  if (generate_all || any_figure_selected("Fig5_legendas_A4")) {
    cfg <- figure_config("Fig5_legendas_A4")
    safe_figure("Fig5_legendas_A4", plot_fig5_legends_only(
      tree = analysis_tree,
      analysis_data = analysis_data,
      stem = cfg$stem,
      width = cfg$width,
      height = cfg$height,
      directory = PATHS$figures_main
    ))
  }

  cfg <- figure_config("Fig5")
  preview_anterior <- CONFIGURACAO_GERAL$mostrar_no_painel
  CONFIGURACAO_GERAL$mostrar_no_painel <- FALSE

  safe_figure("Fig5", SOURCE_ENV$`03`$plot_circular_phylogeny_rings(
    tree = analysis_tree, edge_probabilities = edge_probabilities,
    analysis_data = analysis_data, stem = cfg$stem,
    width = cfg$width, height = cfg$height,
    ring_gap_fraction = CONFIG_FIG1$espaco_entre_aneis,
    ring_width_fraction = CONFIG_FIG1$espessura_aneis,
    ring_start_multiplier = CONFIG_FIG1$distancia_da_arvore,
    branch_width = CONFIG_FIG1$espessura_ramos,
    family_connector_gap_fraction = CONFIG_FIG1$folga_linhas_familias,
    family_connector_width = CONFIG_FIG1$espessura_linhas_familias,
    family_label_cex = CONFIG_FIG1$tamanho_rotulos_familias,
    show_ring_guides = CONFIG_FIG1$mostrar_guia_aneis,
    ring_guide_cex = CONFIG_FIG1$tamanho_texto_guia_aneis,
    show_family_panel = CONFIG_FIG1$mostrar_painel_familias,
    family_panel_cex = CONFIG_FIG1$tamanho_texto_painel_familias))

  CONFIGURACAO_GERAL$mostrar_no_painel <- preview_anterior

  cfg <- figure_config("Fig5B")
  safe_figure("Fig5B", SOURCE_ENV$`03`$plot_ring_identifier_scheme(
    stem = cfg$stem, width = cfg$width, height = cfg$height,
    ring_lwd = CONFIG_FIG1B$espessura_linhas,
    number_cex = CONFIG_FIG1B$tamanho_numeros,
    label_cex = CONFIG_FIG1B$tamanho_rotulos))

  cfg <- figure_config("Fig4_familias")
  safe_figure("Fig4_familias", SOURCE_ENV$`03`$plot_family_dimorphism_summary(
    tree = analysis_tree,
    analysis_data = analysis_data, stem = cfg$stem,
    width = cfg$width, height = cfg$height))

  # FigS4 sem nomes de especies
  cfg <- figure_config("FigS4_filogenias")
  safe_figure("FigS4_filogenias", SOURCE_ENV$`03`$plot_upward_character_phylogenies(
    tree = analysis_tree, analysis_data = analysis_data, stem = cfg$stem,
    width = cfg$width, height = cfg$height, directory = PATHS$figures_supp,
    show_species_names = FALSE))

  # Versao suplementar alternativa, identica, mas com nomes completos
  cfg <- figure_config("FigS4_nomes_especies")
  safe_figure("FigS4_nomes_especies", SOURCE_ENV$`03`$plot_upward_character_phylogenies(
    tree = analysis_tree, analysis_data = analysis_data, stem = cfg$stem,
    width = cfg$width, height = cfg$height, directory = PATHS$figures_supp,
    show_species_names = TRUE))

  cfg <- figure_config("FigS4B")
  safe_figure("FigS4B", SOURCE_ENV$`03`$plot_within_state_phylogenetic_distances(
    tree = analysis_tree, analysis_data = analysis_data, stem = cfg$stem,
    width = cfg$width, height = cfg$height, directory = PATHS$figures_supp))

  rm(prepared01, result03, analysis_data, analysis_tree, edge_probabilities,
     s3_padronizada, analysis_tree_s3, edge_probabilities_s3,
     trait_architecture, trait_variation, trait_size, trait_habit, cfg)
  invisible(gc())
}













# ==============================================================================
# 11. FIGURA DO SCRIPT 04
# ==============================================================================
FIGURAS[FIGURAS$id == "D_dimorfismo", c("width","height","dpi","fit_a4")] <- list(
  width = 7.2, height = 4.8, dpi = 600, fit_a4 = TRUE
)
IDS_04 <- "D_dimorfismo"
if (any_figure_selected(IDS_04) &&
    required_results_available(c("estatistica_D.rds", "04_sinal_filogenetico_dimorfismo.rds"),
      IDS_04, "04_SINAL_FILOGENETICO_DIMORFISMO.R")) {
  console_banner("FIGURA DO SCRIPT 04", "Sinal filogenetico do dimorfismo")
  raw_D <- read_saved_result("estatistica_D.rds")
  summary_D <- read_saved_result("04_sinal_filogenetico_dimorfismo.rds")
  cfg <- figure_config("D_dimorfismo")
  safe_figure("D_dimorfismo", {
    figure_D <- SOURCE_ENV$`04`$plot_phylogenetic_D(raw_D, summary_D)
    save_plot_dual(figure_D, cfg$stem, cfg$width, cfg$height,
                   directory = PATHS$figures_supp)
  })
  rm(raw_D, summary_D, cfg); invisible(gc())
}


# ==============================================================================
# 12. FIGURAS DO SCRIPT 05
# ==============================================================================

FIGURAS[FIGURAS$id == "FigS5_habito",   c("width","height","dpi","fit_a4")] <- list(width = 10.4, height = 7.0, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "Fig2_habito",    c("width","height","dpi","fit_a4")] <- list(width = 13.2, height = 7.2, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS21",         c("width","height","dpi","fit_a4")] <- list(width = 7.8,  height = 4.6, dpi = 600, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS19",         c("width","height","dpi","fit_a4")] <- list(width = 10.8, height = 6.8, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS20",         c("width","height","dpi","fit_a4")] <- list(width = 10.8, height = 6.8, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS22",         c("width","height","dpi","fit_a4")] <- list(width = 13.2, height = 7.6, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS23",         c("width","height","dpi","fit_a4")] <- list(width = 7.8,  height = 3.8, dpi = 600, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS21_S23",     c("width","height","dpi","fit_a4")] <- list(width = 8.0,  height = 7.0, dpi = 600, fit_a4 = TRUE)

# ------------------------------------------------------------------------------
# Figuras S21 e S23 reformuladas: versoes limpas, sem titulos internos, sem
# legendas extensas e prontas para uso isolado ou em prancha.
# ------------------------------------------------------------------------------
SOURCE_ENV$`05`$plot_multihabitat_forest <- function(
    multihabitat_table,
    stem = "FigS21. Dimorphic states versus Monomorphic by habit breadth",
    width = 7.8,
    height = 4.6,
    save = TRUE,
    directory = PATHS$figures_supp
) {
  plot_data <- multihabitat_table[
    multihabitat_table$Status == "OK" &
      is.finite(multihabitat_table$OR) &
      is.finite(multihabitat_table$IC95_inferior) &
      is.finite(multihabitat_table$IC95_superior) &
      multihabitat_table$OR > 0 &
      multihabitat_table$IC95_inferior > 0 &
      multihabitat_table$IC95_superior > 0,
    , drop = FALSE
  ]
  if (nrow(plot_data) == 0L) return(invisible(NULL))

  label_map <- c(
    "Dimorphic vs Monomorphic" = "Dimorphic vs Monomorphic",
    "Hemidimorphic vs Monomorphic" = "Hemidimorphic vs Monomorphic",
    "Holodimorphic vs Monomorphic" = "Holodimorphic vs Monomorphic"
  )

  plot_data$Comparison <- factor(
    unname(label_map[as.character(plot_data$Comparacao)]),
    levels = rev(unname(label_map))
  )

  # Mostra somente o P ajustado por FDR na figura. Os demais valores exatos
  # permanecem na tabela suplementar.
  plot_data$P_FDR_label <- ifelse(
    is.na(plot_data$P_FDR),
    "italic(P)[FDR] == NA",
    ifelse(
      plot_data$P_FDR < 0.001,
      "italic(P)[FDR] < 0.001",
      paste0("italic(P)[FDR] == ", sprintf("%.3f", plot_data$P_FDR))
    )
  )

  plot_data$Point_fill <- ifelse(
    plot_data$Evidencia_FDR %in% TRUE,
    "#2F7C96",
    "#7A8C9B"
  )

  x_min <- min(plot_data$IC95_inferior, na.rm = TRUE)
  x_max <- max(plot_data$IC95_superior, na.rm = TRUE)
  log_min <- log10(x_min)
  log_max <- log10(x_max)
  log_span <- max(log_max - log_min, 0.8)

  # Espaco extra a direita para P_FDR sem cortar o texto.
  p_label_x <- 10^(log_max + 0.12 * log_span)
  x_limits <- c(
    10^(log_min - 0.08 * log_span),
    10^(log_max + 0.38 * log_span)
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = OR, y = Comparison)) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "22",
      linewidth = 0.7,
      colour = "#8A949E"
    ) +
    ggplot2::geom_hline(
      yintercept = seq_along(levels(plot_data$Comparison)),
      linewidth = 0.4,
      colour = "#E6EBEF"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = IC95_inferior,
        xend = IC95_superior,
        yend = Comparison
      ),
      linewidth = 1.35,
      lineend = "round",
      colour = "#8FA3B5"
    ) +
    ggplot2::geom_point(
      shape = 21,
      size = 4.5,
      stroke = 0.6,
      colour = "#29404F",
      fill = plot_data$Point_fill
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = p_label_x,
        label = P_FDR_label
      ),
      hjust = 0,
      size = 3.15,
      colour = "#263238",
      family = "sans",
      parse = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = scales::breaks_log(n = 6),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::labs(
      x = "Odds ratio (95% CI; log scale)",
      y = NULL,
      caption = paste(
        "Point = OR; horizontal line = 95% CI; dashed line = OR = 1.",
        "Predictor: multiple versus single habit types; Monomorphic is the reference state."
      )
    ) +
    ggplot2::theme_classic(base_size = 11.5) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10.2, colour = "#1F2933"),
      axis.text.x = ggplot2::element_text(size = 9.8, colour = "#36454F"),
      axis.title.x = ggplot2::element_text(
        size = 10.8, face = "bold", margin = ggplot2::margin(t = 8)
      ),
      axis.line.x = ggplot2::element_line(linewidth = 0.7, colour = "#3F4A54"),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(linewidth = 0.55, colour = "#3F4A54"),
      plot.caption = ggplot2::element_text(
        size = 8.4,
        colour = "#596874",
        hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      plot.margin = ggplot2::margin(8, 10, 6, 10)
    )

  if (isTRUE(save)) save_plot_dual(p, stem, width, height, directory = directory)
  invisible(p)
}

SOURCE_ENV$`05`$plot_pagel_habit_breadth <- function(
    pagel_table,
    stem = "FigS23. Pagel between habit breadth and dimorphism",
    width = 7.8,
    height = 3.8,
    save = TRUE,
    directory = PATHS$figures_supp
) {
  d <- pagel_table[
    pagel_table$Status == "OK" & is.finite(pagel_table$Delta_AIC_favor_dependente),
    , drop = FALSE
  ]
  if (nrow(d) == 0L) return(invisible(NULL))

  desired_order <- c(
    "Dimorphic vs Monomorphic",
    "Hemidimorphic vs Monomorphic",
    "Holodimorphic vs Monomorphic"
  )
  d$Contrast <- factor(as.character(d$Contraste), levels = rev(desired_order))
  d$Point_fill <- dplyr::case_when(
    d$Delta_AIC_favor_dependente >= 10 ~ "#14526A",
    d$Delta_AIC_favor_dependente >= 4 ~ "#2F7C96",
    d$Delta_AIC_favor_dependente >= 2 ~ "#8AB8C2",
    TRUE ~ "#738392"
  )

  p <- ggplot2::ggplot(d, ggplot2::aes(x = Delta_AIC_favor_dependente, y = Contrast)) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "22",
      linewidth = 0.7,
      colour = "#8A949E"
    ) +
    ggplot2::geom_vline(
      xintercept = c(2, 4, 10),
      linetype = "dotted",
      linewidth = 0.45,
      colour = "#D5DEE5"
    ) +
    ggplot2::geom_hline(
      yintercept = seq_along(levels(d$Contrast)),
      linewidth = 0.4,
      colour = "#E6EBEF"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = Delta_AIC_favor_dependente,
        yend = Contrast
      ),
      linewidth = 1.25,
      lineend = "round",
      colour = "#CAD5DF"
    ) +
    ggplot2::geom_point(
      shape = 21,
      size = 4.6,
      stroke = 0.6,
      colour = "#29404F",
      fill = d$Point_fill
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 6),
      expand = ggplot2::expansion(mult = c(0.03, 0.06))
    ) +
    ggplot2::labs(
      x = "Delta AIC (independent - dependent)",
      y = NULL,
      caption = paste(
        "Point = Delta AIC; dashed line = 0; dotted lines = 2, 4 and 10.",
        "Positive values favor dependent evolution; exact P values are reported in Table S21."
      )
    ) +
    ggplot2::theme_classic(base_size = 11.5) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10.2, colour = "#1F2933"),
      axis.text.x = ggplot2::element_text(size = 9.8, colour = "#36454F"),
      axis.title.x = ggplot2::element_text(
        size = 10.8, face = "bold", margin = ggplot2::margin(t = 8)
      ),
      axis.line.x = ggplot2::element_line(linewidth = 0.7, colour = "#3F4A54"),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(linewidth = 0.55, colour = "#3F4A54"),
      plot.caption = ggplot2::element_text(
        size = 8.3,
        colour = "#596874",
        hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      plot.margin = ggplot2::margin(6, 10, 6, 10)
    )

  if (isTRUE(save)) save_plot_dual(p, stem, width, height, directory = directory)
  invisible(p)
}

SOURCE_ENV$`05`$plot_S21_S23_plate <- function(
    multihabitat_table,
    pagel_table,
    stem = "FigS21-S23. Habit breadth and dimorphism plate",
    width = 8.0,
    height = 6.6,
    save = TRUE,
    directory = PATHS$figures_supp
) {
  p1 <- SOURCE_ENV$`05`$plot_multihabitat_forest(
    multihabitat_table,
    stem = "FigS21. Dimorphic states versus Monomorphic by habit breadth",
    width = 7.8,
    height = 4.6,
    save = FALSE,
    directory = directory
  )
  p2 <- SOURCE_ENV$`05`$plot_pagel_habit_breadth(
    pagel_table,
    stem = "FigS23. Pagel between habit breadth and dimorphism",
    width = 7.8,
    height = 3.8,
    save = FALSE,
    directory = directory
  )
  if (is.null(p1) || is.null(p2)) return(invisible(NULL))

  plate <- (p1 / p2) +
    patchwork::plot_layout(heights = c(1.05, 0.95)) &
    ggplot2::theme(plot.margin = ggplot2::margin(6, 10, 6, 10))

  if (isTRUE(save)) save_plot_dual(plate, stem, width, height, directory = directory)
  invisible(plate)
}

IDS_05 <- c("FigS5_habito","Fig2_habito","FigS21","FigS19","FigS20","FigS22","FigS23","FigS21_S23")
if (any_figure_selected(IDS_05) &&
    required_results_available(c("01_dados_e_filogenia.rds", "05_habito_dimorfismo.rds"),
                               IDS_05, c("01_PREPARAR_DADOS_E_FILOGENIA.R", "05_HABITO_E_DIMORFISMO.R"))) {
  console_banner("FIGURAS DO SCRIPT 05", "Habito e dimorfismo")
  prepared01 <- read_saved_result("01_dados_e_filogenia.rds")
  result05 <- read_saved_result("05_habito_dimorfismo.rds")
  cfg <- figure_config("FigS5_habito")
  safe_figure("FigS5_habito", SOURCE_ENV$`05`$plot_habit_prevalence(prepared01$data, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("Fig2_habito")
  safe_figure("Fig2_habito", SOURCE_ENV$`05`$plot_ecology_forest(result05$ecology$univariate, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS21")
  safe_figure("FigS21", SOURCE_ENV$`05`$plot_multihabitat_forest(result05$multihabitat, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS19")
  safe_figure("FigS19", SOURCE_ENV$`05`$plot_habit_dimorphism_heatmap(result05$exploratory$counts, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS20")
  safe_figure("FigS20", SOURCE_ENV$`05`$plot_habit_dimorphism_residuals(result05$exploratory$residuals, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS22")
  safe_figure("FigS22", SOURCE_ENV$`05`$plot_habit_architecture_heatmap(result05$exploratory$architecture, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS23")
  safe_figure("FigS23", SOURCE_ENV$`05`$plot_pagel_habit_breadth(result05$pagel_habit_breadth$table, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS21_S23")
  safe_figure("FigS21_S23", SOURCE_ENV$`05`$plot_S21_S23_plate(result05$multihabitat, result05$pagel_habit_breadth$table, cfg$stem, cfg$width, cfg$height))
  rm(prepared01, result05, cfg); invisible(gc())
}

# ==============================================================================
# 13. FIGURAS DO SCRIPT 07
# ==============================================================================
FIGURAS[FIGURAS$id == "FigS6",  c("width","height","dpi","fit_a4")] <- list(width = 11, height = 11, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS8",  c("width","height","dpi","fit_a4")] <- list(width = 9, height = 9, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS11", c("width","height","dpi","fit_a4")] <- list(width = 11.69, height = 7.05, dpi = 600, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS9",  c("width","height","dpi","fit_a4")] <- list(width = 9, height = 9, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS12", c("width","height","dpi","fit_a4")] <- list(width = 11.69, height = 7.20, dpi = 600, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS10", c("width","height","dpi","fit_a4")] <- list(width = 9, height = 9, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS13", c("width","height","dpi","fit_a4")] <- list(width = 11.69, height = 6.95, dpi = 600, fit_a4 = FALSE)
FIGURAS[FIGURAS$id == "FigS7",  c("width","height","dpi","fit_a4")] <- list(width = 10, height = 9, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS24", c("width","height","dpi","fit_a4")] <- list(width = 12.5, height = 10.5, dpi = 300, fit_a4 = TRUE)

CONFIG_FigS11 <- list(
  cor_er = "#1769A3",
  paleta_taxas = unname(PALETA_ARQUITETURA[names(PALETA_ARQUITETURA) != "0"]),
  diagonal_fill = "#D9D9D9",
  diagonal_label = "",
  cell_border = "white",
  espessura_bordas = 0.9,
  texto_taxas = 4.15,
  texto_axis = 10.8,
  angulo_rotulos = 30,
  barra_largura_cm = 8.6,
  barra_altura_cm = 0.38,
  texto_titulo_legenda = 9.8,
  texto_legenda = 8.8,
  coord_ratio = 0.62,
  margem_plot = c(1, 1, 1, 1)
)
CONFIG_FigS12 <- CONFIG_FigS11
CONFIG_FigS12$cor_er <- "#7B3598"
CONFIG_FigS12$paleta_taxas <- unname(PALETA_VARIACAO[names(PALETA_VARIACAO) != "0"])
CONFIG_FigS12$coord_ratio <- 0.56
CONFIG_FigS13 <- CONFIG_FigS11
CONFIG_FigS13$cor_er <- "#DD7B2D"
CONFIG_FigS13$paleta_taxas <- unname(PALETA_TAMANHO[names(PALETA_TAMANHO) != "0"])
CONFIG_FigS13$coord_ratio <- 0.50

format_rate_fig12 <- function(x) {
  x <- as.numeric(x)
  ifelse(!is.finite(x), "", ifelse(x <= 1e-8, "≈0",
    ifelse(x < 1e-4, formatC(x, digits = 1, format = "e"),
      ifelse(x < 0.01, formatC(x, digits = 4, format = "f"),
        formatC(x, digits = 3, format = "f")))))
}
format_rate_legend_fig12 <- function(x) {
  x <- as.numeric(x)
  ifelse(!is.finite(x), "", ifelse(x <= 1e-8, "0",
    ifelse(x < 0.001, formatC(x, digits = 1, format = "e"),
      formatC(x, digits = 3, format = "fg"))))
}
rate_breaks_4_fig12 <- function(limits) {
  limits <- as.numeric(limits)
  limits <- limits[is.finite(limits)]
  if (!length(limits)) return(NULL)
  hi <- max(limits)
  if (!is.finite(hi) || hi <= 0) return(NULL)

  # No maximo quatro valores na barra para impedir sobreposicao dos rotulos.
  b <- scales::breaks_extended(n = 4)(c(0, hi))
  b <- b[is.finite(b) & b >= 0 & b <= hi * (1 + 1e-10)]

  if (length(b) > 4L) {
    idx <- unique(round(seq(1, length(b), length.out = 4L)))
    b <- b[idx]
  }
  if (length(b) < 3L) b <- seq(0, hi, length.out = 4L)
  unique(b)
}
wrap_q_labels12 <- function(x, width = 18) {
  x <- as.character(x)
  x <- gsub(" type$", "", x)
  stringr::str_wrap(x, width = width)
}
q_palette_color12 <- function(values, palette_cols) {
  v <- as.numeric(values)
  out <- rep(palette_cols[1], length(v))
  finite_vals <- v[is.finite(v) & v >= 0]
  if (!length(finite_vals)) return(out)
  vmax <- max(finite_vals, na.rm = TRUE)
  if (!is.finite(vmax) || vmax <= 0) return(out)
  pos <- sqrt(pmax(v, 0) / vmax)
  pos[!is.finite(pos)] <- 0
  ramp <- grDevices::colorRampPalette(palette_cols, bias = 1)
  pal <- ramp(256)
  idx <- pmax(1, pmin(256, round(pos * 255) + 1))
  out <- pal[idx]
  out[!is.finite(v)] <- palette_cols[1]
  out
}
luminance_hex12 <- function(hex) {
  rgb <- grDevices::col2rgb(hex)
  0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
}

plot_q_heatmap_final12 <- function(result, trait, stem, width = 10.6, height = 7.2) {
  id_visual <- figure_config_from_stem(stem)$id
  CONFIG_MATRIZES_Q <- switch(id_visual,
    FigS11 = CONFIG_FigS11, FigS12 = CONFIG_FigS12, FigS13 = CONFIG_FigS13,
    stop("Matriz Q sem configuracao visual: ", stem, call. = FALSE))

  d <- result$Q$tidy
  d$Origem_nome <- ifelse(is.na(d$Origem_nome), d$Origem, d$Origem_nome)
  d$Destino_nome <- ifelse(is.na(d$Destino_nome), d$Destino, d$Destino_nome)
  d$Origem_plot <- wrap_q_labels12(d$Origem_nome)
  d$Destino_plot <- wrap_q_labels12(d$Destino_nome)
  d$diag <- as.character(d$Origem_nome) == as.character(d$Destino_nome)

  # A matriz Q tidy normalmente contem apenas transicoes entre estados distintos.
  # Criamos explicitamente a diagonal para que a matriz apareca completa, sem
  # "buracos" brancos quando a figura ocupa toda a largura A4.
  estados_diagonal <- intersect(unique(d$Origem_plot), unique(d$Destino_plot))
  diagonal_q <- data.frame(
    Origem_plot = estados_diagonal,
    Destino_plot = estados_diagonal,
    stringsAsFactors = FALSE
  )

  best_model <- as.character(result$model$best_model)
  rates <- d$Taxa_por_Ma[is.finite(d$Taxa_por_Ma) & d$Taxa_por_Ma > 0]
  unique_rates <- unique(signif(rates, 10))
  is_equal_rate <- length(unique_rates) <= 1L

  d$label_rate <- ifelse(d$diag, CONFIG_MATRIZES_Q$diagonal_label,
    ifelse(d$Taxa_por_Ma > 0, format_rate_fig12(d$Taxa_por_Ma), "≈0"))
  d$fill_hex <- ifelse(d$diag, CONFIG_MATRIZES_Q$diagonal_fill,
    q_palette_color12(d$Taxa_por_Ma, CONFIG_MATRIZES_Q$paleta_taxas))
  lum <- luminance_hex12(d$fill_hex)
  d$text_col <- ifelse(d$diag, NA_character_, ifelse(lum < 145, "white", "black"))

  subtitle_text <- if (identical(best_model, "ER")) {
    shared_rate <- if (length(rates)) rates[1] else NA_real_
    paste0("ER model: one shared transition rate for all state changes (q = ",
      formatC(shared_rate, digits = 7, format = "fg"), " per Ma)")
  } else if (identical(best_model, "SYM")) {
    "SYM model: reciprocal transitions share rates within state pairs"
  } else if (identical(best_model, "ARD")) {
    "ARD model: each directional transition has its own estimated rate"
  } else paste("Transition rates; model", best_model)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = Destino_plot, y = Origem_plot)) +
    ggplot2::geom_tile(
      data = diagonal_q,
      ggplot2::aes(x = Destino_plot, y = Origem_plot),
      inherit.aes = FALSE,
      fill = CONFIG_MATRIZES_Q$diagonal_fill,
      color = CONFIG_MATRIZES_Q$cell_border,
      linewidth = CONFIG_MATRIZES_Q$espessura_bordas
    ) +
    ggplot2::coord_fixed(ratio = CONFIG_MATRIZES_Q$coord_ratio, clip = "off") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::scale_y_discrete(limits = rev, expand = ggplot2::expansion(mult = c(0, 0)))

  if (identical(best_model, "ER")) {
    p <- p +
      ggplot2::geom_tile(
        data = d[!d$diag, , drop = FALSE],
        fill = CONFIG_MATRIZES_Q$cor_er,
        color = CONFIG_MATRIZES_Q$cell_border,
        linewidth = CONFIG_MATRIZES_Q$espessura_bordas
      ) +
      ggplot2::geom_text(
        data = d[!d$diag, , drop = FALSE],
        ggplot2::aes(label = label_rate),
        size = CONFIG_MATRIZES_Q$texto_taxas,
        fontface = "bold",
        colour = ifelse(luminance_hex12(CONFIG_MATRIZES_Q$cor_er) < 145, "white", "black")
      )
  } else {
    p <- p +
      ggplot2::geom_tile(
        data = d[!d$diag, , drop = FALSE],
        ggplot2::aes(fill = Taxa_por_Ma),
        color = CONFIG_MATRIZES_Q$cell_border,
        linewidth = CONFIG_MATRIZES_Q$espessura_bordas
      ) +
      ggplot2::geom_text(
        data = d[!d$diag, , drop = FALSE],
        ggplot2::aes(label = label_rate, colour = I(text_col)),
        size = CONFIG_MATRIZES_Q$texto_taxas,
        fontface = "bold",
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_gradientn(
        colours = CONFIG_MATRIZES_Q$paleta_taxas,
        trans = "sqrt",
        limits = c(0, max(d$Taxa_por_Ma[!d$diag], na.rm = TRUE)),
        name = "Rate per Ma",
        breaks = rate_breaks_4_fig12,
        labels = format_rate_legend_fig12,
        guide = ggplot2::guide_colorbar(
          title.position = "top",
          title.hjust = 0.5,
          label.position = "bottom",
          label.hjust = 0.5,
          barwidth = grid::unit(CONFIG_MATRIZES_Q$barra_largura_cm, "cm"),
          barheight = grid::unit(CONFIG_MATRIZES_Q$barra_altura_cm, "cm"),
          ticks = TRUE,
          frame.colour = NA
        )
      )
  }

  p <- p +
    ggplot2::labs(
      x = "Destination state",
      y = "Origin state"
    ) +
    SOURCE_ENV$`07`$theme_morph() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = CONFIG_MATRIZES_Q$angulo_rotulos,
        hjust = 1, vjust = 1,
        size = CONFIG_MATRIZES_Q$texto_axis,
        margin = ggplot2::margin(t = 1)
      ),
      axis.text.y = ggplot2::element_text(size = CONFIG_MATRIZES_Q$texto_axis, margin = ggplot2::margin(r = 2)),
      axis.title.x = ggplot2::element_text(size = 11.4, face = "bold", margin = ggplot2::margin(t = 4)),
      axis.title.y = ggplot2::element_text(size = 11.4, face = "bold", margin = ggplot2::margin(r = 4)),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "top",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = CONFIG_MATRIZES_Q$texto_titulo_legenda, face = "bold"),
      legend.text = ggplot2::element_text(size = CONFIG_MATRIZES_Q$texto_legenda),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = 1, l = 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.04, "cm"),
      plot.margin = do.call(ggplot2::margin, as.list(CONFIG_MATRIZES_Q$margem_plot))
    )

  if (!identical(best_model, "ER") && is_equal_rate && length(rates) > 1L) {
    SOURCE_ENV$`07`$morph_log("All off-diagonal Q rates are equal for ", trait,
      " even though the selected model is ", best_model,
      ". Inspect model convergence and the Q matrix.", level = "AVISO")
  }
  SOURCE_ENV$`07`$morph_save_plot(p, stem, width, height)
  invisible(p)
}

IDS_07 <- c("FigS6","FigS8","FigS11","FigS9","FigS12","FigS10","FigS13","FigS7","FigS24")
if (any_figure_selected(IDS_07) &&
    required_results_available("07_evolucao_solucoes_morfologicas.rds", IDS_07,
      "07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R")) {
  console_banner("FIGURAS DO SCRIPT 07", "Evolucao das solucoes morfologicas")
  result07 <- read_saved_result("07_evolucao_solucoes_morfologicas.rds")
  rebuild_morph_result <- function(trait) list(model = result07$models[[trait]],
    Q = result07$Q[[trait]], simmap = result07$simmap[[trait]])
  res_architecture <- rebuild_morph_result("Arquitetura_Lamina")
  res_variation <- rebuild_morph_result("Variacao_Morfologica")
  res_size <- rebuild_morph_result("Relacao_Tamanho")
  cfg <- figure_config("FigS6")
  safe_figure("FigS6", SOURCE_ENV$`07`$plot_morph_frequency(result07$frequencies, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS8")
  safe_figure("FigS8", SOURCE_ENV$`07`$plot_transition_network_morph(res_architecture, "Arquitetura_Lamina", cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS11")
  safe_figure("FigS11", plot_q_heatmap_final12(res_architecture, "Arquitetura_Lamina", cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS9")
  safe_figure("FigS9", SOURCE_ENV$`07`$plot_transition_network_morph(res_variation, "Variacao_Morfologica", cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS12")
  safe_figure("FigS12", plot_q_heatmap_final12(res_variation, "Variacao_Morfologica", cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS10")
  safe_figure("FigS10", SOURCE_ENV$`07`$plot_transition_network_morph(res_size, "Relacao_Tamanho", cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS13")
  safe_figure("FigS13", plot_q_heatmap_final12(res_size, "Relacao_Tamanho", cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS7")
  safe_figure("FigS7", SOURCE_ENV$`07`$plot_morph_model_comparison(result07$comparison, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS24")
  safe_figure("FigS24", SOURCE_ENV$`07`$plot_independent_origins_morph(result07$independent_origins, stem = cfg$stem, width = cfg$width, height = cfg$height))
  rm(result07, res_architecture, res_variation, res_size, rebuild_morph_result, cfg)
  invisible(gc())
}

# ==============================================================================
# 14. FIGURAS DO SCRIPT 08
# ==============================================================================
FIGURAS[FIGURAS$id == "FigS17", c("width","height","dpi","fit_a4")] <- list(width = 13, height = 10.5, dpi = 300, fit_a4 = TRUE)

# FigS18 compacta para composicao vertical em uma pagina A4.
# 8.0 in = 20.32 cm: menor que a largura de uma folha A4 (21.0 cm).
# A altura foi reduzida para 5.2 in (13.21 cm), permitindo outra figura acima.
FIGURAS[FIGURAS$id == "FigS18", c("width","height","dpi","fit_a4")] <-
  list(width = 8.0, height = 4.8, dpi = 600, fit_a4 = TRUE)

# ------------------------------------------------------------------------------
# FIGURE S18 - VERSAO COMPACTA E LIMPA
# ------------------------------------------------------------------------------
# A tabela suplementar (Table S20) guarda os valores exatos de D e P.
# A figura mostra apenas o padrao: posicao do D observado, referencias D = 0 e
# D = 1 e a interpretacao estatistica por simbolos. Assim, evita-se repetir
# dezenas de numeros dentro do painel e a figura continua interpretavel sozinha.
SOURCE_ENV$`08`$plot_D_morph <- function(
    signal,
    stem = "FigS18. Sinal filogenetico D da morfologia",
    width = 8.0,
    height = 4.8
) {
  if (is.null(signal) || nrow(signal) == 0L) return(invisible(NULL))

  d <- signal

  # Apenas resultados estimaveis entram como pontos.
  d$Estimable <- d$Status == "OK" & is.finite(d$D)

  # Usa os rotulos em ingles ja salvos pelo Script 08 quando disponiveis.
  if ("Caracter_nome" %in% names(d)) {
    d$Label <- as.character(d$Caracter_nome)
  } else {
    d$Label <- as.character(d$Caracter)
  }

  # Como o cabecalho do painel ja informa o grupo, removemos prefixos repetidos.
  d$Label <- sub("^Architecture: ", "", d$Label)
  d$Label <- sub("^Variation: ", "", d$Label)
  d$Label <- sub("^Size ratio: ", "", d$Label)
  d$Label <- sub("^Change in frond shape$", "Frond shape", d$Label)
  d$Label <- sub("^Change in lamina size$", "Lamina size", d$Label)
  d$Label <- sub("^Change in petiole size$", "Petiole size", d$Label)
  d$Label <- sub("^Variation in ", "", d$Label)

  # Rotulo mais informativo para o contraste que antes aparecia como
  # "Any dimorphism vs monomorphism".
  d$Label[d$Caracter == "Dimorfismo_qualquer"] <-
    "Hemidimorphic + holodimorphic vs monomorphic"

  # Grupos tematicos para distribuir os caracteres em dois blocos de colunas.
  d$Group <- dplyr::case_when(
    d$Caracter %in% c(
      "Hemidimorfia_vs_Monomorfia",
      "Holodimorfia_vs_Monomorfia",
      "Dimorfismo_qualquer"
    ) ~ "Dimorphism contrasts",
    d$Caracter %in% c("Mudanca_forma", "Mudanca_lamina", "Mudanca_peciolo") ~
      "Morphological components",
    grepl("^Arquitetura_", d$Caracter) ~ "Fertile-frond architecture",
    grepl("^Variacao_", d$Caracter) ~ "Morphological variation",
    grepl("^Tamanho_", d$Caracter) ~ "Size ratio",
    TRUE ~ "Other"
  )

  group_order <- c(
    "Dimorphism contrasts",
    "Morphological components",
    "Fertile-frond architecture",
    "Morphological variation",
    "Size ratio",
    "Other"
  )
  d$Group <- factor(d$Group, levels = group_order)

  # ---------------------------------------------------------------------------
  # INTERPRETACAO
  # ---------------------------------------------------------------------------
  # Em vez de chamar de "Intermediate", descrevemos exatamente o resultado
  # estatistico. Se o carater nao difere de D = 0, ele e considerado compativel
  # com o limiar Browniano; se nao difere de D = 1, e compativel com aleatoriedade.
  # Quando difere significativamente de ambos, mostramos "Different from both
  # references", sem sugerir que "intermediario" seja um modelo evolutivo.
  p_random <- d$P_contra_aleatorio_D1
  p_brownian <- d$P_contra_browniano_D0

  d$Pattern <- dplyr::case_when(
    !d$Estimable ~ "Not estimated",
    p_random < 0.05 & p_brownian < 0.05 & d$D < 0 ~
      "More clustered than Brownian",
    p_random < 0.05 & p_brownian >= 0.05 ~
      "Brownian-compatible",
    p_random >= 0.05 & p_brownian < 0.05 ~
      "Random-compatible",
    p_random < 0.05 & p_brownian < 0.05 & d$D > 1 ~
      "More dispersed than random",
    p_random < 0.05 & p_brownian < 0.05 ~
      "Different from both references",
    TRUE ~ "Unresolved"
  )

  pattern_order <- c(
    "More clustered than Brownian",
    "Brownian-compatible",
    "Different from both references",
    "Random-compatible",
    "More dispersed than random",
    "Unresolved",
    "Not estimated"
  )
  d$Pattern <- factor(d$Pattern, levels = pattern_order)

  # Ordem global dos rotulos; facets livres descartam niveis ausentes.
  d$Label_plot <- factor(d$Label, levels = rev(unique(d$Label)))
  d_ok <- d[d$Estimable, , drop = FALSE]

  # Limites dinamicos: D pode ser menor que -1 ou maior que 1.
  d_values <- d_ok$D[is.finite(d_ok$D)]
  if (length(d_values) == 0L) d_values <- c(0, 1)
  raw_min <- min(c(0, 1, d_values))
  raw_max <- max(c(0, 1, d_values))
  span <- max(raw_max - raw_min, 1)
  x_min <- raw_min - 0.06 * span
  x_max <- raw_max + 0.06 * span

  # Mesmas cores usadas na figura detalhada da estatistica D do dimorfismo.
  brownian_col <- "#2A9D8F"
  random_col <- "#2F6690"

  pattern_cols <- c(
    "More clustered than Brownian" = "#18756D",
    "Brownian-compatible" = brownian_col,
    "Different from both references" = "#5E6469",
    "Random-compatible" = random_col,
    "More dispersed than random" = "#244A6B",
    "Unresolved" = "#9A9A9A",
    "Not estimated" = "#BDBDBD"
  )

  p <- ggplot2::ggplot(
    d_ok,
    ggplot2::aes(x = D, y = Label_plot)
  ) +
    # As duas referencias aparecem nas mesmas cores da figura D detalhada.
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = 0.85,
      colour = brownian_col,
      alpha = 0.95
    ) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.85,
      colour = random_col,
      alpha = 0.95
    ) +
    # Segmento discreto entre D = 0 e o valor observado.
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = D, yend = Label_plot),
      linewidth = 0.48,
      colour = "grey78",
      lineend = "round"
    ) +
    # Uma unica linguagem visual: todos os resultados sao circulos; a cor
    # identifica a compatibilidade/padrao.
    ggplot2::geom_point(
      ggplot2::aes(colour = Pattern),
      size = 2.8,
      alpha = 0.98
    ) +
    ggplot2::facet_wrap(
      ~Group,
      ncol = 2,
      scales = "free_y",
      drop = TRUE
    ) +
    ggplot2::scale_colour_manual(
      values = pattern_cols,
      drop = TRUE
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::coord_cartesian(xlim = c(x_min, x_max), clip = "off") +
    ggplot2::labs(
      x = "Phylogenetic D  (0 = Brownian threshold; 1 = random)",
      y = NULL,
      colour = "Phylogenetic pattern"
    ) +
    ggplot2::theme_minimal(base_size = 9.0) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        linewidth = 0.22,
        colour = "grey91"
      ),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 8.8,
        colour = "#263238"
      ),
      strip.background = ggplot2::element_rect(
        fill = "grey96",
        colour = "grey86",
        linewidth = 0.3
      ),
      axis.text.y = ggplot2::element_text(
        size = 7.35,
        colour = "#222222"
      ),
      axis.text.x = ggplot2::element_text(
        size = 7.5,
        colour = "#333333"
      ),
      axis.title.x = ggplot2::element_text(
        size = 8.4,
        face = "bold",
        margin = ggplot2::margin(t = 4)
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        size = 7.8,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(size = 7.1),
      legend.key.width = grid::unit(0.30, "cm"),
      legend.spacing.x = grid::unit(0.07, "cm"),
      panel.spacing.x = grid::unit(0.50, "cm"),
      panel.spacing.y = grid::unit(0.22, "cm"),
      plot.margin = ggplot2::margin(3, 5, 2, 3)
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(size = 3.0)
      )
    )

  save_plot_dual(p, stem, width, height, directory = PATHS$figures_supp)
  invisible(p)
}

IDS_08 <- c("FigS17","FigS18")
if (any_figure_selected(IDS_08) &&
    required_results_available("08_correlacoes_sinal_morfologico.rds", IDS_08,
      "08_CORRELACOES_E_SINAL_MORFOLOGICO.R")) {
  console_banner("FIGURAS DO SCRIPT 08", "Correlacoes e sinal morfologico")
  result08 <- read_saved_result("08_correlacoes_sinal_morfologico.rds")
  cfg <- figure_config("FigS17")
  safe_figure("FigS17", SOURCE_ENV$`08`$plot_pagel_morph(result08$Pagel, cfg$stem, cfg$width, cfg$height))
  cfg <- figure_config("FigS18")
  safe_figure("FigS18", SOURCE_ENV$`08`$plot_D_morph(result08$D, cfg$stem, cfg$width, cfg$height))
  rm(result08, cfg); invisible(gc())
}



plot_morphological_syndromes_panelB_only <- function(
    route_data,
    stem,
    width = 12.4,
    height = 7.0,
    top_n = 8L,
    directory = PATHS$figures_main) {

  required_cols <- c(
    "Pattern", "Architecture", "Variation", "Size_Relation", "N"
  )

  missing_cols <- setdiff(required_cols, names(route_data))
  if (length(missing_cols) > 0L) {
    stop(
      "Colunas ausentes em rotas_morfologicas: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  pattern_levels <- unname(STATE_LABELS[c("1", "2")])

  route_data <- route_data[
    route_data$Pattern %in% pattern_levels,
    required_cols,
    drop = FALSE
  ]

  if (nrow(route_data) == 0L) {
    stop("Nenhum registro hemi/holodimorfico encontrado para a Fig3.", call. = FALSE)
  }

  short_arch <- c(
    "Contracted type" = "Contracted",
    "Spiciform type" = "Spiciform",
    "Paniculiform type" = "Paniculiform",
    "Digitiform type" = "Digitiform",
    "Sporocarp type" = "Sporocarp",
    "Radiciform type" = "Radiciform"
  )

  short_var <- c(
    "Total variation" = "Total variation",
    "Variation in frond shape" = "Shape variation",
    "Variation in frond size" = "Size variation",
    "Variation in petiole size" = "Petiole variation",
    "Variation in frond shape and size" = "Shape + size",
    "Variation in frond shape and petiole size" = "Shape + petiole size",
    "Variation in frond and petiole size" = "Frond + petiole size"
  )

  short_size <- c(
    "Larger fertile frond" = "Larger fertile",
    "Smaller fertile frond" = "Smaller fertile",
    "Similar frond length" = "Similar length",
    "Intra-individual variation" = "Intra-individual"
  )

  map_or_keep <- function(x, map) {
    x_chr <- as.character(x)
    out <- unname(map[x_chr])
    out[is.na(out)] <- x_chr[is.na(out)]
    out
  }

  wrap_combo <- function(x, width = 34) {
    vapply(
      strwrap(x, width = width, simplify = FALSE),
      paste,
      collapse = "\n",
      FUN.VALUE = character(1)
    )
  }

  syndrome_data <- route_data |>
    dplyr::group_by(Pattern, Architecture, Variation, Size_Relation) |>
    dplyr::summarise(N = sum(N, na.rm = TRUE), .groups = "drop")

  pattern_totals <- syndrome_data |>
    dplyr::group_by(Pattern) |>
    dplyr::summarise(Pattern_total = sum(N, na.rm = TRUE), .groups = "drop")

  # Seleciona as combinacoes pela FREQUENCIA TOTAL (hemi + holo),
  # e nao pela diferenca entre os dois padroes.
  syndrome_totals <- syndrome_data |>
    dplyr::left_join(pattern_totals, by = "Pattern") |>
    dplyr::mutate(Percentage = 100 * N / Pattern_total) |>
    dplyr::group_by(Architecture, Variation, Size_Relation) |>
    dplyr::summarise(
      Total = sum(N, na.rm = TRUE),
      Max_pct = max(Percentage, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(Total),
      dplyr::desc(Max_pct),
      Architecture,
      Variation,
      Size_Relation
    )

  # Mantem apenas as oito combinacoes mais frequentes no conjunto total.
  n_top <- min(as.integer(top_n), nrow(syndrome_totals))

  top_syndromes <- syndrome_totals |>
    dplyr::slice_head(n = n_top) |>
    dplyr::mutate(
      Architecture_short = map_or_keep(Architecture, short_arch),
      Variation_short = map_or_keep(Variation, short_var),
      Size_short = map_or_keep(Size_Relation, short_size),
      Syndrome_label = paste(
        Architecture_short,
        Variation_short,
        Size_short,
        sep = "  |  "
      ),
      Syndrome_label = wrap_combo(Syndrome_label, width = 36)
    )

  plot_data <- tidyr::expand_grid(
    Pattern = pattern_levels,
    row_id = seq_len(nrow(top_syndromes))
  ) |>
    dplyr::left_join(
      top_syndromes |>
        dplyr::mutate(row_id = seq_len(dplyr::n())),
      by = "row_id"
    ) |>
    dplyr::left_join(
      syndrome_data,
      by = c("Pattern", "Architecture", "Variation", "Size_Relation")
    ) |>
    dplyr::left_join(pattern_totals, by = "Pattern") |>
    dplyr::mutate(
      N = dplyr::coalesce(N, 0L),
      Percentage = 100 * N / Pattern_total,
      Signed_percentage = dplyr::if_else(
        Pattern == pattern_levels[1],
        -Percentage,
        Percentage
      ),
      Value_label = dplyr::if_else(
        N > 0L,
        paste0(N, " (", sprintf("%.1f", Percentage), "%)"),
        ""
      ),
      Exclusive = dplyr::case_when(
        Pattern == pattern_levels[1] & N > 0 & !((row_id %in% row_id[Pattern == pattern_levels[2] & N > 0])) ~ TRUE,
        Pattern == pattern_levels[2] & N > 0 & !((row_id %in% row_id[Pattern == pattern_levels[1] & N > 0])) ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # Exclusive based on whether the paired pattern has zero count for same row.
  paired <- plot_data |>
    dplyr::select(row_id, Pattern, N) |>
    tidyr::pivot_wider(names_from = Pattern, values_from = N, values_fill = 0)
  paired$Exclusive_hemi <- paired[[pattern_levels[1]]] > 0 & paired[[pattern_levels[2]]] == 0
  paired$Exclusive_holo <- paired[[pattern_levels[2]]] > 0 & paired[[pattern_levels[1]]] == 0
  plot_data <- plot_data |>
    dplyr::left_join(
      paired |>
        dplyr::select(row_id, Exclusive_hemi, Exclusive_holo),
      by = "row_id"
    ) |>
    dplyr::mutate(
      Exclusive = dplyr::case_when(
        Pattern == pattern_levels[1] ~ Exclusive_hemi,
        Pattern == pattern_levels[2] ~ Exclusive_holo,
        TRUE ~ FALSE
      )
    )

  syndrome_order <- rev(top_syndromes$Syndrome_label)

  plot_data <- plot_data |>
    dplyr::mutate(
      Syndrome_label = factor(Syndrome_label, levels = syndrome_order),
      Pattern = factor(Pattern, levels = pattern_levels)
    )

  max_abs <- max(abs(plot_data$Signed_percentage), na.rm = TRUE)
  label_gap <- max(0.8, max_abs * 0.035)
  label_room <- max(5, max_abs * 0.24)
  axis_limit <- ceiling((max_abs + label_room) / 5) * 5

  plot_data <- plot_data |>
    dplyr::mutate(
      Label_x = dplyr::if_else(
        Signed_percentage < 0,
        Signed_percentage - label_gap,
        Signed_percentage + label_gap
      ),
      Label_hjust = dplyr::if_else(Signed_percentage < 0, 1, 0),
      Display_label = dplyr::if_else(
        Exclusive,
        paste0(Value_label, "*"),
        Value_label
      )
    )

  pattern_n <- stats::setNames(pattern_totals$Pattern_total, pattern_totals$Pattern)
  legend_labels <- stats::setNames(
    paste0(pattern_levels, " (n = ", pattern_n[pattern_levels], ")"),
    pattern_levels
  )
  pattern_palette <- stats::setNames(
    unname(PALETTE_DIMORPHISM[c("1", "2")]),
    pattern_levels
  )

  left_title_x <- -axis_limit * 0.55
  right_title_x <- axis_limit * 0.55
  top_y <- length(levels(plot_data$Syndrome_label)) + 0.70

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Signed_percentage, y = Syndrome_label, fill = Pattern)
  ) +
    ggplot2::annotate(
      "rect",
      xmin = -axis_limit,
      xmax = 0,
      ymin = -Inf,
      ymax = Inf,
      fill = "#FFF7EF",
      alpha = 0.65
    ) +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = axis_limit,
      ymin = -Inf,
      ymax = Inf,
      fill = "#F2FAF6",
      alpha = 0.78
    ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.8, colour = "#8A95A1") +
    ggplot2::geom_col(width = 0.60, alpha = 0.97, colour = NA) +
    ggplot2::geom_text(
      ggplot2::aes(x = Label_x, label = Display_label, hjust = Label_hjust),
      size = 3.65,
      fontface = "bold",
      colour = "#17212B",
      show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = left_title_x,
      y = top_y,
      label = legend_labels[[pattern_levels[1]]],
      fontface = "bold",
      size = 5.0,
      colour = "#2F3A43"
    ) +
    ggplot2::annotate(
      "text",
      x = right_title_x,
      y = top_y,
      label = legend_labels[[pattern_levels[2]]],
      fontface = "bold",
      size = 5.0,
      colour = "#2F3A43"
    ) +
    ggplot2::scale_fill_manual(values = pattern_palette, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(-axis_limit, axis_limit),
      breaks = pretty(c(-axis_limit, axis_limit), n = 7),
      labels = function(x) paste0(abs(x), "%"),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(x = "Share within dimorphism pattern", y = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#E1E6EA", linewidth = 0.40),
      axis.title.x = ggplot2::element_text(size = 12.5, face = "bold", colour = "#1F2933", margin = ggplot2::margin(t = 10)),
      axis.text.x = ggplot2::element_text(size = 10.3, colour = "#3F4A54"),
      axis.text.y = ggplot2::element_text(size = 10.9, colour = "#1D2730", lineheight = 0.96, margin = ggplot2::margin(r = 9)),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 22, r = 26, b = 10, l = 12)
    )

  save_plot_dual(
    p,
    stem = stem,
    width = width,
    height = height,
    directory = directory
  )

  invisible(p)
}

# ==============================================================================
# 15. FIGURA DO SCRIPT 09
# ==============================================================================
FIGURAS[FIGURAS$id == "Fig3", c("width","height","dpi","fit_a4")] <- list(
  width = 12.4, height = 7.0, dpi = 300, fit_a4 = TRUE
)
FIG3_NUMERO_DE_SINDROMES <- 8L
IDS_09 <- "Fig3"
if (any_figure_selected(IDS_09) &&
    required_results_available("09_exploratorias.rds", IDS_09,
      "09_ANALISES_DESCRITIVAS_E_EXPLORATORIAS.R")) {
  console_banner("FIGURA DO SCRIPT 09", "Sindromes morfologicas - painel B apenas")
  result09 <- read_saved_result("09_exploratorias.rds")
  cfg <- figure_config("Fig3")
  safe_figure("Fig3", plot_morphological_syndromes_panelB_only(
    result09$rotas_morfologicas, stem = cfg$stem, width = cfg$width,
    height = cfg$height, top_n = FIG3_NUMERO_DE_SINDROMES,
    directory = PATHS$figures_main))
  rm(result09, cfg); invisible(gc())
}

# ==============================================================================
# 16. FIGURAS DO SCRIPT 10
# ==============================================================================
FIGURAS[FIGURAS$id == "FigS25",     c("width","height","dpi","fit_a4")] <- list(width = 9.2, height = 5.8, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS26",     c("width","height","dpi","fit_a4")] <- list(width = 9.2, height = 4.8, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS27",     c("width","height","dpi","fit_a4")] <- list(width = 10.5, height = 6.8, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS28",     c("width","height","dpi","fit_a4")] <- list(width = 10.5, height = 6.6, dpi = 300, fit_a4 = TRUE)
FIGURAS[FIGURAS$id == "FigS25_S28", c("width","height","dpi","fit_a4")] <- list(width = 11.5, height = 8.1, dpi = 300, fit_a4 = TRUE)

IDS_10 <- c("FigS25","FigS26","FigS27","FigS28","FigS25_S28")
if (any_figure_selected(IDS_10) &&
    required_results_available("10_sensibilidade_filogenetica.rds", IDS_10,
      "10_SENSIBILIDADE_FILOGENETICA.R")) {
  console_banner("FIGURAS DO SCRIPT 10", "Sensibilidade filogenetica")
  result10 <- read_saved_result("10_sensibilidade_filogenetica.rds")
  safe_figure(IDS_10[which(vapply(IDS_10, figure_selected, logical(1)))[1]],
    SOURCE_ENV$`10`$sens_build_final_figures(
      mk_table = result10$Mk$table, D_table = result10$D,
      habit_table = result10$habit$results, pagel_table = result10$Pagel$results))
  rm(result10); invisible(gc())
}

# ==============================================================================
# 17. FIGURA DO SCRIPT 11 / VALIDACAO 11B
# ==============================================================================
# A FigS29 usa a tabela VALIDADA produzida pelo 11B minimo.
# Nenhum modelo e reajustado aqui: este Script 12 apenas le o .rds salvo e plota.
# As demais figuras continuam usando normalmente seus respectivos .rds.
FIGURAS[FIGURAS$id == "FigS29", c("width","height","dpi","fit_a4")] <- list(width = 13.8, height = 9.2, dpi = 300, fit_a4 = TRUE)
IDS_11 <- "FigS29"
if (any_figure_selected(IDS_11) &&
    required_results_available("11B_minimo_validacao_critica.rds", IDS_11,
      "11B_MINIMO_VALIDACAO_CRITICA.R")) {
  console_banner("FIGURA DO SCRIPT 11", "Sensibilidade a taxas ocultas - resultado validado")

  result11b <- read_saved_result("11B_minimo_validacao_critica.rds")

  if (is.null(result11b$validated_summary) || !is.data.frame(result11b$validated_summary)) {
    stop(
      "O arquivo 11B_minimo_validacao_critica.rds nao contem 'validated_summary'. ",
      "Use o RDS produzido pelo 11B minimo concluido com sucesso.",
      call. = FALSE
    )
  }

  summary_s29 <- result11b$validated_summary

  # A funcao grafica original do Script 11 espera a coluna Status.
  # O 11B minimo ja contem apenas o resumo final; acrescentamos Status apenas
  # para compatibilidade grafica, sem alterar qualquer resultado numerico.
  if (!"Status" %in% names(summary_s29)) {
    summary_s29$Status <- ifelse(
      is.finite(summary_s29$Delta_AICc_hidden_favor_correlated),
      "OK",
      "NOT_ESTIMABLE"
    )
  }

  cfg <- figure_config("FigS29")
  safe_figure("FigS29", SOURCE_ENV$`11`$plot_hidden_rate_summary11(
    summary_s29, stem = cfg$stem, width = cfg$width, height = cfg$height))

  rm(result11b, summary_s29, cfg); invisible(gc())
}

# ==============================================================================
# 18. RELATORIO FINAL
# ==============================================================================
if (nrow(FIGURE_REPORT) > 0L) {
  utils::write.csv(FIGURE_REPORT, windows_safe_path(REPORT_FILE_FIGURES),
    row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

generated_count <- sum(FIGURE_REPORT$status == "gerada")
error_count <- sum(FIGURE_REPORT$status == "erro")
missing_count <- sum(FIGURE_REPORT$status == "nao_gerada")

cat("\n============================================================\n")
cat("GERACAO DE FIGURAS FINALIZADA\n")
cat("Figuras geradas: ", generated_count, "\n", sep = "")
cat("Figuras nao geradas por falta de resultado: ", missing_count, "\n", sep = "")
cat("Figuras com erro: ", error_count, "\n", sep = "")
cat("Pasta das figuras: ", PATHS$figures, "\n", sep = "")
cat("Relatorio: ", REPORT_FILE_FIGURES, "\n", sep = "")
cat("============================================================\n")

if (error_count > 0L || missing_count > 0L) {
  cat("\nConsulte o relatorio para ver exatamente o que precisa ser corrigido.\n")
}

# ==============================================================================
# FIM DO SCRIPT 12
# ==============================================================================
