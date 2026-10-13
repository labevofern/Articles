# ==============================================================================
# 00_CONFIGURACAO_E_FUNCOES.R
# ==============================================================================
# ESTE ARQUIVO NAO CONTEM ANALISES ESTATISTICAS OU FILOGENETICAS.
#
# Ele existe somente para evitar repetir, em todos os scripts:
#   1. caminhos das pastas do projeto;
#   2. rotulos dos codigos usados na matriz;
#   3. funcoes GENERICAS de log, leitura de .rds e exportacao de arquivos;
#   4. funcoes GENERICAS para salvar graficos em PDF/PNG.
#
# NAO ficam aqui:
#   - execucao dos modelos Mk / corHMM (02 e 10);
#   - estimacao de taxas (a validacao e montagem de Q sao compartilhadas);
#   - SIMMAP;
#   - estatistica D;
#   - regressao filogenetica de habito;
#   - modelos de taxas ocultas;
#   - testes de Pagel;
#   - analises morfologicas;
#   - funcoes que constroem figuras cientificas especificas;
#   - nomes ou numeracao de Table/Fig.
#
# As verificacoes Mk compartilhadas por 02, 03 e 10 ficam ao final deste arquivo.
# A execucao de cada analise continua no script correspondente.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ==============================================================================
# 1. CODIGOS COMPARTILHADOS DA MATRIZ
# ==============================================================================
# Estes vetores apenas traduzem codigos numericos em nomes legiveis.
# Eles NAO executam analise.

STATE_LABELS <- c(
  "0" = "Monomorphic",
  "1" = "Hemidimorphic",
  "2" = "Holodimorphic"
)

PALETTE_DIMORPHISM <- c(
  "0" = "#355C7D",  # azul profundo
  "1" = "#D98C3F",  # terracota/dourado
  "2" = "#4C956C"   # verde suave
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

# ==============================================================================
# 2. CAMINHOS DO PROJETO
# ==============================================================================
# IMPORTANTE: apenas o Script 01 confere se Matriz de dados.xlsx existe.
# Assim, scripts que trabalham apenas com .rds continuam funcionando mesmo
# quando a planilha original nao precisa ser relida.

# Estrutura portatil: Projeto/Scripts_ e Projeto/Tabelas_.
# Nenhum caminho pessoal e necessario. Uma substituicao explicita e opcional.
project_override <- Sys.getenv("LEAF_DIMORPHISM_PROJECT_DIR", "")
if (nzchar(project_override)) {
  project_dir <- normalizePath(project_override, winslash="/", mustWork=TRUE)
} else {
  if (!exists("SCRIPTS_DIR") || basename(SCRIPTS_DIR)!="Scripts_") {
    stop("Execute um script dentro da pasta Scripts_ do projeto, ou defina LEAF_DIMORPHISM_PROJECT_DIR.", call.=FALSE)
  }
  project_dir <- dirname(normalizePath(SCRIPTS_DIR,winslash="/",mustWork=TRUE))
}

project_io_dir <- project_dir
if (.Platform$OS.type == "windows" && dir.exists(project_dir)) {
  short_project_dir <- shortPathName(project_dir)
  if (nzchar(short_project_dir)) project_io_dir <- short_project_dir
}

# Nao usamos setwd() como requisito logico das analises.
# Todos os arquivos sao acessados por caminhos completos construidos abaixo.
tabelas_dir <- file.path(project_io_dir, "Tabelas_")
imagens_dir <- file.path(project_io_dir, "Imagens_")
scripts_dir <- file.path(project_io_dir, "Scripts_")
# Nome padrao da matriz de entrada; pode ser alterado por variavel de ambiente.
data_file <- Sys.getenv("LEAF_DIMORPHISM_DATA_FILE", "")
if (!nzchar(data_file)) data_file <- file.path(tabelas_dir, "Matriz_final_completa.xlsx")
data_processed <- file.path(tabelas_dir, "_Dados_processados")
logs_dir <- file.path(scripts_dir, "_Logs")
models_dir <- file.path(scripts_dir, "_Modelos")
objects_dir <- file.path(scripts_dir, "_Objetos_R")

PATHS <- list(
  data_file = data_file,
  output_root = project_io_dir,
  data = data_processed,
  logs = logs_dir,
  models = models_dir,
  objects = objects_dir,
  tables = tabelas_dir,
  tables_main = tabelas_dir,
  tables_supp = tabelas_dir,
  tables_complete = tabelas_dir,
  figures = imagens_dir,
  figures_main = imagens_dir,
  figures_supp = imagens_dir
)

# Cria somente as pastas gerais. Nada e apagado.
for (folder in c(tabelas_dir, imagens_dir, scripts_dir, data_processed,
                 logs_dir, models_dir, objects_dir)) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
}

LOG_FILE <- file.path(PATHS$logs, "execucao_pipeline.log")

# ==============================================================================
# 3. FUNCOES GENERICAS DE MENSAGENS
# ==============================================================================

is_true <- function(x) {
  tolower(trimws(x)) %in% c("1", "true", "t", "yes", "y", "sim", "s")
}

log_message <- function(..., level = "INFO") {
  message_text <- paste(..., collapse = "")
  line <- sprintf("[%s] [%s] %s",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, message_text)
  cat(line, "\n")
  cat(line, "\n", file = LOG_FILE, append = TRUE)
}

stop_pipeline <- function(...) {
  log_message(..., level = "ERRO")
  stop(paste(..., collapse = ""), call. = FALSE)
}

console_banner <- function(script, title) {
  cat("\n============================================================\n")
  cat(script, " - ", title, "\n", sep = "")
  cat("============================================================\n")
}

console_step <- function(step, total, text) {
  cat(sprintf("\n>>> PASSO %d/%d - %s\n", step, total, text))
}

console_note <- function(text) cat("    NOTA: ", text, "\n", sep = "")
console_output <- function(text) cat("    SALVO: ", text, "\n", sep = "")
console_done <- function(script, next_script = NULL) {
  cat("\n>>> ", script, " CONCLUIDO COM SUCESSO.\n", sep = "")
  if (!is.null(next_script)) cat(">>> PROXIMO: ", next_script, "\n", sep = "")
}

# ==============================================================================
# 4. PACOTES - FUNCOES GENERICAS
# ==============================================================================
# Cada script informa SUA propria lista de pacotes.
# Assim o 00 nao precisa saber quais analises serao executadas.

check_packages <- function(required_packages) {
  missing <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop(
      "Pacotes ausentes: ", paste(missing, collapse = ", "),
      "\nInstale antes de continuar:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

load_packages <- function(required_packages) {
  suppressPackageStartupMessages({
    for (pkg in required_packages) {
      library(pkg, character.only = TRUE)
    }
  })
  invisible(TRUE)
}

# ==============================================================================
# 5. FUNCOES GENERICAS DE ARQUIVOS
# ==============================================================================

windows_safe_path <- function(path) {
  if (.Platform$OS.type != "windows") return(path)
  destination_folder <- dirname(path)
  if (!dir.exists(destination_folder)) return(path)
  short_folder <- shortPathName(destination_folder)
  if (!nzchar(short_folder)) return(path)
  file.path(short_folder, basename(path))
}

write_csv_utf8 <- function(x, path, row.names = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    x, windows_safe_path(path), row.names = row.names,
    fileEncoding = "UTF-8", na = ""
  )
  invisible(path)
}

require_saved_rds <- function(filename, previous_script) {
  path <- file.path(PATHS$objects, filename)
  if (!file.exists(path)) {
    stop(
      "Nao encontrei o resultado necessario:\n", path,
      "\n\nRode primeiro: ", previous_script,
      call. = FALSE
    )
  }
  readRDS(path)
}

save_pipeline_rds <- function(object, filename) {
  path <- file.path(PATHS$objects, filename)
  saveRDS(object, windows_safe_path(path))
  invisible(path)
}

# ==============================================================================
# 6. FUNCOES GENERICAS PARA EXPORTAR FIGURAS
# ==============================================================================
# Estas funcoes NAO constroem nenhuma figura cientifica.
# Elas apenas recebem um grafico pronto e gravam PDF/PNG.

fit_a4_dimensions <- function(width, height) {
  if (width >= height) {
    max_width <- 11.69; max_height <- 8.27
  } else {
    max_width <- 8.27; max_height <- 11.69
  }
  scale_factor <- min(1, max_width / width, max_height / height)
  c(width = width * scale_factor, height = height * scale_factor)
}

save_ggplot_png <- function(plot, path, width, height, dpi = 300) {
  dimensions <- fit_a4_dimensions(width, height)
  grDevices::png(
    windows_safe_path(path),
    width = unname(dimensions["width"]),
    height = unname(dimensions["height"]),
    units = "in", res = dpi, bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(path)
}

preview_ggplot <- function(plot) {
  if (interactive()) print(plot)
  invisible(plot)
}

save_plot_dual <- function(plot, stem, width, height,
                           directory = PATHS$figures) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  dimensions <- fit_a4_dimensions(width, height)
  width <- unname(dimensions["width"])
  height <- unname(dimensions["height"])
  preview_ggplot(plot)
  ggplot2::ggsave(
    filename = windows_safe_path(file.path(directory, paste0(stem, ".pdf"))),
    plot = plot, width = width, height = height, units = "in",
    device = grDevices::cairo_pdf, bg = "white"
  )
  save_ggplot_png(
    plot, file.path(directory, paste0(stem, ".png")),
    width, height, dpi = 300
  )
  invisible(stem)
}

save_base_plot_dual <- function(stem, width, height, plot_function,
                                directory = PATHS$figures,
                                fit_to_a4 = TRUE) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (fit_to_a4) {
    dimensions <- fit_a4_dimensions(width, height)
    width <- unname(dimensions["width"])
    height <- unname(dimensions["height"])
  }
  if (interactive()) plot_function()
  grDevices::pdf(
    windows_safe_path(file.path(directory, paste0(stem, ".pdf"))),
    width = width, height = height, useDingbats = FALSE
  )
  plot_function(); grDevices::dev.off()
  grDevices::png(
    windows_safe_path(file.path(directory, paste0(stem, ".png"))),
    width = width, height = height, units = "in", res = 300, bg = "white"
  )
  plot_function(); grDevices::dev.off()
  invisible(stem)
}

# ==============================================================================
# FIM DO 00
# ==============================================================================

# ==============================================================================
# DIAGNOSTICOS COMPARTILHADOS DOS MODELOS Mk (02, 03 E 10)
# ==============================================================================
# Taxa no limite inferior e um diagnostico, nao um veto ao modelo.
# AICc finito sozinho nao comprova convergencia. O corHMM pode nao devolver
# o codigo do otimizador; nesse caso registramos "Nao informado".
MK_POLICY_VERSION <- "2026-09-08-lower-bound-audit-v1"

mk_signature <- function(tree, trait_data, n_starts, lower, upper) {
  list(policy = MK_POLICY_VERSION, edge = tree$edge,
    edge.length = tree$edge.length, tip.label = tree$tip.label,
    Nnode = tree$Nnode, root.edge = tree$root.edge,
    species = as.character(trait_data[, 1]), states = as.character(trait_data[, 2]),
    n_starts = n_starts, lower = lower, upper = upper, root.p = "yang",
    corHMM = as.character(utils::packageVersion("corHMM")), seed = 20260908L)
}

mk_check_input <- function(signature, tree, trait_data) {
  current <- list(edge = tree$edge, edge.length = tree$edge.length,
    tip.label = tree$tip.label, Nnode = tree$Nnode, root.edge = tree$root.edge,
    species = as.character(trait_data[, 1]), states = as.character(trait_data[, 2]))
  if (is.null(signature) || !all(vapply(names(current), function(n)
    identical(signature[[n]], current[[n]]), logical(1)))) {
    stop("A arvore ou os estados mudaram desde o Script 02. Rode o Script 02 novamente.", call. = FALSE)
  }
  invisible(TRUE)
}

mk_diagnostics <- function(result, lower, upper) {
  fit <- result$fit
  rates <- numeric(0)
  if (!is.null(fit) && is.matrix(fit$solution) && is.matrix(fit$index.mat) &&
      identical(dim(fit$solution), dim(fit$index.mat))) {
    mask <- !is.na(fit$index.mat) & fit$index.mat > 0 & row(fit$index.mat) != col(fit$index.mat)
    rates <- fit$solution[mask]
  }
  finite_scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  numeric_ok <- !is.null(fit) && length(rates) > 0L && all(is.finite(rates)) &&
    all(rates >= 0) && finite_scalar(fit$loglik) && finite_scalar(fit$AIC) &&
    finite_scalar(fit$AICc) && fit$loglik != -1e6
  # corHMM 2.x nao expoe normalmente o codigo de convergencia em seu resultado.
  convergence <- "Nao informado pelo corHMM"
  failed <- FALSE
  if (!is.null(fit$convergence)) {
    failed <- !identical(as.numeric(fit$convergence), 0)
    convergence <- paste("Codigo:", paste(fit$convergence, collapse = ","))
  }
  serious_warning <- any(grepl("failed to optimize|failed to converge|did not converge|non.converg|maximum.*evaluations|maxeval.*reached",
    result$warnings, ignore.case = TRUE))
  numeric_ok <- numeric_ok && !failed && !serious_warning
  low <- length(rates) > 0L && any(is.finite(rates) & rates <= lower * 10)
  high <- length(rates) > 0L && any(is.finite(rates) & rates >= upper * 0.99)
  status <- if (is.null(fit)) "Falhou" else if (!numeric_ok) "Falha numerica/convergencia" else
    if (high) "Limite superior" else if (low) "Taxa aproximadamente zero" else "OK"
  list(rates = rates, low = low, high = high, numeric_ok = numeric_ok,
    eligible = numeric_ok && !high, status = status, convergence = convergence)
}

mk_model_row <- function(result, group, model, lower, upper, language = "pt") {
  d <- mk_diagnostics(result, lower, upper)
  fit <- result$fit
  number <- function(x) if (is.numeric(x) && length(x) == 1L) x else NA_real_
  ids <- if (!is.null(fit$index.mat)) fit$index.mat else numeric(0)
  ids <- unique(ids[!is.na(ids) & ids > 0])
  row <- data.frame(Esquema = group, Modelo = model,
    LogLik = number(fit$loglik), K = if (length(ids)) length(ids) else NA_integer_,
    AIC = number(fit$AIC), AICc = number(fit$AICc),
    Taxa_minima = if (length(d$rates) && all(is.finite(d$rates))) min(d$rates) else NA_real_,
    Taxa_maxima = if (length(d$rates) && all(is.finite(d$rates))) max(d$rates) else NA_real_,
    Status = d$status,
    Avisos = paste(stats::na.omit(c(result$error, result$warnings)), collapse = " | "),
    Limite_inferior = d$low, Limite_superior = d$high,
    Ajuste_numerico_valido = d$numeric_ok,
    Convergencia = d$convergence, Elegivel_reconstrucao = d$eligible,
    Ajustes_auditados = if (is.null(result$audit)) 0L else nrow(result$audit),
    Ajustes_proximos_melhor_LogLik = if (is.null(result$audit)) NA_integer_ else
      sum(result$audit$Numerico_valido & abs(result$audit$LogLik - number(fit$loglik)) <= 1e-4, na.rm = TRUE),
    stringsAsFactors = FALSE)
  if (language == "en") {
    names(row)[match(c("Esquema", "Modelo", "Taxa_minima", "Taxa_maxima", "Avisos"), names(row))] <-
      c("Tree", "Model", "Min_rate", "Max_rate", "Warnings")
  }
  row
}

mk_select <- function(comparison, model_col, weight_col) {
  valid <- comparison$Ajuste_numerico_valido & is.finite(comparison$AICc)
  eligible <- valid & comparison$Elegivel_reconstrucao
  if (!any(eligible)) stop("Nenhum modelo Mk elegivel. Confira erros, avisos e limite superior; nenhuma Q sera usada.", call. = FALSE)
  comparison$Delta_AICc <- NA_real_
  comparison[[weight_col]] <- NA_real_
  comparison$Delta_AICc[valid] <- comparison$AICc[valid] - min(comparison$AICc[valid])
  weights <- exp(-0.5 * comparison$Delta_AICc[valid])
  comparison[[weight_col]][valid] <- weights / sum(weights)
  # A tabela mantem AICc e pesos dos ajustes numericamente validos, inclusive
  # os limitados superiormente; a selecao e explicitamente entre os elegiveis.
  selected <- which(eligible)[which.min(comparison$AICc[eligible])]
  comparison$Selecionado <- seq_len(nrow(comparison)) == selected
  comparison[order(comparison$AICc), , drop = FALSE]
}

mk_fit_audited <- function(arguments) {
  # Cada linha e o resultado de uma chamada independente com nstarts = 1.
  # Nao afirmamos recuperar os reinicios internos de objetos antigos.
  n <- max(1L, as.integer(arguments$nstarts))
  arguments$nstarts <- 1L
  arguments$n.cores <- 1L
  lower <- arguments$lower.bound
  upper <- arguments$upper.bound
  best <- NULL
  best_ll <- -Inf
  audit <- vector("list", n)
  all_warnings <- character(0)
  all_errors <- character(0)
  for (i in seq_len(n)) {
    set.seed(20260908L + i)
    warns <- character(0)
    fit <- tryCatch(withCallingHandlers(do.call(corHMM::corHMM, arguments),
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
      error = function(e) e)
    err <- if (inherits(fit, "error")) conditionMessage(fit) else NA_character_
    if (inherits(fit, "error")) fit <- NULL
    ans <- list(fit = fit, error = err, warnings = unique(warns))
    d <- mk_diagnostics(ans, lower, upper)
    ll <- if (!is.null(fit$loglik) && length(fit$loglik) == 1L) fit$loglik else NA_real_
    row <- data.frame(Ajuste = i, Semente = 20260908L + i, LogLik = ll,
      Numerico_valido = d$numeric_ok, Limite_inferior = d$low,
      Limite_superior = d$high, Status = d$status, Convergencia = d$convergence,
      Avisos = paste(stats::na.omit(c(err, warns)), collapse = " | "))
    # Audita cada taxa estimada com indices internos do corHMM (linha -> coluna).
    if (!is.null(fit$solution) && is.matrix(fit$solution)) {
      for (r in seq_len(nrow(fit$solution))) for (c in seq_len(ncol(fit$solution))) {
        if (r != c) row[[paste0("q_", r, "_", c)]] <- fit$solution[r, c]
      }
    }
    audit[[i]] <- row
    all_warnings <- unique(c(all_warnings, warns))
    all_errors <- unique(c(all_errors, stats::na.omit(err)))
    if (d$numeric_ok && ll > best_ll) { best <- ans; best_ll <- ll }
    if (i == 1L || i %% 10L == 0L || i == n) message("Mk: ", i, "/", n, " ajustes auditados; melhor LogLik = ", best_ll)
  }
  fields <- unique(unlist(lapply(audit, names)))
  audit <- do.call(rbind, lapply(audit, function(x) {
    for (field in setdiff(fields, names(x))) x[[field]] <- NA
    x[, fields, drop = FALSE]
  }))
  if (is.null(best)) best <- list(fit = NULL,
    error = paste(c("Nenhum ajuste numericamente valido.", all_errors), collapse = " | "), warnings = all_warnings)
  best$audit <- audit
  best$all_start_warnings <- all_warnings
  best$all_start_errors <- all_errors
  best
}

mk_validate_q <- function(q, states) {
  if (!is.matrix(q) || !is.numeric(q) || nrow(q) != length(states) || ncol(q) != length(states) ||
      !setequal(rownames(q), states) || !setequal(colnames(q), states) || any(!is.finite(q))) {
    stop("Matriz Q invalida: dimensoes, estados ou taxas nao finitas.", call. = FALSE)
  }
  q <- q[states, states, drop = FALSE]
  if (any(q[row(q) != col(q)] < 0) || any(diag(q) > 0) ||
      any(abs(rowSums(q)) > 1e-10 * max(1, max(abs(q))))) {
    stop("Matriz Q invalida: confira sinais e soma das linhas.", call. = FALSE)
  }
  invisible(TRUE)
}

mk_q_matrix <- function(fit, state_codes) {
  q <- fit$solution
  idx <- fit$index.mat
  if (!is.matrix(q) || !identical(dim(q), dim(idx)) || nrow(q) != length(state_codes))
    stop("Dimensoes da solucao Mk incompativeis.", call. = FALSE)
  estimated <- !is.na(idx) & idx > 0 & row(idx) != col(idx)
  if (any(!is.finite(q[estimated])) || any(q[estimated] < 0))
    stop("Taxas estimadas invalidas na matriz Q.", call. = FALSE)
  q[!estimated] <- 0
  # Recupera a correspondencia entre estados originais e internos por especie.
  # Nunca pressupoe silenciosamente que as posicoes 1,2,3 significam 0,1,2.
  dat <- fit$data
  legend <- fit$data.legend
  if (is.null(dat) || is.null(legend)) stop("Falta a correspondencia dos estados no ajuste corHMM.", call. = FALSE)
  m <- match(as.character(dat[, 1]), as.character(legend[, 1]))
  mapping <- unique(data.frame(original = as.character(dat[, 2]), internal = as.character(legend[m, 2])))
  internal <- mapping$internal[match(state_codes, mapping$original)]
  if (nrow(mapping) != length(state_codes) || anyNA(internal) || anyDuplicated(internal))
    stop("Correspondencia de estados ambigua no ajuste corHMM.", call. = FALSE)
  expected_names <- paste0("(", internal, ",R1)")
  if (!setequal(rownames(q), expected_names) || !setequal(colnames(q), expected_names))
    stop("Rotulos internos inesperados na solucao corHMM.", call. = FALSE)
  q <- q[expected_names, expected_names, drop = FALSE]
  dimnames(q) <- list(state_codes, state_codes)
  diag(q) <- -rowSums(q)
  mk_validate_q(q, state_codes)
  q
}

# Correcoes de extracao e resumo. Nenhuma funcao abaixo ajusta modelos.
POSTPROCESS_VERSION <- "2026-09-09-v3"

simmap_root_probabilities <- function(description, maps, tree, state_codes) {
  root <- setdiff(unique(tree$edge[,1]), tree$edge[,2])
  if (length(root) != 1L) stop("Nao foi possivel identificar uma raiz unica.")
  a <- description$ace
  key <- as.character(root)
  if (is.null(a) || is.null(rownames(a)) || !key %in% rownames(a) ||
      !all(state_codes %in% colnames(a))) stop("Raiz/estados ausentes em description$ace.")
  probs <- as.numeric(a[key, state_codes, drop=TRUE])
  if (any(!is.finite(probs)) || any(probs < 0 | probs > 1) || abs(sum(probs)-1)>1e-10)
    stop("Probabilidades invalidas para a raiz.")
  # Conferencia independente: primeiro estado em cada ramo que sai da raiz.
  sampled <- vapply(maps, function(m) {
    r <- setdiff(unique(m$edge[,1]), m$edge[,2])
    if (length(r)!=1L) stop("Mapa sem raiz unica.")
    outgoing <- which(m$edge[,1] == r)
    states <- vapply(m$maps[outgoing], function(x) names(x)[1], character(1))
    if (!length(states) || anyNA(states) || length(unique(states))!=1L)
      stop("Estados inconsistentes nos ramos que saem da raiz.")
    states[1]
  }, character(1))
  if (!length(sampled) || !all(sampled %in% state_codes)) stop("Estados de raiz invalidos nos mapas.")
  counted <- as.numeric(table(factor(sampled, levels=state_codes)))/length(sampled)
  if (max(abs(counted-probs))>1e-10) stop("Raiz de description$ace difere dos mapas brutos.")
  setNames(probs, state_codes)
}

sens_habit_probabilities <- function(tab, alpha=0.05) {
  required <- c("Tree","Estimate","Std_error","Z","Status")
  if (!all(required %in% names(tab))) stop("Tabela de habito sem coeficientes/erros padrao.")
  ok <- !is.na(tab$Status) & tab$Status == "OK"
  if (any(!is.finite(tab$Estimate[ok])) || any(!is.finite(tab$Std_error[ok])) ||
      any(tab$Std_error[ok]<=0)) stop("Coeficientes/erros padrao invalidos.")
  z <- tab$Estimate[ok]/tab$Std_error[ok]
  if (any(!is.finite(tab$Z[ok])) || any(abs(z-tab$Z[ok])>1e-6)) stop("Z difere de Estimate/Std_error.")
  tab$P <- tab$P_FDR <- rep(NA_real_,nrow(tab))
  tab$P[ok] <- 2*stats::pnorm(-abs(z))
  for (tr in unique(tab$Tree[ok])) {
    i <- which(ok & tab$Tree==tr)
    tab$P_FDR[i] <- stats::p.adjust(tab$P[i], method="BH")
  }
  tab$FDR_evidence <- ifelse(is.finite(tab$P_FDR),tab$P_FDR<alpha,NA)
  if (any(tab$P[ok]<0 | tab$P[ok]>1 | tab$P_FDR[ok]<0 | tab$P_FDR[ok]>1))
    stop("P ou P_FDR fora de [0,1].")
  tab
}

sens_pagel_probabilities <- function(tab, alpha=0.05) {
  ok <- !is.na(tab$Status) & tab$Status=="OK" & is.finite(tab$P)
  if (any(tab$P[ok]<0 | tab$P[ok]>1)) stop("P de Pagel fora de [0,1].")
  tab$P_FDR <- rep(NA_real_,nrow(tab))
  for (tr in unique(tab$Tree[ok])) {
    i <- which(ok & tab$Tree==tr)
    tab$P_FDR[i] <- stats::p.adjust(tab$P[i],method="BH")
  }
  tab$FDR_evidence <- ifelse(is.finite(tab$P_FDR),tab$P_FDR<alpha,NA)
  tab
}

sens_topology_keys <- function(tree) {
  tips <- tree$tip.label
  if (anyDuplicated(tips) || anyNA(tips)) stop("Rotulos de pontas invalidos.")
  edge <- tree$edge
  descendants <- vector("list",max(edge))
  ids <- match(tips,sort(tips))
  for (i in seq_along(tips)) descendants[[i]] <- ids[i]
  pending <- unique(edge[,1])
  while(length(pending)) {
    complete <- integer(0)
    for (node in pending) {
      children <- edge[edge[,1]==node,2]
      if (all(vapply(descendants[children],length,integer(1))>0L)) {
        descendants[[node]] <- sort(unique(unlist(descendants[children])))
        complete <- c(complete,node)
      }
    }
    if (!length(complete)) stop("Arvore invalida ao comparar topologias.")
    pending <- setdiff(pending,complete)
  }
  n <- length(tips)
  internal <- descendants[unique(edge[,1])]
  rooted <- sort(unique(vapply(Filter(function(x) length(x)>1L && length(x)<n,internal),
    paste,character(1),collapse=",")))
  split_keys <- vapply(descendants[edge[,2]],function(x) {
    other <- setdiff(seq_len(n),x)
    if (min(length(x),length(other))<2L) return(NA_character_)
    a <- paste(x,collapse=","); b <- paste(other,collapse=",")
    if(length(x)<length(other)) a else if(length(other)<length(x)) b else sort(c(a,b))[1]
  },character(1))
  list(rooted=rooted, unrooted=sort(unique(stats::na.omit(split_keys))))
}

sens_topology_check <- function(main, alternative) {
  if (!setequal(main$tip.label,alternative$tip.label)) stop("As arvores nao possuem as mesmas especies.")
  a <- sens_topology_keys(main); b <- sens_topology_keys(alternative)
  distance <- function(x,y) length(setdiff(x,y))+length(setdiff(y,x))
  list(unrooted_distance=distance(a$unrooted,b$unrooted),
    rooted_distance=distance(a$rooted,b$rooted),rooted_identical=identical(a$rooted,b$rooted))
}

sens_robustness_summary <- function(mk_main,mk_alt,D_table,habit_comparison,pagel_comparison) {
  count <- function(z) paste0(sum(z %in% TRUE),"/",sum(!is.na(z)))
  robust <- function(z) if(all(is.na(z))) NA else all(z[!is.na(z)])
  dm <- D_table$Interpretation[D_table$Tree=="Main_consensus"]
  da <- D_table$Interpretation[D_table$Tree=="Alternative_ML"]
  if(length(dm)!=1L || length(da)!=1L) stop("Interpretacoes D ausentes/duplicadas.")
  p <- pagel_comparison$Same_FDR_decision
  data.frame(Analysis=c("Mk best model","Phylogenetic D interpretation",
    "Habit effects: same direction and FDR decision","Pagel tests: same FDR decision among estimated tests"),
    Main_tree=c(mk_main$best_model,dm,count(habit_comparison$Robust_direction_and_FDR),count(p)),
    Alternative_tree=c(mk_alt$best_model,da,"See TableS34","See TableS36"),
    Robust=c(mk_main$best_model==mk_alt$best_model,dm==da,
      robust(habit_comparison$Robust_direction_and_FDR),robust(p)),
    Note=c("","","",paste(sum(is.na(p)),"planned test(s) not comparable; excluded from denominator")),
    stringsAsFactors=FALSE)
}
