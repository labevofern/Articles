# ==============================================================================
# 11B_MINIMO_VALIDACAO_CRITICA.R
# ==============================================================================
# OBJETIVO
#   Resolver de forma direcionada o problema dos ajustes de taxa oculta que
#   atingiram o limite superior 100 no Script 11, sem repetir dezenas de
#   otimizacoes que nao alterariam a conclusao biologica.
#
# ESTRATEGIA
#   1. Mantem como elegiveis os ajustes originais com Rate_max < 99.
#   2. Trata ajustes originais com Rate_max >= 99 como boundary-constrained e,
#      portanto, nao elegiveis para selecao de modelos.
#   3. Faz uma UNICA validacao intensiva no caso que pode mudar a conclusao:
#        S05_Holodimorphic_vs_Monomorphic
#        Hidden Markov Independent (HMIM)
#      Esse modelo e reajustado em upper.bound = 500 e 1000, com 50 random
#      restarts, n.cores = 1.
#   4. Se esse HMIM estabilizar longe do novo teto, ele e recuperado e seus
#      valores do ajuste em 1000 substituem os valores originais.
#   5. Recalcula Delta AICc e pesos apenas entre modelos elegiveis.
#   6. Gera a tabela completa validada e o resumo independente vs correlacionado.
#
# POR QUE APENAS ESSE MODELO?
#   Entre os ajustes que atingiram >=99, este e o unico cuja exclusao muda a
#   interpretacao qualitativa do par de caracteres. Nos demais pares, retirar o
#   ajuste boundary-constrained nao muda a conclusao independente/correlacionada.
#
# TEMPO ESPERADO
#   Apenas 2 ajustes pesados: upper=500 e upper=1000, ambos com 50 restarts.
#
# ENTRADA
#   Scripts_/_Objetos_R/11_sensibilidade_pagel_hidden_rates.rds
#
# SAIDAS
#   Tabelas_/TableS38_VALIDATED_MINIMAL. Hiddenrate_model_comparison.csv
#   Tabelas_/TableS39_VALIDATED_MINIMAL. Hiddenrate_sensitivity_summary.csv
#   Tabelas_/TableS41_MINIMAL. Critical_boundary_diagnostic.csv
#   Scripts_/_Objetos_R/11B_minimo_validacao_critica.rds
#
# IMPORTANTE
#   - Este script NAO sobrescreve o objeto do Script 11.
#   - Usa n.cores = 1 para evitar reentrada/recursao no Windows.
#   - Possui cache proprio. Se for interrompido depois de concluir um limite,
#     esse limite sera reutilizado na proxima execucao.
# ==============================================================================


# ==============================================================================
# 0. LOCALIZAR Scripts_ E CARREGAR 00
# ==============================================================================

get_scripts_dir_11bmin <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(dirname(normalizePath(frame$ofile, winslash = "/")))
    }
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) {
      return(dirname(normalizePath(p, winslash = "/")))
    }
  }

  normalizePath(getwd(), winslash = "/")
}

SCRIPTS_DIR <- get_scripts_dir_11bmin()
source(file.path(SCRIPTS_DIR, "00_CONFIGURACAO_E_FUNCOES.R"), chdir = TRUE)


# ==============================================================================
# 1. PACOTES E CONFIGURACAO
# ==============================================================================

required_packages <- c("corHMM")
check_packages(required_packages)
load_packages(required_packages)

CONFIG11B_MIN <- list(
  seed = 20261010L,
  upper_bounds = c(500, 1000),
  lower_bound = 1e-9,
  n_starts = 50L,
  n_cores = 1L,
  original_boundary_cutoff = 99,
  boundary_fraction = 0.99,
  loglik_tolerance = 0.10,
  parameter_relative_tolerance = 0.20,
  parameter_relevance_floor = 1e-5,
  delta_threshold = 2
)

TARGET_TEST <- "S05_Holodimorphic_vs_Monomorphic"
TARGET_MODEL_OBJECT <- "hidden_Markov_independent_model_fit"

CACHE11B_MIN <- file.path(PATHS$objects, "_HiddenRate_BoundaryValidation_MINIMAL")
dir.create(CACHE11B_MIN, recursive = TRUE, showWarnings = FALSE)

LOG11B_MIN <- file.path(PATHS$logs, "execucao_11B_minimo_validacao_critica.log")
if (file.exists(LOG11B_MIN)) file.remove(LOG11B_MIN)

log_min <- function(..., level = "INFO") {
  txt <- paste(..., collapse = "")
  line <- sprintf(
    "[%s] [%s] %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    level,
    txt
  )
  cat(line, "\n")
  cat(line, "\n", file = windows_safe_path(LOG11B_MIN), append = TRUE)
  invisible(line)
}

write_min <- function(x, filename) {
  path <- file.path(PATHS$tables_supp, filename)
  write_csv_utf8(x, path)
  log_min("Tabela salva: ", path)
  invisible(path)
}


# ==============================================================================
# 2. FUNCOES DE EXTRACAO E DIAGNOSTICO
# ==============================================================================

count_parameters_min <- function(fit) {
  if (is.null(fit) || is.null(fit$index.mat)) return(NA_integer_)
  idx <- as.numeric(fit$index.mat)
  idx <- idx[is.finite(idx) & idx > 0]
  if (!length(idx)) return(0L)
  length(unique(idx))
}

extract_parameters_min <- function(fit) {
  if (is.null(fit) || is.null(fit$index.mat) || is.null(fit$solution)) {
    return(numeric(0))
  }

  idx <- as.numeric(fit$index.mat)
  sol <- as.numeric(fit$solution)

  if (length(idx) != length(sol)) return(numeric(0))

  ids <- sort(unique(idx[is.finite(idx) & idx > 0]))
  if (!length(ids)) return(numeric(0))

  out <- vapply(ids, function(id) {
    vals <- sol[is.finite(idx) & idx == id & is.finite(sol)]
    if (!length(vals)) NA_real_ else vals[1]
  }, numeric(1))

  names(out) <- paste0("q", ids)
  out
}

fit_stats_min <- function(fit, upper_bound, n_tips) {
  pars <- extract_parameters_min(fit)
  pars <- pars[is.finite(pars)]

  loglik <- if (!is.null(fit$loglik) && length(fit$loglik) >= 1L) {
    as.numeric(fit$loglik)[1]
  } else {
    NA_real_
  }

  aic <- if (!is.null(fit$AIC) && length(fit$AIC) >= 1L) {
    as.numeric(fit$AIC)[1]
  } else {
    NA_real_
  }

  k <- count_parameters_min(fit)

  aicc <- if (
    is.finite(aic) && is.finite(k) && is.finite(n_tips) &&
      n_tips > (k + 1)
  ) {
    aic + (2 * k * (k + 1)) / (n_tips - k - 1)
  } else {
    NA_real_
  }

  rate_min <- if (length(pars)) min(pars) else NA_real_
  rate_max <- if (length(pars)) max(pars) else NA_real_

  list(
    logLik = loglik,
    AIC = aic,
    AICc = aicc,
    K = k,
    Rate_min = rate_min,
    Rate_max = rate_max,
    Upper_boundary = is.finite(rate_max) &&
      rate_max >= upper_bound * CONFIG11B_MIN$boundary_fraction,
    Finite = is.finite(loglik) && is.finite(aic) && is.finite(aicc) &&
      length(pars) > 0L && all(is.finite(pars)),
    Parameters = pars
  )
}

parameter_stability_min <- function(fit500, fit1000) {
  p1 <- extract_parameters_min(fit500)
  p2 <- extract_parameters_min(fit1000)

  common <- intersect(names(p1), names(p2))
  if (!length(common)) {
    return(list(stable = FALSE, max_relative_change = NA_real_, n = 0L))
  }

  p1 <- p1[common]
  p2 <- p2[common]
  ok <- is.finite(p1) & is.finite(p2)
  p1 <- p1[ok]
  p2 <- p2[ok]

  if (!length(p1)) {
    return(list(stable = FALSE, max_relative_change = NA_real_, n = 0L))
  }

  relevant <- pmax(abs(p1), abs(p2)) >= CONFIG11B_MIN$parameter_relevance_floor

  if (!any(relevant)) {
    return(list(stable = TRUE, max_relative_change = 0, n = 0L))
  }

  p1 <- p1[relevant]
  p2 <- p2[relevant]

  rel <- abs(p2 - p1) / pmax(abs(p1), abs(p2))
  max_rel <- max(rel, na.rm = TRUE)

  list(
    stable = is.finite(max_rel) &&
      max_rel <= CONFIG11B_MIN$parameter_relative_tolerance,
    max_relative_change = max_rel,
    n = length(rel)
  )
}


# ==============================================================================
# 3. AJUSTE corHMM COM CACHE PROPRIO
# ==============================================================================

refit_target_min <- function(original_fit, upper_bound, seed) {
  root_setting <- if (!is.null(original_fit$root.p)) original_fit$root.p else "yang"

  set.seed(seed)

  corHMM::corHMM(
    phy = original_fit$phy,
    data = original_fit$data,
    rate.cat = original_fit$rate.cat,
    rate.mat = original_fit$index.mat,
    model = "ARD",
    node.states = "none",
    root.p = root_setting,
    nstarts = CONFIG11B_MIN$n_starts,
    n.cores = CONFIG11B_MIN$n_cores,
    collapse = TRUE,
    lower.bound = CONFIG11B_MIN$lower_bound,
    upper.bound = upper_bound
  )
}

cache_file_min <- function(upper_bound) {
  file.path(
    CACHE11B_MIN,
    paste0(
      TARGET_TEST,
      "__",
      TARGET_MODEL_OBJECT,
      "__upper_",
      upper_bound,
      ".rds"
    )
  )
}

fit_bound_min <- function(original_fit, upper_bound, n_tips) {
  cache_file <- cache_file_min(upper_bound)

  if (file.exists(cache_file)) {
    z <- tryCatch(readRDS(cache_file), error = function(e) NULL)

    if (
      !is.null(z) &&
        identical(z$upper_bound, upper_bound) &&
        identical(z$n_starts, CONFIG11B_MIN$n_starts) &&
        !is.null(z$fit)
    ) {
      s <- fit_stats_min(z$fit, upper_bound, n_tips)

      if (isTRUE(s$Finite)) {
        log_min("Reutilizando cache VALIDO: upper=", upper_bound)
        return(list(fit = z$fit, stats = s, from_cache = TRUE))
      }
    }

    log_min("Cache invalido ignorado: upper=", upper_bound, level = "WARN")
  }

  seed_i <- as.integer(CONFIG11B_MIN$seed + upper_bound)

  log_min(
    "Ajustando caso critico | upper=", upper_bound,
    " | nstarts=", CONFIG11B_MIN$n_starts,
    " | n.cores=", CONFIG11B_MIN$n_cores
  )

  fit <- tryCatch(
    refit_target_min(original_fit, upper_bound, seed_i),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    stop(
      "Falha no ajuste upper=", upper_bound, ": ",
      conditionMessage(fit),
      call. = FALSE
    )
  }

  s <- fit_stats_min(fit, upper_bound, n_tips)

  if (!isTRUE(s$Finite)) {
    stop(
      "O ajuste upper=", upper_bound,
      " terminou sem solucao numerica finita. O cache nao sera salvo.",
      call. = FALSE
    )
  }

  saveRDS(
    list(
      upper_bound = upper_bound,
      n_starts = CONFIG11B_MIN$n_starts,
      corHMM_version = as.character(utils::packageVersion("corHMM")),
      fit = fit
    ),
    windows_safe_path(cache_file),
    compress = "xz"
  )

  list(fit = fit, stats = s, from_cache = FALSE)
}


# ==============================================================================
# 4. SELECAO DE MODELOS APOS A REGRA DE LIMITE
# ==============================================================================

recalculate_selection_min <- function(tab) {
  tab$Delta_AICc <- NA_real_
  tab$Akaike_weight_AICc <- NA_real_

  for (test_id in unique(tab$Test_ID)) {
    ii <- which(
      tab$Test_ID == test_id &
        tab$Estimable %in% TRUE &
        is.finite(tab$AICc)
    )

    if (!length(ii)) next

    best <- min(tab$AICc[ii], na.rm = TRUE)
    delta <- tab$AICc[ii] - best
    raw_w <- exp(-0.5 * delta)
    w <- raw_w / sum(raw_w)

    tab$Delta_AICc[ii] <- delta
    tab$Akaike_weight_AICc[ii] <- w
  }

  tab
}

interpret_delta_min <- function(delta) {
  if (!is.finite(delta)) return("Not estimated")
  if (delta >= CONFIG11B_MIN$delta_threshold) {
    return("Correlation supported after boundary validation")
  }
  if (delta <= -CONFIG11B_MIN$delta_threshold) {
    return("Independent/hidden-rate explanation better supported")
  }
  "Ambiguous support after boundary validation"
}

build_summary_min <- function(tab) {
  rows <- list()
  ids <- unique(tab$Test_ID)

  for (i in seq_along(ids)) {
    test_id <- ids[i]
    d <- tab[tab$Test_ID == test_id, , drop = FALSE]
    e <- d[d$Estimable %in% TRUE & is.finite(d$AICc), , drop = FALSE]

    ind <- e[e$Model_class == "Independent", , drop = FALSE]
    cor <- e[e$Model_class == "Correlated", , drop = FALSE]

    best_ind <- if (nrow(ind)) ind[which.min(ind$AICc), , drop = FALSE] else NULL
    best_cor <- if (nrow(cor)) cor[which.min(cor$AICc), , drop = FALSE] else NULL
    best_all <- if (nrow(e)) e[which.min(e$AICc), , drop = FALSE] else NULL

    delta <- if (!is.null(best_ind) && !is.null(best_cor)) {
      best_ind$AICc[1] - best_cor$AICc[1]
    } else {
      NA_real_
    }

    rows[[i]] <- data.frame(
      Test_ID = test_id,
      Source_script = d$Source_script[1],
      Family = d$Family[1],
      X = d$X[1],
      Y = d$Y[1],
      N = d$N[1],
      Best_independent_model = if (!is.null(best_ind)) best_ind$Model[1] else NA_character_,
      Best_independent_AICc = if (!is.null(best_ind)) best_ind$AICc[1] else NA_real_,
      Best_correlated_model = if (!is.null(best_cor)) best_cor$Model[1] else NA_character_,
      Best_correlated_AICc = if (!is.null(best_cor)) best_cor$AICc[1] else NA_real_,
      Delta_AICc_hidden_favor_correlated = delta,
      Best_overall_model = if (!is.null(best_all)) best_all$Model[1] else NA_character_,
      Best_overall_class = if (!is.null(best_all)) best_all$Model_class[1] else NA_character_,
      Hidden_rate_interpretation = interpret_delta_min(delta),
      N_models_total = nrow(d),
      N_models_estimable = sum(d$Estimable %in% TRUE),
      N_models_excluded_boundary = sum(d$Validation_status == "EXCLUDED: original Rate_max >= 99"),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}


# ==============================================================================
# 5. EXECUCAO
# ==============================================================================

console_banner(
  "SCRIPT 11B MINIMO",
  "Validacao dirigida do unico modelo de fronteira que pode mudar a conclusao"
)

console_step(1, 6, "Carregando o resultado do Script 11")

result11 <- require_saved_rds(
  "11_sensibilidade_pagel_hidden_rates.rds",
  "11_SENSIBILIDADE_PAGEL_HIDDEN_RATES.R"
)

if (is.null(result11$model_comparison) || is.null(result11$results)) {
  stop("O RDS do Script 11 nao contem model_comparison/results esperados.", call. = FALSE)
}

orig <- result11$model_comparison

required_cols <- c(
  "Test_ID", "Source_script", "Family", "X", "Y", "N",
  "Model_object", "Model", "Model_class", "Hidden_rates", "K",
  "LogLik", "AIC", "AICc", "Rate_min", "Rate_max"
)

missing_cols <- setdiff(required_cols, names(orig))
if (length(missing_cols)) {
  stop(
    "Colunas ausentes em model_comparison: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

console_step(2, 6, "Aplicando a regra Rate_max >= 99 aos ajustes originais")

validated <- orig
validated$Original_Rate_max <- validated$Rate_max
validated$Original_upper_boundary <- is.finite(validated$Rate_max) &
  validated$Rate_max >= CONFIG11B_MIN$original_boundary_cutoff
validated$Estimable <- is.finite(validated$AICc) & !validated$Original_upper_boundary
validated$Validation_status <- ifelse(
  !is.finite(validated$AICc),
  "EXCLUDED: non-finite original AICc",
  ifelse(
    validated$Original_upper_boundary,
    "EXCLUDED: original Rate_max >= 99",
    "PASS: original fit below upper-bound cutoff"
  )
)
validated$Boundary_refit <- FALSE
validated$Final_upper_bound <- 100

boundary_rows <- validated[validated$Original_upper_boundary %in% TRUE, c(
  "Test_ID", "Model", "Rate_max", "AICc"
), drop = FALSE]

cat("\nAjustes originais com Rate_max >= 99:\n")
print(boundary_rows, row.names = FALSE)

console_step(3, 6, "Reajustando APENAS o HMIM critico em upper=500 e 1000")

res_target <- result11$results[[TARGET_TEST]]
if (is.null(res_target) || is.null(res_target$fits[[TARGET_MODEL_OBJECT]])) {
  stop("Nao encontrei o ajuste-alvo dentro de result11$results.", call. = FALSE)
}

original_fit <- res_target$fits[[TARGET_MODEL_OBJECT]]
n_tips <- length(original_fit$phy$tip.label)

fit500 <- fit_bound_min(original_fit, 500, n_tips)
fit1000 <- fit_bound_min(original_fit, 1000, n_tips)

stab <- parameter_stability_min(fit500$fit, fit1000$fit)
loglik_change <- abs(fit1000$stats$logLik - fit500$stats$logLik)
loglik_stable <- is.finite(loglik_change) &&
  loglik_change <= CONFIG11B_MIN$loglik_tolerance
parameter_stable <- isTRUE(stab$stable)
final_off_boundary <- !isTRUE(fit1000$stats$Upper_boundary)

recoverable <- isTRUE(fit500$stats$Finite) &&
  isTRUE(fit1000$stats$Finite) &&
  final_off_boundary &&
  loglik_stable &&
  parameter_stable

diagnostic <- data.frame(
  Test_ID = TARGET_TEST,
  Model_object = TARGET_MODEL_OBJECT,
  Model = "Hidden Markov Independent (HMIM)",
  Original_Rate_max = validated$Original_Rate_max[
    validated$Test_ID == TARGET_TEST &
      validated$Model_object == TARGET_MODEL_OBJECT
  ][1],
  LogLik_500 = fit500$stats$logLik,
  AICc_500 = fit500$stats$AICc,
  Rate_min_500 = fit500$stats$Rate_min,
  Rate_max_500 = fit500$stats$Rate_max,
  Upper_boundary_500 = fit500$stats$Upper_boundary,
  LogLik_1000 = fit1000$stats$logLik,
  AICc_1000 = fit1000$stats$AICc,
  Rate_min_1000 = fit1000$stats$Rate_min,
  Rate_max_1000 = fit1000$stats$Rate_max,
  Upper_boundary_1000 = fit1000$stats$Upper_boundary,
  LogLik_change_500_to_1000 = loglik_change,
  LogLik_stable = loglik_stable,
  Max_parameter_relative_change = stab$max_relative_change,
  Parameters_stable = parameter_stable,
  Recoverable = recoverable,
  stringsAsFactors = FALSE
)

cat("\nDIAGNOSTICO DO MODELO CRITICO:\n")
print(diagnostic, row.names = FALSE)

idx_target <- which(
  validated$Test_ID == TARGET_TEST &
    validated$Model_object == TARGET_MODEL_OBJECT
)

if (length(idx_target) != 1L) {
  stop("O modelo-alvo nao corresponde a exatamente uma linha da tabela.", call. = FALSE)
}

if (recoverable) {
  validated$LogLik[idx_target] <- fit1000$stats$logLik
  validated$AIC[idx_target] <- fit1000$stats$AIC
  validated$AICc[idx_target] <- fit1000$stats$AICc
  validated$K[idx_target] <- fit1000$stats$K
  validated$Rate_min[idx_target] <- fit1000$stats$Rate_min
  validated$Rate_max[idx_target] <- fit1000$stats$Rate_max
  validated$Estimable[idx_target] <- TRUE
  validated$Validation_status[idx_target] <- "PASS: recovered after upper-bound validation"
  validated$Boundary_refit[idx_target] <- TRUE
  validated$Final_upper_bound[idx_target] <- 1000
} else {
  validated$Estimable[idx_target] <- FALSE
  validated$Validation_status[idx_target] <- paste0(
    "EXCLUDED after targeted validation: ",
    if (!final_off_boundary) "still at upper boundary; " else "",
    if (!loglik_stable) "logLik unstable; " else "",
    if (!parameter_stable) "parameters unstable; " else ""
  )
  validated$Boundary_refit[idx_target] <- TRUE
  validated$Final_upper_bound[idx_target] <- 1000
}

console_step(4, 6, "Recalculando Delta AICc e pesos entre modelos elegiveis")

validated <- recalculate_selection_min(validated)
summary_min <- build_summary_min(validated)

console_step(5, 6, "Salvando tabelas finais da validacao minima")

write_min(
  validated,
  "TableS38_VALIDATED_MINIMAL. Hiddenrate_model_comparison.csv"
)

write_min(
  summary_min,
  "TableS39_VALIDATED_MINIMAL. Hiddenrate_sensitivity_summary.csv"
)

write_min(
  diagnostic,
  "TableS41_MINIMAL. Critical_boundary_diagnostic.csv"
)

console_step(6, 6, "Salvando objeto e mostrando a conclusao")

result_min <- list(
  config = CONFIG11B_MIN,
  target_test = TARGET_TEST,
  target_model_object = TARGET_MODEL_OBJECT,
  diagnostic = diagnostic,
  validated_models = validated,
  validated_summary = summary_min,
  fit500 = fit500$fit,
  fit1000 = fit1000$fit
)

save_pipeline_rds(
  result_min,
  "11B_minimo_validacao_critica.rds"
)

cat("\nRESULTADO DO CASO CRITICO:\n")
if (recoverable) {
  cat("  HMIM RECUPERADO: o ajuste estabilizou longe do teto.\n")
} else {
  cat("  HMIM NAO RECUPERADO: permanece inelegivel apos a validacao dirigida.\n")
}

cat("\nRESUMO FINAL:\n")
print(
  summary_min[, c(
    "Test_ID",
    "Best_independent_model",
    "Best_correlated_model",
    "Delta_AICc_hidden_favor_correlated",
    "Hidden_rate_interpretation",
    "N_models_excluded_boundary"
  )],
  row.names = FALSE
)

console_output("TableS38_VALIDATED_MINIMAL. Hiddenrate_model_comparison.csv")
console_output("TableS39_VALIDATED_MINIMAL. Hiddenrate_sensitivity_summary.csv")
console_output("TableS41_MINIMAL. Critical_boundary_diagnostic.csv")
console_output("11B_minimo_validacao_critica.rds")
console_output("execucao_11B_minimo_validacao_critica.log")

log_min(
  "SCRIPT 11B MINIMO concluido. Modelo critico recoverable=",
  recoverable
)

console_done(
  "SCRIPT 11B MINIMO",
  "Envie TableS41_MINIMAL e TableS39_VALIDATED_MINIMAL para interpretacao final."
)

# ==============================================================================
# FIM
# ==============================================================================
source(rstudioapi::getActiveDocumentContext()$path, chdir = TRUE)
