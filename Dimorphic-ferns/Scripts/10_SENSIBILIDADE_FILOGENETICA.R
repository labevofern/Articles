# ==============================================================================
# 10_SENSIBILIDADE_FILOGENETICA.R
# ==============================================================================
# OBJETIVO
# ------------------------------------------------------------------------------
# Testar se as PRINCIPAIS conclusoes do artigo dependem da hipotese filogenetica.
#
# ARVORE PRINCIPAL:
#   FTOL consensus tree, dated/ultrametric (a mesma preparada no Script 01).
#
# ARVORE ALTERNATIVA:
#   FTOL maximum-likelihood (ML) tree, dated/ultrametric, obtida da MESMA versao
#   da FTOL instalada no computador.
#
# REGRA CENTRAL DESTA ANALISE:
#   As duas arvores sao podadas para EXATAMENTE o mesmo conjunto de especies.
#   Assim, diferencas entre os resultados nao podem ser atribuidas a uma mudanca
#   de amostragem taxonomica.
#
# ANALISES REPETIDAS NAS DUAS ARVORES:
#   1. Modelos Mk do dimorfismo: ER, SYM, ARD, Ordered_SYM e Ordered_ARD.
#   2. Matriz Q do melhor modelo Mk.
#   3. Estatistica D para presenca/ausencia de dimorfismo.
#   4. Regressao logistica filogenetica: Habit x Dimorphism.
#   5. Os seis testes planejados de Pagel do Script 08.
#
# IMPORTANTE
# ------------------------------------------------------------------------------
# - Este script NAO substitui os Scripts 01-09.
# - Este script NAO escolhe "a melhor arvore".
# - A arvore consenso continua sendo a arvore PRINCIPAL do manuscrito.
# - A arvore ML funciona apenas como teste de sensibilidade/robustez.
# - Nenhum arquivo original dos Scripts 01-09 e apagado ou sobrescrito.
#
# ANTES DE RODAR:
#   Rode pelo menos o Script 01 na versao final da matriz.
#
# MODO FINAL:
#   Por padrao, este script usa:
#     - 50 reinicios aleatorios por modelo Mk;
#     - 5000 permutacoes para D;
#     - 1000 bootstraps em phyloglm.
#
# MODO RAPIDO APENAS PARA TESTAR O CODIGO:
#   Sys.setenv(LEAF_DIMORPHISM_SENS_QUICK = "true")
#   source("10_SENSIBILIDADE_FILOGENETICA.R")
#
# Para os resultados finais, deixe LEAF_DIMORPHISM_SENS_QUICK = false.
# ============================================================================== 


# ==============================================================================
# 0. LOCALIZAR A PASTA DOS SCRIPTS E CARREGAR O SCRIPT 00
# ==============================================================================

get_scripts_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  }
  # Ao usar source(), identifica o proprio arquivo antes do documento ativo.
  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) return(dirname(normalizePath(frame$ofile, winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p, winslash = "/")))
  }
  normalizePath(getwd(), winslash = "/")
}

SCRIPTS_DIR <- get_scripts_dir()
source(file.path(SCRIPTS_DIR, "00_CONFIGURACAO_E_FUNCOES.R"), chdir = TRUE)

required_packages <- c(
  "ape",
  "caper",
  "corHMM",
  "dplyr",
  "ftolr",
  "ggplot2",
  "patchwork",
  "phylolm",
  "phytools",
  "scales"
)
check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# 1. CONFIGURACAO DESTA ANALISE
# ==============================================================================

SENS_QUICK <- is_true(Sys.getenv("LEAF_DIMORPHISM_SENS_QUICK", "false"))

SENS_CONFIG <- list(
  seed = 20260821L,
  quick_mode = SENS_QUICK,
  n_starts = if (SENS_QUICK) 1L else 50L,
  reuse_models = is_true(Sys.getenv("LEAF_DIMORPHISM_REUSE_MODELS", "false")),
  n_permutations_D = if (SENS_QUICK) 100L else 5000L,
  phyloglm_boot = if (SENS_QUICK) 0L else 1000L,
  min_group = 5L,
  alpha = 0.05,
  n_cores = 1L,
  lower_rate_bound = 1e-10,
  upper_rate_bound = 100,
  ultrametric_tolerance = 1e-4
)

set.seed(SENS_CONFIG$seed)

SENS_PATHS <- list(
  data = PATHS$data,
  tables = PATHS$tables_supp,
  figures = PATHS$figures_supp,
  objects = PATHS$objects,
  logs = PATHS$logs
)

SENS_LOG <- file.path(SENS_PATHS$logs, "execucao_sensibilidade_filogenetica.log")
if (file.exists(SENS_LOG)) file.remove(SENS_LOG)

sens_log <- function(..., level = "INFO") {
  txt <- paste(..., collapse = "")
  line <- sprintf(
    "[%s] [%s] %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    level,
    txt
  )
  cat(line, "\n")
  cat(line, "\n", file = windows_safe_path(SENS_LOG), append = TRUE)
  invisible(line)
}

sens_write_csv <- function(x, filename) {
  path <- file.path(SENS_PATHS$tables, filename)
  write_csv_utf8(x, path)
  sens_log("Tabela salva: ", path)
  invisible(path)
}

sens_save_plot <- function(plot, stem, width = 10, height = 7) {
  save_plot_dual(
    plot,
    stem,
    width,
    height,
    directory = SENS_PATHS$figures
  )
}

if (SENS_CONFIG$quick_mode) {
  warning(
    paste0(
      "MODO RAPIDO ATIVADO. Os resultados deste modo NAO devem ser usados no manuscrito. ",
      "n_starts=", SENS_CONFIG$n_starts,
      ", D permutations=", SENS_CONFIG$n_permutations_D,
      ", phyloglm bootstrap=", SENS_CONFIG$phyloglm_boot, "."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# 2. FUNCOES DE CONTROLE DAS ARVORES
# ==============================================================================

sens_correct_near_ultrametric <- function(tree, tolerance = 1e-4) {
  depths <- ape::node.depth.edgelength(tree)[seq_len(ape::Ntip(tree))]
  spread <- max(depths) - min(depths)
  
  if (spread <= .Machine$double.eps^0.5) return(tree)
  
  if (spread > tolerance) {
    stop(
      paste0(
        "A arvore apresenta diferenca de profundidade entre pontas de ",
        signif(spread, 6),
        ", acima da tolerancia de ", tolerance,
        ". A analise foi interrompida para evitar corrigir a arvore silenciosamente."
      ),
      call. = FALSE
    )
  }
  
  target_depth <- max(depths)
  terminal_rows <- match(seq_len(ape::Ntip(tree)), tree$edge[, 2])
  adjustments <- target_depth - depths
  tree$edge.length[terminal_rows] <- tree$edge.length[terminal_rows] + adjustments
  tree
}

sens_validate_tree <- function(tree, tree_name) {
  if (!inherits(tree, "phylo")) {
    stop(tree_name, " nao e um objeto phylo.", call. = FALSE)
  }
  if (!ape::is.rooted(tree)) {
    stop(tree_name, " nao esta enraizada.", call. = FALSE)
  }
  if (is.null(tree$edge.length)) {
    stop(tree_name, " nao possui comprimentos de ramos.", call. = FALSE)
  }
  if (any(!is.finite(tree$edge.length)) || any(tree$edge.length <= 0)) {
    stop(tree_name, " possui comprimentos de ramos invalidos.", call. = FALSE)
  }
  invisible(TRUE)
}


# ==============================================================================
# 3. FUNCOES DOS MODELOS Mk
# ==============================================================================

sens_renumber_rate_matrix <- function(rate_matrix) {
  values <- sort(unique(as.integer(rate_matrix[rate_matrix > 0])))
  output <- rate_matrix
  if (length(values) > 0L) {
    for (i in seq_along(values)) {
      output[rate_matrix == values[i]] <- i
    }
  }
  output
}

sens_make_ordered_rate_matrix <- function(trait_data, symmetric = FALSE) {
  rate_matrix <- corHMM::getStateMat4Dat(trait_data, model = "ARD")$rate.mat
  n_states <- nrow(rate_matrix)
  
  for (i in seq_len(n_states)) {
    for (j in seq_len(n_states)) {
      if (i == j || abs(i - j) > 1L) {
        rate_matrix[i, j] <- 0
      }
    }
  }
  
  if (symmetric) {
    rate_matrix[,] <- 0
    parameter <- 1L
    for (i in seq_len(n_states - 1L)) {
      rate_matrix[i, i + 1L] <- parameter
      rate_matrix[i + 1L, i] <- parameter
      parameter <- parameter + 1L
    }
  }
  
  sens_renumber_rate_matrix(rate_matrix)
}

sens_safe_corhmm_fit <- function(tree, trait_data, specification) {
  args <- list(phy = tree, data = trait_data,
    rate.cat = specification$rate_cat, model = specification$model,
    node.states = "none", root.p = "yang", nstarts = SENS_CONFIG$n_starts,
    n.cores = SENS_CONFIG$n_cores, lower.bound = SENS_CONFIG$lower_rate_bound,
    upper.bound = SENS_CONFIG$upper_rate_bound)
  if (!is.null(specification$rate_matrix)) args$rate.mat <- specification$rate_matrix
  mk_fit_audited(args)

}

sens_fit_mk <- function(tree, trait_data, tree_id) {
  signature <- mk_signature(tree, trait_data, SENS_CONFIG$n_starts,
    SENS_CONFIG$lower_rate_bound, SENS_CONFIG$upper_rate_bound)
  ordered_sym <- sens_make_ordered_rate_matrix(trait_data, symmetric = TRUE)
  ordered_ard <- sens_make_ordered_rate_matrix(trait_data, symmetric = FALSE)
  
  specifications <- list(
    ER = list(model = "ER", rate_cat = 1L, rate_matrix = NULL),
    SYM = list(model = "SYM", rate_cat = 1L, rate_matrix = NULL),
    ARD = list(model = "ARD", rate_cat = 1L, rate_matrix = NULL),
    Ordered_SYM = list(model = "ARD", rate_cat = 1L, rate_matrix = ordered_sym),
    Ordered_ARD = list(model = "ARD", rate_cat = 1L, rate_matrix = ordered_ard)
  )
  
  cache_file <- file.path(
    SENS_PATHS$objects,
    paste0("10_sensitivity_Mk_", tree_id, ".rds")
  )
  
  if (SENS_CONFIG$reuse_models && file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    cache_ok <- identical(cached$signature, signature)
    
    if (cache_ok) {
      sens_log("Reutilizando cache Mk: ", tree_id)
      return(cached$result)
    }
  }
  
  results <- vector("list", length(specifications))
  names(results) <- names(specifications)
  rows <- vector("list", length(specifications))
  
  for (i in seq_along(specifications)) {
    model_name <- names(specifications)[i]
    sens_log("Mk [", tree_id, "] - ajustando ", model_name, "...")
    
    ans <- sens_safe_corhmm_fit(
      tree,
      trait_data,
      specifications[[model_name]]
    )
    results[[model_name]] <- ans
    
    rows[[i]] <- mk_model_row(ans, tree_id, model_name,
      SENS_CONFIG$lower_rate_bound, SENS_CONFIG$upper_rate_bound, "en")
    if (!is.null(ans$audit)) write_csv_utf8(ans$audit,
      file.path(SENS_PATHS$objects, paste0("Diagnostico_reinicios_", tree_id, "_", model_name, ".csv")))
  }
  
  comparison <- dplyr::bind_rows(rows)
  comparison <- mk_select(comparison, "Model", "Akaike_weight")
  best_model <- comparison$Model[comparison$Selecionado]
  comparison$Selected <- comparison$Selecionado

  result <- list(
    comparison = comparison,
    fits = results,
    specifications = specifications,
    best_model = best_model,
    best_fit = results[[best_model]]$fit
  )
  
  saveRDS(
    list(
      signature = signature,
      tree_tip_labels = tree$tip.label,
      trait_species = as.character(trait_data$species),
      trait_states = as.character(trait_data$state),
      n_starts = SENS_CONFIG$n_starts,
      result = result
    ),
    windows_safe_path(cache_file),
    compress = "xz"
  )
  
  result
}

sens_extract_Q <- function(mk_result, tree_id) {
  state_codes <- names(STATE_LABELS)
  q <- mk_q_matrix(mk_result$best_fit, state_codes)

  tab <- expand.grid(
    Origin = state_codes,
    Destination = state_codes,
    stringsAsFactors = FALSE
  )
  tab$Rate_per_Ma <- mapply(
    function(a, b) q[a, b],
    tab$Origin,
    tab$Destination
  )
  tab <- tab[tab$Origin != tab$Destination, , drop = FALSE]
  tab$Origin_name <- unname(STATE_LABELS[tab$Origin])
  tab$Destination_name <- unname(STATE_LABELS[tab$Destination])
  tab$Tree <- tree_id
  tab$Selected_model <- mk_result$best_model
  
  list(matrix = q, table = tab)
}


# ==============================================================================
# 4. ESTATISTICA D
# ==============================================================================

sens_interpret_D <- function(D, p_random, p_brownian, alpha = 0.05) {
  if (p_random < alpha && p_brownian >= alpha) {
    return("Phylogenetic signal; compatible with Brownian threshold")
  }
  if (p_random >= alpha && p_brownian < alpha) {
    return("Compatible with an approximately random distribution")
  }
  if (p_random < alpha && p_brownian < alpha) {
    if (D < 0) return("More clustered than Brownian threshold")
    if (D >= 0 && D <= 1) return("Intermediate between Brownian threshold and random")
    if (D > 1) return("More dispersed than random")
  }
  if (p_random >= alpha && p_brownian >= alpha) {
    return("Inconclusive between D = 0 and D = 1")
  }
  "Not automatically classified"
}

sens_run_D <- function(tree, analysis_data, tree_id, seed_offset = 0L) {
  d <- analysis_data[, c("species", "Dimorfismo_binario"), drop = FALSE]
  d <- d[match(tree$tip.label, d$species), , drop = FALSE]
  
  if (anyNA(d$Dimorfismo_binario)) {
    stop("Valores NA em Dimorfismo_binario na sensibilidade.", call. = FALSE)
  }
  if (length(unique(d$Dimorfismo_binario)) < 2L) {
    stop("Dimorfismo_binario possui apenas um estado.", call. = FALSE)
  }
  
  signal_tree <- tree
  signal_tree$node.label <- NULL
  
  signal_data <- data.frame(
    species = as.character(d$species),
    dimorphism = as.numeric(d$Dimorfismo_binario),
    stringsAsFactors = FALSE
  )
  
  set.seed(SENS_CONFIG$seed + seed_offset)
  fit <- caper::phylo.d(
    data = signal_data,
    phy = signal_tree,
    names.col = species,
    binvar = dimorphism,
    permut = SENS_CONFIG$n_permutations_D
  )
  
  D <- as.numeric(fit$DEstimate)
  p_random <- as.numeric(fit$Pval1)
  p_brownian <- as.numeric(fit$Pval0)
  
  data.frame(
    Tree = tree_id,
    N = nrow(signal_data),
    D = D,
    P_vs_random_D1 = p_random,
    P_vs_Brownian_D0 = p_brownian,
    Interpretation = sens_interpret_D(
      D, p_random, p_brownian, SENS_CONFIG$alpha
    ),
    N_permutations = SENS_CONFIG$n_permutations_D,
    stringsAsFactors = FALSE
  )
}


# ==============================================================================
# 5. REGRESSAO LOGISTICA FILOGENETICA: HABIT x DIMORPHISM
# ==============================================================================

sens_extract_boot_ci <- function(fit, coefficient_names) {
  out <- data.frame(
    Term = coefficient_names,
    Boot_CI95_low_beta = NA_real_,
    Boot_CI95_high_beta = NA_real_,
    stringsAsFactors = FALSE
  )
  
  ci <- fit$bootconfint95
  if (is.null(ci)) return(out)
  
  ci <- as.matrix(ci)
  
  # Forma mais comum: uma linha por parametro e duas colunas de IC.
  if (nrow(ci) >= length(coefficient_names) && ncol(ci) >= 2L) {
    rn <- rownames(ci)
    if (!is.null(rn) && all(coefficient_names %in% rn)) {
      ci2 <- ci[coefficient_names, 1:2, drop = FALSE]
    } else {
      ci2 <- ci[seq_along(coefficient_names), 1:2, drop = FALSE]
      rownames(ci2) <- coefficient_names
    }
    out$Boot_CI95_low_beta <- as.numeric(ci2[, 1])
    out$Boot_CI95_high_beta <- as.numeric(ci2[, 2])
    return(out)
  }
  
  # Forma transposta: duas linhas e uma coluna por parametro.
  if (nrow(ci) >= 2L && ncol(ci) >= length(coefficient_names)) {
    cn <- colnames(ci)
    if (!is.null(cn) && all(coefficient_names %in% cn)) {
      ci2 <- t(ci[1:2, coefficient_names, drop = FALSE])
    } else {
      ci2 <- t(ci[1:2, seq_along(coefficient_names), drop = FALSE])
      rownames(ci2) <- coefficient_names
    }
    out$Boot_CI95_low_beta <- as.numeric(ci2[, 1])
    out$Boot_CI95_high_beta <- as.numeric(ci2[, 2])
  }
  
  out
}

sens_run_phyloglm <- function(tree, analysis_data, tree_id, seed_offset = 0L) {
  ecology <- analysis_data[, c(
    "species", "Dimorfismo_binario", "Habito", "Habito_nome"
  ), drop = FALSE]
  
  ecology$Habito_nome <- factor(
    ecology$Habito_nome,
    levels = unname(HABIT_LABELS)
  )
  
  counts <- table(ecology$Habito_nome)
  valid_levels <- names(counts[counts >= SENS_CONFIG$min_group])
  
  if (length(valid_levels) < 2L) {
    return(data.frame(
      Tree = tree_id,
      Status = "Not estimated: fewer than two habit categories with n >= 5",
      stringsAsFactors = FALSE
    ))
  }
  
  d <- ecology[ecology$Habito_nome %in% valid_levels, , drop = FALSE]
  d$Habito_nome <- droplevels(d$Habito_nome)
  
  preferred_reference <- intersect(
    unname(HABIT_LABELS["0"]),
    levels(d$Habito_nome)
  )
  reference_level <- if (length(preferred_reference)) {
    preferred_reference[1]
  } else {
    levels(d$Habito_nome)[1]
  }
  d$Habito_nome <- stats::relevel(d$Habito_nome, ref = reference_level)
  
  outcome_counts <- table(d$Dimorfismo_binario)
  if (length(outcome_counts) < 2L || min(outcome_counts) < SENS_CONFIG$min_group) {
    return(data.frame(
      Tree = tree_id,
      Status = "Not estimated: insufficient binary response",
      stringsAsFactors = FALSE
    ))
  }
  
  tr <- ape::keep.tip(tree, d$species)
  d <- d[match(tr$tip.label, d$species), , drop = FALSE]
  rownames(d) <- d$species
  
  set.seed(SENS_CONFIG$seed + seed_offset)
  fit <- tryCatch(
    phylolm::phyloglm(
      Dimorfismo_binario ~ Habito_nome,
      data = d,
      phy = tr,
      method = "logistic_MPLE",
      btol = 30,
      boot = SENS_CONFIG$phyloglm_boot,
      full.matrix = FALSE
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(data.frame(
      Tree = tree_id,
      Status = paste0("Failed: ", conditionMessage(fit)),
      stringsAsFactors = FALSE
    ))
  }
  
  coefficient_matrix <- summary(fit)$coefficients
  required_columns <- c("Estimate", "StdErr", "z.value", "p.value")
  if (!all(required_columns %in% colnames(coefficient_matrix))) {
    stop("Colunas inesperadas no resumo phyloglm; nao usar posicoes como substituto.", call. = FALSE)
  }
  coef_tab <- as.data.frame(coefficient_matrix[, required_columns, drop = FALSE])
  coef_tab$Term <- rownames(coef_tab)
  rownames(coef_tab) <- NULL
  names(coef_tab)[1:4] <- c("Estimate", "Std_error", "Z", "P")
  if (any(!is.finite(coef_tab$P)) || any(coef_tab$P < 0 | coef_tab$P > 1))
    stop("Valores de P invalidos no resumo phyloglm.", call. = FALSE)

  coef_tab <- coef_tab[coef_tab$Term != "(Intercept)", , drop = FALSE]
  
  boot_ci <- sens_extract_boot_ci(fit, names(fit$coefficients))
  boot_ci <- boot_ci[boot_ci$Term != "(Intercept)", , drop = FALSE]
  coef_tab <- dplyr::left_join(coef_tab, boot_ci, by = "Term")
  
  coef_tab$Tree <- tree_id
  coef_tab$Habit <- sub("^Habito_nome", "", coef_tab$Term)
  coef_tab$Reference_habit <- reference_level
  coef_tab$OR <- exp(coef_tab$Estimate)
  coef_tab$CI95_low <- exp(coef_tab$Estimate - 1.96 * coef_tab$Std_error)
  coef_tab$CI95_high <- exp(coef_tab$Estimate + 1.96 * coef_tab$Std_error)
  coef_tab$Boot_OR_CI95_low <- exp(coef_tab$Boot_CI95_low_beta)
  coef_tab$Boot_OR_CI95_high <- exp(coef_tab$Boot_CI95_high_beta)
  coef_tab$Alpha_phylogenetic <- fit$alpha
  coef_tab$AIC <- fit$aic
  coef_tab$N <- nrow(d)
  coef_tab$P_FDR <- stats::p.adjust(coef_tab$P, method = "BH")
  coef_tab$FDR_evidence <- coef_tab$P_FDR < SENS_CONFIG$alpha
  coef_tab$Bootstrap_replicates <- SENS_CONFIG$phyloglm_boot
  coef_tab$Bootstrap_failures <- if (!is.null(fit$bootnumFailed)) fit$bootnumFailed else NA_integer_
  coef_tab$Status <- "OK"
  
  coef_tab[, c(
    "Tree", "Habit", "Reference_habit", "Estimate", "Std_error", "Z", "P",
    "P_FDR", "FDR_evidence", "OR", "CI95_low", "CI95_high",
    "Boot_OR_CI95_low", "Boot_OR_CI95_high", "Alpha_phylogenetic", "AIC",
    "N", "Bootstrap_replicates", "Bootstrap_failures", "Status"
  )]
}


# ==============================================================================
# 6. PAGEL: OS MESMOS SEIS TESTES PLANEJADOS DO SCRIPT 08
# ==============================================================================

sens_add_binary_components <- function(data) {
  out <- data
  v <- out$Variacao_Morfologica
  
  out$Mudanca_forma <- ifelse(
    is.na(v),
    NA_integer_,
    as.integer(v %in% c(1L, 4L, 5L, 7L))
  )
  out$Mudanca_lamina <- ifelse(
    is.na(v),
    NA_integer_,
    as.integer(v %in% c(2L, 4L, 6L, 7L))
  )
  out$Mudanca_peciolo <- ifelse(
    is.na(v),
    NA_integer_,
    as.integer(v %in% c(3L, 5L, 6L, 7L))
  )
  
  # Contraste usado nos testes de Pagel exatamente como no Script 08:
  # 0 = hemidimorfa; 1 = holodimorfa; monomorfas = NA.
  p <- out$Padrao_Dimorfismo
  out$Hemidimorfia_vs_Holodimorfia <- ifelse(
    is.na(p) | p == 0L, NA_integer_, as.integer(p == 2L)
  )
  out
}

sens_pagel_one <- function(tree, data, x_name, y_name, family_name, tree_id) {
  d <- data[, c("species", x_name, y_name), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  
  empty_row <- function(status) {
    data.frame(
      Tree = tree_id,
      Family = family_name,
      X = x_name,
      Y = y_name,
      N = nrow(d),
      AIC_independent = NA_real_,
      AIC_dependent = NA_real_,
      Delta_AIC_favor_dependent = NA_real_,
      LR = NA_real_,
      P = NA_real_,
      Status = status,
      stringsAsFactors = FALSE
    )
  }
  
  if (nrow(d) < 10L) return(empty_row("Not estimated: insufficient N"))
  
  tx <- table(d[[x_name]])
  ty <- table(d[[y_name]])
  if (
    length(tx) < 2L || length(ty) < 2L ||
    min(tx) < SENS_CONFIG$min_group ||
    min(ty) < SENS_CONFIG$min_group
  ) {
    return(empty_row("Not estimated: rare binary state"))
  }
  
  joint <- table(d[[x_name]], d[[y_name]])
  if (sum(joint > 0) < 4L) {
    return(empty_row("Not estimated: incomplete 0/1 combinations"))
  }
  
  tr <- ape::keep.tip(tree, d$species)
  d <- d[match(tr$tip.label, d$species), , drop = FALSE]
  
  x <- setNames(as.integer(d[[x_name]]), d$species)
  y <- setNames(as.integer(d[[y_name]]), d$species)
  
  fit <- tryCatch(
    phytools::fitPagel(
      tr,
      x,
      y,
      method = "fitDiscrete",
      model = "ARD",
      dep.var = "xy"
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(empty_row(paste0("Failed: ", conditionMessage(fit))))
  }
  
  data.frame(
    Tree = tree_id,
    Family = family_name,
    X = x_name,
    Y = y_name,
    N = nrow(d),
    AIC_independent = fit$independent.AIC,
    AIC_dependent = fit$dependent.AIC,
    Delta_AIC_favor_dependent = fit$independent.AIC - fit$dependent.AIC,
    LR = fit$lik.ratio,
    P = fit$P,
    Status = "OK",
    stringsAsFactors = FALSE
  )
}

sens_run_pagel <- function(tree, analysis_data, tree_id) {
  dat <- sens_add_binary_components(analysis_data)
  
  pairs <- data.frame(
    Family = c(
      rep("Morphological_components", 3),
      rep("Hemidimorphy_vs_holodimorphy_components", 3)
    ),
    X = c(
      "Mudanca_forma",
      "Mudanca_forma",
      "Mudanca_lamina",
      rep("Hemidimorfia_vs_Holodimorfia", 3)
    ),
    Y = c(
      "Mudanca_lamina",
      "Mudanca_peciolo",
      "Mudanca_peciolo",
      c("Mudanca_forma", "Mudanca_lamina", "Mudanca_peciolo")
    ),
    stringsAsFactors = FALSE
  )
  
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    sens_log(
      "Pagel [", tree_id, "] - ", pairs$X[i], " x ", pairs$Y[i], "..."
    )
    sens_pagel_one(
      tree = tree,
      data = dat,
      x_name = pairs$X[i],
      y_name = pairs$Y[i],
      family_name = pairs$Family[i],
      tree_id = tree_id
    )
  })
  
  out <- dplyr::bind_rows(rows)
  out$P_FDR <- NA_real_
  ok <- out$Status == "OK" & is.finite(out$P)
  if (any(ok)) out$P_FDR[ok] <- stats::p.adjust(out$P[ok], method = "BH")
  out$FDR_evidence <- ifelse(is.finite(out$P_FDR), out$P_FDR < SENS_CONFIG$alpha, NA)
  
  out$Support_class <- dplyr::case_when(
    !is.finite(out$Delta_AIC_favor_dependent) ~ "Not estimated",
    out$Delta_AIC_favor_dependent >= 10 ~ "Strong",
    out$Delta_AIC_favor_dependent >= 4 ~ "Moderate",
    out$Delta_AIC_favor_dependent >= 2 ~ "Some",
    TRUE ~ "Little or none"
  )
  
  out
}


# ==============================================================================
# 7. FUNCOES DE COMPARACAO ENTRE AS DUAS ARVORES
# ==============================================================================

sens_compare_habit <- function(habit_results) {
  d <- habit_results[habit_results$Status == "OK", , drop = FALSE]
  main <- d[d$Tree == "Main_consensus", , drop = FALSE]
  alt <- d[d$Tree == "Alternative_ML", , drop = FALSE]
  
  keep_main <- main[, c(
    "Habit", "OR", "P", "P_FDR", "FDR_evidence",
    "CI95_low", "CI95_high", "Boot_OR_CI95_low", "Boot_OR_CI95_high"
  ), drop = FALSE]
  keep_alt <- alt[, c(
    "Habit", "OR", "P", "P_FDR", "FDR_evidence",
    "CI95_low", "CI95_high", "Boot_OR_CI95_low", "Boot_OR_CI95_high"
  ), drop = FALSE]
  
  names(keep_main)[-1] <- paste0(names(keep_main)[-1], "_Main")
  names(keep_alt)[-1] <- paste0(names(keep_alt)[-1], "_Alternative")
  
  cmp <- merge(keep_main, keep_alt, by = "Habit", all = TRUE)
  cmp$Same_direction <- with(
    cmp,
    ifelse(
      is.na(OR_Main) | is.na(OR_Alternative),
      NA,
      (OR_Main > 1) == (OR_Alternative > 1)
    )
  )
  cmp$Same_FDR_decision <- with(
    cmp,
    ifelse(
      is.na(FDR_evidence_Main) | is.na(FDR_evidence_Alternative),
      NA,
      FDR_evidence_Main == FDR_evidence_Alternative
    )
  )
  # Mantemos a coluna antiga por transparencia, mas ela e deliberadamente
  # conservadora: qualquer cruzamento de OR em torno de 1 e marcado como mudanca
  # de direcao, mesmo quando o efeito e nao significativo nas duas arvores.
  cmp$Robust_direction_and_FDR <- with(
    cmp,
    ifelse(
      is.na(Same_direction) | is.na(Same_FDR_decision),
      NA,
      Same_direction & Same_FDR_decision
    )
  )

  # Regra final de robustez: a decisao FDR deve ser a mesma nas duas arvores.
  # A direcao do efeito so e exigida quando o efeito recebe suporte FDR em pelo
  # menos uma das arvores. Assim, um OR praticamente igual a 1 que cruza de
  # 1.07 para 0.999, por exemplo, nao transforma sozinho um resultado
  # nao-significativo em "nao robusto".
  cmp$Direction_relevant_for_robustness <- with(
    cmp,
    ifelse(
      is.na(FDR_evidence_Main) | is.na(FDR_evidence_Alternative),
      NA,
      FDR_evidence_Main | FDR_evidence_Alternative
    )
  )
  cmp$Robust_effect <- with(
    cmp,
    ifelse(
      is.na(Same_FDR_decision) | is.na(Direction_relevant_for_robustness),
      NA,
      Same_FDR_decision & (!Direction_relevant_for_robustness | Same_direction)
    )
  )
  cmp
}

# Sobrescreve localmente a funcao generica carregada pelo Script 00.
# O resumo de robustez do habito passa a usar `Robust_effect`, mantendo em nota
# quantas comparacoes conservaram tambem a mesma direcao de OR.
sens_robustness_summary <- function(mk_main, mk_alt, D_table,
                                    habit_comparison, pagel_comparison) {
  count <- function(z) paste0(sum(z %in% TRUE), "/", sum(!is.na(z)))
  robust <- function(z) if (all(is.na(z))) NA else all(z[!is.na(z)])

  dm <- D_table$Interpretation[D_table$Tree == "Main_consensus"]
  da <- D_table$Interpretation[D_table$Tree == "Alternative_ML"]
  if (length(dm) != 1L || length(da) != 1L) {
    stop("Interpretacoes D ausentes/duplicadas.", call. = FALSE)
  }

  p <- pagel_comparison$Same_FDR_decision
  habit_robust <- if ("Robust_effect" %in% names(habit_comparison)) {
    habit_comparison$Robust_effect
  } else {
    habit_comparison$Same_FDR_decision
  }

  same_fdr_habit <- habit_comparison$Same_FDR_decision
  same_direction_habit <- habit_comparison$Same_direction
  habit_note <- paste0(
    "Same FDR decision: ", count(same_fdr_habit),
    "; same OR direction: ", count(same_direction_habit),
    ". Direction is required for robustness only when an effect is FDR-supported."
  )

  data.frame(
    Analysis = c(
      "Mk best model",
      "Phylogenetic D interpretation",
      "Habit effects: robust FDR decision across trees",
      "Pagel tests: same FDR decision among estimated tests"
    ),
    Main_tree = c(
      mk_main$best_model,
      dm,
      count(habit_robust),
      count(p)
    ),
    Alternative_tree = c(
      mk_alt$best_model,
      da,
      "See TableS34",
      "See TableS36"
    ),
    Robust = c(
      mk_main$best_model == mk_alt$best_model,
      dm == da,
      robust(habit_robust),
      robust(p)
    ),
    Note = c(
      "",
      "",
      habit_note,
      paste(sum(is.na(p)), "planned test(s) not comparable; excluded from denominator")
    ),
    stringsAsFactors = FALSE
  )
}

sens_compare_pagel <- function(pagel_results) {
  d <- pagel_results
  main <- d[d$Tree == "Main_consensus", , drop = FALSE]
  alt <- d[d$Tree == "Alternative_ML", , drop = FALSE]
  
  main$Test <- paste(main$X, main$Y, sep = " x ")
  alt$Test <- paste(alt$X, alt$Y, sep = " x ")
  
  keep_main <- main[, c(
    "Test", "Family", "Delta_AIC_favor_dependent", "P", "P_FDR",
    "FDR_evidence", "Support_class", "Status"
  ), drop = FALSE]
  keep_alt <- alt[, c(
    "Test", "Family", "Delta_AIC_favor_dependent", "P", "P_FDR",
    "FDR_evidence", "Support_class", "Status"
  ), drop = FALSE]
  
  names(keep_main)[-(1:2)] <- paste0(names(keep_main)[-(1:2)], "_Main")
  names(keep_alt)[-(1:2)] <- paste0(names(keep_alt)[-(1:2)], "_Alternative")
  
  cmp <- merge(keep_main, keep_alt, by = c("Test", "Family"), all = TRUE)
  cmp$Same_FDR_decision <- with(
    cmp,
    ifelse(
      is.na(FDR_evidence_Main) | is.na(FDR_evidence_Alternative) |
        Status_Main != "OK" | Status_Alternative != "OK",
      NA,
      FDR_evidence_Main == FDR_evidence_Alternative
    )
  )
  cmp$Same_support_class <- with(
    cmp,
    ifelse(
      is.na(Support_class_Main) | is.na(Support_class_Alternative) |
        Status_Main != "OK" | Status_Alternative != "OK",
      NA,
      Support_class_Main == Support_class_Alternative
    )
  )
  cmp
}


# ==============================================================================
# 8. FIGURAS DE SENSIBILIDADE
# ==============================================================================

sens_plot_mk <- function(mk_table) {
  d <- mk_table[is.finite(mk_table$Akaike_weight), , drop = FALSE]
  d$Tree <- factor(
    d$Tree,
    levels = c("Main_consensus", "Alternative_ML"),
    labels = c("Main: FTOL consensus", "Alternative: FTOL ML")
  )
  d$Model <- factor(
    d$Model,
    levels = c("ER", "SYM", "ARD", "Ordered_SYM", "Ordered_ARD")
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = Model, y = Akaike_weight, fill = Tree)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.75),
      width = 0.68
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(Akaike_weight < 0.001, "<0.001", sprintf("%.3f", Akaike_weight))
      ),
      position = ggplot2::position_dodge(width = 0.75),
      vjust = -0.35,
      size = 3.4
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.08),
      breaks = seq(0, 1, 0.25),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "Evolutionary model",
      y = "Akaike weight",
      fill = NULL
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(face = "bold")
    )
  
  sens_save_plot(
    p,
    "FigS25. Phylosensitivity - Mk model support",
    width = 9.2,
    height = 6.2
  )
  p
}

sens_plot_D <- function(D_table) {
  d <- D_table
  d$Tree <- factor(
    d$Tree,
    levels = c("Main_consensus", "Alternative_ML"),
    labels = c("Main: FTOL consensus", "Alternative: FTOL ML")
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = D, y = Tree)
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          "D = ", sprintf("%.3f", D),
          "; P[D=0] = ", ifelse(P_vs_Brownian_D0 < 0.001, "<0.001", sprintf("%.3f", P_vs_Brownian_D0)),
          "; P[D=1] = ", ifelse(P_vs_random_D1 < 0.001, "<0.001", sprintf("%.3f", P_vs_random_D1))
        )
      ),
      hjust = -0.08,
      size = 3.5
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.08, 0.55))
    ) +
    ggplot2::labs(
      x = "Phylogenetic D",
      y = NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, 180, 10, 10),
      axis.text.y = ggplot2::element_text(face = "bold")
    )
  
  sens_save_plot(
    p,
    "FigS26. Phylosensitivity - D statistic",
    width = 11.5,
    height = 4.8
  )
  p
}

sens_plot_habit <- function(habit_results) {
  d <- habit_results[habit_results$Status == "OK", , drop = FALSE]
  if (nrow(d) == 0L) return(invisible(NULL))
  
  d$Tree <- factor(
    d$Tree,
    levels = c("Main_consensus", "Alternative_ML"),
    labels = c("Main: FTOL consensus", "Alternative: FTOL ML")
  )
  
  # Preferir IC bootstrap quando disponivel; caso contrario, usar IC assintotico.
  d$Plot_low <- ifelse(
    is.finite(d$Boot_OR_CI95_low),
    d$Boot_OR_CI95_low,
    d$CI95_low
  )
  d$Plot_high <- ifelse(
    is.finite(d$Boot_OR_CI95_high),
    d$Boot_OR_CI95_high,
    d$CI95_high
  )
  
  d$Habit <- factor(d$Habit, levels = rev(unique(d$Habit)))
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = OR, y = Habit, shape = Tree)
  ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = Plot_low, xmax = Plot_high),
      height = 0.18,
      position = ggplot2::position_dodge(width = 0.45)
    ) +
    ggplot2::geom_point(
      size = 3.2,
      position = ggplot2::position_dodge(width = 0.45)
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Odds ratio (log scale)",
      y = NULL,
      shape = NULL
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y = ggplot2::element_text(face = "bold")
    )
  
  sens_save_plot(
    p,
    "FigS27. Phylosensitivity - habit association",
    width = 10.5,
    height = 7.2
  )
  p
}

sens_plot_pagel <- function(pagel_results) {
  d <- pagel_results
  d$Test <- paste(d$X, d$Y, sep = " x ")
  d$Tree <- factor(
    d$Tree,
    levels = c("Main_consensus", "Alternative_ML"),
    labels = c("Main: FTOL consensus", "Alternative: FTOL ML")
  )
  d$Test <- factor(d$Test, levels = rev(unique(d$Test)))
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = Delta_AIC_favor_dependent,
      y = Test,
      shape = Tree
    )
  ) +
    ggplot2::geom_vline(xintercept = 2, linetype = "dashed") +
    ggplot2::geom_point(
      ggplot2::aes(size = FDR_evidence),
      position = ggplot2::position_dodge(width = 0.45)
    ) +
    ggplot2::scale_size_manual(values = c(`FALSE` = 2.8, `TRUE` = 4.3)) +
    ggplot2::labs(
      x = expression(Delta*AIC~"(independent - dependent)"),
      y = NULL,
      shape = NULL,
      size = expression(italic(P)[FDR] < 0.05)
    ) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  sens_save_plot(
    p,
    "FigS28. Phylosensitivity - Pagel tests",
    width = 11.0,
    height = 7.8
  )
  p
}


# ==============================================================================
# 8B. FIGURAS FINAIS EM INGLES: SEPARADAS E PRANCHA CONJUNTA
# ==============================================================================
# As funcoes abaixo substituem apenas a apresentacao grafica. Todas as analises,
# tabelas, comparacoes, objetos e arquivos de log do script permanecem intactos.

SENS_TREE_LEVELS <- c("Main_consensus", "Alternative_ML")
SENS_TREE_LABELS <- c(
  "Main_consensus" = "FTOL consensus",
  "Alternative_ML" = "FTOL maximum likelihood"
)
SENS_TREE_COLORS <- c(
  "FTOL consensus" = "#216A5A",
  "FTOL maximum likelihood" = "#D27D2D"
)
SENS_TREE_SHAPES <- c(
  "FTOL consensus" = 16,
  "FTOL maximum likelihood" = 17
)

sens_tree_factor <- function(x) {
  factor(
    x,
    levels = SENS_TREE_LEVELS,
    labels = unname(SENS_TREE_LABELS[SENS_TREE_LEVELS])
  )
}

sens_theme_figure <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 1.5,
        color = "#111827",
        hjust = 0,
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 1,
        color = "#374151",
        hjust = 0,
        margin = ggplot2::margin(b = 5)
      ),
      axis.title = ggplot2::element_text(
        face = "bold",
        size = base_size,
        color = "#111827"
      ),
      axis.text = ggplot2::element_text(
        size = base_size - 1,
        color = "#111827"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = "#E5E7EB",
        linewidth = 0.35
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        face = "bold",
        size = base_size - 0.5,
        color = "#111827"
      ),
      legend.text = ggplot2::element_text(
        size = base_size - 1,
        color = "#111827"
      ),
      legend.key.width = grid::unit(0.65, "cm"),
      legend.key.height = grid::unit(0.42, "cm"),
      plot.margin = ggplot2::margin(7, 9, 6, 7)
    )
}

sens_add_tree_scales <- function(plot) {
  plot +
    ggplot2::scale_color_manual(
      name = "FTOL tree",
      values = SENS_TREE_COLORS,
      drop = FALSE
    ) +
    ggplot2::scale_shape_manual(
      name = "FTOL tree",
      values = SENS_TREE_SHAPES,
      drop = FALSE
    )
}

sens_plot_mk_final <- function(mk_table) {
  d <- mk_table[is.finite(mk_table$Akaike_weight), , drop = FALSE]
  d$Tree_plot <- sens_tree_factor(d$Tree)
  d$Model <- factor(
    d$Model,
    levels = c("ER", "SYM", "ARD", "Ordered_SYM", "Ordered_ARD"),
    labels = c("ER", "SYM", "ARD", "Ordered SYM", "Ordered ARD")
  )
  d$Weight_label <- ifelse(
    d$Akaike_weight < 0.001,
    "<0.001",
    sprintf("%.3f", d$Akaike_weight)
  )
  d$Weight_label_y <- d$Akaike_weight + 0.045
  dodge <- ggplot2::position_dodge(width = 0.98)
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = Model,
      y = Akaike_weight,
      color = Tree_plot,
      shape = Tree_plot,
      group = Tree_plot
    )
  ) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = 0, ymax = Akaike_weight),
      position = dodge,
      linewidth = 1.1,
      alpha = 0.35,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(position = dodge, size = 3.1) +
    ggplot2::geom_text(
      ggplot2::aes(y = Weight_label_y, label = Weight_label),
      position = dodge,
      vjust = 0,
      size = 2.85,
      color = "#111827",
      show.legend = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-0.045, 1.08),
      breaks = c(0, 0.25, 0.50, 0.75, 1.00),
      labels = scales::label_number(accuracy = 0.01),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "Mk model support",
      subtitle = "Relative support is nearly identical between tree hypotheses.",
      x = "Evolutionary model",
      y = "Akaike weight"
    ) +
    sens_theme_figure(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        face = "bold",
        size = 9.5,
        angle = 18,
        hjust = 1
      )
    )
  
  sens_add_tree_scales(p)
}

sens_plot_D_final <- function(D_table) {
  d <- D_table
  d$Tree_plot <- sens_tree_factor(d$Tree)
  probability_plotmath <- function(x) {
    ifelse(
      is.na(x),
      "==NA",
      ifelse(
        x < 0.001,
        "<'0.001'",
        paste0("=='", sprintf("%.3f", x), "'")
      )
    )
  }
  d$D_label <- paste0(
    "atop(italic(D)==", sprintf("%.3f", d$D), ",",
    "italic(P)*'('*italic(D)*'= 0)'", probability_plotmath(d$P_vs_Brownian_D0),
    "~';'~italic(P)*'('*italic(D)*'= 1)'", probability_plotmath(d$P_vs_random_D1),
    ")"
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = D,
      y = Tree_plot,
      color = Tree_plot,
      shape = Tree_plot
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = 0,
      xmax = 1,
      ymin = -Inf,
      ymax = Inf,
      fill = "#EAF2EE",
      alpha = 0.75
    ) +
    ggplot2::geom_vline(
      xintercept = c(0, 1),
      linetype = "dashed",
      color = "#6B7280",
      linewidth = 0.55
    ) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::geom_text(
      ggplot2::aes(x = 0.05, label = D_label),
      hjust = 0,
      lineheight = 0.95,
      size = 3.0,
      color = "#111827",
      parse = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-0.12, 1.08),
      breaks = c(0, 0.5, 1.0),
      labels = scales::label_number(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "Phylogenetic signal",
      subtitle = expression(
        italic(D) == 0~"denotes Brownian-threshold expectation;"~italic(D) == 1~"denotes randomness."
      ),
      x = expression("Phylogenetic "*italic(D)),
      y = NULL
    ) +
    sens_theme_figure(base_size = 10) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9.5),
      axis.text.y = ggplot2::element_text(face = "bold"),
      panel.grid.major.y = ggplot2::element_blank()
    )
  
  sens_add_tree_scales(p)
}

sens_plot_habit_final <- function(habit_results) {
  d <- habit_results[habit_results$Status == "OK", , drop = FALSE]
  if (nrow(d) == 0L) {
    stop("Nao ha estimativas validas para o painel de habito.", call. = FALSE)
  }
  
  d$Tree_plot <- sens_tree_factor(d$Tree)
  d$Plot_low <- ifelse(
    is.finite(d$Boot_OR_CI95_low),
    d$Boot_OR_CI95_low,
    d$CI95_low
  )
  d$Plot_high <- ifelse(
    is.finite(d$Boot_OR_CI95_high),
    d$Boot_OR_CI95_high,
    d$CI95_high
  )
  d$OR_label <- sprintf("%.2f", d$OR)
  
  habit_order <- unname(HABIT_LABELS[as.character(1:6)])
  observed_habits <- unique(as.character(d$Habit))
  habit_order <- c(
    habit_order[habit_order %in% observed_habits],
    setdiff(observed_habits, habit_order)
  )
  d$Habit <- factor(d$Habit, levels = rev(habit_order))
  dodge <- ggplot2::position_dodge(width = 0.92)
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = OR,
      y = Habit,
      color = Tree_plot,
      shape = Tree_plot,
      group = Tree_plot
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      color = "#6B7280",
      linewidth = 0.55
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = Plot_low, xmax = Plot_high),
      width = 0.14,
      orientation = "y",
      linewidth = 0.65,
      position = dodge
    ) +
    ggplot2::geom_point(size = 2.8, position = dodge) +
    ggplot2::geom_text(
      ggplot2::aes(label = OR_label),
      position = dodge,
      hjust = 0.5,
      vjust = -1.05,
      size = 2.90,
      color = "#111827",
      show.legend = FALSE
    ) +
    ggplot2::scale_x_log10(
      breaks = c(0.5, 1, 2, 5, 10, 20),
      labels = scales::label_number(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0.08, 0.18))
    ) +
    ggplot2::labs(
      title = "Habit association with dimorphism",
      subtitle = "Odds ratios and 95% bootstrap confidence intervals; terrestrial habit is the reference.",
      x = "Odds ratio (log scale)",
      y = NULL
    ) +
    sens_theme_figure(base_size = 9.5) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9.2),
      axis.text.y = ggplot2::element_text(face = "bold", size = 7.8)
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  sens_add_tree_scales(p)
}

sens_plot_pagel_final <- function(pagel_results) {
  d <- pagel_results
  variable_labels <- c(
    "Mudanca_forma" = "Shape change",
    "Mudanca_lamina" = "Lamina architecture change",
    "Mudanca_peciolo" = "Petiole change",
    "Hemidimorfia_vs_Holodimorfia" = "Hemidimorphy vs holodimorphy"
  )
  
  d$X_english <- unname(variable_labels[d$X])
  d$Y_english <- unname(variable_labels[d$Y])
  d$X_english[is.na(d$X_english)] <- d$X[is.na(d$X_english)]
  d$Y_english[is.na(d$Y_english)] <- d$Y[is.na(d$Y_english)]
  d$Test_label <- paste(d$X_english, "vs", d$Y_english)
  d$Tree_plot <- sens_tree_factor(d$Tree)
  d$Delta_label <- ifelse(
    is.finite(d$Delta_AIC_favor_dependent),
    sprintf("%.1f", d$Delta_AIC_favor_dependent),
    ""
  )
  
  preferred_tests <- c(
    "Shape change vs Lamina architecture change",
    "Shape change vs Petiole change",
    "Lamina architecture change vs Petiole change",
    "Hemidimorphy vs holodimorphy vs Shape change",
    "Hemidimorphy vs holodimorphy vs Lamina architecture change",
    "Hemidimorphy vs holodimorphy vs Petiole change"
  )
  present_tests <- unique(d$Test_label)
  test_order <- c(
    preferred_tests[preferred_tests %in% present_tests],
    setdiff(present_tests, preferred_tests)
  )
  d$Test_label <- factor(d$Test_label, levels = rev(test_order))
  
  finite_delta <- d$Delta_AIC_favor_dependent[
    is.finite(d$Delta_AIC_favor_dependent)
  ]
  x_range <- range(finite_delta)
  x_span <- diff(x_range)
  if (!is.finite(x_span) || x_span <= 0) x_span <- 1
  x_missing <- x_range[1] - 0.03 * x_span
  
  d_estimated <- d[is.finite(d$Delta_AIC_favor_dependent), , drop = FALSE]
  missing_tests <- unique(
    d[!is.finite(d$Delta_AIC_favor_dependent), "Test_label", drop = FALSE]
  )
  dodge <- ggplot2::position_dodge(width = 0.54)
  
  p <- ggplot2::ggplot(
    d_estimated,
    ggplot2::aes(
      x = Delta_AIC_favor_dependent,
      y = Test_label,
      color = Tree_plot,
      shape = Tree_plot,
      size = FDR_evidence,
      group = Tree_plot
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = "#9CA3AF",
      linewidth = 0.45
    ) +
    ggplot2::geom_vline(
      xintercept = 2,
      linetype = "dashed",
      color = "#9A6700",
      linewidth = 0.6
    ) +
    ggplot2::geom_point(position = dodge) +
    ggplot2::geom_text(
      ggplot2::aes(label = Delta_label),
      position = dodge,
      hjust = -0.78,
      size = 2.90,
      color = "#111827",
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = missing_tests,
      ggplot2::aes(
        x = x_missing,
        y = Test_label,
        label = "Not estimated"
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 2.80,
      color = "#6B7280"
    ) +
    ggplot2::scale_size_manual(
      name = expression(italic(P)[FDR] < 0.05),
      values = c(`FALSE` = 2.6, `TRUE` = 3.8),
      labels = c(`FALSE` = "No", `TRUE` = "Yes"),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.07, 0.22)),
      labels = scales::label_number(accuracy = 1)
    ) +
    ggplot2::labs(
      title = "Pagel correlated-evolution tests",
      subtitle = "Positive values favor the dependent model; point size denotes FDR evidence.",
      x = expression(Delta*AIC~"(independent - dependent)"),
      y = NULL
    ) +
    sens_theme_figure(base_size = 9.5) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9.2),
      axis.text.y = ggplot2::element_text(size = 7.6)
    )
  
  sens_add_tree_scales(p)
}

sens_build_final_figures <- function(
    mk_table,
    D_table,
    habit_table,
    pagel_table
) {
  required_objects_ok <-
    is.data.frame(mk_table) &&
    is.data.frame(D_table) &&
    is.data.frame(habit_table) &&
    is.data.frame(pagel_table)
  
  if (!required_objects_ok) {
    stop("As tabelas necessarias para construir as figuras nao estao disponiveis.", call. = FALSE)
  }
  
  p_mk <- sens_plot_mk_final(mk_table)
  p_D <- sens_plot_D_final(D_table)
  p_habit <- sens_plot_habit_final(habit_table)
  p_pagel <- sens_plot_pagel_final(pagel_table)
  
  n_all <- unique(D_table$N[is.finite(D_table$N)])[1]
  pagel_n_values <- sort(unique(pagel_table$N[is.finite(pagel_table$N)]))
  pagel_n_label <- if (length(pagel_n_values) == 1L) {
    as.character(pagel_n_values)
  } else {
    paste0(min(pagel_n_values), "–", max(pagel_n_values), " depending on contrast")
  }
  
  plate_theme <- ggplot2::theme(
    legend.position = "none",
    plot.subtitle = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      size = 10.2,
      margin = ggplot2::margin(b = 3)
    ),
    axis.title = ggplot2::element_text(size = 8.7),
    axis.text = ggplot2::element_text(size = 7.9),
    plot.margin = ggplot2::margin(3, 5, 3, 4)
  )
  
  p_mk_plate <- p_mk + plate_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7.6, angle = 18))
  p_D_plate <- p_D + plate_theme
  p_habit_plate <- p_habit + plate_theme +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7.4, face = "bold"))
  p_pagel_plate <- p_pagel + plate_theme +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7.1, face = "bold"))
  
  plate <- (p_mk_plate | p_D_plate) / (p_habit_plate | p_pagel_plate) +
    patchwork::plot_layout(
      heights = c(0.78, 1.55),
      widths = c(1, 1)
    ) +
    patchwork::plot_annotation(
      title = "Phylogenetic sensitivity across FTOL tree hypotheses",
      subtitle = paste0(
        "Green circles: FTOL consensus; orange triangles: FTOL maximum likelihood. ",
        "Same taxon set (n = ", n_all,
        "; Pagel n = ", pagel_n_label, ")."
      ),
      caption = expression(
        paste(
          "Values come from the sensitivity analyses above; plotting does not refit any model. Reference lines: ",
          italic(D), " = 0 and 1, odds ratio = 1, and ", Delta, "AIC = 2. Larger points in the Pagel panel indicate FDR-adjusted evidence (",
          italic(P)[FDR], " < 0.05)."
        )
      ),
      tag_levels = "A",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          size = 15.5,
          color = "#111827",
          hjust = 0
        ),
        plot.subtitle = ggplot2::element_text(
          size = 8.8,
          color = "#374151",
          hjust = 0,
          margin = ggplot2::margin(b = 5)
        ),
        plot.caption = ggplot2::element_text(
          size = 7.5,
          color = "#4B5563",
          hjust = 0,
          margin = ggplot2::margin(t = 4)
        ),
        plot.tag = ggplot2::element_text(
          face = "bold",
          size = 11.5,
          color = "#111827"
        ),
        plot.margin = ggplot2::margin(8, 9, 7, 9)
      )
    )
  
  sens_save_plot(
    p_mk,
    "FigS25. Phylogenetic sensitivity - Mk model support",
    width = 9.2,
    height = 5.8
  )
  sens_save_plot(
    p_D,
    "FigS26. Phylogenetic sensitivity - D statistic",
    width = 9.2,
    height = 4.8
  )
  sens_save_plot(
    p_habit,
    "FigS27. Phylogenetic sensitivity - habit association",
    width = 10.5,
    height = 6.8
  )
  sens_save_plot(
    p_pagel,
    "FigS28. Phylogenetic sensitivity - Pagel tests",
    width = 10.5,
    height = 10.2
  )
  sens_save_plot(
    plate,
    "FigS25-S28. Phylogenetic sensitivity plate",
    width = 11.5,
    height = 10.5
  )
  
  sens_log(
    "Figuras individuais e prancha conjunta salvas em: ",
    SENS_PATHS$figures
  )
  
  invisible(
    list(
      Mk = p_mk,
      D = p_D,
      habit = p_habit,
      Pagel = p_pagel,
      plate = plate
    )
  )
}


# ==============================================================================
# 9. EXECUCAO DA ANALISE
# ==============================================================================

console_banner(
  "SCRIPT 10",
  "Phylogenetic sensitivity: FTOL consensus versus FTOL ML"
)

# ------------------------------------------------------------------------------
# PASSO 1 - CARREGAR OS DADOS E A ARVORE PRINCIPAL DO SCRIPT 01
# ------------------------------------------------------------------------------
console_step(1, 8, "Carregando dados e arvore principal do Script 01")

prepared <- require_saved_rds(
  "01_dados_e_filogenia.rds",
  "01_PREPARAR_DADOS_E_FILOGENIA.R"
)

analysis_data_full <- prepared$data
main_tree_full <- prepared$tree

# Confere se o Script 01 foi executado com a mesma versao da FTOL que esta
# instalada agora. Isso evita comparar arvores de versoes diferentes da FTOL.
main_ftol_version <- NA_character_
if (!is.null(prepared$qc) && all(c("Item", "Valor") %in% names(prepared$qc))) {
  idx_version <- prepared$qc$Item == "FTOL data version"
  if (any(idx_version)) {
    main_ftol_version <- as.character(prepared$qc$Valor[idx_version][1])
  }
}
current_ftol_version <- as.character(ftolr::ft_data_ver())

if (
  !is.na(main_ftol_version) &&
  nzchar(main_ftol_version) &&
  !identical(main_ftol_version, current_ftol_version)
) {
  stop(
    paste0(
      "A arvore principal foi preparada com FTOL ", main_ftol_version,
      ", mas a FTOL instalada agora e ", current_ftol_version, ". ",
      "Rode novamente o Script 01 antes do Script 10 para que as duas arvores ",
      "venham da mesma versao da FTOL."
    ),
    call. = FALSE
  )
}

sens_validate_tree(main_tree_full, "Main FTOL consensus tree")
sens_log("FTOL version usada na sensibilidade: ", current_ftol_version)


# ------------------------------------------------------------------------------
# PASSO 2 - CARREGAR A ARVORE ALTERNATIVA ML DA MESMA FTOL
# ------------------------------------------------------------------------------
console_step(2, 8, "Carregando FTOL maximum-likelihood tree alternativa")

alternative_tree_full <- tryCatch(
  ftolr::ft_tree(
    branch_len = "ultra",
    consensus = FALSE,
    rooted = TRUE,
    drop_og = TRUE
  ),
  error = function(e) e
)

if (inherits(alternative_tree_full, "error")) {
  stop(
    paste0(
      "Nao foi possivel carregar a arvore alternativa FTOL ML datada: ",
      conditionMessage(alternative_tree_full),
      ". Confira a instalacao/versao do pacote ftolr."
    ),
    call. = FALSE
  )
}

sens_validate_tree(alternative_tree_full, "Alternative FTOL ML tree")


# ------------------------------------------------------------------------------
# PASSO 3 - FORCAR O MESMO CONJUNTO DE ESPECIES NAS DUAS ARVORES
# ------------------------------------------------------------------------------
console_step(3, 8, "Mantendo exatamente as mesmas especies nas duas arvores")

common_species <- Reduce(
  intersect,
  list(
    as.character(analysis_data_full$species),
    main_tree_full$tip.label,
    alternative_tree_full$tip.label
  )
)

# Preserva a ordem da arvore principal.
common_species <- main_tree_full$tip.label[
  main_tree_full$tip.label %in% common_species
]

if (length(common_species) < 10L) {
  stop(
    "Menos de 10 especies sao compartilhadas entre dados e as duas arvores.",
    call. = FALSE
  )
}

main_tree <- ape::keep.tip(main_tree_full, common_species)
alternative_tree <- ape::keep.tip(alternative_tree_full, common_species)

main_tree <- sens_correct_near_ultrametric(
  main_tree,
  SENS_CONFIG$ultrametric_tolerance
)
alternative_tree <- sens_correct_near_ultrametric(
  alternative_tree,
  SENS_CONFIG$ultrametric_tolerance
)

sens_validate_tree(main_tree, "Pruned main consensus tree")
sens_validate_tree(alternative_tree, "Pruned alternative ML tree")

analysis_data <- analysis_data_full[
  analysis_data_full$species %in% common_species,
  ,
  drop = FALSE
]

if (nrow(analysis_data) != length(common_species)) {
  stop("Falha ao construir o subconjunto comum de especies.", call. = FALSE)
}

# Compara splits sem raiz e clados com raiz separadamente.
topology_check <- sens_topology_check(main_tree, alternative_tree)
topological_distance <- topology_check$unrooted_distance
topology_identical <- topology_check$rooted_identical

coverage_table <- data.frame(
  Item = c(
    "FTOL data version",
    "Species in prepared data",
    "Species in main consensus tree",
    "Species in alternative ML tree",
    "Species shared by data and both trees",
    "Species removed from prepared data for sensitivity",
    "Main tree ultrametric after pruning",
    "Alternative tree ultrametric after pruning",
    "Unrooted split distance",
    "Topologies identical (rooted clades)"
  ),
  Value = c(
    current_ftol_version,
    nrow(analysis_data_full),
    ape::Ntip(main_tree_full),
    ape::Ntip(alternative_tree_full),
    length(common_species),
    nrow(analysis_data_full) - length(common_species),
    ape::is.ultrametric(main_tree),
    ape::is.ultrametric(alternative_tree),
    topological_distance,
    topology_identical
  ),
  stringsAsFactors = FALSE
)

print(coverage_table)
sens_write_csv(
  coverage_table,
  "TableS29. Phylosensitivity_tree_and_taxon_audit.csv"
)

ape::write.tree(
  main_tree,
  file = windows_safe_path(
    file.path(SENS_PATHS$data, "sensitivity_main_FTOL_consensus.tre")
  )
)
ape::write.tree(
  alternative_tree,
  file = windows_safe_path(
    file.path(SENS_PATHS$data, "sensitivity_alternative_FTOL_ML.tre")
  )
)

excluded_species <- data.frame(
  species = setdiff(analysis_data_full$species, common_species),
  stringsAsFactors = FALSE
)
write_csv_utf8(
  excluded_species,
  file.path(SENS_PATHS$data, "sensitivity_species_excluded_from_common_set.csv")
)


# ------------------------------------------------------------------------------
# PASSO 4 - MODELOS Mk NAS DUAS ARVORES
# ------------------------------------------------------------------------------
console_step(4, 8, "Repetindo os modelos Mk nas duas hipoteses filogeneticas")

trait_main <- data.frame(
  species = main_tree$tip.label,
  state = as.integer(
    analysis_data$Estado[match(main_tree$tip.label, analysis_data$species)]
  ),
  stringsAsFactors = FALSE
)

trait_alternative <- data.frame(
  species = alternative_tree$tip.label,
  state = as.integer(
    analysis_data$Estado[match(alternative_tree$tip.label, analysis_data$species)]
  ),
  stringsAsFactors = FALSE
)

mk_main <- sens_fit_mk(main_tree, trait_main, "Main_consensus")
mk_alt <- sens_fit_mk(alternative_tree, trait_alternative, "Alternative_ML")

mk_table <- dplyr::bind_rows(
  mk_main$comparison,
  mk_alt$comparison
)

print(mk_table)
sens_write_csv(
  mk_table,
  "TableS30. Phylosensitivity_Mk_models.csv"
)

q_main <- sens_extract_Q(mk_main, "Main_consensus")
q_alt <- sens_extract_Q(mk_alt, "Alternative_ML")
q_table <- dplyr::bind_rows(q_main$table, q_alt$table)

sens_write_csv(
  q_table,
  "TableS31. Phylosensitivity_selected_Q_matrices.csv"
)


# ------------------------------------------------------------------------------
# PASSO 5 - ESTATISTICA D NAS DUAS ARVORES
# ------------------------------------------------------------------------------
console_step(5, 8, "Repetindo a estatistica D nas duas arvores")

D_main <- sens_run_D(
  main_tree,
  analysis_data,
  "Main_consensus",
  seed_offset = 10L
)
D_alt <- sens_run_D(
  alternative_tree,
  analysis_data,
  "Alternative_ML",
  seed_offset = 20L
)

D_table <- dplyr::bind_rows(D_main, D_alt)
print(D_table)
sens_write_csv(
  D_table,
  "TableS32. Phylosensitivity_D_statistic.csv"
)


# ------------------------------------------------------------------------------
# PASSO 6 - HABIT x DIMORPHISM NAS DUAS ARVORES
# ------------------------------------------------------------------------------
console_step(6, 8, "Repetindo a regressao filogenetica de habito")

habit_main <- sens_run_phyloglm(
  main_tree,
  analysis_data,
  "Main_consensus",
  seed_offset = 30L
)
habit_alt <- sens_run_phyloglm(
  alternative_tree,
  analysis_data,
  "Alternative_ML",
  seed_offset = 40L
)

habit_table <- dplyr::bind_rows(habit_main, habit_alt)
print(habit_table)
sens_write_csv(
  habit_table,
  "TableS33. Phylosensitivity_habit_association.csv"
)

habit_comparison <- sens_compare_habit(habit_table)
sens_write_csv(
  habit_comparison,
  "TableS34. Phylosensitivity_habit_comparison.csv"
)


# ------------------------------------------------------------------------------
# PASSO 7 - PAGEL NAS DUAS ARVORES
# ------------------------------------------------------------------------------
console_step(7, 8, "Repetindo os seis testes planejados de Pagel")

# Os testes morfologicos mantem as mesmas especies das duas arvores completas.
# Monomorficas entram como ausencia (0) dos componentes, como no Script 08.

pagel_main <- sens_run_pagel(
  main_tree,
  analysis_data,
  "Main_consensus"
)
pagel_alt <- sens_run_pagel(
  alternative_tree,
  analysis_data,
  "Alternative_ML"
)

pagel_table <- dplyr::bind_rows(pagel_main, pagel_alt)
print(pagel_table)
sens_write_csv(
  pagel_table,
  "TableS35. Phylosensitivity_Pagel_tests.csv"
)

pagel_comparison <- sens_compare_pagel(pagel_table)
sens_write_csv(
  pagel_comparison,
  "TableS36. Phylosensitivity_Pagel_comparison.csv"
)


# ------------------------------------------------------------------------------
# PASSO 8 - RESUMO FINAL E FIGURAS
# ------------------------------------------------------------------------------
console_step(8, 8, "Gerando resumo final da robustez e figuras")

mk_best_main <- mk_main$best_model
mk_best_alt <- mk_alt$best_model

summary_table <- sens_robustness_summary(
  mk_main, mk_alt, D_table, habit_comparison, pagel_comparison)

print(summary_table)
sens_write_csv(
  summary_table,
  "TableS37. Phylosensitivity_robustness_summary.csv"
)

# Figuras suplementares finais em ingles: quatro arquivos separados e prancha.
sensitivity_figures <- sens_build_final_figures(
  mk_table = mk_table,
  D_table = D_table,
  habit_table = habit_table,
  pagel_table = pagel_table
)


# ------------------------------------------------------------------------------
# SALVAR OBJETO COMPLETO
# ------------------------------------------------------------------------------

sensitivity_results <- list(
  config = SENS_CONFIG,
  ftol_version = current_ftol_version,
  audit = coverage_table,
  common_species = common_species,
  main_tree = main_tree,
  alternative_tree = alternative_tree,
  Mk = list(main = mk_main, alternative = mk_alt, table = mk_table),
  Q = list(main = q_main, alternative = q_alt, table = q_table),
  D = D_table,
  habit = list(results = habit_table, comparison = habit_comparison),
  Pagel = list(results = pagel_table, comparison = pagel_comparison),
  robustness_summary = summary_table
)

saveRDS(
  sensitivity_results,
  windows_safe_path(
    file.path(SENS_PATHS$objects, "10_sensibilidade_filogenetica.rds")
  ),
  compress = "xz"
)

console_output("TableS29-TableS37")
console_output("FigS25-FigS28 e FigS25-S28. Phylogenetic sensitivity plate")
console_output("Scripts_/_Objetos_R/10_sensibilidade_filogenetica.rds")
console_output("Scripts_/_Logs/execucao_sensibilidade_filogenetica.log")

console_done(
  "SCRIPT 10",
  "Fim do pipeline analitico. Compare a coluna Robust em TableS37."
)

# ==============================================================================
# FIM
# ==============================================================================
