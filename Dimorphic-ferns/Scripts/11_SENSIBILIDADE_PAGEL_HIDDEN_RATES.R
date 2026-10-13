# ==============================================================================
# 11_SENSIBILIDADE_PAGEL_HIDDEN_RATES.R
# ==============================================================================
# ------------------------------------------------------------------------------
# GUIA RAPIDO PARA NAO SE PERDER
# ------------------------------------------------------------------------------
# COMO EXECUTAR:
#   1. Rode primeiro os Scripts 01, 05, 07 e 08 com a matriz final.
#   2. Abra ESTE arquivo no RStudio.
#   3. Nao precisa selecionar linhas.
#   4. Clique em Source.
#   5. Esta etapa pode ser demorada: fitCorrelationTest ajusta quatro modelos
#      para cada comparacao valida.
#
# IMPORTANTE:
#   - Este script NAO substitui os testes de Pagel dos Scripts 05 e 08.
#   - Ele e uma ANALISE DE SENSIBILIDADE aos modelos de taxas ocultas.
#   - Os Scripts 05 e 08 permanecem intactos.
#   - Todos os Pagel validos dos Scripts 05 e 08 sao verificados, nao apenas os
#     que foram significativos no teste classico.
#
# FUNDAMENTO:
#   O teste classico de Pagel compara um modelo independente com um modelo
#   dependente/correlacionado. Se existir heterogeneidade de taxas nao modelada,
#   o modelo dependente pode receber suporte excessivo. A funcao
#   corHMM::fitCorrelationTest() inclui um Hidden Markov Independent Model (HMIM),
#   que permite heterogeneidade de taxas sem atribui-la aos caracteres focais.
#
# MODELOS COMPARADOS PARA CADA PAR DE CARACTERES:
#   1. Independent
#   2. Hidden Markov Independent (HMIM)
#   3. Correlated
#   4. Hidden Markov Correlated
#
# INTERPRETACAO PRINCIPAL:
#   Delta_AICc_hidden = melhor AICc independente - melhor AICc correlacionado
#
#   >= +2 : a correlacao permanece apoiada mesmo permitindo taxas ocultas
#   -2 a 2: resultado ambiguo
#   <= -2 : modelo independente (possivelmente HMIM) e melhor apoiado
#
# TESTES ORIUNDOS DO SCRIPT 05:
#   - Dimorphic vs Monomorphic x Multiple habit types
#   - Hemidimorphic vs Monomorphic x Multiple habit types
#   - Holodimorphic vs Monomorphic x Multiple habit types
#
# TESTES ORIUNDOS DO SCRIPT 08:
#   - 3 comparacoes entre componentes morfologicos
#   - Hemidimorphic vs Holodimorphic x cada um dos 3 componentes
#   Total: 6 testes planejados no Script 08.
#
# TABELAS GERADAS:
#   TableS38. Hiddenrate_sensitivity_model_comparison.csv
#   TableS39. Hiddenrate_sensitivity_summary.csv
#   TableS40. Classical_Pagel_versus_hiddenrate_sensitivity.csv
#
# FIGURA GERADA:
#   FigS29. Hiddenrate_sensitivity_of_Pagel_tests.pdf/png
#
# OBJETO SALVO:
#   Scripts_/_Objetos_R/11_sensibilidade_pagel_hidden_rates.rds
#
# CACHE DOS MODELOS:
#   Scripts_/_Objetos_R/_HiddenRate_Pagel/
#
# REFERENCIA METODOLOGICA PRINCIPAL:
#   Boyko JD, Beaulieu JM. 2023. Reducing the Biases in False Correlations
#   Between Discrete Characters. Systematic Biology 72:476-488.
#   doi:10.1093/sysbio/syac066
# ==============================================================================


# ==============================================================================
# 1. LOCALIZAR A PASTA DOS SCRIPTS
# ==============================================================================

get_scripts_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p, winslash = "/")))
  }
  normalizePath(getwd(), winslash = "/")
}

SCRIPTS_DIR <- get_scripts_dir()
source(file.path(SCRIPTS_DIR, "00_CONFIGURACAO_E_FUNCOES.R"), chdir = TRUE)


# ==============================================================================
# 2. PACOTES
# ==============================================================================

required_packages <- c(
  "ape",
  "corHMM",
  "dplyr",
  "ggplot2",
  "scales"
)

check_packages(required_packages)
load_packages(required_packages)

if (!exists("fitCorrelationTest", where = asNamespace("corHMM"), inherits = FALSE)) {
  stop(
    paste0(
      "A funcao corHMM::fitCorrelationTest() nao foi encontrada. ",
      "Atualize/instale o pacote corHMM antes de rodar o Script 11."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# 3. CONFIGURACAO EXCLUSIVA DO SCRIPT 11
# ==============================================================================

quick_mode <- is_true(Sys.getenv("LEAF_DIMORPHISM_QUICK", "false"))

CONFIG11 <- list(
  seed = 20260821L,
  quick_mode = quick_mode,
  min_binary_group = 5L,
  reuse_cache = is_true(Sys.getenv("LEAF_DIMORPHISM_REUSE_HIDDEN_PAGEL", "false")),
  simplified_models = FALSE,
  delta_threshold = 2
)

set.seed(CONFIG11$seed)

CACHE_DIR <- file.path(PATHS$objects, "_HiddenRate_Pagel")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

LOG11 <- file.path(PATHS$logs, "execucao_11_sensibilidade_pagel_hidden_rates.log")

log11 <- function(..., level = "INFO") {
  txt <- paste(..., collapse = "")
  line <- sprintf(
    "[%s] [%s] %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    level,
    txt
  )
  cat(line, "\n")
  cat(line, "\n", file = windows_safe_path(LOG11), append = TRUE)
}


# ==============================================================================
# 4. FUNCOES AUXILIARES
# ==============================================================================

safe_name11 <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- sub("^_", "", x)
  x <- sub("_$", "", x)
  x
}

count_parameters11 <- function(fit) {
  if (is.null(fit) || is.null(fit$index.mat)) return(NA_integer_)
  idx <- as.numeric(fit$index.mat)
  idx <- idx[is.finite(idx) & idx > 0]
  if (length(idx) == 0L) return(0L)
  length(unique(idx))
}

extract_rates11 <- function(fit) {
  if (is.null(fit) || is.null(fit$solution)) return(numeric(0))
  x <- as.numeric(fit$solution)
  x[is.finite(x) & x > 0]
}

model_identity11 <- function(model_name) {
  hidden <- grepl("hidden", model_name, ignore.case = TRUE)
  correlated <- grepl("correlated", model_name, ignore.case = TRUE) &&
    !grepl("independent", model_name, ignore.case = TRUE)
  independent <- grepl("independent", model_name, ignore.case = TRUE)
  
  model_label <- dplyr::case_when(
    hidden & independent ~ "Hidden Markov Independent (HMIM)",
    hidden & correlated ~ "Hidden Markov Correlated",
    !hidden & independent ~ "Independent",
    !hidden & correlated ~ "Correlated",
    TRUE ~ model_name
  )
  
  model_class <- if (independent) {
    "Independent"
  } else if (correlated) {
    "Correlated"
  } else {
    "Other"
  }
  
  list(
    model_label = model_label,
    model_class = model_class,
    hidden = hidden
  )
}

extract_model_table11 <- function(fits, test_id, source_script, family, x_label, y_label, n_tips) {
  rows <- lapply(names(fits), function(nm) {
    fit <- fits[[nm]]
    id <- model_identity11(nm)
    
    k <- count_parameters11(fit)
    aic <- if (!is.null(fit$AIC) && is.finite(fit$AIC)) as.numeric(fit$AIC) else NA_real_
    loglik <- if (!is.null(fit$loglik) && is.finite(fit$loglik)) as.numeric(fit$loglik) else NA_real_
    
    aicc <- NA_real_
    if (is.finite(aic) && is.finite(k) && (n_tips - k - 1) > 0) {
      aicc <- aic + (2 * k * (k + 1)) / (n_tips - k - 1)
    }
    
    rates <- extract_rates11(fit)
    rate_min <- if (length(rates) > 0L) min(rates) else NA_real_
    rate_max <- if (length(rates) > 0L) max(rates) else NA_real_
    
    # fitCorrelationTest usa os limites padrao do corHMM. Este diagnostico apenas
    # sinaliza solucoes muito proximas dos limites numericos usuais.
    boundary <- FALSE
    if (length(rates) > 0L) {
      boundary <- any(rates <= 1e-8) || any(rates >= 99)
    }
    
    data.frame(
      Test_ID = test_id,
      Source_script = source_script,
      Family = family,
      X = x_label,
      Y = y_label,
      N = n_tips,
      Model_object = nm,
      Model = id$model_label,
      Model_class = id$model_class,
      Hidden_rates = id$hidden,
      K = k,
      LogLik = loglik,
      AIC = aic,
      AICc = aicc,
      Rate_min = rate_min,
      Rate_max = rate_max,
      Boundary_warning = boundary,
      stringsAsFactors = FALSE
    )
  })
  
  tab <- dplyr::bind_rows(rows)
  valid <- is.finite(tab$AICc)
  
  tab$Delta_AICc <- NA_real_
  tab$Akaike_weight_AICc <- NA_real_
  
  if (any(valid)) {
    tab$Delta_AICc[valid] <- tab$AICc[valid] - min(tab$AICc[valid])
    w <- exp(-0.5 * tab$Delta_AICc[valid])
    tab$Akaike_weight_AICc[valid] <- w / sum(w)
  }
  
  tab
}

validate_binary_pair11 <- function(tree, data, x_col, y_col, min_group = 5L) {
  d <- data[, c("species", x_col, y_col), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  
  common <- intersect(tree$tip.label, d$species)
  if (length(common) < 10L) {
    return(list(ok = FALSE, status = "Ignored: insufficient N", data = d, tree = NULL))
  }
  
  tr <- ape::keep.tip(tree, common)
  d <- d[match(tr$tip.label, d$species), , drop = FALSE]
  
  tx <- table(d[[x_col]])
  ty <- table(d[[y_col]])
  joint <- table(d[[x_col]], d[[y_col]])
  
  if (length(tx) < 2L || length(ty) < 2L || min(tx) < min_group || min(ty) < min_group) {
    return(list(ok = FALSE, status = "Ignored: rare binary state", data = d, tree = tr))
  }
  
  if (sum(joint > 0L) < 4L) {
    return(list(
      ok = FALSE,
      status = "Ignored: incomplete 0/1 combinations",
      data = d,
      tree = tr
    ))
  }
  
  list(ok = TRUE, status = "OK", data = d, tree = tr)
}

run_hidden_correlation_one11 <- function(
    tree,
    data,
    x_col,
    y_col,
    test_id,
    source_script,
    family,
    x_label,
    y_label,
    original_p = NA_real_,
    original_p_fdr = NA_real_,
    original_delta_aic = NA_real_) {
  
  checked <- validate_binary_pair11(
    tree = tree,
    data = data,
    x_col = x_col,
    y_col = y_col,
    min_group = CONFIG11$min_binary_group
  )
  
  if (!checked$ok) {
    summary_row <- data.frame(
      Test_ID = test_id,
      Source_script = source_script,
      Family = family,
      X = x_label,
      Y = y_label,
      N = nrow(checked$data),
      Original_P = original_p,
      Original_P_FDR = original_p_fdr,
      Original_Delta_AIC_favor_dependent = original_delta_aic,
      Original_FDR_evidence = is.finite(original_p_fdr) && original_p_fdr < 0.05,
      Best_independent_model = NA_character_,
      Best_independent_AICc = NA_real_,
      Best_correlated_model = NA_character_,
      Best_correlated_AICc = NA_real_,
      Delta_AICc_hidden_favor_correlated = NA_real_,
      Akaike_weight_independent = NA_real_,
      Akaike_weight_correlated = NA_real_,
      Best_overall_model = NA_character_,
      Best_overall_class = NA_character_,
      Hidden_rate_interpretation = "Not estimated",
      Robustness = "Not estimated",
      Status = checked$status,
      stringsAsFactors = FALSE
    )
    return(list(models = data.frame(), summary = summary_row, fits = NULL))
  }
  
  tr <- checked$tree
  d <- checked$data
  
  cor_data <- data.frame(
    species = d$species,
    X = as.integer(d[[x_col]]),
    Y = as.integer(d[[y_col]]),
    stringsAsFactors = FALSE
  )
  
  cache_file <- file.path(
    CACHE_DIR,
    paste0("hidden_rate_", safe_name11(test_id), ".rds")
  )
  
  cache_ok <- FALSE
  cached <- NULL
  
  # Assinatura completa da arvore usada no teste. Assim um cache antigo nao e
  # reutilizado se topologia ou comprimentos de ramos mudarem mantendo as mesmas
  # especies.
  tree_signature11 <- list(
    tip.label = as.character(tr$tip.label),
    edge = unclass(tr$edge),
    edge.length = as.numeric(tr$edge.length),
    Nnode = as.integer(tr$Nnode),
    rooted = ape::is.rooted(tr)
  )
  
  if (CONFIG11$reuse_cache && file.exists(cache_file)) {
    cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(cached)) {
      cache_ok <-
        !is.null(cached$tree_signature) &&
        identical(cached$tree_signature, tree_signature11) &&
        identical(cached$X, cor_data$X) &&
        identical(cached$Y, cor_data$Y) &&
        identical(cached$corHMM_version, as.character(utils::packageVersion("corHMM"))) &&
        identical(cached$simplified_models, CONFIG11$simplified_models)
    }
  }
  
  if (cache_ok) {
    log11("Reusing hidden-rate cache: ", test_id)
    fits <- cached$fits
  } else {
    log11("Fitting hidden-rate correlation model set: ", test_id)
    
    fits <- tryCatch(
      corHMM::fitCorrelationTest(
        phy = tr,
        data = cor_data,
        simplified_models = CONFIG11$simplified_models
      ),
      error = function(e) e
    )
    
    if (inherits(fits, "error")) {
      summary_row <- data.frame(
        Test_ID = test_id,
        Source_script = source_script,
        Family = family,
        X = x_label,
        Y = y_label,
        N = nrow(d),
        Original_P = original_p,
        Original_P_FDR = original_p_fdr,
        Original_Delta_AIC_favor_dependent = original_delta_aic,
        Original_FDR_evidence = is.finite(original_p_fdr) && original_p_fdr < 0.05,
        Best_independent_model = NA_character_,
        Best_independent_AICc = NA_real_,
        Best_correlated_model = NA_character_,
        Best_correlated_AICc = NA_real_,
        Delta_AICc_hidden_favor_correlated = NA_real_,
        Akaike_weight_independent = NA_real_,
        Akaike_weight_correlated = NA_real_,
        Best_overall_model = NA_character_,
        Best_overall_class = NA_character_,
        Hidden_rate_interpretation = "Not estimated",
        Robustness = "Not estimated",
        Status = paste0("Failed: ", conditionMessage(fits)),
        stringsAsFactors = FALSE
      )
      return(list(models = data.frame(), summary = summary_row, fits = NULL))
    }
    
    saveRDS(
      list(
        fits = fits,
        tree_signature = tree_signature11,
        tree_tips = tr$tip.label,
        X = cor_data$X,
        Y = cor_data$Y,
        corHMM_version = as.character(utils::packageVersion("corHMM")),
        simplified_models = CONFIG11$simplified_models,
        test_id = test_id
      ),
      windows_safe_path(cache_file),
      compress = "xz"
    )
  }
  
  model_table <- extract_model_table11(
    fits = fits,
    test_id = test_id,
    source_script = source_script,
    family = family,
    x_label = x_label,
    y_label = y_label,
    n_tips = nrow(d)
  )
  
  valid_ind <- model_table$Model_class == "Independent" & is.finite(model_table$AICc)
  valid_cor <- model_table$Model_class == "Correlated" & is.finite(model_table$AICc)
  valid_all <- is.finite(model_table$AICc)
  
  if (!any(valid_ind) || !any(valid_cor) || !any(valid_all)) {
    summary_row <- data.frame(
      Test_ID = test_id,
      Source_script = source_script,
      Family = family,
      X = x_label,
      Y = y_label,
      N = nrow(d),
      Original_P = original_p,
      Original_P_FDR = original_p_fdr,
      Original_Delta_AIC_favor_dependent = original_delta_aic,
      Original_FDR_evidence = is.finite(original_p_fdr) && original_p_fdr < 0.05,
      Best_independent_model = NA_character_,
      Best_independent_AICc = NA_real_,
      Best_correlated_model = NA_character_,
      Best_correlated_AICc = NA_real_,
      Delta_AICc_hidden_favor_correlated = NA_real_,
      Akaike_weight_independent = sum(model_table$Akaike_weight_AICc[valid_ind], na.rm = TRUE),
      Akaike_weight_correlated = sum(model_table$Akaike_weight_AICc[valid_cor], na.rm = TRUE),
      Best_overall_model = NA_character_,
      Best_overall_class = NA_character_,
      Hidden_rate_interpretation = "Not estimated",
      Robustness = "Not estimated",
      Status = "Failed: incomplete model set",
      stringsAsFactors = FALSE
    )
    return(list(models = model_table, summary = summary_row, fits = fits))
  }
  
  best_ind_idx <- which(valid_ind)[which.min(model_table$AICc[valid_ind])]
  best_cor_idx <- which(valid_cor)[which.min(model_table$AICc[valid_cor])]
  best_all_idx <- which(valid_all)[which.min(model_table$AICc[valid_all])]
  
  best_ind_aicc <- model_table$AICc[best_ind_idx]
  best_cor_aicc <- model_table$AICc[best_cor_idx]
  delta_hidden <- best_ind_aicc - best_cor_aicc
  
  w_ind <- sum(
    model_table$Akaike_weight_AICc[
      model_table$Model_class == "Independent"
    ],
    na.rm = TRUE
  )
  w_cor <- sum(
    model_table$Akaike_weight_AICc[
      model_table$Model_class == "Correlated"
    ],
    na.rm = TRUE
  )
  
  hidden_interpretation <- dplyr::case_when(
    delta_hidden >= CONFIG11$delta_threshold ~
      "Correlation supported after accounting for hidden-rate heterogeneity",
    delta_hidden <= -CONFIG11$delta_threshold ~
      "Independent/hidden-rate explanation better supported",
    TRUE ~
      "Ambiguous support after hidden-rate comparison"
  )
  
  original_sig <- is.finite(original_p_fdr) && original_p_fdr < 0.05
  
  robustness <- dplyr::case_when(
    original_sig && delta_hidden >= CONFIG11$delta_threshold ~
      "ROBUST: classical Pagel evidence persists with hidden-rate models",
    original_sig && delta_hidden <= -CONFIG11$delta_threshold ~
      "NOT ROBUST: classical Pagel evidence is better explained by independence/rate heterogeneity",
    original_sig ~
      "SENSITIVE: classical Pagel evidence becomes ambiguous with hidden-rate models",
    !original_sig && delta_hidden >= CONFIG11$delta_threshold ~
      "Hidden-rate model set favors correlation, but classical Pagel was not FDR-supported",
    TRUE ~
      "No robust evidence for correlation"
  )
  
  summary_row <- data.frame(
    Test_ID = test_id,
    Source_script = source_script,
    Family = family,
    X = x_label,
    Y = y_label,
    N = nrow(d),
    Original_P = original_p,
    Original_P_FDR = original_p_fdr,
    Original_Delta_AIC_favor_dependent = original_delta_aic,
    Original_FDR_evidence = original_sig,
    Best_independent_model = model_table$Model[best_ind_idx],
    Best_independent_AICc = best_ind_aicc,
    Best_correlated_model = model_table$Model[best_cor_idx],
    Best_correlated_AICc = best_cor_aicc,
    Delta_AICc_hidden_favor_correlated = delta_hidden,
    Akaike_weight_independent = w_ind,
    Akaike_weight_correlated = w_cor,
    Best_overall_model = model_table$Model[best_all_idx],
    Best_overall_class = model_table$Model_class[best_all_idx],
    Hidden_rate_interpretation = hidden_interpretation,
    Robustness = robustness,
    Status = "OK",
    stringsAsFactors = FALSE
  )
  
  list(models = model_table, summary = summary_row, fits = fits)
}


# ==============================================================================
# 5. PREPARAR OS TRES TESTES DO SCRIPT 05
# ==============================================================================

build_script05_tests11 <- function(prepared01, result05) {
  data05 <- prepared01$data
  tree05 <- prepared01$tree
  
  specs <- list(
    Dimorphic_vs_Monomorphic = list(
      keep = c(0L, 1L, 2L),
      response = function(z) as.integer(z > 0L),
      label = "Dimorphic vs Monomorphic"
    ),
    Hemidimorphic_vs_Monomorphic = list(
      keep = c(0L, 1L),
      response = function(z) as.integer(z == 1L),
      label = "Hemidimorphic vs Monomorphic"
    ),
    Holodimorphic_vs_Monomorphic = list(
      keep = c(0L, 2L),
      response = function(z) as.integer(z == 2L),
      label = "Holodimorphic vs Monomorphic"
    )
  )
  
  original_table <- result05$pagel_habit_breadth$table
  tests <- list()
  
  for (nm in names(specs)) {
    spec <- specs[[nm]]
    
    d <- data05[
      data05$Padrao_Dimorfismo %in% spec$keep,
      c("species", "Padrao_Dimorfismo", "Habito"),
      drop = FALSE
    ]
    d <- d[stats::complete.cases(d), , drop = FALSE]
    d$Dimorphism_contrast <- spec$response(d$Padrao_Dimorfismo)
    d$Multiple_habit_types <- as.integer(d$Habito == 6L)
    
    orig <- original_table[original_table$Contraste == spec$label, , drop = FALSE]
    
    tests[[paste0("S05_", nm)]] <- list(
      tree = tree05,
      data = d,
      x_col = "Dimorphism_contrast",
      y_col = "Multiple_habit_types",
      test_id = paste0("S05_", nm),
      source_script = "05_HABITO_E_DIMORFISMO.R",
      family = "Habit breadth vs dimorphism",
      x_label = spec$label,
      y_label = "Multiple habit types",
      original_p = if (nrow(orig)) orig$P[1] else NA_real_,
      original_p_fdr = if (nrow(orig)) orig$P_FDR[1] else NA_real_,
      original_delta_aic = if (nrow(orig)) orig$Delta_AIC_favor_dependente[1] else NA_real_
    )
  }
  
  tests
}


# ==============================================================================
# 6. PREPARAR OS SEIS TESTES DO SCRIPT 08
# ==============================================================================

build_script08_tests11 <- function(core07, result08) {
  dat08 <- result08$data
  required_contrasts08 <- c(
    "Hemidimorfia_vs_Holodimorfia"
  )
  missing_contrasts08 <- setdiff(required_contrasts08, names(dat08))
  if (length(missing_contrasts08) > 0L) {
    stop(
      "O resultado do Script 08 nao contem o contraste Hemidimorfia_vs_Holodimorfia: ",
      paste(missing_contrasts08, collapse = ", "),
      ". Rode novamente o Script 08 ajustado antes do Script 11.",
      call. = FALSE
    )
  }
  if (!any(dat08$Padrao_Dimorfismo == 0L, na.rm = TRUE)) {
    stop(
      "O resultado do Script 08 ainda e da versao antiga sem monomorficas. ",
      "Rode novamente os Scripts 07 e 08 antes do Script 11.",
      call. = FALSE
    )
  }
  # O Script 11 exige explicitamente a arvore salva pelo Script 08.
  # Nao ha fallback para objetos antigos: incompatibilidade deve interromper a analise.
  if (is.null(result08$tree)) {
    stop(
      "O objeto do Script 08 nao contem a arvore usada na analise. Rode novamente 08.",
      call. = FALSE
    )
  }
  tree08 <- result08$tree

  same_tree <-
    identical(as.character(tree08$tip.label), as.character(core07$tree$tip.label)) &&
    identical(unclass(tree08$edge), unclass(core07$tree$edge)) &&
    isTRUE(all.equal(as.numeric(tree08$edge.length), as.numeric(core07$tree$edge.length), tolerance = 0)) &&
    identical(as.integer(tree08$Nnode), as.integer(core07$tree$Nnode))
  same_species <- identical(as.character(dat08$species), as.character(core07$data$species))

  if (!same_tree || !same_species) {
    stop(
      "Os objetos dos Scripts 07 e 08 sao incompatíveis. Rode 07 e 08 novamente antes do 11.",
      call. = FALSE
    )
  }
  original <- result08$Pagel
  
  pair_defs <- data.frame(
    Family_code = c(
      rep("Componentes_morfologicos", 3),
      rep("Hemi_vs_Holo_componentes", 3)
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
    Family_label = c(
      rep("Coevolution among morphological components", 3),
      rep("Hemidimorphy vs holodimorphy × morphological components", 3)
    ),
    X_label = c(
      "Change in frond shape",
      "Change in frond shape",
      "Change in lamina size",
      rep("Hemidimorphic vs holodimorphic", 3)
    ),
    Y_label = c(
      "Change in lamina size",
      "Change in petiole size",
      "Change in petiole size",
      c("Change in frond shape", "Change in lamina size", "Change in petiole size")
    ),
    stringsAsFactors = FALSE
  )
  
  tests <- list()
  
  for (i in seq_len(nrow(pair_defs))) {
    p <- pair_defs[i, , drop = FALSE]
    id <- paste0("S08_", sprintf("%02d", i), "_", p$X, "_", p$Y)
    
    orig <- original[
      original$Familia == p$Family_code &
        original$X == p$X &
        original$Y == p$Y,
      , drop = FALSE
    ]
    
    tests[[id]] <- list(
      tree = tree08,
      data = dat08,
      x_col = p$X,
      y_col = p$Y,
      test_id = id,
      source_script = "08_CORRELACOES_E_SINAL_MORFOLOGICO.R",
      family = p$Family_label,
      x_label = p$X_label,
      y_label = p$Y_label,
      original_p = if (nrow(orig)) orig$P[1] else NA_real_,
      original_p_fdr = if (nrow(orig)) orig$P_FDR[1] else NA_real_,
      original_delta_aic = if (nrow(orig)) orig$Delta_AIC_favor_dependente[1] else NA_real_
    )
  }
  
  tests
}


# ==============================================================================
# 7. FIGURA DE RESUMO
# ==============================================================================

plot_hidden_rate_summary11 <- function(
    summary_table,
    stem = "FigS29. Hiddenrate_sensitivity_of_Pagel_tests",
    width = 13.8,
    height = 14.5) {
  
  d <- summary_table[
    summary_table$Status == "OK" &
      is.finite(summary_table$Delta_AICc_hidden_favor_correlated),
    , drop = FALSE
  ]
  
  if (nrow(d) == 0L) {
    log11("FigS29 not generated: no valid hiddenrate comparisons.", level = "WARNING")
    return(invisible(NULL))
  }
  
  d$Test_label <- ifelse(
    d$Source_script == "05_HABITO_E_DIMORFISMO.R",
    d$X,
    paste(d$X, "×", d$Y)
  )
  
  d$Panel <- dplyr::recode(
    d$Family,
    "Habit breadth vs dimorphism" = "Habit breadth vs dimorphism",
    "Coevolution among morphological components" = "Coevolution among morphological components",
    "Hemidimorphy vs holodimorphy × morphological components" = "Hemidimorphy vs holodimorphy × morphological components",
    .default = d$Family
  )
  
  d$Support <- dplyr::case_when(
    d$Delta_AICc_hidden_favor_correlated >= CONFIG11$delta_threshold ~ "Correlation supported",
    d$Delta_AICc_hidden_favor_correlated <= -CONFIG11$delta_threshold ~ "Independence supported",
    TRUE ~ "Ambiguous"
  )
  
  # Ordenacao
  d <- d[order(d$Panel, d$Delta_AICc_hidden_favor_correlated), , drop = FALSE]
  d$Test_label <- factor(d$Test_label, levels = unique(d$Test_label))
  
  # Rotulos numericos afastados dos pontos. O deslocamento cresce levemente
  # com a magnitude de Delta AICc para evitar que o ponto cubra o primeiro digito.
  d$Delta_label <- sprintf("%.2f", d$Delta_AICc_hidden_favor_correlated)
  label_offset <- pmax(1.6, 0.02 * abs(d$Delta_AICc_hidden_favor_correlated))
  d$Label_x <- ifelse(
    d$Delta_AICc_hidden_favor_correlated >= 0,
    d$Delta_AICc_hidden_favor_correlated + label_offset,
    d$Delta_AICc_hidden_favor_correlated - label_offset
  )
  d$Label_hjust <- ifelse(d$Delta_AICc_hidden_favor_correlated >= 0, 0, 1)
  
  # Paleta
  support_colors <- c(
    "Correlation supported" = "#1B9E77",
    "Ambiguous" = "#D9A441",
    "Independence supported" = "#D95F02"
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = Delta_AICc_hidden_favor_correlated,
      y = Test_label
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = -CONFIG11$delta_threshold,
      ymin = -Inf,
      ymax = Inf,
      fill = "#FDE6D8",
      alpha = 0.55
    ) +
    ggplot2::annotate(
      "rect",
      xmin = -CONFIG11$delta_threshold,
      xmax = CONFIG11$delta_threshold,
      ymin = -Inf,
      ymax = Inf,
      fill = "#F5F5F5",
      alpha = 0.9
    ) +
    ggplot2::annotate(
      "rect",
      xmin = CONFIG11$delta_threshold,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      fill = "#DDF3EC",
      alpha = 0.55
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "solid",
      linewidth = 0.5,
      color = "#5B6573"
    ) +
    ggplot2::geom_vline(
      xintercept = c(-CONFIG11$delta_threshold, CONFIG11$delta_threshold),
      linetype = "dashed",
      linewidth = 0.55,
      color = "#7E8A97"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = Delta_AICc_hidden_favor_correlated,
        yend = Test_label,
        color = Support
      ),
      linewidth = 1.0,
      alpha = 0.65,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = Support,
        fill = Support,
        shape = Support
      ),
      size = 3.8,
      stroke = 0.8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = Label_x,
        label = Delta_label,
        color = Support
      ),
      hjust = d$Label_hjust,
      size = 3.0,
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::facet_wrap(~Panel, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(values = support_colors) +
    ggplot2::scale_fill_manual(values = support_colors) +
    ggplot2::scale_shape_manual(
      values = c(
        "Correlation supported" = 16,
        "Ambiguous" = 18,
        "Independence supported" = 15
      )
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.16, 0.24))
    ) +
    ggplot2::labs(
      x = expression(Delta*AIC[c]~"(best independent - best correlated)"),
      y = NULL,
      color = NULL,
      fill = NULL,
      shape = NULL,
      title = "Sensitivity of Pagel tests to hidden-rate models",
      subtitle = paste0(
        "Positive values favor correlated models; thresholds at ±",
        CONFIG11$delta_threshold,
        "."
      ),
      caption = "Green = correlation remains supported; orange = independence is better supported; yellow = ambiguous."
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 11.5) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#D9DEE5", linewidth = 0.35),
      strip.text = ggplot2::element_text(face = "bold", size = 11.2, color = "#1F2933"),
      axis.text.y = ggplot2::element_text(size = 9.3, color = "#1F2933"),
      axis.text.x = ggplot2::element_text(size = 9.5, color = "#1F2933"),
      axis.title.x = ggplot2::element_text(size = 10.5, face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.spacing.y = grid::unit(0.55, "cm"),
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 10.2, color = "#4B5563"),
      plot.caption = ggplot2::element_text(size = 8.7, color = "#6B7280", hjust = 0),
      plot.margin = ggplot2::margin(10, 24, 10, 10)
    )
  
  save_plot_dual(
    p,
    stem,
    width,
    height,
    directory = PATHS$figures_supp
  )
  
  invisible(p)
}


# ==============================================================================
# 8. EXECUCAO
# ==============================================================================

console_banner("SCRIPT 11", "Sensibilidade dos testes de Pagel a taxas ocultas")

console_step(1, 6, "Carregando os resultados dos Scripts 01, 05, 07 e 08")

prepared01 <- require_saved_rds(
  "01_dados_e_filogenia.rds",
  "01_PREPARAR_DADOS_E_FILOGENIA.R"
)

result05 <- require_saved_rds(
  "05_habito_dimorfismo.rds",
  "05_HABITO_E_DIMORFISMO.R"
)

core07 <- require_saved_rds(
  "resultados_centrais_morfologia.rds",
  "07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R"
)

result08 <- require_saved_rds(
  "08_correlacoes_sinal_morfologico.rds",
  "08_CORRELACOES_E_SINAL_MORFOLOGICO.R"
)

# Validacao encadeada 01 -> 07 -> 08. Nenhum resultado antigo e aceito silenciosamente.
same_tree_01_07 <-
  identical(as.character(prepared01$tree$tip.label), as.character(core07$tree$tip.label)) &&
  identical(unclass(prepared01$tree$edge), unclass(core07$tree$edge)) &&
  isTRUE(all.equal(as.numeric(prepared01$tree$edge.length), as.numeric(core07$tree$edge.length), tolerance = 0))
same_tree_07_08 <- !is.null(result08$tree) &&
  identical(as.character(core07$tree$tip.label), as.character(result08$tree$tip.label)) &&
  identical(unclass(core07$tree$edge), unclass(result08$tree$edge)) &&
  isTRUE(all.equal(as.numeric(core07$tree$edge.length), as.numeric(result08$tree$edge.length), tolerance = 0))
same_species <-
  identical(as.character(prepared01$data$species), as.character(core07$data$species)) &&
  identical(as.character(core07$data$species), as.character(result08$data$species))

if (!same_tree_01_07 || !same_tree_07_08 || !same_species) {
  stop(
    "Incompatibilidade entre os resultados dos Scripts 01, 07 e 08. ",
    "Rode novamente 07 e 08 com a matriz final antes do Script 11.",
    call. = FALSE
  )
}

console_step(2, 6, "Reconstruindo exatamente os tres pares binarios testados no Script 05")
tests05 <- build_script05_tests11(prepared01, result05)

console_step(3, 6, "Reconstruindo exatamente os doze pares binarios testados no Script 08")
tests08 <- build_script08_tests11(core07, result08)

all_tests <- c(tests05, tests08)

cat("\nTotal de testes preparados: ", length(all_tests), "\n", sep = "")
cat("  Script 05: ", length(tests05), "\n", sep = "")
cat("  Script 08: ", length(tests08), "\n\n", sep = "")

console_step(4, 6, "Ajustando Independent, HMIM, Correlated e Hidden Markov Correlated")

results11 <- vector("list", length(all_tests))
names(results11) <- names(all_tests)

for (i in seq_along(all_tests)) {
  spec <- all_tests[[i]]
  
  cat(
    sprintf(
      "\n[%d/%d] %s | %s x %s\n",
      i,
      length(all_tests),
      spec$source_script,
      spec$x_label,
      spec$y_label
    )
  )
  
  results11[[i]] <- run_hidden_correlation_one11(
    tree = spec$tree,
    data = spec$data,
    x_col = spec$x_col,
    y_col = spec$y_col,
    test_id = spec$test_id,
    source_script = spec$source_script,
    family = spec$family,
    x_label = spec$x_label,
    y_label = spec$y_label,
    original_p = spec$original_p,
    original_p_fdr = spec$original_p_fdr,
    original_delta_aic = spec$original_delta_aic
  )
}

model_tables <- dplyr::bind_rows(lapply(results11, `[[`, "models"))
summary_table <- dplyr::bind_rows(lapply(results11, `[[`, "summary"))

console_step(5, 6, "Gerando tabelas comparativas e figura de robustez")

# Tabela completa dos quatro modelos por teste.
write_csv_utf8(
  model_tables,
  file.path(
    PATHS$tables_supp,
    "TableS38. Hiddenrate_sensitivity_model_comparison.csv"
  )
)

# Resumo de um registro por teste.
write_csv_utf8(
  summary_table,
  file.path(
    PATHS$tables_supp,
    "TableS39. Hiddenrate_sensitivity_summary.csv"
  )
)

# Tabela editorial: teste classico versus conclusao apos incluir HMIM.
comparison_editorial <- summary_table %>%
  dplyr::select(
    Source_script,
    Family,
    X,
    Y,
    N,
    Original_P,
    Original_P_FDR,
    Original_Delta_AIC_favor_dependent,
    Original_FDR_evidence,
    Best_independent_model,
    Best_independent_AICc,
    Best_correlated_model,
    Best_correlated_AICc,
    Delta_AICc_hidden_favor_correlated,
    Akaike_weight_independent,
    Akaike_weight_correlated,
    Best_overall_model,
    Best_overall_class,
    Hidden_rate_interpretation,
    Robustness,
    Status
  )

write_csv_utf8(
  comparison_editorial,
  file.path(
    PATHS$tables_supp,
    "TableS40. Classical_Pagel_versus_hiddenrate_sensitivity.csv"
  )
)

plot_hidden_rate_summary11(summary_table)

cat("\n--- HIDDEN-RATE SENSITIVITY SUMMARY ---\n")
print(
  summary_table %>%
    dplyr::select(
      Source_script,
      X,
      Y,
      Original_P_FDR,
      Delta_AICc_hidden_favor_correlated,
      Best_overall_model,
      Robustness,
      Status
    )
)

console_step(6, 6, "Salvando o objeto completo do Script 11")

result_object11 <- list(
  config = CONFIG11,
  corHMM_version = as.character(utils::packageVersion("corHMM")),
  tests = all_tests,
  results = results11,
  model_comparison = model_tables,
  summary = summary_table,
  classical_vs_hidden = comparison_editorial
)

save_pipeline_rds(
  result_object11,
  "11_sensibilidade_pagel_hidden_rates.rds"
)

console_output("TableS38. Hiddenrate_sensitivity_model_comparison.csv")
console_output("TableS39. Hiddenrate_sensitivity_summary.csv")
console_output("TableS40. Classical_Pagel_versus_hiddenrate_sensitivity.csv")
console_output("FigS29. Hiddenrate_sensitivity_of_Pagel_tests.pdf/png")
console_output("11_sensibilidade_pagel_hidden_rates.rds")

log11("SCRIPT 11 concluido com ", nrow(summary_table), " testes resumidos.")
console_done("SCRIPT 11")

# ==============================================================================
# FIM
# ==============================================================================