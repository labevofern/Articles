# ==============================================================================
# 06_TAXAS_OCULTAS.R
# ==============================================================================
# ------------------------------------------------------------------------------
# GUIA RAPIDO PARA NAO SE PERDER
# ------------------------------------------------------------------------------
# COMO EXECUTAR:
#   1. Abra ESTE arquivo no RStudio.
#   2. Nao precisa selecionar linhas.
#   3. Clique em Source, no canto superior direito do editor.
#   4. Acompanhe as mensagens >>> PASSO no Console.
#
# ANTES DE RODAR ESTE SCRIPT:
#   01_PREPARAR_DADOS_E_FILOGENIA.R
#
# SE APARECER ERRO DIZENDO QUE UM .RDS NAO EXISTE:
#   Rode primeiro o script indicado em DEPENDENCIAS no cabecalho.
#   Nao apague manualmente arquivos de Scripts_/_Objetos_R/ ou _Modelos/.
#
# O QUE VOCE NORMALMENTE NAO PRECISA EDITAR:
#   - funcoes;
#   - nomes dos objetos;
#   - caminhos de saida;
#   - nomes das tabelas e figuras.
#
# ATENCAO ESPECIAL:
#   Esta etapa pode ser lenta e e pulada no modo rapido.
#
# QUANDO TERMINAR:
#   Depois, rode 07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R.
# ------------------------------------------------------------------------------
# PERGUNTA:
# A velocidade de ganho/perda do dimorfismo e constante em toda a filogenia ou
# existem categorias ocultas de taxas evolutivas?
#
# ANALISE:
# Modelos binarios com uma ou duas categorias de taxas.
#
# PARA QUE SERVE:
# Testa heterogeneidade na velocidade de evolucao do dimorfismo entre linhagens.
# Uma categoria oculta nao identifica automaticamente sua causa biologica.
#
# O QUE OLHAR:
# AICc, Delta AICc e peso de Akaike.
#
# DEPENDENCIAS:
# 01_PREPARAR_DADOS_E_FILOGENIA.R
#
# LE:
# Scripts_/_Objetos_R/01_dados_e_filogenia.rds
#
# OBJETO SALVO:
# Scripts_/_Objetos_R/06_taxas_ocultas.rds
#
# TABELAS GERADAS NESTE SCRIPT:
# TableS10. Modelos de taxas ocultas.csv
#
# FIGURAS GERADAS NESTE SCRIPT:
# Nenhuma.
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
required_packages <- c("ape", "corHMM", "dplyr")
check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# CONFIGURACAO E FUNCOES EXCLUSIVAS DO SCRIPT 06
# ============================================================================== 
quick_mode <- is_true(Sys.getenv("LEAF_DIMORPHISM_QUICK", "false"))
CONFIG <- list(
  run_hidden_rate_models = !quick_mode,
  n_starts = if (quick_mode) 0L else 100L,
  n_cores = 1L,
  lower_rate_bound = 1e-10,
  upper_rate_bound = 100
)

# Funcao segura para ajustar corHMM e capturar erros sem derrubar todo o script.
safe_corhmm_fit <- function(tree, trait_data, specification, node_states = "none") {
  warnings_found <- character(0)

  fit <- tryCatch(
    withCallingHandlers(
      {
        arguments <- list(
          phy = tree,
          data = trait_data,
          rate.cat = specification$rate_cat,
          model = specification$model,
          node.states = node_states,
          root.p = "yang",
          nstarts = CONFIG$n_starts,
          n.cores = CONFIG$n_cores,
          lower.bound = CONFIG$lower_rate_bound,
          upper.bound = CONFIG$upper_rate_bound
        )

        if (!is.null(specification$rate_matrix)) {
          arguments$rate.mat <- specification$rate_matrix
        }

        do.call(corHMM::corHMM, arguments)
      },
      warning = function(warning_condition) {
        warnings_found <<- c(warnings_found, conditionMessage(warning_condition))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error_condition) error_condition
  )

  if (inherits(fit, "error")) {
    return(list(
      fit = NULL,
      error = conditionMessage(fit),
      warnings = unique(warnings_found)
    ))
  }

  list(fit = fit, error = NA_character_, warnings = unique(warnings_found))
}

# Compara modelos binarios com um ou dois regimes de taxas.
# A taxa no limite inferior e tratada como aproximadamente zero e nao exclui
# automaticamente um modelo; falha numerica e limite superior sao impeditivos.
fit_hidden_rate_binary <- function(tree, analysis_data) {
  trait_data <- data.frame(
    species = analysis_data$species,
    state = analysis_data$Dimorfismo_binario
  )
  trait_data <- trait_data[match(tree$tip.label, trait_data$species), ]

  if (!identical(as.character(trait_data$species), as.character(tree$tip.label))) {
    stop("Falha ao alinhar o carater binario e a arvore no Script 06.", call. = FALSE)
  }

  specifications <- list(
    ER_1_categoria = list(model = "ER", rate_cat = 1L, rate_matrix = NULL),
    ARD_1_categoria = list(model = "ARD", rate_cat = 1L, rate_matrix = NULL),
    ARD_2_categorias_ocultas = list(model = "ARD", rate_cat = 2L, rate_matrix = NULL)
  )

  results <- list()
  rows <- list()

  for (model_name in names(specifications)) {
    log_message("Modelo binario de taxas ocultas: ", model_name, "...")
    result <- safe_corhmm_fit(tree, trait_data, specifications[[model_name]])
    results[[model_name]] <- result

    d <- mk_diagnostics(result, CONFIG$lower_rate_bound, CONFIG$upper_rate_bound)
    fit <- result$fit
    ids <- if (!is.null(fit) && !is.null(fit$index.mat)) {
      unique(stats::na.omit(as.vector(fit$index.mat)))
    } else numeric(0)
    ids <- ids[ids > 0]

    rows[[model_name]] <- data.frame(
      Modelo = model_name,
      LogLik = if (!is.null(fit) && is.finite(fit$loglik)) fit$loglik else NA_real_,
      K = if (length(ids)) length(ids) else NA_integer_,
      AIC = if (!is.null(fit) && is.finite(fit$AIC)) fit$AIC else NA_real_,
      AICc = if (!is.null(fit) && is.finite(fit$AICc)) fit$AICc else NA_real_,
      Taxa_minima = if (length(d$rates) && all(is.finite(d$rates))) min(d$rates) else NA_real_,
      Taxa_maxima = if (length(d$rates) && all(is.finite(d$rates))) max(d$rates) else NA_real_,
      Limite_inferior = d$low,
      Limite_superior = d$high,
      Ajuste_numerico_valido = d$numeric_ok,
      Status = d$status,
      Convergencia = d$convergence,
      Avisos = paste(stats::na.omit(c(result$error, result$warnings)), collapse = " | "),
      stringsAsFactors = FALSE
    )
  }

  comparison <- dplyr::bind_rows(rows)

  # Taxa aproximadamente zero e um diagnostico, NAO um veto ao modelo.
  # Limite superior e falha numerica/convergencia tornam o ajuste inelegivel.
  valid <- comparison$Ajuste_numerico_valido &
    !comparison$Limite_superior &
    is.finite(comparison$AICc)

  if (!any(valid)) {
    stop(
      "Nenhum modelo de taxas ocultas apresentou ajuste numericamente valido e elegivel.",
      call. = FALSE
    )
  }

  comparison$Delta_AICc <- NA_real_
  comparison$Peso_Akaike <- NA_real_
  comparison$Delta_AICc[valid] <- comparison$AICc[valid] - min(comparison$AICc[valid])
  weights <- exp(-0.5 * comparison$Delta_AICc[valid])
  comparison$Peso_Akaike[valid] <- weights / sum(weights)
  comparison$Selecionado <- FALSE
  comparison$Selecionado[which(valid)[which.min(comparison$AICc[valid])]] <- TRUE
  comparison <- comparison[order(comparison$AICc), , drop = FALSE]

  saveRDS(
    list(
      comparison = comparison,
      results = results,
      tree_signature = list(
        tip.label = tree$tip.label,
        edge = unclass(tree$edge),
        edge.length = as.numeric(tree$edge.length),
        Nnode = tree$Nnode
      ),
      species = as.character(trait_data$species),
      states = as.integer(trait_data$state),
      policy = MK_POLICY_VERSION
    ),
    windows_safe_path(file.path(PATHS$objects, "modelos_taxas_ocultas_binario.rds"))
  )

  comparison
}

# ----------------------------------------------------------------------------  
# 3. OBSERVACAO SOBRE SAIDAS
# ----------------------------------------------------------------------------  
# Este script 00 nao define nomes, numeracao ou destino cientifico de tabelas e


# ==============================================================================
# EXECUCAO DA ANALISE 06
# ==============================================================================
console_banner("SCRIPT 06", "Modelos de taxas ocultas")
console_step(1, 3, "Carregando dados e arvore do Script 01")
# Carrega a arvore e o estado binario de dimorfismo do Script 01.
prepared <- require_saved_rds("01_dados_e_filogenia.rds", "01_PREPARAR_DADOS_E_FILOGENIA.R")
console_step(2, 3, "Comparando modelos com um ou mais regimes de taxas evolutivas")
console_note("No modo rapido esta etapa e pulada de proposito para economizar tempo.")
# A chave run_hidden_rate_models vem do 00.
# Em modo rapido ela fica FALSE para que voce teste o pipeline sem esperar esta etapa pesada.
if (!CONFIG$run_hidden_rate_models) {
  hidden_models <- data.frame(Modelo = "Nao executado no modo rapido", Status = "Ignorado")
} else {
  # Ajusta os modelos de taxas ocultas ao carater binario de dimorfismo.
  # Este resultado compara a possibilidade de um unico regime de taxas com regimes ocultos adicionais.
  hidden_models <- fit_hidden_rate_binary(prepared$tree, prepared$data)
}
# Mostra a comparacao no Console. O foco e AICc/Delta/Peso, nao interpretar a categoria oculta como uma causa biologica.
print(hidden_models)
console_step(3, 3, "Salvando a comparacao de modelos")
# Salva a comparacao completa no material suplementar.
write_csv_utf8(hidden_models, file.path(PATHS$tables_supp, "TableS10. Modelos de taxas ocultas.csv"))

# ------------------------------------------------------------------------------
# FIGURA DESTA ANALISE
# ------------------------------------------------------------------------------
# ESTA ANALISE NAO GERA UMA FIGURA ESPECIFICA no pipeline atual.
# O resultado e apresentado na tabela e reutilizado nas interpretacoes do artigo.
# Nao foi criada uma figura artificial apenas para preencher esta etapa.

# Guarda o resultado em .rds para documentacao e reutilizacao.
save_pipeline_rds(hidden_models, "06_taxas_ocultas.rds")
console_output("TableS10 e 06_taxas_ocultas.rds")
log_message("SCRIPT 06 concluido.")
console_done("SCRIPT 06", "07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R")
