# ==============================================================================
# 05_HABITO_E_DIMORFISMO.R
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
#   O codigo 6 representa dois ou mais tipos de habito e entra nas analises como categoria real.
#
# QUANDO TERMINAR:
#   Depois, rode 06_TAXAS_OCULTAS.R.
# ------------------------------------------------------------------------------
# PERGUNTAS:
# 1. O habito influencia a probabilidade de uma especie ser dimorfica?
# 2. Ter dois ou mais tipos de habito (codigo 6) influencia o dimorfismo?
# 3. O multihabito (codigo 6) esta relacionado aos diferentes estados dimorficos, sempre em comparacao com monomorfia?
#
# ANALISES:
# Regressao logistica filogenetica com 7 categorias; modelos binarios de
# multihabito; frequencias/proporcoes; qui-quadrado; residuos padronizados;
# arquitetura das especies dimorficas por habito; e teste de Pagel entre
# amplitude de habito e tres contrastes de dimorfismo.
#
# PARA QUE SERVE:
# Testa tanto o efeito do tipo de habito quanto o efeito da amplitude de habito.
# O codigo 6 entra explicitamente nas analises e nao e excluido.
#
# O QUE OLHAR:
# OR, IC95%, P, P_FDR, proporcoes e residuos padronizados.
#
# DEPENDENCIAS:
# 01_PREPARAR_DADOS_E_FILOGENIA.R
#
# LE:
# Scripts_/_Objetos_R/01_dados_e_filogenia.rds
#
# OBJETO SALVO:
# Scripts_/_Objetos_R/05_habito_e_dimorfismo.rds
#
# TABELAS GERADAS NESTE SCRIPT:
# Table4. Relacao entre dimorfismo e habito.csv              
# TableS9. Habito_Dimorfismo.csv      
# TableS22. Modelos binarios de multihabito.csv
# TableS23. Freq_Habito_Dimorf.csv       
# TableS24. Residuos_Habito_Dimorf.csv           
# TableS25. Qui2_Habito_Dimorf.csv             
# TableS26. Habito_Arquitetura.csv          
# TableS21. Pagel_Habito_Dimorf.csv          
#
# FIGURAS GERADAS NESTE SCRIPT:
# Fig2. Associacao entre dimorfismo e habito
# FigS5. Prevalencia do dimorfismo por habito
# FigS19. Heatmap habito x dimorfismo
# FigS20. Residuos habito x dimorfismo
# FigS21. Modelos de multihabito
# FigS22. Heatmap habito x arquitetura
# FigS23. Pagel entre amplitude de habito e dimorfismo
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
required_packages <- c("ape", "dplyr", "ggplot2", "phylolm", "phytools", "scales", "tidyr")
check_packages(required_packages)
load_packages(required_packages)

# Numero de replicacoes bootstrap usado em todas as regressoes logisticas
# filogeneticas deste script. A semente fixa torna os resultados reproduziveis.
PHYLOGLM_BOOT <- 1000L
set.seed(20260821L)

# Configuracao do P global do qui-quadrado Habito x Estado de dimorfismo.
# O P e estimado por Monte Carlo porque algumas celulas da tabela de contingencia
# apresentam frequencias esperadas < 5. A estatistica de Pearson e os residuos
# padronizados continuam sendo calculados pelo teste de Pearson convencional.
CHI_SQUARE_MC_B <- 99999L
CHI_SQUARE_MC_SEED <- 20260821L


# ==============================================================================
# FUNCOES ANALITICAS EXCLUSIVAS DO SCRIPT 05
# ============================================================================== 
# Aqui ficam: regressao filogenetica do habito, contrastes de multihabito e
# tabelas exploratorias. Nenhuma dessas funcoes permanece no Script 00.
extract_phyloglm_coefficients <- function(fit, model_name) {
  model_summary <- summary(fit)
  coefficients <- as.data.frame(model_summary$coefficients)
  coefficients$Termo <- rownames(coefficients)
  rownames(coefficients) <- NULL
  
  required_columns <- c(
    "Estimate", "StdErr", "z.value", "p.value",
    "lowerbootCI", "upperbootCI"
  )
  missing_columns <- setdiff(required_columns, names(coefficients))
  
  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Intervalos bootstrap nao encontrados no resultado do phyloglm. ",
        "Colunas ausentes: ",
        paste(missing_columns, collapse = ", "),
        ". Confira se boot > 0."
      ),
      call. = FALSE
    )
  }
  
  output <- data.frame(
    Modelo = model_name,
    Termo = coefficients$Termo,
    Estimativa = coefficients$Estimate,
    Erro_padrao = coefficients$StdErr,
    Z = coefficients$z.value,
    P = coefficients$p.value,
    OR = exp(coefficients$Estimate),
    IC95_inferior = exp(coefficients$lowerbootCI),
    IC95_superior = exp(coefficients$upperbootCI),
    Alpha_filogenetico = fit$alpha,
    AIC = fit$aic,
    stringsAsFactors = FALSE
  )
  
  output
}

# Modelo filogenetico principal para Habito x Dimorfismo.
# Habito e tratado como fator com sete categorias; o codigo 6 representa dois ou mais tipos de habito.
run_phylogenetic_logistic_models <- function(tree, analysis_data) {
  ecology <- analysis_data[, c(
    "species", "Dimorfismo_binario", "Habito", "Habito_nome"
  )]
  
  ecology$Habito_nome <- factor(
    ecology$Habito_nome,
    levels = unname(HABIT_LABELS)
  )
  
  counts <- table(ecology$Habito_nome)
  valid_levels <- names(counts[counts >= 5L])
  if (length(valid_levels) < 2L) {
    log_message(
      "Regressao de habito nao executada: menos de duas categorias com n >= 5.",
      level = "AVISO"
    )
    return(list(univariate = data.frame(), multivariable = data.frame(), fit = NULL))
  }
  
  model_data <- ecology[ecology$Habito_nome %in% valid_levels, , drop = FALSE]
  model_data$Habito_nome <- droplevels(model_data$Habito_nome)
  
  preferred_reference <- unname(HABIT_LABELS["0"])
  reference_level <- if (preferred_reference %in% levels(model_data$Habito_nome)) {
    preferred_reference
  } else {
    levels(model_data$Habito_nome)[1]
  }
  model_data$Habito_nome <- stats::relevel(model_data$Habito_nome, ref = reference_level)
  
  outcome_counts <- table(model_data$Dimorfismo_binario)
  if (length(outcome_counts) < 2L || min(outcome_counts) < 5L) {
    log_message(
      "Regressao de habito nao executada: desfecho com amostra insuficiente.",
      level = "AVISO"
    )
    return(list(univariate = data.frame(), multivariable = data.frame(), fit = NULL))
  }
  
  model_tree <- ape::keep.tip(tree, model_data$species)
  model_data <- model_data[match(model_tree$tip.label, model_data$species), , drop = FALSE]
  rownames(model_data) <- model_data$species
  
  log_message(
    "Regressao logistica filogenetica com Habito como fator de sete categorias."
  )
  
  fit <- tryCatch(
    phylolm::phyloglm(
      Dimorfismo_binario ~ Habito_nome,
      data = model_data,
      phy = model_tree,
      method = "logistic_MPLE",
      btol = 30,
      boot = PHYLOGLM_BOOT
    ),
    error = function(error_condition) error_condition
  )
  
  if (inherits(fit, "error")) {
    log_message(
      "Falha na regressao filogenetica de habito: ", conditionMessage(fit),
      level = "AVISO"
    )
    return(list(univariate = data.frame(), multivariable = data.frame(), fit = NULL))
  }
  
  contrasts <- extract_phyloglm_coefficients(fit, "Habito_categorico")
  contrasts <- contrasts[contrasts$Termo != "(Intercept)", , drop = FALSE]
  contrasts$Categoria <- sub("^Habito_nome", "", contrasts$Termo)
  contrasts$Categoria_referencia <- reference_level
  contrasts$N <- nrow(model_data)
  contrasts$P_FDR <- stats::p.adjust(contrasts$P, method = "BH")
  contrasts$Evidencia_FDR <- contrasts$P_FDR < 0.05
  contrasts <- contrasts[, c(
    "Modelo", "Categoria", "Categoria_referencia", "Estimativa", "Erro_padrao",
    "Z", "P", "P_FDR", "Evidencia_FDR", "OR", "IC95_inferior",
    "IC95_superior", "Alpha_filogenetico", "AIC", "N"
  )]
  
  saveRDS(
    fit,
    windows_safe_path(file.path(PATHS$objects, "modelo_habito_categorico.rds"))
  )
  list(
    univariate = contrasts,
    multivariable = data.frame(),
    fit = fit,
    reference = reference_level,
    valid_levels = valid_levels
  )
}

# 2.6B MULTIHABITO E CONTRASTES ENTRE ESTADOS DE DIMORFISMO ----------------------
# Complementa a analise categorial de Habito com contrastes binarios focados na
# amplitude de habito. Todos os modelos usam Monomorphic como estado de referencia.
#
# Sao feitas tres comparacoes biologicas:
#   1. Dimorphic vs Monomorphic
#      - Hemidimorphic + Holodimorphic sao reunidas como Dimorphic.
#   2. Hemidimorphic vs Monomorphic
#      - especies Holodimorphic sao excluidas deste contraste.
#   3. Holodimorphic vs Monomorphic
#      - especies Hemidimorphic sao excluidas deste contraste.
#
# Para cada comparacao e testado um unico preditor:
#   - Multiple habit types: codigo 6 (dois ou mais tipos de habito) vs codigos 0-5.
#
# Dessa forma, a FigS21 mostra tres modelos filogeneticos e deixa Monomorphic
# explicitamente como referencia, sem criar um modelo redundante de monomorfia.
run_multihabitat_models <- function(tree, analysis_data) {
  state_monomorphic <- unname(STATE_LABELS["0"])
  state_hemidimorphic <- unname(STATE_LABELS["1"])
  state_holodimorphic <- unname(STATE_LABELS["2"])

  model_data <- analysis_data[, c("species", "Padrao_Dimorfismo", "Habito")]
  model_data$Multihabito <- as.integer(model_data$Habito == 6L)
  
  # Constroi cada resposta garantindo que Monomorphic (codigo 0) seja a referencia.
  build_comparison_data <- function(target_state) {
    if (identical(target_state, "Dimorphic")) {
      subset_data <- model_data[model_data$Padrao_Dimorfismo %in% c(0L, 1L, 2L), , drop = FALSE]
      subset_data$Resposta_binaria <- as.integer(subset_data$Padrao_Dimorfismo %in% c(1L, 2L))
    } else if (identical(target_state, state_hemidimorphic)) {
      subset_data <- model_data[model_data$Padrao_Dimorfismo %in% c(0L, 1L), , drop = FALSE]
      subset_data$Resposta_binaria <- as.integer(subset_data$Padrao_Dimorfismo == 1L)
    } else if (identical(target_state, state_holodimorphic)) {
      subset_data <- model_data[model_data$Padrao_Dimorfismo %in% c(0L, 2L), , drop = FALSE]
      subset_data$Resposta_binaria <- as.integer(subset_data$Padrao_Dimorfismo == 2L)
    } else {
      stop("Estado alvo desconhecido: ", target_state)
    }
    
    subset_data$Estado_alvo <- rep(target_state, nrow(subset_data))
    subset_data$Estado_referencia <- rep(state_monomorphic, nrow(subset_data))
    subset_data
  }
  
  run_one_binary_model <- function(comparison_data, target_state, predictor, predictor_label) {
    reference_state <- state_monomorphic
    model_label <- paste0(target_state, " vs ", reference_state, " ~ ", predictor_label)
    
    subset_data <- comparison_data[, c(
      "species", "Resposta_binaria", predictor, "Estado_alvo", "Estado_referencia"
    ), drop = FALSE]
    subset_data <- subset_data[stats::complete.cases(subset_data), , drop = FALSE]
    
    empty_result <- function(status_text, n_value = nrow(subset_data), alpha = NA_real_, aic = NA_real_) {
      data.frame(
        Modelo = model_label,
        Comparacao = paste0(target_state, " vs ", reference_state),
        Estado_alvo = target_state,
        Estado_referencia = reference_state,
        Resposta = "Resposta_binaria",
        Preditor = predictor,
        Preditor_label = predictor_label,
        Categoria = predictor,
        Estimativa = NA_real_,
        Erro_padrao = NA_real_,
        Z = NA_real_,
        P = NA_real_,
        OR = NA_real_,
        IC95_inferior = NA_real_,
        IC95_superior = NA_real_,
        Alpha_filogenetico = alpha,
        AIC = aic,
        N = n_value,
        Status = status_text,
        stringsAsFactors = FALSE
      )
    }
    
    if (length(unique(subset_data$Resposta_binaria)) < 2L ||
        length(unique(subset_data[[predictor]])) < 2L) {
      return(empty_result("Variacao insuficiente"))
    }
    
    freq_response <- table(subset_data$Resposta_binaria)
    freq_predictor <- table(subset_data[[predictor]])
    if (min(freq_response) < 5L || min(freq_predictor) < 5L) {
      return(empty_result("Amostra insuficiente"))
    }
    
    local_tree <- ape::keep.tip(tree, subset_data$species)
    subset_data <- subset_data[match(local_tree$tip.label, subset_data$species), , drop = FALSE]
    rownames(subset_data) <- subset_data$species
    
    fit <- tryCatch(
      phylolm::phyloglm(
        stats::as.formula(paste("Resposta_binaria ~", predictor)),
        data = subset_data,
        phy = local_tree,
        method = "logistic_MPLE",
        btol = 30,
        boot = PHYLOGLM_BOOT
      ),
      error = function(error_condition) error_condition
    )
    
    if (inherits(fit, "error")) {
      return(empty_result(paste("Falhou:", conditionMessage(fit))))
    }
    
    coef_table <- as.data.frame(summary(fit)$coefficients)
    coef_table$Termo <- rownames(coef_table)
    coef_table <- coef_table[coef_table$Termo == predictor, , drop = FALSE]
    
    if (nrow(coef_table) == 0L) {
      return(empty_result(
        "Coeficiente ausente",
        n_value = nrow(subset_data),
        alpha = fit$alpha,
        aic = fit$aic
      ))
    }
    
    required_columns <- c(
      "Estimate", "StdErr", "z.value", "p.value",
      "lowerbootCI", "upperbootCI"
    )
    missing_columns <- setdiff(required_columns, names(coef_table))
    
    if (length(missing_columns) > 0L) {
      return(empty_result(
        paste0(
          "Intervalo bootstrap ausente: ",
          paste(missing_columns, collapse = ", ")
        ),
        n_value = nrow(subset_data),
        alpha = fit$alpha,
        aic = fit$aic
      ))
    }
    
    est <- coef_table$Estimate[1]
    se <- coef_table$StdErr[1]
    zval <- coef_table$z.value[1]
    pval <- coef_table$p.value[1]
    boot_ci_lower <- coef_table$lowerbootCI[1]
    boot_ci_upper <- coef_table$upperbootCI[1]
    
    data.frame(
      Modelo = model_label,
      Comparacao = paste0(target_state, " vs ", reference_state),
      Estado_alvo = target_state,
      Estado_referencia = reference_state,
      Resposta = "Resposta_binaria",
      Preditor = predictor,
      Preditor_label = predictor_label,
      Categoria = predictor,
      Estimativa = est,
      Erro_padrao = se,
      Z = zval,
      P = pval,
      OR = exp(est),
      IC95_inferior = exp(boot_ci_lower),
      IC95_superior = exp(boot_ci_upper),
      Alpha_filogenetico = fit$alpha,
      AIC = fit$aic,
      N = nrow(subset_data),
      Status = "OK",
      stringsAsFactors = FALSE
    )
  }
  
  dimorphic_data <- build_comparison_data("Dimorphic")
  hemidimorphic_data <- build_comparison_data(state_hemidimorphic)
  holodimorphic_data <- build_comparison_data(state_holodimorphic)
  
  output <- dplyr::bind_rows(
    # Dimorphic vs Monomorphic
    run_one_binary_model(dimorphic_data, "Dimorphic", "Multihabito", "Multiple habit types"),
    # Hemidimorphic vs Monomorphic
    run_one_binary_model(hemidimorphic_data, state_hemidimorphic, "Multihabito", "Multiple habit types"),
    # Holodimorphic vs Monomorphic
    run_one_binary_model(holodimorphic_data, state_holodimorphic, "Multihabito", "Multiple habit types")
  )
  
  # Correcao FDR aplicada conjuntamente aos tres testes da familia da FigS21.
  output$P_FDR <- stats::p.adjust(output$P, method = "BH")
  output$Evidencia_FDR <- output$P_FDR < 0.05
  
  output
}

# Tabelas exploratorias para visualizar onde as categorias de habito concentram
# monomorfia, hemidimorfia e holodimorfia. Tambem calcula residuos padronizados
# do qui-quadrado para destacar excessos e deficits relativos.
run_habit_exploratory_tables <- function(analysis_data) {
  counts <- analysis_data %>%
    dplyr::mutate(
      Habito_nome = factor(unname(HABIT_LABELS[as.character(Habito)]), levels = unname(HABIT_LABELS)),
      Estado_nome = factor(unname(STATE_LABELS[as.character(Padrao_Dimorfismo)]), levels = unname(STATE_LABELS))
    ) %>%
    dplyr::count(Habito_nome, Estado_nome, name = "N") %>%
    tidyr::complete(Habito_nome, Estado_nome, fill = list(N = 0L)) %>%
    dplyr::group_by(Habito_nome) %>%
    dplyr::mutate(Proporcao = if (sum(N) > 0) N / sum(N) else rep(NA_real_, dplyr::n())) %>%
    dplyr::ungroup()
  
  xtab <- xtabs(N ~ Habito_nome + Estado_nome, data = counts)

  # Teste de Pearson convencional: fornece a estatistica, frequencias esperadas
  # e residuos padronizados. O warning sobre aproximacao assintotica e suprimido
  # porque o P global abaixo e estimado separadamente por Monte Carlo.
  chisq <- tryCatch(
    suppressWarnings(stats::chisq.test(xtab)),
    error = function(e) NULL
  )

  # P global por Monte Carlo. A semente e temporaria: o estado do gerador
  # aleatorio e restaurado ao sair deste bloco para nao alterar resultados
  # estocasticos das analises executadas depois deste teste.
  chisq_mc <- NULL
  if (!is.null(chisq)) {
    had_random_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_random_seed) {
      old_random_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }

    chisq_mc <- tryCatch(
      {
        set.seed(CHI_SQUARE_MC_SEED)
        suppressWarnings(
          stats::chisq.test(
            xtab,
            simulate.p.value = TRUE,
            B = CHI_SQUARE_MC_B
          )
        )
      },
      error = function(e) NULL,
      finally = {
        if (had_random_seed) {
          assign(".Random.seed", old_random_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }
    )
  }
  
  # IMPORTANTE: nunca associar chisq$stdres a `counts` com as.vector().
  # Matrizes em R sao vetorizadas por colunas, enquanto `counts` pode estar em
  # outra ordem. Isso deslocava residuos entre as combinacoes habito x estado.
  # A conversao via as.table() preserva explicitamente os nomes de linha/coluna.
  if (!is.null(chisq)) {
    habit_levels_res <- if (is.factor(counts$Habito_nome)) levels(counts$Habito_nome) else unique(as.character(counts$Habito_nome))
    state_levels_res <- if (is.factor(counts$Estado_nome)) levels(counts$Estado_nome) else unique(as.character(counts$Estado_nome))

    residuals_table <- as.data.frame(
      as.table(chisq$stdres),
      responseName = "Residuo_padronizado",
      stringsAsFactors = FALSE
    )
    names(residuals_table)[1:2] <- c("Habito_nome", "Estado_nome")

    residuals_table$Habito_nome <- factor(
      as.character(residuals_table$Habito_nome),
      levels = habit_levels_res
    )
    residuals_table$Estado_nome <- factor(
      as.character(residuals_table$Estado_nome),
      levels = state_levels_res
    )

    residuals_table <- residuals_table[order(
      residuals_table$Habito_nome,
      residuals_table$Estado_nome
    ), , drop = FALSE]
    rownames(residuals_table) <- NULL
  } else {
    residuals_table <- counts[, c("Habito_nome", "Estado_nome")]
    residuals_table$Residuo_padronizado <- NA_real_
  }
  
  test_table <- if (!is.null(chisq)) {
    data.frame(
      Estatistica = unname(chisq$statistic),
      gl = (nrow(xtab) - 1L) * (ncol(xtab) - 1L),
      P = if (!is.null(chisq_mc)) chisq_mc$p.value else chisq$p.value,
      Metodo = if (!is.null(chisq_mc)) {
        paste0(
          "Pearson's Chi-squared test; P by Monte Carlo simulation (",
          CHI_SQUARE_MC_B,
          " replicates)"
        )
      } else {
        chisq$method
      },
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Estatistica = NA_real_,
      gl = NA_real_,
      P = NA_real_,
      Metodo = "Qui-quadrado nao executado",
      stringsAsFactors = FALSE
    )
  }
  
  architecture_counts <- analysis_data %>%
    dplyr::mutate(
      Habito_nome = factor(unname(HABIT_LABELS[as.character(Habito)]), levels = unname(HABIT_LABELS)),
      Arquitetura_nome = factor(unname(ARCHITECTURE_LABELS[as.character(Arquitetura_Lamina)]), levels = unname(ARCHITECTURE_LABELS[as.character(0:6)]))
    ) %>%
    dplyr::count(Habito_nome, Arquitetura_nome, name = "N") %>%
    tidyr::complete(Habito_nome, Arquitetura_nome, fill = list(N = 0L)) %>%
    dplyr::group_by(Habito_nome) %>%
    dplyr::mutate(Proporcao = if (sum(N) > 0) N / sum(N) else rep(NA_real_, dplyr::n())) %>%
    dplyr::ungroup()
  
  list(
    counts = counts,
    residuals = residuals_table,
    test = test_table,
    architecture = architecture_counts
  )
}

# 2.7 SINAL FILOGENETICO E TAXAS OCULTAS --------------------------------------
# Calcula a estatistica D para dimorfismo ausente versus presente.


# ==============================================================================
# FUNCOES ESPECIFICAS DESTA ANALISE
# Estas funcoes ficam aqui para que a estrutura de tabelas/figuras desta etapa
# seja visivel no proprio script e nao fique escondida no script 00.
# ==============================================================================

# FIGURA DESCRITIVA: calcula e mostra a proporcao de especies dimorficas em cada habito.
# Ela serve para visualizar o padrao bruto; a inferencia formal vem da regressao filogenetica.
plot_habit_prevalence <- function(analysis_data, stem = "FigS5. Prevalence of dimorphism by habit", width = 10.4, height = 7.0) {
  monomorphic_label <- unname(STATE_LABELS["0"])
  # Resume, para cada categoria de habito, total de especies e quantas sao dimorficas.
  habit_summary <- analysis_data %>%
    dplyr::mutate(
      Habito_nome = unname(HABIT_LABELS[as.character(Habito)])
    ) %>%
    dplyr::group_by(Habito_nome) %>%
    dplyr::summarise(
      Total = dplyr::n(),
      N_dimorficas = sum(Dimorfismo_binario == 1L),
      Proporcao_dimorficas = N_dimorficas / Total,
      .groups = "drop"
    ) %>%
    dplyr::arrange(Proporcao_dimorficas)
  
  plot_data <- dplyr::bind_rows(
    habit_summary %>%
      dplyr::transmute(
        Habito_nome, Dimorfismo = "Dimorphic", N = N_dimorficas,
        Proporcao = Proporcao_dimorficas, Total
      ),
    habit_summary %>%
      dplyr::transmute(
        Habito_nome, Dimorfismo = monomorphic_label, N = Total - N_dimorficas,
        Proporcao = 1 - Proporcao_dimorficas, Total
      )
  ) %>%
    dplyr::ungroup()
  
  order_habits <- habit_summary$Habito_nome
  plot_data$Habito_nome <- factor(plot_data$Habito_nome, levels = order_habits)
  
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Proporcao, y = Habito_nome, fill = Dimorfismo)
  ) +
    ggplot2::geom_col(width = 0.74) +
    ggplot2::geom_text(
      data = plot_data[plot_data$Dimorfismo == "Dimorphic", ],
      ggplot2::aes(
        y = Habito_nome,
        label = paste0(scales::percent(Proporcao, accuracy = 1), "  (n=", Total, ")")
      ),
      x = 1.02,
      hjust = 0,
      size = 3.1,
      color = "#263238",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      c(unname(PALETTE_DIMORPHISM["0"]), "#D95F02"),
      c(monomorphic_label, "Dimorphic")
    )) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.25),
      limits = c(0, 1.12),
      expand = c(0, 0)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = "Observed prevalence of dimorphism by habit",
      subtitle = "Descriptive pattern; formal inference is based on phylogenetic logistic regression",
      x = "Proportion of species",
      y = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "top",
      plot.margin = ggplot2::margin(10, 65, 10, 10)
    )
  
  save_plot_dual(plot, stem, width, height)
}

# FOREST PLOT: representa os odds ratios estimados pela regressao filogenetica.
# OR > 1 sugere maior chance relativa de dimorfismo; OR < 1 sugere menor chance, sempre considerando IC e P.
plot_ecology_forest <- function(
    univariate_table,
    stem = "Fig2. Associacao entre dimorfismo e habito",
    width = 13.2,
    height = 7.2
) {
  if (nrow(univariate_table) == 0L) return(invisible(NULL))
  
  # ---------------------------------------------------------------------------
  # FIGURA 2 PRINCIPAL DO ARTIGO
  # Forest plot da regressao logistica filogenetica.
  #
  # Objetivo visual:
  # - destacar magnitude do efeito (OR) e incerteza (IC95%);
  # - manter OR = 1 como referencia biologica;
  # - diferenciar resultados com e sem evidencia apos correcao FDR;
  # - mostrar OR, IC95%, P bruto e P_FDR diretamente na figura;
  # - mostrar P em italico e FDR como subscrito;
  # - evitar titulo/subtitulo dentro do painel, deixando a figura pronta para
  #   receber a legenda editorial do manuscrito.
  # ---------------------------------------------------------------------------
  
  plot_data <- univariate_table[
    is.finite(univariate_table$OR) &
      is.finite(univariate_table$IC95_inferior) &
      is.finite(univariate_table$IC95_superior) &
      univariate_table$OR > 0 &
      univariate_table$IC95_inferior > 0 &
      univariate_table$IC95_superior > 0,
    ,
    drop = FALSE
  ]
  
  if (nrow(plot_data) == 0L) return(invisible(NULL))
  
  # Mantem o forest plot organizado pela magnitude do OR.
  plot_data <- plot_data[order(plot_data$OR, decreasing = FALSE), , drop = FALSE]
  plot_data$Categoria_plot <- factor(
    as.character(plot_data$Categoria),
    levels = as.character(plot_data$Categoria)
  )
  
  # Status de evidencia apos correcao por FDR.
  plot_data$Evidencia_plot <- factor(
    ifelse(plot_data$Evidencia_FDR %in% TRUE, "FDR < 0.05", "FDR \u2265 0.05"),
    levels = c("FDR \u2265 0.05", "FDR < 0.05")
  )
  
  # Formata P bruto e P ajustado com o mesmo criterio.
  format_p_value <- function(x) {
    ifelse(
      is.na(x),
      "NA",
      ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
    )
  }
  
  # Bloco numerico da direita.
  # plotmath permite P em italico e FDR como subscrito.
  plot_data$Resultado_plot <- paste0(
    "\"OR = ", sprintf("%.2f", plot_data$OR),
    "  [", sprintf("%.2f", plot_data$IC95_inferior),
    "\u2013", sprintf("%.2f", plot_data$IC95_superior), "]    \"",
    " * italic(P) * ",
    "\" = ", format_p_value(plot_data$P), "    \"",
    " * italic(P)[FDR] * ",
    "\" = ", format_p_value(plot_data$P_FDR), "\""
  )
  
  # Rotulo do P bruto acima do ponto/IC.
  plot_data$P_bruto_label <- ifelse(
    is.na(plot_data$P),
    "italic(P) == NA",
    ifelse(
      plot_data$P < 0.001,
      "italic(P) < 0.001",
      paste0("italic(P) == ", sprintf("%.3f", plot_data$P))
    )
  )
  
  # Categoria de referencia do modelo, para deixar a figura autoexplicativa.
  reference_level <- unique(stats::na.omit(plot_data$Categoria_referencia))
  reference_note <- if (length(reference_level) > 0L) {
    paste0(
      "Reference category: ", reference_level[1],
      ". Points = odds ratios; horizontal lines = 95% confidence intervals."
    )
  } else {
    "Points = odds ratios; horizontal lines = 95% confidence intervals."
  }
  
  # Limites robustos para a escala logaritmica.
  x_min <- min(plot_data$IC95_inferior, na.rm = TRUE)
  x_max <- max(plot_data$IC95_superior, na.rm = TRUE)
  
  log_min <- log10(x_min)
  log_max <- log10(x_max)
  log_span <- max(log_max - log_min, 0.8)
  
  x_limits <- c(
    10^(log_min - 0.08 * log_span),
    10^(log_max + 0.08 * log_span)
  )
  
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = OR,
      y = Categoria_plot,
      color = Evidencia_plot
    )
  ) +
    
    # Linha nula: OR = 1.
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "22",
      linewidth = 0.78,
      color = "#8896A3"
    ) +
    
    # Guias horizontais discretas para acompanhar cada categoria.
    ggplot2::geom_hline(
      yintercept = seq_along(levels(plot_data$Categoria_plot)),
      linewidth = 0.42,
      color = "#EDF1F4"
    ) +
    
    # Intervalos de confianca.
    ggplot2::geom_segment(
      ggplot2::aes(
        x = IC95_inferior,
        xend = IC95_superior,
        yend = Categoria_plot
      ),
      linewidth = 1.25,
      lineend = "round",
      alpha = 0.95
    ) +
    
    # Ponto central do odds ratio.
    ggplot2::geom_point(
      size = 4.4,
      stroke = 0.75
    ) +
    
    # P bruto acima do ponto/intervalo.
    ggplot2::geom_text(
      ggplot2::aes(label = P_bruto_label),
      nudge_y = 0.30,
      vjust = 0,
      size = 3.1,
      color = "#263238",
      family = "sans",
      parse = TRUE,
      show.legend = FALSE
    ) +
    
    # OR, IC95%, P bruto e P_FDR em uma unica linha por categoria.
    ggplot2::geom_text(
      ggplot2::aes(
        x = Inf,
        label = Resultado_plot
      ),
      hjust = -0.045,
      size = 3.45,
      color = "#263238",
      family = "sans",
      parse = TRUE,
      show.legend = FALSE
    ) +
    
    # Escala logaritmica adequada para odds ratios.
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = scales::breaks_log(n = 7),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    
    # Paleta editorial.
    ggplot2::scale_color_manual(
      values = c(
        "FDR \u2265 0.05" = "#667784",
        "FDR < 0.05" = "#B94E3D"
      ),
      drop = FALSE
    ) +
    
    ggplot2::labs(
      x = "Odds ratio (95% CI; log scale)",
      y = NULL,
      color = expression("FDR-adjusted " * italic(P)),
      caption = reference_note
    ) +
    
    ggplot2::coord_cartesian(clip = "off") +
    
    ggplot2::theme_classic(base_size = 12) +
    
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = 11.4,
        color = "#1F2933",
        margin = ggplot2::margin(r = 10)
      ),
      axis.text.x = ggplot2::element_text(
        size = 10.6,
        color = "#36454F"
      ),
      axis.title.x = ggplot2::element_text(
        size = 11.7,
        face = "bold",
        margin = ggplot2::margin(t = 10)
      ),
      
      axis.line.x = ggplot2::element_line(
        linewidth = 0.68,
        color = "#3F4A54"
      ),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(
        linewidth = 0.55,
        color = "#3F4A54"
      ),
      
      panel.grid = ggplot2::element_blank(),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 10.5),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.width = grid::unit(1.15, "cm"),
      legend.margin = ggplot2::margin(t = 5),
      
      plot.caption = ggplot2::element_text(
        size = 9.3,
        color = "#5E6A73",
        hjust = 0,
        margin = ggplot2::margin(t = 13)
      ),
      
      # Margem direita ampliada para acomodar OR, IC95%, P e P_FDR.
      plot.margin = ggplot2::margin(
        t = 12,
        r = 300,
        b = 10,
        l = 10
      )
    )
  
  save_plot_dual(
    plot,
    stem,
    width,
    height,
    directory = PATHS$figures_main
  )
}
# ------------------------------------------------------------------------------
# VISUALIZACAO EM INGLES - FIGURAS SUPLEMENTARES INDIVIDUAIS S19-S22
# ------------------------------------------------------------------------------
# Esta secao gera apenas as quatro figuras suplementares individuais.
# As FigS19, FigS20, FigS21 e FigS22 sao salvas separadamente.
# Principios usados:
# - nenhuma legenda pode ser cortada;
# - textos longos sao quebrados quando necessario;
# - a FigS21 mostra Monomorphic explicitamente como estado de referencia;
# - P e sempre mostrado em italico nas anotacoes estatisticas.


translate_by_map <- function(x, map) {
  x_chr <- as.character(x)
  translated <- unname(map[x_chr])
  translated[is.na(translated)] <- x_chr[is.na(translated)]
  translated
}

wrap_plot_labels <- function(x, width = 15) {
  vapply(
    as.character(x),
    function(value) paste(strwrap(value, width = width), collapse = "\n"),
    character(1)
  )
}

HABIT_LABELS_EN <- c(
  "Terrícola" = unname(HABIT_LABELS["0"]),
  "Terricola" = unname(HABIT_LABELS["0"]),
  "Epífita" = unname(HABIT_LABELS["2"]),
  "Epifita" = unname(HABIT_LABELS["2"]),
  "Hemiepífita" = unname(HABIT_LABELS["4"]),
  "Hemiepifita" = unname(HABIT_LABELS["4"]),
  "Rupícola" = unname(HABIT_LABELS["1"]),
  "Rupicola" = unname(HABIT_LABELS["1"]),
  "Aquática" = unname(HABIT_LABELS["3"]),
  "Aquatica" = unname(HABIT_LABELS["3"]),
  "Arborescente" = "Arborescent",
  "Escandente" = unname(HABIT_LABELS["5"]),
  "Trepadora" = unname(HABIT_LABELS["5"]),
  "Liana" = unname(HABIT_LABELS["5"]),
  "Dois ou mais hábitos" = unname(HABIT_LABELS["6"]),
  "Dois ou mais habitos" = unname(HABIT_LABELS["6"]),
  "Múltiplos hábitos" = "Multiple habits",
  "Multiplos habitos" = "Multiple habits",
  "Multihábito" = "Multihabit",
  "Multihabito" = "Multihabit",
  "Reófita" = "Rheophytic",
  "Reofita" = "Rheophytic",
  "Paludosa" = "Marsh-dwelling"
)

STATE_LABELS_EN <- c(
  "Monomorfa" = unname(STATE_LABELS["0"]),
  "Hemidimorfa" = unname(STATE_LABELS["1"]),
  "Holodimorfa" = unname(STATE_LABELS["2"])
)

ARCHITECTURE_LABELS_EN <- c(
  "Contraidiforme" = unname(ARCHITECTURE_LABELS["1"]),
  "Especiforme" = unname(ARCHITECTURE_LABELS["2"]),
  "Espiciforme" = unname(ARCHITECTURE_LABELS["2"]),
  "Paniculiforme" = unname(ARCHITECTURE_LABELS["3"]),
  "Digitiforme" = unname(ARCHITECTURE_LABELS["4"]),
  "Esporocárpico" = unname(ARCHITECTURE_LABELS["5"]),
  "Esporocarpico" = unname(ARCHITECTURE_LABELS["5"]),
  "Radiciforme" = unname(ARCHITECTURE_LABELS["6"]),
  "Monomorfa" = unname(ARCHITECTURE_LABELS["0"])
)

S21_OUTCOME_LEVELS <- c(
  "Dim vs Mon",
  "Hem vs Mon",
  "Hol vs Mon"
)

S21_PREDICTOR_LEVELS <- c("Multiple habit types")

format_p_fdr_supp <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )
}

# ------------------------------------------------------------------------------
# FIGURE S19 - DIMORPHIC-STATE PROFILE BY HABIT
# ------------------------------------------------------------------------------
plot_habit_dimorphism_heatmap <- function(
    count_table,
    stem = "FigS19. Dimorphic-state profile by habit",
    width = 10.8,
    height = 6.8,
    save = TRUE,
    directory = PATHS$figures_supp
) {
  plot_data <- count_table
  
  habit_levels_raw <- if (is.factor(plot_data$Habito_nome)) {
    levels(plot_data$Habito_nome)
  } else {
    unique(plot_data$Habito_nome)
  }
  state_levels_raw <- if (is.factor(plot_data$Estado_nome)) {
    levels(plot_data$Estado_nome)
  } else {
    unique(plot_data$Estado_nome)
  }
  
  plot_data$Habit <- factor(
    translate_by_map(plot_data$Habito_nome, HABIT_LABELS_EN),
    levels = translate_by_map(habit_levels_raw, HABIT_LABELS_EN)
  )
  plot_data$Dimorphic_state <- factor(
    translate_by_map(plot_data$Estado_nome, STATE_LABELS_EN),
    levels = translate_by_map(state_levels_raw, STATE_LABELS_EN)
  )
  
  # UM UNICO ROTULO, CENTRALIZADO: 10 - (35%)
  plot_data$Label <- paste0(
    plot_data$N,
    " - (",
    scales::percent(plot_data$Proporcao, accuracy = 1),
    ")"
  )
  
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Dimorphic_state, y = Habit, fill = Proporcao)
  ) +
    ggplot2::geom_tile(
      color = "white",
      linewidth = 1.35,
      width = 0.965,
      height = 0.965
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = Label),
      hjust = 0.5,
      vjust = 0.5,
      size = 3.35,
      color = "#10212B",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#F7FBFC", "#E1EFF3", "#B7D8E1", "#75AFC1", "#2F7C96", "#14526A"),
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      name = "Within-habit proportion"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(6.0, "cm"),
        barheight = grid::unit(0.42, "cm"),
        ticks = FALSE
      )
    ) +
    ggplot2::labs(
      title = "Dimorphic-state profile by habit",
      subtitle = "Species count and within-habit proportion",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        face = "bold",
        size = 10.0,
        color = "#20303B",
        margin = ggplot2::margin(t = 7)
      ),
      axis.text.y = ggplot2::element_text(
        face = "bold",
        size = 9.6,
        color = "#20303B",
        margin = ggplot2::margin(r = 7)
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 13.0,
        color = "#10212B",
        margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9.8,
        color = "#596874",
        margin = ggplot2::margin(b = 9)
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(face = "bold", size = 9.6),
      legend.text = ggplot2::element_text(size = 8.9),
      legend.margin = ggplot2::margin(t = 7),
      plot.margin = ggplot2::margin(12, 14, 8, 14),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  if (isTRUE(save)) {
    save_plot_dual(plot, stem, width, height, directory = directory)
  }
  plot
}

# ------------------------------------------------------------------------------
# FIGURE S20 - STANDARDIZED RESIDUALS
# ------------------------------------------------------------------------------
plot_habit_dimorphism_residuals <- function(
    residual_table,
    stem = "FigS20. Standardized residuals for habit x dimorphism",
    width = 10.8,
    height = 6.8,
    save = TRUE,
    directory = PATHS$figures_supp
) {
  plot_data <- residual_table
  if (all(is.na(plot_data$Residuo_padronizado))) return(invisible(NULL))
  
  habit_levels_raw <- if (is.factor(plot_data$Habito_nome)) {
    levels(plot_data$Habito_nome)
  } else {
    unique(plot_data$Habito_nome)
  }
  state_levels_raw <- if (is.factor(plot_data$Estado_nome)) {
    levels(plot_data$Estado_nome)
  } else {
    unique(plot_data$Estado_nome)
  }
  
  plot_data$Habit <- factor(
    translate_by_map(plot_data$Habito_nome, HABIT_LABELS_EN),
    levels = translate_by_map(habit_levels_raw, HABIT_LABELS_EN)
  )
  plot_data$Dimorphic_state <- factor(
    translate_by_map(plot_data$Estado_nome, STATE_LABELS_EN),
    levels = translate_by_map(state_levels_raw, STATE_LABELS_EN)
  )
  
  limit <- max(abs(plot_data$Residuo_padronizado), na.rm = TRUE)
  limit <- max(limit, 1)
  
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Dimorphic_state, y = Habit, fill = Residuo_padronizado)
  ) +
    ggplot2::geom_tile(
      color = "white",
      linewidth = 1.35,
      width = 0.965,
      height = 0.965
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", Residuo_padronizado)),
      hjust = 0.5,
      vjust = 0.5,
      size = 3.35,
      color = "#10212B",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#315DA8",
      mid = "#FAFAFA",
      high = "#C94A3A",
      midpoint = 0,
      limits = c(-limit, limit),
      name = "Standardized residual"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(6.0, "cm"),
        barheight = grid::unit(0.42, "cm"),
        ticks = FALSE
      )
    ) +
    ggplot2::labs(
      title = "Standardized residual structure",
      subtitle = "Positive values = over-representation; negative values = under-representation",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        face = "bold",
        size = 10.0,
        color = "#20303B",
        margin = ggplot2::margin(t = 7)
      ),
      axis.text.y = ggplot2::element_text(
        face = "bold",
        size = 9.6,
        color = "#20303B",
        margin = ggplot2::margin(r = 7)
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 13.0,
        color = "#10212B",
        margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9.8,
        color = "#596874",
        margin = ggplot2::margin(b = 9)
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(face = "bold", size = 9.6),
      legend.text = ggplot2::element_text(size = 8.9),
      legend.margin = ggplot2::margin(t = 7),
      plot.margin = ggplot2::margin(12, 14, 8, 14),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  if (isTRUE(save)) {
    save_plot_dual(plot, stem, width, height, directory = directory)
  }
  plot
}

# ------------------------------------------------------------------------------
# FIGURE S21 - DIMORPHIC STATES VS MONOMORPHIC
# ------------------------------------------------------------------------------
plot_multihabitat_forest <- function(
    multihabitat_table,
    stem = "FigS21. Dimorphic states versus Monomorphic by habit breadth",
    width = 12.8,
    height = 9.2,
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
    ,
    drop = FALSE
  ]
  if (nrow(plot_data) == 0L) return(invisible(NULL))
  
  outcome_map <- c(
    "Dimorphic vs Monomorphic" = "Dim vs Mon",
    "Hemidimorphic vs Monomorphic" = "Hem vs Mon",
    "Holodimorphic vs Monomorphic" = "Hol vs Mon"
  )
  
  plot_data$Outcome <- factor(
    unname(outcome_map[as.character(plot_data$Comparacao)]),
    levels = S21_OUTCOME_LEVELS
  )
  plot_data$Predictor <- factor(
    plot_data$Preditor_label,
    levels = S21_PREDICTOR_LEVELS
  )
  plot_data$Support <- factor(
    ifelse(plot_data$Evidencia_FDR %in% TRUE, "FDR < 0.05", "FDR ≥ 0.05"),
    levels = c("FDR ≥ 0.05", "FDR < 0.05")
  )
  
  plot_data$Result_label <- paste0(
    "\"OR = ", sprintf("%.2f", plot_data$OR),
    "  [", sprintf("%.2f", plot_data$IC95_inferior),
    "–", sprintf("%.2f", plot_data$IC95_superior), "]    \"",
    " * italic(P)[FDR] * ",
    "\" = ", format_p_fdr_supp(plot_data$P_FDR), "\""
  )
  
  # Rotulo do P bruto acima do ponto/IC.
  plot_data$P_bruto_label <- ifelse(
    is.na(plot_data$P),
    "italic(P) == NA",
    ifelse(
      plot_data$P < 0.001,
      "italic(P) < 0.001",
      paste0("italic(P) == ", sprintf("%.3f", plot_data$P))
    )
  )
  
  reference_note <- paste(
    "Reference state: Monomorphic.",
    "Points = odds ratios; horizontal lines = 95% confidence intervals."
  )
  
  x_min <- min(plot_data$IC95_inferior, na.rm = TRUE)
  x_max <- max(plot_data$IC95_superior, na.rm = TRUE)
  log_min <- log10(x_min)
  log_max <- log10(x_max)
  log_span <- max(log_max - log_min, 0.8)
  
  x_limits <- c(
    10^(log_min - 0.08 * log_span),
    10^(log_max + 0.08 * log_span)
  )
  
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = OR, y = Predictor, color = Support)
  ) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "22",
      linewidth = 0.75,
      color = "#8A949E"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = IC95_inferior,
        xend = IC95_superior,
        yend = Predictor
      ),
      linewidth = 1.2,
      lineend = "round",
      alpha = 0.95
    ) +
    ggplot2::geom_point(
      size = 4.2,
      stroke = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = P_bruto_label),
      nudge_y = 0.28,
      vjust = 0,
      size = 3.0,
      color = "#263238",
      family = "sans",
      parse = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = Inf,
        label = Result_label
      ),
      hjust = -0.06,
      size = 3.45,
      color = "#263238",
      family = "sans",
      parse = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(Outcome),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = scales::breaks_log(n = 7),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "FDR ≥ 0.05" = "#667784",
        "FDR < 0.05" = "#B94E3D"
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = "Odds ratio (95% CI; log scale)",
      y = NULL,
      color = expression("FDR-adjusted " * italic(P)),
      caption = reference_note
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = 10.6,
        color = "#1F2933",
        margin = ggplot2::margin(r = 8)
      ),
      axis.text.x = ggplot2::element_text(
        size = 10.5,
        color = "#36454F"
      ),
      axis.title.x = ggplot2::element_text(
        size = 11.5,
        face = "bold",
        margin = ggplot2::margin(t = 10)
      ),
      axis.line.x = ggplot2::element_line(
        linewidth = 0.65,
        color = "#3F4A54"
      ),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(
        linewidth = 0.55,
        color = "#3F4A54"
      ),
      panel.grid.major.y = ggplot2::element_line(
        linewidth = 0.45,
        color = "#EEF1F4"
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        face = "bold",
        size = 10.1,
        color = "#1F2933",
        margin = ggplot2::margin(r = 10)
      ),
      panel.spacing.y = grid::unit(0.55, "cm"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 10.5),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.width = grid::unit(1.15, "cm"),
      legend.margin = ggplot2::margin(t = 4),
      plot.caption = ggplot2::element_text(
        size = 9.2,
        color = "#5E6A73",
        hjust = 0,
        margin = ggplot2::margin(t = 12)
      ),
      plot.margin = ggplot2::margin(
        t = 10,
        r = 205,
        b = 10,
        l = 10
      )
    )
  
  if (isTRUE(save)) {
    save_plot_dual(plot, stem, width, height, directory = directory)
  }
}

# ------------------------------------------------------------------------------
# FIGURE S22 - FERTILE-FROND ARCHITECTURE BUBBLE PLOT
# ------------------------------------------------------------------------------
plot_habit_architecture_heatmap <- function(
    architecture_table,
    stem = "FigS22. Fertile-frond architecture by habit",
    width = 13.2,
    height = 7.6,
    save = TRUE,
    directory = PATHS$figures_supp
) {
  plot_data <- architecture_table
  
  habit_levels_raw <- if (is.factor(plot_data$Habito_nome)) {
    levels(plot_data$Habito_nome)
  } else {
    unique(plot_data$Habito_nome)
  }
  arch_levels_raw <- if (is.factor(plot_data$Arquitetura_nome)) {
    levels(plot_data$Arquitetura_nome)
  } else {
    unique(plot_data$Arquitetura_nome)
  }
  
  habit_labels <- translate_by_map(plot_data$Habito_nome, HABIT_LABELS_EN)
  habit_levels <- translate_by_map(habit_levels_raw, HABIT_LABELS_EN)
  
  arch_labels <- translate_by_map(plot_data$Arquitetura_nome, ARCHITECTURE_LABELS_EN)
  arch_levels <- translate_by_map(arch_levels_raw, ARCHITECTURE_LABELS_EN)
  
  arch_labels_wrapped <- wrap_plot_labels(arch_labels, width = 11)
  arch_levels_wrapped <- wrap_plot_labels(arch_levels, width = 11)
  
  plot_data$Habit <- factor(habit_labels, levels = habit_levels)
  plot_data$Architecture <- factor(
    arch_labels_wrapped,
    levels = arch_levels_wrapped
  )
  
  plot_data$Bubble_label <- ifelse(
    plot_data$Proporcao >= 0.10,
    scales::percent(plot_data$Proporcao, accuracy = 1),
    ""
  )
  
  size_breaks <- unique(round(scales::pretty_breaks(n = 4)(c(0, max(plot_data$N, na.rm = TRUE)))))
  size_breaks <- size_breaks[size_breaks >= 0]
  
  plot_data$Architecture_id <- as.numeric(plot_data$Architecture)
  plot_data$Habit_id <- as.numeric(plot_data$Habit)
  
  if (length(unique(plot_data$N)) > 1) {
    plot_data$Offset_x <- scales::rescale(plot_data$N, to = c(0.07, 0.16))
    plot_data$Offset_y <- scales::rescale(plot_data$N, to = c(0.04, 0.10))
  } else {
    plot_data$Offset_x <- 0.10
    plot_data$Offset_y <- 0.06
  }
  
  label_data <- plot_data[plot_data$Bubble_label != "", , drop = FALSE]
  
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = Architecture_id, y = Habit_id)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(size = N, fill = Proporcao),
      shape = 21,
      color = "#34434C",
      stroke = 0.45,
      alpha = 0.98
    ) +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(
        x = Architecture_id + Offset_x,
        y = Habit_id + Offset_y,
        label = Bubble_label
      ),
      hjust = 0,
      vjust = 0,
      size = 2.55,
      color = "#16232A",
      fontface = "bold",
      show.legend = FALSE,
      inherit.aes = FALSE,
      check_overlap = TRUE
    ) +
    ggplot2::scale_size_area(
      max_size = 10.5,
      breaks = size_breaks,
      name = "Species (n)"
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#FFF8F0", "#F6D7AE", "#E59B56", "#A55825"),
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      name = "Within-habit proportion"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(arch_levels_wrapped),
      labels = arch_levels_wrapped,
      expand = ggplot2::expansion(mult = c(0.02, 0.08))
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(habit_levels),
      labels = habit_levels,
      expand = ggplot2::expansion(mult = c(0.05, 0.10))
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        order = 1,
        title.position = "left",
        title.hjust = 1,
        barwidth = grid::unit(4.6, "cm"),
        barheight = grid::unit(0.34, "cm"),
        ticks = FALSE
      ),
      size = ggplot2::guide_legend(
        order = 2,
        title.position = "left",
        nrow = 1,
        byrow = TRUE,
        override.aes = list(fill = "white", color = "#60717C")
      )
    ) +
    ggplot2::labs(
      title = "Fertile-frond architecture across habits",
      subtitle = "Bubble size shows species count and fill shows within-habit proportion",
      x = NULL,
      y = NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 10.4) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        linewidth = 0.38,
        color = "#E9EEF1"
      ),
      axis.text.x = ggplot2::element_text(
        face = "bold",
        size = 8.4,
        color = "#20303B",
        lineheight = 0.90,
        margin = ggplot2::margin(t = 6)
      ),
      axis.text.y = ggplot2::element_text(
        face = "bold",
        size = 9.1,
        color = "#20303B",
        margin = ggplot2::margin(r = 6)
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 12.1,
        color = "#10212B",
        margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9.4,
        color = "#596874",
        margin = ggplot2::margin(b = 7)
      ),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.just = "left",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(face = "bold", size = 9.2),
      legend.text = ggplot2::element_text(size = 8.5),
      legend.key.width = grid::unit(0.70, "cm"),
      legend.spacing.y = grid::unit(0.10, "cm"),
      legend.margin = ggplot2::margin(t = 6),
      plot.margin = ggplot2::margin(12, 24, 10, 12),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  
  if (isTRUE(save)) {
    save_plot_dual(plot, stem, width, height, directory = directory)
  }
  plot
}


# ============================================================================== 
# ANALISE COMPLEMENTAR - PAGEL: AMPLITUDE DE HABITO x DIMORFISMO
# ============================================================================== 
# PERGUNTA:
# A evolucao da capacidade de ocupar dois ou mais tipos de habito esta
# historicamente associada a evolucao do dimorfismo foliar?
#
# IMPORTANTE:
# O teste de Pagel trabalha com dois caracteres BINARIOS. Para nao perder a
# diferenca biologica entre Hemidimorphic e Holodimorphic, fazemos TRES testes:
#   1. Dimorphic vs Monomorphic
#      0 = Monomorphic
#      1 = Hemidimorphic + Holodimorphic
#   2. Hemidimorphic vs Monomorphic
#      0 = Monomorphic
#      1 = Hemidimorphic
#      Holodimorphic e excluida SOMENTE deste contraste.
#   3. Holodimorphic vs Monomorphic
#      0 = Monomorphic
#      1 = Holodimorphic
#      Hemidimorphic e excluida SOMENTE deste contraste.
#
# O segundo carater e a amplitude de habito:
#   0 = um unico tipo de habito (codigos 0-5)
#   1 = dois ou mais tipos de habito (codigo 6)
#
# Assim, o contraste geral diz se DIMORFISMO como um todo esta associado a
# multihabito, enquanto os dois contrastes especificos mostram se o padrao e
# principalmente hemidimorfico ou holodimorfico.
# ------------------------------------------------------------------------------
run_pagel_habit_breadth <- function(tree, analysis_data, min_group = 5L) {
  contrast_specs <- list(
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
  
  rows <- list()
  fits <- list()
  
  for (nm in names(contrast_specs)) {
    spec <- contrast_specs[[nm]]
    
    d <- analysis_data[
      analysis_data$Padrao_Dimorfismo %in% spec$keep,
      c("species", "Padrao_Dimorfismo", "Habito"),
      drop = FALSE
    ]
    d <- d[stats::complete.cases(d), , drop = FALSE]
    d$Dimorfismo_contraste <- spec$response(d$Padrao_Dimorfismo)
    d$Multihabito <- as.integer(d$Habito == 6L)
    
    common <- intersect(tree$tip.label, d$species)
    tr <- ape::keep.tip(tree, common)
    d <- d[match(tr$tip.label, d$species), , drop = FALSE]
    
    response_counts <- table(d$Dimorfismo_contraste)
    habit_counts <- table(d$Multihabito)
    joint_counts <- table(d$Dimorfismo_contraste, d$Multihabito)
    
    status <- "OK"
    fit <- NULL
    
    if (nrow(d) < 10L) {
      status <- "Ignorado: N insuficiente"
    } else if (length(response_counts) < 2L || length(habit_counts) < 2L ||
               min(response_counts) < min_group || min(habit_counts) < min_group) {
      status <- "Ignorado: estado binario raro"
    } else if (sum(joint_counts > 0L) < 4L) {
      status <- "Ignorado: nem todas as combinacoes 0/1 estao representadas"
    } else {
      x <- setNames(as.integer(d$Dimorfismo_contraste), d$species)
      y <- setNames(as.integer(d$Multihabito), d$species)
      
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
        status <- paste0("Falhou: ", conditionMessage(fit))
        fit <- NULL
      }
    }
    
    fits[[nm]] <- fit
    
    if (is.null(fit)) {
      rows[[nm]] <- data.frame(
        Contraste = spec$label,
        N = nrow(d),
        N_Monomorphic_ref = sum(d$Dimorfismo_contraste == 0L),
        N_Estado_focal = sum(d$Dimorfismo_contraste == 1L),
        N_Habito_unico = sum(d$Multihabito == 0L),
        N_Multihabito = sum(d$Multihabito == 1L),
        AIC_independente = NA_real_,
        AIC_dependente = NA_real_,
        Delta_AIC_favor_dependente = NA_real_,
        LR = NA_real_,
        P = NA_real_,
        Status = status,
        stringsAsFactors = FALSE
      )
    } else {
      rows[[nm]] <- data.frame(
        Contraste = spec$label,
        N = nrow(d),
        N_Monomorphic_ref = sum(d$Dimorfismo_contraste == 0L),
        N_Estado_focal = sum(d$Dimorfismo_contraste == 1L),
        N_Habito_unico = sum(d$Multihabito == 0L),
        N_Multihabito = sum(d$Multihabito == 1L),
        AIC_independente = fit$independent.AIC,
        AIC_dependente = fit$dependent.AIC,
        Delta_AIC_favor_dependente = fit$independent.AIC - fit$dependent.AIC,
        LR = fit$lik.ratio,
        P = fit$P,
        Status = status,
        stringsAsFactors = FALSE
      )
    }
  }
  
  tab <- dplyr::bind_rows(rows)
  tab$P_FDR <- NA_real_
  ok <- tab$Status == "OK" & is.finite(tab$P)
  if (any(ok)) {
    tab$P_FDR[ok] <- stats::p.adjust(tab$P[ok], method = "BH")
  }
  tab$Evidencia_FDR <- !is.na(tab$P_FDR) & tab$P_FDR < 0.05
  tab$Interpretacao_AIC <- dplyr::case_when(
    !is.finite(tab$Delta_AIC_favor_dependente) ~ "Nao estimado",
    tab$Delta_AIC_favor_dependente >= 10 ~ "Forte suporte ao modelo dependente",
    tab$Delta_AIC_favor_dependente >= 4 ~ "Suporte moderado ao modelo dependente",
    tab$Delta_AIC_favor_dependente >= 2 ~ "Algum suporte ao modelo dependente",
    TRUE ~ "Pouco ou nenhum suporte ao modelo dependente"
  )
  
  list(table = tab, fits = fits)
}

plot_pagel_habit_breadth <- function(
    pagel_table,
    stem = "FigS23. Pagel entre amplitude de habito e dimorfismo",
    width = 10.8,
    height = 6.8) {
  
  d <- pagel_table[
    pagel_table$Status == "OK" &
      is.finite(pagel_table$Delta_AIC_favor_dependente),
    , drop = FALSE
  ]
  
  if (nrow(d) == 0L) {
    log_message("FigS23 nao gerada: nenhum teste de Pagel valido.", level = "AVISO")
    return(invisible(NULL))
  }
  
  desired_order <- c(
    "Dimorphic vs Monomorphic",
    "Hemidimorphic vs Monomorphic",
    "Holodimorphic vs Monomorphic"
  )
  d$Contraste <- factor(d$Contraste, levels = rev(desired_order))
  
  format_p_pagel <- function(x) {
    ifelse(
      is.na(x),
      "NA",
      ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
    )
  }
  
  d$Rotulo_P_bruto <- ifelse(
    is.na(d$P),
    "italic(P) == NA",
    ifelse(
      d$P < 0.001,
      "italic(P) < 0.001",
      paste0("italic(P) == ", sprintf("%.3f", d$P))
    )
  )
  
  d$Rotulo_P_lateral <- paste0(
    "italic(P) * ",
    "\" = ", format_p_pagel(d$P), "    \"",
    " * italic(P)[FDR] * ",
    "\" = ", format_p_pagel(d$P_FDR), "\""
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = Delta_AIC_favor_dependente, y = Contraste)
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.6,
      color = "#8A949E"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = Delta_AIC_favor_dependente,
        yend = Contraste
      ),
      linewidth = 0.95,
      color = "#CBD5E1",
      lineend = "round"
    ) +
    ggplot2::geom_point(
      size = 3.8,
      color = "#2F4858"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = Rotulo_P_bruto),
      nudge_y = 0.24,
      vjust = 0,
      size = 3.25,
      color = "#263238",
      family = "sans",
      parse = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = Inf,
        label = Rotulo_P_lateral
      ),
      hjust = -0.08,
      size = 3.45,
      color = "#263238",
      family = "sans",
      parse = TRUE
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.08, 0.34)),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    ggplot2::labs(
      x = "ΔAIC (independent − dependent)
← Independent evolution        Dependent evolution →",
      y = NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 11.5, color = "#1F2933"),
      axis.text.x = ggplot2::element_text(size = 10.8, color = "#36454F"),
      axis.title.x = ggplot2::element_text(size = 12.5, face = "bold"),
      plot.margin = ggplot2::margin(10, 210, 10, 10)
    )
  
  save_plot_dual(
    p, stem, width, height,
    directory = PATHS$figures_supp
  )
  
  invisible(p)
}


# ==============================================================================
# EXECUCAO DA ANALISE 05
# ==============================================================================
console_banner("SCRIPT 05", "Habito, multihabito e dimorfismo")

console_step(1, 7, "Carregando dados e arvore do Script 01")
prepared <- require_saved_rds(
  "01_dados_e_filogenia.rds",
  "01_PREPARAR_DADOS_E_FILOGENIA.R"
)
data <- prepared$data
tree <- prepared$tree

# ==============================================================================
# ANALISE 05A - HABITO (7 CATEGORIAS) x DIMORFISMO
# ==============================================================================
# PERGUNTA DESTE BLOCO:
# O habito esta relacionado a chance de uma especie ser dimorfica, controlando
# a nao-independencia filogenetica?
# O codigo 6 entra como categoria real: nao e excluido.
console_step(2, 7, "Regressao filogenetica com Habito como fator de sete categorias")

ecological_models <- run_phylogenetic_logistic_models(tree, data)

# PRINT DA ANALISE 05A
cat("\n--- RESULTADO: HABITO CATEGORICO x DIMORFISMO ---\n")
print(ecological_models$univariate)

# TABELAS DA ANALISE 05A
write_csv_utf8(
  ecological_models$univariate,
  file.path(PATHS$tables_main, "Table4. Relacao entre dimorfismo e habito.csv")
)
write_csv_utf8(
  ecological_models$univariate,
  file.path(PATHS$tables_supp, "TableS9. Habito_Dimorfismo.csv")
)

# ==============================================================================
# FIGURAS DA ANALISE 05A
# ==============================================================================
# FigS5 = prevalencia observada (descritiva).
# Fig2  = forest plot do modelo filogenetico (inferencial).
# Para mudar apenas tamanho/nome, altere estas linhas.
FIGS4_NOME <- "FigS5. Prevalence of dimorphism by habit"
FIGS4_LARGURA <- 10.4
FIGS4_ALTURA <- 7.0

FIG2_NOME <- "Fig2. Associacao entre dimorfismo e habito"
FIG2_LARGURA <- 13.2
FIG2_ALTURA <- 7.2

console_step(3, 7, "Gerando as figuras da analise 05A")
plot_habit_prevalence(
  data,
  stem = FIGS4_NOME,
  width = FIGS4_LARGURA,
  height = FIGS4_ALTURA
)
plot_ecology_forest(
  ecological_models$univariate,
  stem = FIG2_NOME,
  width = FIG2_LARGURA,
  height = FIG2_ALTURA
)

# ==============================================================================
# ANALISE 05B - MULTIHABITO x ESTADOS DE DIMORFISMO
# ==============================================================================
# Aqui testamos a categoria multihabito (codigo 6: dois ou mais tipos) para cada contraste:
# - Dimorphic vs Monomorphic;
# - Hemidimorphic vs Monomorphic;
# - Holodimorphic vs Monomorphic.
# Em todos os casos, Monomorphic e a referencia.
console_step(4, 7, "Testando multihabito (dois ou mais tipos de habito)")

multihabitat_models <- run_multihabitat_models(tree, data)

# PRINT DA ANALISE 05B
cat("\n--- RESULTADO: MULTIHABITO ---\n")
print(multihabitat_models)

# TABELA DA ANALISE 05B
write_csv_utf8(
  multihabitat_models,
  file.path(PATHS$tables_supp, "TableS22. Modelos binarios de multihabito.csv")
)

# FIGURA DA ANALISE 05B - FigS21
FIGS21_NOME <- "FigS21. Dimorphic states versus Monomorphic by habit breadth"
FIGS21_LARGURA <- 12.8
FIGS21_ALTURA <- 9.2

plot_multihabitat_forest(
  multihabitat_models,
  stem = FIGS21_NOME,
  width = FIGS21_LARGURA,
  height = FIGS21_ALTURA
)

# ==============================================================================
# ANALISE 05C - EXPLORACAO HABITO x DIMORFISMO / ARQUITETURA
# ==============================================================================
# Este bloco e exploratorio: frequencias, proporcoes, qui-quadrado (P por Monte Carlo) e residuos.
# Ele ajuda a enxergar onde estao os padroes antes da interpretacao biologica.
console_step(5, 7, "Gerando associacoes exploratorias, proporcoes, residuos e heatmaps")

exploratory_habit <- run_habit_exploratory_tables(data)

# PRINT DA ANALISE 05C
cat("\n--- FREQUENCIAS E PROPORCOES ---\n")
print(exploratory_habit$counts)
cat("\n--- TESTE DE QUI-QUADRADO ---\n")
print(exploratory_habit$test)
cat("\n--- RESIDUOS PADRONIZADOS ---\n")
print(exploratory_habit$residuals)

# TABELAS DA ANALISE 05C
write_csv_utf8(
  exploratory_habit$counts,
  file.path(PATHS$tables_supp, "TableS23. Freq_Habito_Dimorf.csv")
)
write_csv_utf8(
  exploratory_habit$residuals,
  file.path(PATHS$tables_supp, "TableS24. Residuos_Habito_Dimorf.csv")
)
write_csv_utf8(
  exploratory_habit$test,
  file.path(PATHS$tables_supp, "TableS25. Qui2_Habito_Dimorf.csv")
)
write_csv_utf8(
  exploratory_habit$architecture,
  file.path(PATHS$tables_supp, "TableS26. Habito_Arquitetura.csv")
)

# ==============================================================================
# FIGURAS DA ANALISE 05C
# ==============================================================================
FIGS19_NOME <- "FigS19. Dimorphic-state profile by habit"
FIGS19_LARGURA <- 10.8
FIGS19_ALTURA <- 6.8

FIGS20_NOME <- "FigS20. Standardized residuals for habit x dimorphism"
FIGS20_LARGURA <- 10.8
FIGS20_ALTURA <- 6.8

FIGS22_NOME <- "FigS22. Fertile-frond architecture by habit"
FIGS22_LARGURA <- 13.2
FIGS22_ALTURA <- 7.6


plot_habit_dimorphism_heatmap(
  exploratory_habit$counts,
  stem = FIGS19_NOME,
  width = FIGS19_LARGURA,
  height = FIGS19_ALTURA
)
plot_habit_dimorphism_residuals(
  exploratory_habit$residuals,
  stem = FIGS20_NOME,
  width = FIGS20_LARGURA,
  height = FIGS20_ALTURA
)
plot_habit_architecture_heatmap(
  exploratory_habit$architecture,
  stem = FIGS22_NOME,
  width = FIGS22_LARGURA,
  height = FIGS22_ALTURA
)


# ============================================================================== 
# ANALISE 05D - PAGEL: AMPLITUDE DE HABITO x DIMORFISMO
# ============================================================================== 
# Esta analise NAO substitui as regressoes anteriores. Ela acrescenta uma pergunta
# historica: as mudancas evolutivas em amplitude de habito e dimorfismo parecem
# depender uma da outra ao longo da filogenia?
console_step(6, 7, "Teste de Pagel entre amplitude de habito e dimorfismo")

pagel_habit_breadth <- run_pagel_habit_breadth(tree, data)

cat("\n--- PAGEL: AMPLITUDE DE HABITO x DIMORFISMO ---\n")
print(pagel_habit_breadth$table)

write_csv_utf8(
  pagel_habit_breadth$table,
  file.path(
    PATHS$tables_supp,
    "TableS21_Pagel_Habito_Dimorf.csv"
  )
)

saveRDS(
  pagel_habit_breadth$fits,
  windows_safe_path(
    file.path(PATHS$objects, "Pagel_amplitude_habito_dimorfismo.rds")
  )
)

FIGS23_NOME <- "FigS23. Pagel entre amplitude de habito e dimorfismo"
FIGS23_LARGURA <- 10.8
FIGS23_ALTURA <- 6.8

plot_pagel_habit_breadth(
  pagel_habit_breadth$table,
  stem = FIGS23_NOME,
  width = FIGS23_LARGURA,
  height = FIGS23_ALTURA
)

# ==============================================================================
# SALVAMENTO DO OBJETO COMPLETO DO SCRIPT 05
# ==============================================================================
console_step(7, 7, "Salvando todos os resultados de habito para reutilizacao")
result <- list(
  ecology = ecological_models,
  multihabitat = multihabitat_models,
  exploratory = exploratory_habit,
  pagel_habit_breadth = pagel_habit_breadth
)
save_pipeline_rds(result, "05_habito_dimorfismo.rds")

console_output("05_habito_dimorfismo.rds")
console_output("Table4, TableS9, TableS21-S26, Fig2, FigS5, FigS19-FigS23")
log_message("SCRIPT 05 concluido.")
console_done("SCRIPT 05", "06_TAXAS_OCULTAS.R")