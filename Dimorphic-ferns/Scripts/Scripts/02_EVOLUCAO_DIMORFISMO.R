# ==============================================================================
# 02_EVOLUCAO_DIMORFISMO.R
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
#   Se voce apenas mudar a aparencia de uma figura, NAO precisa rodar este script de novo.
#
# QUANDO TERMINAR:
#   Depois, rode 03_SIMMAP_DIMORFISMO.R.
# ------------------------------------------------------------------------------
# PERGUNTA:
# Como Monomorfia, Hemidimorfia e Holodimorfia se transformam ao longo da
# filogenia? As taxas sao iguais, simetricas ou diferentes entre direcoes?
#
# ANALISE:
# Modelos Mk (ER, SYM, ARD e modelos ordenados) + matriz Q.
#
# PARA QUE SERVE:
# Compara hipoteses sobre a forma das transicoes evolutivas e estima a taxa de
# cada mudanca entre os tres estados de dimorfismo.
#
# O QUE OLHAR:
# AICc, Delta AICc, peso de Akaike, modelo selecionado e taxas da matriz Q.
#
# DEPENDENCIAS:
# 01_PREPARAR_DADOS_E_FILOGENIA.R
#
# LE:
# Scripts_/_Objetos_R/01_dados_e_filogenia.rds
#
# OBJETO SALVO:
# Scripts_/_Objetos_R/02_evolucao_dimorfismo.rds
#
# TABELAS GERADAS NESTE SCRIPT:
# Table1. Modelos de evolucao do dimorfismo.csv
# TableS3. Comparacao dos modelos de dimorfismo.csv
# TableS4. Matriz Q do dimorfismo.csv
#
# FIGURAS GERADAS NESTE SCRIPT:
# FigS1. Comparacao de modelos do dimorfismo
# FigS2. Rede de transicoes do dimorfismo
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
required_packages <- c("ape", "corHMM", "dplyr", "ggplot2", "scales")
check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# CONFIGURACAO E FUNCOES EXCLUSIVAS DO SCRIPT 02
# ============================================================================== 
# Aqui ficam os parametros dos modelos Mk. Nenhum outro script precisa deles.
quick_mode <- is_true(Sys.getenv("LEAF_DIMORPHISM_QUICK", "false"))
CONFIG <- list(
  reuse_models = is_true(Sys.getenv("LEAF_DIMORPHISM_REUSE_MODELS", "false")),
  resume_checkpoints = is_true(Sys.getenv("LEAF_DIMORPHISM_RESUME_CHECKPOINTS", "false")),
  n_starts = if (quick_mode) 0L else 100L,
  n_cores = 1L,
  lower_rate_bound = 1e-10,
  upper_rate_bound = 100
)

# Tudo deste bloco pertence a comparacao ER/SYM/ARD, modelos ordenados e matriz Q.
# Se voce estiver estudando o script, pode pular estas funcoes e ir direto ao
# bloco "EXECUCAO DA ANALISE" mais abaixo; elas apenas definem como o modelo roda.
renumber_rate_matrix <- function(rate_matrix) {
  values <- sort(unique(as.integer(rate_matrix[rate_matrix > 0])))
  output <- rate_matrix
  if (length(values) > 0L) {
    for (index in seq_along(values)) {
      output[rate_matrix == values[index]] <- index
    }
  }
  output
}

# Cria modelos ordenados: permite apenas mudancas entre estados vizinhos.
# symmetric = TRUE obriga ida e volta a compartilharem a mesma taxa.
make_ordered_rate_matrix <- function(trait_data, symmetric = FALSE) {
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
  
  renumber_rate_matrix(rate_matrix)
}

# Executa um modelo corHMM com captura de avisos e erros.
# Assim, a falha de um modelo nao derruba todo o conjunto de comparacoes.
safe_corhmm_fit <- function(tree, trait_data, specification, node_states = "none") {
  arguments <- list(phy = tree, data = trait_data,
    rate.cat = specification$rate_cat, model = specification$model,
    node.states = node_states, root.p = "yang", nstarts = CONFIG$n_starts,
    n.cores = CONFIG$n_cores, lower.bound = CONFIG$lower_rate_bound,
    upper.bound = CONFIG$upper_rate_bound)
  if (!is.null(specification$rate_matrix)) arguments$rate.mat <- specification$rate_matrix
  mk_fit_audited(arguments)

}

# Ajusta ER, SYM, ARD e dois modelos ordenados para um esquema de estados.
# Calcula AICc e peso de Akaike, identifica taxas no limite e seleciona o
# melhor modelo numericamente estavel para as reconstrucoes ancestrais.
# Funcao generica dos modelos Mk do dimorfismo.
# Ajusta varias estruturas de taxa, compara AICc e devolve o melhor ajuste junto da tabela completa.
fit_model_set <- function(tree, trait_data, scheme_name) {
  signature <- mk_signature(tree, trait_data, CONFIG$n_starts,
    CONFIG$lower_rate_bound, CONFIG$upper_rate_bound)
  cache_path <- file.path(PATHS$objects, paste0("modelos_", scheme_name, ".rds"))
  if (CONFIG$reuse_models && file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    if (identical(cached$signature, signature)) {
      log_message("Reutilizando modelos auditados: ", scheme_name)
      return(list(results = cached$results, comparison = cached$comparison,
        best_model = cached$best_model,
        best_fit = cached$results[[cached$best_model]]$fit,
        specification = cached$specifications[[cached$best_model]]))
    }
    log_message(
      "Cache completo ignorado para ", scheme_name,
      " porque a arvore, os estados ou o numero de reinicios mudaram.",
      level = "AVISO"
    )
  }
  
  ordered_sym <- make_ordered_rate_matrix(trait_data, symmetric = TRUE)
  ordered_ard <- make_ordered_rate_matrix(trait_data, symmetric = FALSE)
  
  specifications <- list(
    ER = list(model = "ER", rate_cat = 1L, rate_matrix = NULL),
    SYM = list(model = "SYM", rate_cat = 1L, rate_matrix = NULL),
    ARD = list(model = "ARD", rate_cat = 1L, rate_matrix = NULL),
    Ordenado_SYM = list(model = "ARD", rate_cat = 1L, rate_matrix = ordered_sym),
    Ordenado_ARD = list(model = "ARD", rate_cat = 1L, rate_matrix = ordered_ard)
  )
  
  results <- vector("list", length(specifications))
  names(results) <- names(specifications)
  table_rows <- vector("list", length(specifications))
  
  for (i in seq_along(specifications)) {
    model_name <- names(specifications)[i]
    checkpoint_path <- file.path(
      PATHS$objects,
      paste0("checkpoint_", scheme_name, "_", model_name, ".rds")
    )
    checkpoint_ok <- FALSE
    if (CONFIG$resume_checkpoints && file.exists(checkpoint_path)) {
      checkpoint <- readRDS(checkpoint_path)
      checkpoint_ok <- identical(checkpoint$signature, signature) &&
        identical(checkpoint$specification, specifications[[i]])
      if (checkpoint_ok) {
        log_message(
          "Retomando checkpoint de ", scheme_name, " - modelo ", model_name, "."
        )
        result <- checkpoint$result
        model_row <- checkpoint$model_row
      }
    }
    
    if (!checkpoint_ok) {
      log_message("Ajustando ", scheme_name, " - modelo ", model_name, "...")
      result <- safe_corhmm_fit(tree, trait_data, specifications[[i]])
      
      model_row <- mk_model_row(result, scheme_name, model_name,
        CONFIG$lower_rate_bound, CONFIG$upper_rate_bound, "pt")

      saveRDS(
        list(
          signature = signature, specification = specifications[[i]],
          tree_tip_labels = tree$tip.label,
          trait_species = as.character(trait_data$species),
          trait_states = as.character(trait_data$state),
          n_starts = CONFIG$n_starts,
          result = result,
          model_row = model_row
        ),
        windows_safe_path(checkpoint_path)
      )
    }
    
    model_row <- mk_model_row(result, scheme_name, model_name,
      CONFIG$lower_rate_bound, CONFIG$upper_rate_bound, "pt")
    if (!is.null(result$audit)) write_csv_utf8(result$audit,
      file.path(PATHS$tables_supp, paste0("Diagnostico_reinicios_", scheme_name, "_", model_name, ".csv")))
    results[[model_name]] <- result
    table_rows[[i]] <- model_row
  }
  
  comparison <- dplyr::bind_rows(table_rows)
  comparison <- mk_select(comparison, "Modelo", "Peso_Akaike")
  best_model <- comparison$Modelo[comparison$Selecionado]
  best_fit <- results[[best_model]]$fit
  
  saveRDS(
    list(
      signature = signature,
      scheme = scheme_name,
      tree_tip_labels = tree$tip.label,
      trait_species = as.character(trait_data$species),
      trait_states = as.character(trait_data$state),
      n_starts = CONFIG$n_starts,
      specifications = specifications,
      results = results,
      comparison = comparison,
      best_model = best_model
    ),
    windows_safe_path(cache_path)
  )
  
  log_message("Melhor modelo em ", scheme_name, ": ", best_model)
  list(
    results = results,
    comparison = comparison,
    best_model = best_model,
    best_fit = best_fit,
    specification = specifications[[best_model]]
  )
}

# Transforma a solucao do melhor corHMM em uma matriz Q completa e em uma
# tabela longa com taxas de transicao por milhao de anos.
extract_q_matrix <- function(fit, state_codes, state_labels) {
  q_matrix <- mk_q_matrix(fit, state_codes)
  
  tidy <- expand.grid(
    Origem = state_codes,
    Destino = state_codes,
    stringsAsFactors = FALSE
  )
  tidy$Taxa_por_Ma <- mapply(
    function(origin, destination) q_matrix[origin, destination],
    tidy$Origem,
    tidy$Destino
  )
  tidy <- tidy[tidy$Origem != tidy$Destino, ]
  tidy$Origem_nome <- unname(state_labels[tidy$Origem])
  tidy$Destino_nome <- unname(state_labels[tidy$Destino])
  tidy <- tidy[, c("Origem", "Origem_nome", "Destino", "Destino_nome", "Taxa_por_Ma")]
  
  list(matrix = q_matrix, tidy = tidy)
}

# ==============================================================================
# EXECUCAO DA ANALISE 02
# ==============================================================================
# A partir daqui a ordem sera sempre:
#
#   ANALISE -> PRINT -> TABELA(S) -> FIGURA
#
# Assim, cada figura fica imediatamente abaixo do resultado que a originou.
# Se quiser alterar apenas a aparencia de uma figura, procure o bloco com o nome
# exato da figura e edite SOMENTE a parte grafica daquele bloco.
# ==============================================================================

console_banner("SCRIPT 02", "Modelos Mk e matriz Q do dimorfismo")

# ------------------------------------------------------------------------------
# PASSO 1 - CARREGAR OS RESULTADOS PREPARADOS NO SCRIPT 01
# ------------------------------------------------------------------------------
console_step(1, 5, "Carregando dados e arvore preparados no Script 01")

# Carrega o objeto salvo pelo Script 01.
# Aqui NAO lemos o Excel nem a FTOL novamente. Isso garante que esta analise use
# exatamente a mesma matriz e a mesma arvore que ja foram conferidas anteriormente.
prepared <- require_saved_rds(
  "01_dados_e_filogenia.rds",
  "01_PREPARAR_DADOS_E_FILOGENIA.R"
)

# Separamos apenas os componentes necessários neste script.
data <- prepared$data
tree <- prepared$tree
state_counts <- prepared$state_counts


# ------------------------------------------------------------------------------
# PASSO 2 - PREPARAR O CARACTER ANALISADO
# ------------------------------------------------------------------------------
console_step(
  2, 5,
  "Preparando o caracter de tres estados: Monomorfa, Hemidimorfa e Holodimorfa"
)

# O corHMM precisa de uma tabela simples com duas colunas:
#   species = nome da especie
#   state   = codigo do estado de dimorfismo
# Os outros caracteres da matriz nao entram nesta analise.
trait_data <- data.frame(
  species = data$species,
  state = as.integer(data$Estado)
)


# ==============================================================================
# ANALISE 02A - COMPARACAO DOS MODELOS Mk
# ==============================================================================
# PERGUNTA DESTA ETAPA:
# As transicoes entre Monomorfia, Hemidimorfia e Holodimorfia apresentam taxas
# iguais, simetricas, diferentes ou seguem um modelo ordenado?
#
# RESULTADOS GERADOS LOGO ABAIXO:
#   Table1  = tabela selecionada para o texto principal
#   TableS3 = comparacao completa para o suplemento
#   FigS1   = representacao grafica do suporte aos modelos
# ==============================================================================
console_step(3, 5, "Comparando os modelos Mk: ER, SYM, ARD e modelos ordenados")

# Ajusta todos os modelos candidatos.
# O objeto 'models' guarda:
#   models$comparison = tabela com AICc, Delta AICc e peso de Akaike
#   models$best_model = nome do modelo selecionado
#   models$best_fit   = ajuste do melhor modelo, usado depois na matriz Q
models <- fit_model_set(tree, trait_data, "Tres_estados")

# Mostra a tabela no Console imediatamente depois da analise.
print(models$comparison)

# ------------------------------------------------------------------------------
# TABELAS DA ANALISE 02A
# ------------------------------------------------------------------------------
# Table1 e TableS3 usam o MESMO resultado estatistico.
# A duplicacao existe apenas porque uma versao foi reservada para o texto e outra
# para o material suplementar.
write_csv_utf8(
  models$comparison,
  file.path(PATHS$tables_main, "Table1. Modelos de evolucao do dimorfismo.csv")
)
write_csv_utf8(
  models$comparison,
  file.path(PATHS$tables_supp, "TableS3. Comparacao dos modelos de dimorfismo.csv")
)

# ------------------------------------------------------------------------------
# FIGURA DA ANALISE 02A - FigS1
# ------------------------------------------------------------------------------
# IMPORTANTE PARA EDICAO:
# Tudo que controla a aparencia da FigS1 esta neste bloco logo abaixo.
# Alterar titulo, cores, tamanho de texto, largura das barras ou tema NAO muda
# o resultado dos modelos. Para mudar apenas a figura, edite esta funcao.

# Esta funcao transforma a tabela de comparacao dos modelos em uma figura.
# Ela NAO recalcula modelos; apenas usa AICc e peso de Akaike ja estimados.
plot_model_comparison <- function(comparison_table) {
  # Mantem somente modelos com AICc valido.
  plot_data <- comparison_table[is.finite(comparison_table$AICc), , drop = FALSE]
  plot_data <- plot_data[order(plot_data$AICc), , drop = FALSE]

  model_labels <- c(
    "SYM" = "SYM (symmetrical)",
    "ARD" = "ARD (asymmetric)",
    "ER" = "ER (single-rate)",
    "Ordenado_ARD" = "Ordered ARD",
    "Ordenado_SYM" = "Ordered SYM",
    "Ordered_ARD" = "Ordered ARD",
    "Ordered_SYM" = "Ordered SYM"
  )

  plot_data$Modelo_label <- unname(model_labels[as.character(plot_data$Modelo)])
  missing_lab <- is.na(plot_data$Modelo_label)
  plot_data$Modelo_label[missing_lab] <- as.character(plot_data$Modelo[missing_lab])
  plot_data$Modelo_label <- factor(plot_data$Modelo_label, levels = rev(plot_data$Modelo_label))

  plot_data$Peso_label <- ifelse(plot_data$Peso_Akaike < 0.001, "< 0.001", sprintf("%.3f", plot_data$Peso_Akaike))
  plot_data$AICc_label <- sprintf("%.1f", plot_data$AICc)

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Peso_Akaike, y = Modelo_label)) +
    ggplot2::geom_col(width = 0.66, fill = "#2A9D8F") +
    ggplot2::geom_text(ggplot2::aes(label = Peso_label), hjust = -0.06, size = 4.5, color = "#20262C") +
    ggplot2::geom_vline(xintercept = 1.22, linewidth = 0.35, color = "#D1D5DB") +
    ggplot2::geom_text(ggplot2::aes(x = 1.34, label = AICc_label), hjust = 0, size = 4.5, color = "#20262C") +
    ggplot2::annotate("text", x = 1.34, y = length(levels(plot_data$Modelo_label)) + 0.42,
                      label = "AICc", hjust = 0, fontface = "bold", size = 4.8) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.25, 0.50, 0.75, 1.00),
      labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
      limits = c(0, 1.55), expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1.55), clip = "off") +
    ggplot2::labs(x = "Akaike weight (support)", y = NULL) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.35, color = "#E5E7EB"),
      axis.title.x = ggplot2::element_text(size = 15.5, face = "bold", margin = ggplot2::margin(t = 9)),
      axis.text.x = ggplot2::element_text(size = 12.5, color = "#20262C"),
      axis.text.y = ggplot2::element_text(size = 14.0, color = "#20262C", face = "bold"),
      plot.margin = ggplot2::margin(22, 80, 12, 10)
    )

  save_plot_dual(plot, "FigS1. Comparacao de modelos do dimorfismo", 8.6, 4.9)
  invisible(plot)
}


# Gera e salva a FigS1 usando a tabela que acabou de ser produzida acima.
plot_model_comparison(models$comparison)


# ==============================================================================
# ANALISE 02B - MATRIZ Q DO MELHOR MODELO
# ==============================================================================
# PERGUNTA DESTA ETAPA:
# Depois de selecionar o melhor modelo, quais sao as taxas estimadas para cada
# direcao de mudanca entre os tres estados de dimorfismo?
#
# RESULTADOS GERADOS LOGO ABAIXO:
#   TableS4 = taxas de transicao da matriz Q
#   FigS2   = rede grafica dessas transicoes
# ==============================================================================
console_step(4, 5, "Extraindo a matriz Q do melhor modelo")

# A matriz Q e extraida SOMENTE do melhor modelo selecionado na Analise 02A.
# Cada valor fora da diagonal representa uma taxa de transicao origem -> destino.
q_dimorphism <- extract_q_matrix(
  models$best_fit,
  names(STATE_LABELS),
  STATE_LABELS
)

# A versao 'tidy' transforma a matriz em uma tabela longa mais facil de ler.
q_dimorphism$tidy$Modelo <- models$best_model
q_dimorphism$tidy$Proxima_de_zero <- q_dimorphism$tidy$Taxa_por_Ma <= CONFIG$lower_rate_bound * 10
print(q_dimorphism$tidy)

# ------------------------------------------------------------------------------
# TABELA DA ANALISE 02B - TableS4
# ------------------------------------------------------------------------------
write_csv_utf8(
  q_dimorphism$tidy,
  file.path(PATHS$tables_supp, "TableS4. Matriz Q do dimorfismo.csv")
)

# ------------------------------------------------------------------------------
# FIGURA DA ANALISE 02B - FigS2
# ------------------------------------------------------------------------------
# Rede de transicoes do melhor modelo Mk.
#
# Cada direcao tem sua propria seta e taxa, inclusive quando as taxas sao iguais.

plot_transition_network <- function(q_matrix, state_labels, palette,
                                    state_counts, scheme_name, model_name, stem) {
  codes <- names(state_labels)
  mk_validate_q(q_matrix, codes)
  nodes <- data.frame(state = c("0", "1", "2"), label = c("Mon", "Hem", "Hol"),
    x = c(-0.8, 0, 0.8), y = c(0, 1.38, 0))
  edges <- expand.grid(from = codes, to = codes, stringsAsFactors = FALSE)
  edges <- edges[edges$from != edges$to, ]
  edges$rate <- mapply(function(a, b) q_matrix[a, b], edges$from, edges$to)
  a <- match(edges$from, nodes$state); b <- match(edges$to, nodes$state)
  dx <- nodes$x[b] - nodes$x[a]; dy <- nodes$y[b] - nodes$y[a]
  len <- sqrt(dx^2 + dy^2); ux <- dx/len; uy <- dy/len
  # Setas paralelas em sentidos opostos, separadas para nao ocultar a assimetria.
  offset <- 0.075; inset <- 0.22
  edges$x <- nodes$x[a] + inset*ux - offset*uy
  edges$y <- nodes$y[a] + inset*uy + offset*ux
  edges$xend <- nodes$x[b] - inset*ux - offset*uy
  edges$yend <- nodes$y[b] - inset*uy + offset*ux
  edges$label_x <- (nodes$x[a]+nodes$x[b])/2 - 0.17*uy
  edges$label_y <- (nodes$y[a]+nodes$y[b])/2 + 0.17*ux
  edges$angle <- atan2(dy, dx)*180/pi
  edges$angle <- ifelse(edges$angle > 90, edges$angle-180,
    ifelse(edges$angle < -90, edges$angle+180, edges$angle))
  edges$label <- ifelse(edges$rate == 0, "0 (fixed)",
    ifelse(edges$rate <= CONFIG$lower_rate_bound*10, "\u2248 0", formatC(edges$rate, format="g", digits=3)))
  plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(data=edges, ggplot2::aes(x=x,y=y,xend=xend,yend=yend),
      linewidth=0.85, color="#38434F",
      arrow=grid::arrow(length=grid::unit(0.11,"inches"),type="closed",ends="last")) +
    ggplot2::geom_label(data=edges, ggplot2::aes(x=label_x,y=label_y,label=label,angle=angle),
      size=3.7, linewidth=0, fill="white", label.padding=grid::unit(0.08,"lines")) +
    ggplot2::geom_point(data=nodes, ggplot2::aes(x=x,y=y,fill=state),shape=21,size=21,
      color="#263238",stroke=1,show.legend=FALSE) +
    ggplot2::geom_text(data=nodes, ggplot2::aes(x=x,y=y,label=label),
      color="white",fontface="bold",size=5.5) +
    ggplot2::scale_fill_manual(values=palette) +
    ggplot2::coord_equal(xlim=c(-1.15,1.15),ylim=c(-0.4,1.7),clip="off") +
    ggplot2::labs(caption=paste0("Directional transition rates — selected model: ",model_name,
      "\nRates per Ma; \u2248 0 denotes an estimate near the lower bound.")) +
    ggplot2::theme_void(base_size=12) +
    ggplot2::theme(plot.caption=ggplot2::element_text(hjust=0.5),
      plot.margin=ggplot2::margin(10,15,10,15))
  save_plot_dual(plot,stem,7.2,5.8)
}


# ------------------------------------------------------------------------------
# GERAR FigS2
# ------------------------------------------------------------------------------
plot_transition_network(
  q_dimorphism$matrix,
  STATE_LABELS,
  PALETTE_DIMORPHISM,
  state_counts,
  "Tres estados",
  models$best_model,
  "FigS2. Rede de transicoes do dimorfismo"
)


# ==============================================================================
# PASSO 5 - SALVAR OS RESULTADOS PARA O SCRIPT 03
# ==============================================================================
console_step(5, 5, "Salvando resultados para o SIMMAP e registrando as saidas")

# Guarda os modelos e a matriz Q juntos em um unico objeto.
# O Script 03 (SIMMAP) precisa principalmente da matriz Q, portanto este arquivo
# deve ser mantido em Scripts_/_Objetos_R/.
result <- list(
  selection_policy = MK_POLICY_VERSION,
  input_signature = mk_signature(tree, trait_data, CONFIG$n_starts,
    CONFIG$lower_rate_bound, CONFIG$upper_rate_bound),
  models = models,
  Q = q_dimorphism
)

# Salva o resultado calculado para reutilizacao.
# Isso evita ter que ajustar todos os modelos Mk novamente apenas para executar
# o SIMMAP ou redesenhar uma figura.
save_pipeline_rds(result, "02_evolucao_dimorfismo.rds")

console_output("02_evolucao_dimorfismo.rds")
console_output("Table1, TableS3, TableS4, FigS1 e FigS2")
log_message("SCRIPT 02 concluido. Melhor modelo: ", models$best_model)
console_done("SCRIPT 02", "03_SIMMAP_DIMORFISMO.R")