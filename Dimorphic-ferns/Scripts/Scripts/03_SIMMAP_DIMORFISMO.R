# ==============================================================================
# 03_SIMMAP_DIMORFISMO.R
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
#   01_PREPARAR_DADOS_E_FILOGENIA.R + 02_EVOLUCAO_DIMORFISMO.R
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
#   O SIMMAP pode ser uma das etapas mais demoradas. Evite apagar o .rds salvo.
#
# QUANDO TERMINAR:
#   Depois, rode 04_SINAL_FILOGENETICO_DIMORFISMO.R.
# ------------------------------------------------------------------------------
# PERGUNTA:
# Quantas transicoes entre os estados provavelmente ocorreram, quanto tempo foi
# passado em cada estado e qual estado e mais provavel na raiz?
#
# ANALISE:
# Mapeamento estocastico (SIMMAP) condicionado a matriz Q da Analise 02.
#
# PARA QUE SERVE:
# Resume historias evolutivas plausiveis sobre a arvore e quantifica transicoes,
# permanencia em estados e incerteza ancestral.
#
# O QUE OLHAR:
# Media e IC95% das transicoes, tempo nos estados e probabilidades da raiz.
#
# DEPENDENCIAS:
# 01_PREPARAR_DADOS_E_FILOGENIA.R
# 02_EVOLUCAO_DIMORFISMO.R
#
# LE:
# 01_dados_e_filogenia.rds
# 02_evolucao_dimorfismo.rds
#
# OBJETOS SALVOS:
# Scripts_/_Objetos_R/simmap_Tres_estados.rds
# Scripts_/_Objetos_R/03_simmap_dimorfismo.rds
#
# TABELAS GERADAS NESTE SCRIPT:
# Table2. Estado provavel da raiz.csv
# Table3. Transicoes principais do SIMMAP.csv
# TableS5. Estado da raiz no SIMMAP.csv           
# TableS6. Transicoes do SIMMAP.csv
# TableS7. Tempo nos estados do SIMMAP.csv
# TableS28. Rotas de entrada em holodimorfia.csv
#
# FIGURAS GERADAS NESTE SCRIPT:
# Fig5. Filogenia circular com aneis de caracteres
# Fig5B. Esquema de identificacao dos aneis
# Fig4. Proporcao de dimorfismo por familia
# FigS3. Reconstrucao filogenetica do dimorfismo
# FigS4. Filogenias ascendentes dos caracteres em hemi e holodimorfas
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
required_packages <- c("ape", "dplyr", "ggplot2", "phytools", "scales")
check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# CONFIGURACAO E FUNCOES EXCLUSIVAS DO SCRIPT 03
# ============================================================================== 
quick_mode <- is_true(Sys.getenv("LEAF_DIMORPHISM_QUICK", "false"))
CONFIG <- list(
  seed = 20260803L,
  n_simmap = if (quick_mode) 5L else 500L
)
set.seed(CONFIG$seed)

# ==============================================================================
# PALETAS COORDENADAS COMPARTILHADAS PELAS FIGURAS DO SCRIPT 03
# ==============================================================================
# Estas paletas ficam FORA das funcoes para poderem ser reutilizadas tanto pela
# Fig5 (aneis da filogenia circular) quanto pela FigS4 (filogenias ascendentes).
# Assim, as mesmas categorias recebem exatamente as mesmas cores nas duas figuras.

# Arquitetura = degrade de azul
architecture_palette <- c(
  "0" = "#D9D9D9",
  "1" = "#DCEAF7",
  "2" = "#B7D4EC",
  "3" = "#7EB6DD",
  "4" = "#438FC5",
  "5" = "#1769A3",
  "6" = "#083B73"
)

# Variacao = degrade de lilas/roxo
variation_palette <- c(
  "0" = "#D9D9D9",
  "1" = "#F0E6F6",
  "2" = "#D9BFDF",
  "3" = "#BE91C9",
  "4" = "#9D61B2",
  "5" = "#7B3598",
  "6" = "#561A78",
  "7" = "#31004F"
)

# Tamanho = degrade de laranja/terracota
size_palette <- c(
  "0" = "#D9D9D9",
  "1" = "#FBE3C2",
  "2" = "#F3B66D",
  "3" = "#DD7B2D",
  "4" = "#9E3F13"
)

# Habito = degrade de verde/azulado
habit_palette <- c(
  "0" = "#E1F3EF",
  "1" = "#C1E4DC",
  "2" = "#91CDBF",
  "3" = "#5BAF9E",
  "4" = "#318D7D",
  "5" = "#176B62",
  "6" = "#0A4E4A"
)

# ============================================================================
# ROTULOS EM INGLES USADOS NAS FIGURAS DESTE SCRIPT
# Todos os rotulos de estados sao herdados do Script 00. Os aliases abaixo
# existem apenas para preservar os nomes internos ja usados pelas figuras.
# ============================================================================
# As figuras internas podem ficar em ingles, mas mantemos os nomes dos arquivos
# e das tabelas em portugues.
STATE_LABELS_FIG <- STATE_LABELS
ARCHITECTURE_LABELS_FIG <- ARCHITECTURE_LABELS
HABIT_LABELS_FIG <- HABIT_LABELS

# Estas funcoes fazem exclusivamente o mapeamento estocastico e seus resumos.
# Elas nao ajustam novamente os modelos Mk: a matriz Q vem pronta do Script 02.
ensure_multisimmap <- function(maps) {
  if (inherits(maps, "simmap") && !inherits(maps, "multiSimmap")) {
    maps <- list(maps)
    class(maps) <- c("multiSimmap", "multiPhylo")
  }
  maps
}

# Completa estados ausentes em mapped.edge com colunas de zeros.
mapped_edge_complete <- function(simmap_tree, states) {
  output <- matrix(
    0,
    nrow = nrow(simmap_tree$edge),
    ncol = length(states),
    dimnames = list(NULL, states)
  )
  present <- intersect(colnames(simmap_tree$mapped.edge), states)
  output[, present] <- simmap_tree$mapped.edge[, present, drop = FALSE]
  output
}

# Resume todos os mapas: numero de transicoes, tempo em cada estado,
# probabilidades do estado da raiz e incerteza de estado ao longo dos ramos.
summarize_simmap <- function(maps, tree, state_codes, state_labels, scheme_name) {
  maps <- ensure_multisimmap(maps)
  n_maps <- length(maps)
  n_states <- length(state_codes)
  n_edges <- nrow(tree$edge)
  
  transition_array <- array(
    0,
    dim = c(n_maps, n_states, n_states),
    dimnames = list(NULL, state_codes, state_codes)
  )
  dwell_matrix <- matrix(
    0,
    nrow = n_maps,
    ncol = n_states,
    dimnames = list(NULL, state_codes)
  )
  
  mapped_edges <- vector("list", n_maps)
  endpoint_counts <- matrix(
    0,
    nrow = n_edges,
    ncol = n_states,
    dimnames = list(NULL, state_codes)
  )
  
  for (i in seq_along(maps)) {
    counted <- phytools::countSimmap(maps[[i]], states = state_codes, message = FALSE)
    transition_array[i, , ] <- counted$Tr[state_codes, state_codes, drop = FALSE]
    mapped_edges[[i]] <- mapped_edge_complete(maps[[i]], state_codes)
    dwell_matrix[i, ] <- colSums(mapped_edges[[i]])
    
    # Estado no FINAL de cada ramo (no descendente) em cada SIMMAP.
    endpoint_states_i <- vapply(
      maps[[i]]$maps,
      function(segments) {
        segment_states <- names(segments)
        if (length(segment_states) == 0L) return(NA_character_)
        tail(segment_states, 1L)
      },
      character(1)
    )
    
    if (length(endpoint_states_i) != n_edges) {
      stop(
        "O numero de ramos em maps[[i]]$maps nao corresponde a tree$edge.",
        call. = FALSE
      )
    }
    
    for (state in state_codes) {
      endpoint_counts[, state] <- endpoint_counts[, state] +
        as.integer(endpoint_states_i == state)
    }
  }
  
  transition_rows <- list()
  row_index <- 1L
  for (origin in state_codes) {
    for (destination in state_codes) {
      if (origin == destination) next
      values <- transition_array[, origin, destination]
      transition_rows[[row_index]] <- data.frame(
        Esquema = scheme_name,
        Origem = origin,
        Origem_nome = unname(state_labels[origin]),
        Destino = destination,
        Destino_nome = unname(state_labels[destination]),
        Media = mean(values),
        Mediana = stats::median(values),
        IC95_inferior = unname(stats::quantile(values, 0.025, names = FALSE)),
        IC95_superior = unname(stats::quantile(values, 0.975, names = FALSE))
      )
      row_index <- row_index + 1L
    }
  }
  transition_summary <- dplyr::bind_rows(transition_rows)
  
  total_tree_length <- sum(tree$edge.length)
  dwell_summary <- dplyr::bind_rows(lapply(state_codes, function(state) {
    values <- dwell_matrix[, state] / total_tree_length
    data.frame(
      Esquema = scheme_name,
      Estado = state,
      Estado_nome = unname(state_labels[state]),
      Proporcao_media = mean(values),
      Proporcao_mediana = stats::median(values),
      IC95_inferior = unname(stats::quantile(values, 0.025, names = FALSE)),
      IC95_superior = unname(stats::quantile(values, 0.975, names = FALSE))
    )
  }))
  
  description <- phytools::describe.simmap(maps, plot = FALSE)
  root_probs <- simmap_root_probabilities(description, maps, tree, state_codes)
  root_summary <- data.frame(
    Esquema = scheme_name,
    Estado = state_codes,
    Estado_nome = unname(state_labels[state_codes]),
    Probabilidade_raiz = root_probs
  )
  
  # Mantemos a proporcao de TEMPO em cada estado para auditoria.
  edge_state_time <- Reduce(`+`, mapped_edges) / n_maps
  edge_time_proportions <- edge_state_time / rowSums(edge_state_time)
  
  # Para COLORIR uma arvore em que cada ramo tem uma unica cor, usamos a
  # probabilidade do estado NO FINAL do ramo entre os mapas. Assim, todo ramo
  # terminal obrigatoriamente termina no mesmo estado observado da especie.
  edge_probabilities <- endpoint_counts / n_maps
  
  list(
    transitions = transition_summary,
    dwell = dwell_summary,
    root = root_summary,
    edge_probabilities = edge_probabilities,
    edge_time_proportions = edge_time_proportions,
    description = description
  )
}

# Simula historias evolutivas usando a matriz Q do melhor modelo.
# Salva os mapas brutos em RDS e devolve as estatisticas resumidas ao script da analise.
# Funcao generica de mapeamento estocastico.
# Recebe arvore, estados e matriz Q ja definidos e devolve mapas + resumos.
run_simmap <- function(tree, trait_vector, q_matrix, state_codes, state_labels,
                       scheme_name, n_simulations) {
  log_message(
    "Gerando ", n_simulations, " mapas estocasticos para ", scheme_name, "..."
  )
  
  mk_validate_q(q_matrix, state_codes)
  q_matrix <- q_matrix[state_codes, state_codes, drop = FALSE]
  maps <- phytools::make.simmap(
    tree = tree,
    x = trait_vector,
    model = "ARD",
    nsim = n_simulations,
    Q = q_matrix,
    pi = "estimated",
    message = FALSE
  )
  maps <- ensure_multisimmap(maps)
  
  # Controle de qualidade: o SIMMAP e condicionado aos estados observados nas
  # pontas. Portanto, o ultimo estado de cada ramo terminal deve coincidir com
  # o estado da especie em TODOS os mapas.
  terminal_rows <- match(seq_len(ape::Ntip(tree)), tree$edge[, 2])
  observed_tip_states <- as.character(trait_vector[tree$tip.label])
  
  if (anyNA(terminal_rows)) {
    stop("Nao foi possivel localizar todos os ramos terminais.", call. = FALSE)
  }
  if (anyNA(observed_tip_states)) {
    stop("Existem pontas sem estado observado antes do SIMMAP.", call. = FALSE)
  }
  
  for (map_i in seq_along(maps)) {
    terminal_states_i <- vapply(
      maps[[map_i]]$maps[terminal_rows],
      function(segments) tail(names(segments), 1L),
      character(1)
    )
    mismatch_i <- which(terminal_states_i != observed_tip_states)
    if (length(mismatch_i) > 0L) {
      stop(
        "Falha de alinhamento no SIMMAP (mapa ", map_i, "). Especies: ",
        paste(tree$tip.label[mismatch_i], collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  log_message("QC terminal do SIMMAP OK: todas as pontas coincidem com os dados observados.")
  
  saveRDS(
    maps,
    windows_safe_path(
      file.path(PATHS$objects, paste0("simmap_", scheme_name, ".rds"))
    ),
    compress = "xz"
  )
  
  summary <- summarize_simmap(
    maps, tree, state_codes, state_labels, scheme_name
  )
  
  list(maps = maps, summary = summary)
}

# 2.6 HABITO E REGRESSAO LOGISTICA FILOGENETICA -------------------------------
#
# Com a codificacao final, Habito e UM fator com sete categorias (0-6). O codigo
# 6 representa especies documentadas em dois ou mais tipos de habito; ele resume
# amplitude de habito e nao informa quais habitos compoem a combinacao.
#
# O modelo compara cada categoria com uma referencia. Terrícola e usada como
# referencia quando possui amostra suficiente; caso contrario, usa-se a primeira


# ==============================================================================
# FUNCOES ESPECIFICAS DESTA ANALISE
# Estas funcoes ficam aqui para que a estrutura de tabelas/figuras desta etapa
# seja visivel no proprio script e nao fique escondida no script 00.
# ==============================================================================

# Funcao de figura: resume o estado reconstruido ao longo da arvore.
# Ela recebe probabilidades ja calculadas pelo SIMMAP; nao executa novas simulacoes.
plot_phylogeny_summary <- function(tree, edge_probabilities, trait_vector,
                                   state_labels, palette, scheme_name, stem,
                                   width = 10, height = 10) {
  state_codes <- colnames(edge_probabilities)
  dominant_index <- max.col(edge_probabilities, ties.method = "first")
  dominant_state <- state_codes[dominant_index]
  confidence <- edge_probabilities[cbind(seq_len(nrow(edge_probabilities)), dominant_index)]
  edge_colors <- mapply(
    function(color, alpha) grDevices::adjustcolor(color, alpha.f = alpha),
    unname(palette[dominant_state]),
    0.25 + 0.75 * confidence,
    USE.NAMES = FALSE
  )
  tip_states <- as.character(trait_vector[tree$tip.label])
  tip_colors <- unname(palette[tip_states])
  tip_colors[is.na(tip_colors)] <- "#D9D9D9"
  
  # Ramos terminais ficam com a MESMA cor solida da ponta observada.
  terminal_rows <- match(seq_len(ape::Ntip(tree)), tree$edge[, 2])
  edge_colors[terminal_rows] <- tip_colors
  
  draw_tree <- function() {
    graphics::layout(
      matrix(c(1, 2), ncol = 1),
      heights = c(5.2, 1.6)
    )
    graphics::par(oma = c(0, 0, 0, 0), xpd = NA)
    
    graphics::par(mar = c(0.4, 1, 3.4, 1), xpd = NA)
    ape::plot.phylo(
      tree,
      type = "fan",
      show.tip.label = FALSE,
      edge.color = edge_colors,
      edge.width = 1.5,
      no.margin = FALSE,
      main = paste0("Historia evolutiva do dimorfismo - ", scheme_name)
    )
    ape::tiplabels(
      pch = 21,
      bg = tip_colors,
      col = NA,
      cex = 0.22
    )
    graphics::mtext(
      "Branch color = modal state at descendant node; transparency = uncertainty",
      side = 1,
      line = -1,
      cex = 0.65,
      col = "#5F6B75"
    )
    
    graphics::par(mar = c(0.1, 0.1, 0.1, 0.1), xpd = NA)
    graphics::plot.new()
    graphics::legend(
      "center",
      legend = unname(state_labels[state_codes]),
      fill = unname(palette[state_codes]),
      border = NA,
      bty = "n",
      cex = 0.8,
      title = "State",
      ncol = 1,
      x.intersp = 0.7,
      y.intersp = 0.95
    )
  }
  
  save_base_plot_dual(stem, width, height, draw_tree)
}

# Funcao de figura principal: combina filogenia circular, estados de dimorfismo e aneis de caracteres.
# Toda a parte abaixo desta funcao e desenho/organizacao visual, nao inferencia estatistica.
plot_circular_phylogeny_rings <- function(tree, edge_probabilities,
                                          analysis_data, stem,
                                          width = 16, height = 14,
                                          ring_gap_fraction = 0.004,
                                          ring_width_fraction = 0.028,
                                          ring_start_multiplier = 1.028,
                                          branch_width = 0.95,
                                          family_connector_gap_fraction = 0.012,
                                          family_connector_width = 0.95,
                                          family_label_cex = 1.32,
                                          show_ring_guides = TRUE,
                                          ring_guide_cex = 0.92,
                                          show_family_panel = TRUE,
                                          family_panel_cex = 0.92) {
  
  # --------------------------------------------------------------------------
  # 1. CHECAGENS BASICAS
  # --------------------------------------------------------------------------
  # A Fig5 precisa destes caracteres porque cada um ocupa um anel.
  required_columns <- c(
    "species", "family", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho", "Habito"
  )
  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop(
      "Colunas ausentes para a Figura 1 circular: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  # edge_probabilities contem uma linha para cada ramo da arvore.
  if (nrow(edge_probabilities) != nrow(tree$edge)) {
    stop(
      "As probabilidades dos ramos nao correspondem a arvore da Figura 1.",
      call. = FALSE
    )
  }
  
  # --------------------------------------------------------------------------
  # 2. PALETA COMPARTILHADA DO DIMORFISMO
  # --------------------------------------------------------------------------
  # Dimorfismo: usa diretamente a paleta canonica definida no Script 00.
  dimorphism_palette_fig1 <- PALETTE_DIMORPHISM
  
  # As paletas de Arquitetura, Variacao, Tamanho e Habito foram definidas
  # no bloco global no inicio do Script 03 para serem compartilhadas com a FigS4.
  
  # --------------------------------------------------------------------------
  # 3. CORES E ESPESSURA DOS RAMOS DO SIMMAP
  # --------------------------------------------------------------------------
  state_codes <- colnames(edge_probabilities)
  dominant_index <- max.col(edge_probabilities, ties.method = "first")
  dominant_state <- state_codes[dominant_index]
  confidence <- edge_probabilities[
    cbind(seq_len(nrow(edge_probabilities)), dominant_index)
  ]
  confidence[!is.finite(confidence)] <- 0
  
  # Alfa minimo maior para os ramos nao ficarem apagados.
  branch_colors <- mapply(
    function(color, certainty) {
      grDevices::adjustcolor(color, alpha.f = 0.52 + 0.43 * certainty)
    },
    unname(dimorphism_palette_fig1[dominant_state]),
    confidence,
    USE.NAMES = FALSE
  )
  
  # --------------------------------------------------------------------------
  # 4. ALINHAMENTO DA MATRIZ COM AS PONTAS DA ARVORE
  # --------------------------------------------------------------------------
  plot_data <- analysis_data[
    match(tree$tip.label, analysis_data$species),
    , drop = FALSE
  ]
  if (anyNA(plot_data$species) || anyNA(plot_data$family)) {
    stop("Falha ao alinhar dados e arvore na Figura 1 circular.", call. = FALSE)
  }
  
  # Ramos terminais da Fig5 recebem a cor solida do estado observado de
  # dimorfismo. Assim, o ramo encosta no anel 1 sem mudar de cor.
  terminal_rows_fig1 <- match(seq_len(ape::Ntip(tree)), tree$edge[, 2])
  terminal_states_fig1 <- as.character(plot_data$Padrao_Dimorfismo)
  branch_colors[terminal_rows_fig1] <- unname(
    dimorphism_palette_fig1[terminal_states_fig1]
  )
  
  # Cada item desta lista corresponde a UM anel da Fig5.
  tracks <- list(
    list(
      title = "1 Pattern type",
      values = as.character(plot_data$Padrao_Dimorfismo),
      labels = STATE_LABELS_FIG,
      palette = dimorphism_palette_fig1
    ),
    list(
      title = "2 Fertile frond Architecture",
      values = as.character(plot_data$Arquitetura_Lamina),
      labels = ARCHITECTURE_LABELS_FIG,
      palette = architecture_palette
    ),
    list(
      title = "3 Morphological variation fertile",
      values = as.character(plot_data$Variacao_Morfologica),
      labels = VARIATION_LABELS,
      palette = variation_palette
    ),
    list(
      title = "4 Size ratio",
      values = as.character(plot_data$Relacao_Tamanho),
      labels = SIZE_LABELS,
      palette = size_palette
    ),
    list(
      title = "5 Habit",
      values = as.character(plot_data$Habito),
      labels = HABIT_LABELS_FIG,
      palette = habit_palette
    )
  )
  
  # --------------------------------------------------------------------------
  # 5. FUNCOES AUXILIARES EXCLUSIVAS DA Fig5
  # --------------------------------------------------------------------------
  make_family_codes <- function(family_names) {
    stems <- toupper(sub("ACEAE$", "", family_names, ignore.case = TRUE))
    codes <- substr(stems, 1L, 3L)
    repeat {
      duplicated_codes <- duplicated(codes) | duplicated(codes, fromLast = TRUE)
      if (!any(duplicated_codes)) break
      previous <- codes
      codes[duplicated_codes] <- vapply(
        which(duplicated_codes),
        function(i) {
          width_i <- min(nchar(stems[i]), nchar(codes[i]) + 1L)
          substr(stems[i], 1L, width_i)
        },
        character(1)
      )
      if (identical(previous, codes)) {
        codes <- make.unique(codes, sep = "")
        break
      }
    }
    setNames(codes, family_names)
  }
  
  draw_compact_legend <- function(title, labels, palette, observed = NULL,
                                  cex = 0.76, ncol = 1L) {
    codes <- names(labels)
    if (!is.null(observed)) {
      codes <- codes[codes %in% unique(as.character(observed))]
    }
    graphics::par(mar = c(0.1, 0.1, 0.1, 0.1), xpd = NA)
    graphics::plot.new()
    graphics::legend(
      "center",
      title = title,
      legend = unname(labels[codes]),
      pch = 15,
      col = unname(palette[codes]),
      pt.cex = 1.15,
      cex = cex,
      y.intersp = 0.90,
      x.intersp = 0.52,
      ncol = ncol,
      bty = "n"
    )
  }
  
  # --------------------------------------------------------------------------
  # 6. DESENHO COMPLETO DA PRANCHA
  # --------------------------------------------------------------------------
  draw_atlas <- function() {
    # A primeira linha e dividida em dois paineis:
    #   1 = filogenia circular com aneis;
    #   2 = composicao de Mon/Hem/Hol por familia.
    # As duas linhas inferiores continuam reservadas para as legendas.
    if (isTRUE(show_family_panel)) {
      graphics::layout(
        matrix(
          c(
            rep(1, 8), rep(2, 4),
            3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
            rep(9, 12)
          ),
          nrow = 3,
          byrow = TRUE
        ),
        heights = c(10.2, 1.95, 1.65),
        widths = c(rep(1, 8), rep(1.06, 4))
      )
    } else {
      # Quando o painel de proporcoes nao deve aparecer junto da filogenia,
      # usamos toda a primeira linha para a arvore e mantemos as legendas abaixo.
      graphics::layout(
        matrix(
          c(
            rep(1, 12),
            2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7,
            rep(8, 12)
          ),
          nrow = 3,
          byrow = TRUE
        ),
        heights = c(10.8, 1.95, 1.65),
        widths = rep(1, 12)
      )
    }
    
    tree_height <- max(
      ape::node.depth.edgelength(tree)[seq_len(ape::Ntip(tree))]
    )
    
    # ------------------------------------------------------------------------
    # CONTROLE DOS ANEIS
    # ------------------------------------------------------------------------
    # Uma folga radial separa as pontas da arvore do primeiro anel.
    ring_gap <- tree_height * ring_gap_fraction
    ring_width <- tree_height * ring_width_fraction
    ring_start <- tree_height * ring_start_multiplier
    
    family_width <- tree_height * 0.038
    # Inclui um gap adicional antes do anel de familias.
    family_inner <- ring_start + length(tracks) * ring_width +
      length(tracks) * ring_gap
    family_outer <- family_inner + family_width
    # Reduzimos o limite externo para a arvore preencher melhor o painel.
    outer_limit <- tree_height * 1.62
    
    graphics::par(mar = c(0.05, 0.05, 0.05, 0.05), xpd = NA)
    
    # ------------------------------------------------------------------------
    # ARVORE
    # ------------------------------------------------------------------------
    ape::plot.phylo(
      tree,
      type = "fan",
      show.tip.label = FALSE,
      edge.color = branch_colors,
      edge.width = branch_width,
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
    
    # ------------------------------------------------------------------------
    # NOS INTERNOS
    # ------------------------------------------------------------------------
    # Nesta versao da Fig5 NAO desenhamos pizzas nos nos.
    # As probabilidades ancestrais continuam salvas no objeto SIMMAP e nas
    # tabelas; apenas retiramos esse elemento visual para deixar a arvore limpa.
    
    # ------------------------------------------------------------------------
    # FAIXAS ANULARES CONTINUAS
    # ------------------------------------------------------------------------
    # Nao ha espacos entre especies DENTRO do mesmo anel; o pequeno espaco
    # branco aparece somente ENTRE um anel e o seguinte.
    draw_continuous_annulus <- function(inner_radius, outer_radius, colors) {
      angle_order_local <- order(tip_angle_0_2pi)
      ordered_angles_local <- tip_angle_0_2pi[angle_order_local]
      n_tips_local <- length(ordered_angles_local)
      
      previous_angles <- c(
        ordered_angles_local[n_tips_local] - 2 * pi,
        ordered_angles_local[-n_tips_local]
      )
      next_angles <- c(
        ordered_angles_local[-1L],
        ordered_angles_local[1L] + 2 * pi
      )
      
      left_boundaries <- (previous_angles + ordered_angles_local) / 2
      right_boundaries <- (ordered_angles_local + next_angles) / 2
      overlap <- 1e-5
      
      for (j in seq_len(n_tips_local)) {
        theta <- seq(
          left_boundaries[j] - overlap,
          right_boundaries[j] + overlap,
          length.out = 10L
        )
        color_j <- colors[angle_order_local[j]]
        if (is.na(color_j) || !nzchar(color_j)) color_j <- "#F3F4F6"
        
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
    }
    
    # ------------------------------------------------------------------------
    # CINCO ANEIS DE CARACTERES
    # ------------------------------------------------------------------------
    for (track_index in seq_along(tracks)) {
      track <- tracks[[track_index]]
      inner_radius <- ring_start +
        (track_index - 1L) * (ring_width + ring_gap)
      outer_radius <- inner_radius + ring_width
      colors <- unname(track$palette[track$values])
      colors[is.na(colors)] <- "#F3F4F6"
      
      draw_continuous_annulus(
        inner_radius = inner_radius,
        outer_radius = outer_radius,
        colors = colors
      )
    }
    
    # ------------------------------------------------------------------------
    # ANEL EXTERNO DE FAMILIAS
    # ------------------------------------------------------------------------
    family_by_tip <- as.character(plot_data$family)
    angle_order <- order(tip_angle_0_2pi)
    family_order <- unique(family_by_tip[angle_order])
    n_families <- length(family_order)
    
    family_colors <- rep(
      c("#344955", "#657D89", "#9EADB4"),
      length.out = n_families
    )
    family_palette <- setNames(family_colors, family_order)
    family_codes <- make_family_codes(family_order)
    
    draw_continuous_annulus(
      inner_radius = family_inner,
      outer_radius = family_outer,
      colors = unname(family_palette[family_by_tip])
    )
    
    # Linhas brancas finas marcam apenas mudancas de familia.
    ordered_angles <- tip_angle_0_2pi[angle_order]
    ordered_families <- family_by_tip[angle_order]
    next_index <- c(seq.int(2L, length(angle_order)), 1L)
    family_changes <- which(
      ordered_families != ordered_families[next_index]
    )
    
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
    
    # ------------------------------------------------------------------------
    # ABREVIACOES DAS FAMILIAS E LINHAS EXTERNAS
    # ------------------------------------------------------------------------
    family_mid_angle <- vapply(family_order, function(family_name) {
      indices <- which(family_by_tip == family_name)
      atan2(mean(sin(tip_angle[indices])), mean(cos(tip_angle[indices])))
    }, numeric(1))
    
    label_level <- (seq_along(family_order) - 1L) %% 7L
    # Afastamos mais os nomes das familias para a linha externa nao passar
    # por cima do texto e para a leitura ficar mais confortavel.
    label_radius <- family_outer + tree_height * (0.095 + 0.028 * label_level)
    
    # Cria uma folga antes de cada linha externa para ela nao tocar o anel.
    connector_gap <- tree_height * family_connector_gap_fraction
    
    for (i in seq_along(family_order)) {
      angle <- family_mid_angle[i]
      connector_start <- family_outer + connector_gap
      # Termina a linha com uma folga maior antes do texto para evitar
      # sobreposicao visual entre o segmento e a abreviacao da familia.
      connector_end <- label_radius[i] - tree_height * 0.090
      
      graphics::segments(
        cos(angle) * connector_start,
        sin(angle) * connector_start,
        cos(angle) * connector_end,
        sin(angle) * connector_end,
        col = "#475D68",
        lwd = family_connector_width,
        lend = "round"
      )
      
      angle_degrees <- (angle * 180 / pi + 360) %% 360
      text_rotation <- angle_degrees
      if (angle_degrees > 90 && angle_degrees < 270) {
        text_rotation <- angle_degrees + 180
      }
      
      graphics::text(
        cos(angle) * label_radius[i],
        sin(angle) * label_radius[i],
        labels = family_codes[family_order[i]],
        srt = text_rotation,
        cex = family_label_cex,
        font = 2,
        col = "#263238"
      )
    }
    
    # ------------------------------------------------------------------------
    # GUIA COM SETAS PARA IDENTIFICAR OS ANEIS
    # ------------------------------------------------------------------------
    # Opcional: guia com setas para identificar os aneis.
    # Nesta versao final deixamos desligado por padrao, porque a figura
    # ficou mais limpa sem as setas e o painel lateral permaneceu suficiente.
    if (isTRUE(show_ring_guides)) {
      guide_angle <- 18 * pi / 180
      guide_anchor_x <- tree_height * 1.26
      guide_text_x <- tree_height * 1.56
      
      guide_labels <- c(
        "1. Pattern type",
        "2. Fertile frond Architecture",
        "3. Morphological variation fertile",
        "4. Size ratio",
        "5. Habit",
        "F. Families"
      )
      
      guide_target_radii <- c(
        vapply(seq_along(tracks), function(track_index) {
          inner_radius <- ring_start + (track_index - 1L) * (ring_width + ring_gap)
          outer_radius <- inner_radius + ring_width
          (inner_radius + outer_radius) / 2
        }, numeric(1)),
        (family_inner + family_outer) / 2
      )
      
      guide_target_x <- cos(guide_angle) * guide_target_radii
      guide_target_y <- sin(guide_angle) * guide_target_radii
      
      base_y <- seq(tree_height * 0.66, tree_height * 0.25, length.out = length(guide_labels))
      
      for (i in seq_along(guide_labels)) {
        graphics::segments(
          x0 = guide_anchor_x,
          y0 = base_y[i],
          x1 = guide_target_x[i],
          y1 = guide_target_y[i],
          col = "#5A6B78",
          lwd = 1.15,
          xpd = NA
        )
        graphics::arrows(
          x0 = guide_anchor_x,
          y0 = base_y[i],
          x1 = guide_target_x[i],
          y1 = guide_target_y[i],
          length = 0.07,
          angle = 18,
          code = 2,
          col = "#5A6B78",
          lwd = 1.15,
          xpd = NA
        )
        graphics::text(
          x = guide_text_x,
          y = base_y[i],
          labels = guide_labels[i],
          adj = c(0, 0.5),
          cex = ring_guide_cex,
          font = 2,
          col = "#23313B",
          xpd = NA
        )
      }
    }
    
    # ------------------------------------------------------------------------
    # PAINEL LATERAL - DIMORFISMO POR FAMILIA
    # ------------------------------------------------------------------------
    # Este painel resume os estados OBSERVADOS nas especies atuais.
    # Cada barra corresponde a uma familia e soma 100%.
    # Azul = Monomorfa; dourado = Hemidimorfa; verde = Holodimorfa.
    # Diferentemente dos ramos da arvore, este painel NAO e uma reconstrucao
    # ancestral: ele e apenas um resumo descritivo da composicao de cada familia.
    if (isTRUE(show_family_panel)) {
      dimorphism_by_family <- table(
        factor(family_by_tip, levels = family_order),
        factor(
          as.character(plot_data$Padrao_Dimorfismo),
          levels = c("0", "1", "2")
        )
      )
      
      family_totals <- rowSums(dimorphism_by_family)
      family_prop <- sweep(
        dimorphism_by_family,
        1,
        pmax(family_totals, 1),
        "/"
      )
      
      graphics::par(
        mar = c(7.0, 5.4, 1.2, 0.9),
        xpd = NA,
        las = 1
      )
      
      family_prop_plot <- family_prop
      family_codes_plot <- unname(family_codes[rownames(family_prop_plot)])
      
      graphics::barplot(
        t(family_prop_plot),
        horiz = FALSE,
        beside = FALSE,
        col = unname(dimorphism_palette_fig1[c("0", "1", "2")]),
        border = NA,
        names.arg = family_codes_plot,
        cex.names = family_panel_cex * 0.92,
        las = 2,
        ylim = c(0, 1.02),
        axes = FALSE,
        space = 0.10
      )
      
      graphics::axis(
        2,
        at = c(0, 0.25, 0.50, 0.75, 1.00),
        labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
        cex.axis = family_panel_cex * 1.04,
        tck = -0.012,
        las = 1
      )
      graphics::mtext(
        "Proporcao",
        side = 2,
        line = 3.0,
        cex = family_panel_cex * 1.14,
        font = 2
      )
      graphics::mtext(
        "Familia",
        side = 1,
        line = 5.0,
        cex = family_panel_cex * 1.14,
        font = 2
      )
      
      graphics::legend(
        "top",
        legend = c("Monomorphic (Mon)", "Hemidimorphic (Hem)", "Holodimorphic (Hol)"),
        fill = unname(dimorphism_palette_fig1[c("0", "1", "2")]),
        border = NA,
        bty = "n",
        horiz = TRUE,
        cex = family_panel_cex * 0.88,
        x.intersp = 0.55,
        y.intersp = 0.9,
        inset = c(0, -0.02)
      )
    }
    
    # ------------------------------------------------------------------------
    # LEGENDAS
    # ------------------------------------------------------------------------
    draw_compact_legend(
      "Ramos do SIMMAP", STATE_LABELS, dimorphism_palette_fig1, cex = 1.16
    )
    draw_compact_legend(
      tracks[[1]]$title, tracks[[1]]$labels, tracks[[1]]$palette,
      observed = tracks[[1]]$values, cex = 1.12
    )
    draw_compact_legend(
      tracks[[2]]$title, tracks[[2]]$labels, tracks[[2]]$palette,
      observed = tracks[[2]]$values, cex = 1.05
    )
    draw_compact_legend(
      tracks[[3]]$title, tracks[[3]]$labels, tracks[[3]]$palette,
      observed = tracks[[3]]$values, cex = 1.00
    )
    draw_compact_legend(
      tracks[[4]]$title, tracks[[4]]$labels, tracks[[4]]$palette,
      observed = tracks[[4]]$values, cex = 1.06
    )
    draw_compact_legend(
      tracks[[5]]$title, tracks[[5]]$labels, tracks[[5]]$palette,
      observed = tracks[[5]]$values, cex = 0.98, ncol = 2L
    )
    
    graphics::par(mar = c(0.05, 0.05, 0.05, 0.05), xpd = NA)
    graphics::plot.new()
    graphics::legend(
      "center",
      title = "F  Families - abbreviations in the outer ring",
      legend = paste0(
        unname(family_codes[family_order]), "  ", family_order
      ),
      cex = 1.04,
      y.intersp = 1.08,
      x.intersp = 0.20,
      ncol = 5,
      bty = "n"
    )
  }
  
  # Salva PDF vetorial e PNG sem recalcular o SIMMAP.
  save_base_plot_dual(
    stem,
    width = width,
    height = height,
    plot_function = draw_atlas,
    directory = PATHS$figures_main,
    fit_to_a4 = FALSE
  )
}

# ==============================================================================
# EXECUCAO DA ANALISE 03
# ==============================================================================
console_banner("SCRIPT 03", "Mapeamento estocastico SIMMAP do dimorfismo")

# ------------------------------------------------------------------------------
# ANALISE 03A - SIMMAP: HISTORIAS EVOLUTIVAS DO DIMORFISMO
# ------------------------------------------------------------------------------
# ORDEM DESTE BLOCO:
#   1. carregar os resultados anteriores;
#   2. executar o SIMMAP;
#   3. imprimir os resultados;
#   4. salvar as tabelas;
#   5. configurar e gerar as figuras DESTA analise;
#   6. salvar o objeto .rds para reutilizacao.
# ------------------------------------------------------------------------------

console_step(1, 6, "Carregando dados do Script 01 e matriz Q do Script 02")
prepared <- require_saved_rds(
  "01_dados_e_filogenia.rds",
  "01_PREPARAR_DADOS_E_FILOGENIA.R"
)
evolution <- require_saved_rds(
  "02_evolucao_dimorfismo.rds",
  "02_EVOLUCAO_DIMORFISMO.R"
)

data <- prepared$data
tree <- prepared$tree
q_dimorphism <- evolution$Q
if (!identical(evolution$selection_policy, MK_POLICY_VERSION)) {
  stop("Rode o Script 02 ajustado antes do SIMMAP: o objeto salvo usa uma regra antiga.", call. = FALSE)
}
mk_check_input(evolution$input_signature, tree, data.frame(
  species = data$species, state = as.integer(data$Estado)))
selected <- evolution$models$comparison
selected <- selected[selected$Modelo == evolution$models$best_model, , drop = FALSE]
if (nrow(selected) != 1L || !isTRUE(selected$Elegivel_reconstrucao) || !isTRUE(selected$Selecionado)) {
  stop("O modelo salvo pelo Script 02 nao e elegivel/selecionado. Confira o diagnostico.", call. = FALSE)
}
mk_validate_q(q_dimorphism$matrix, names(STATE_LABELS))
expected_q <- mk_q_matrix(evolution$models$best_fit, names(STATE_LABELS))
if (!isTRUE(all.equal(q_dimorphism$matrix, expected_q, tolerance = 1e-12))) {
  stop("A Q salva difere da Q do modelo selecionado. Rode novamente o Script 02.", call. = FALSE)
}
log_message("SIMMAP usando o modelo ", evolution$models$best_model,
  " e a matriz Q salva pelo Script 02.")

console_step(2, 6, "Montando o vetor de estados na mesma ordem da arvore")
# Cada ponta da arvore precisa receber o estado da especie correta.
trait_vector <- setNames(data$Estado, data$species)[tree$tip.label]

# QC antes do SIMMAP -----------------------------------------------------------
if (!identical(as.character(data$species), as.character(tree$tip.label))) {
  stop(
    "01_dados_e_filogenia.rds desalinhado: data$species e tree$tip.label nao coincidem.",
    call. = FALSE
  )
}
if (anyNA(trait_vector)) {
  stop("Existem estados ausentes apos alinhar os dados com a arvore.", call. = FALSE)
}
if (!setequal(rownames(q_dimorphism$matrix), names(STATE_LABELS)) ||
    !setequal(colnames(q_dimorphism$matrix), names(STATE_LABELS))) {
  stop(
    "A matriz Q do Script 02 nao possui exatamente os estados esperados (0, 1 e 2).",
    call. = FALSE
  )
}
console_note("QC pre-SIMMAP OK: arvore, dados, estados e matriz Q estao alinhados.")

console_step(3, 6, "Executando o SIMMAP. Esta etapa pode demorar no modo completo")
console_note(paste0("Numero de simulacoes configurado: ", CONFIG$n_simmap))

# >>> ANALISE ESTATISTICA <<<
# Aqui sao simuladas as historias evolutivas usando a matriz Q selecionada no Script 02.
simmap <- run_simmap(
  tree,
  trait_vector,
  q_dimorphism$matrix,
  names(STATE_LABELS),
  STATE_LABELS,
  "Tres_estados",
  CONFIG$n_simmap
)

# Conferencia da matriz usada para colorir FigS3 e Fig5.
terminal_rows_check <- match(seq_len(ape::Ntip(tree)), tree$edge[, 2])
endpoint_modal <- colnames(simmap$summary$edge_probabilities)[
  max.col(simmap$summary$edge_probabilities, ties.method = "first")
]
terminal_modal <- endpoint_modal[terminal_rows_check]
terminal_observed <- as.character(trait_vector[tree$tip.label])
terminal_mismatch <- which(terminal_modal != terminal_observed)

cat("\n--- CONTROLE DOS RAMOS TERMINAIS ---\n")
cat("Ramos terminais conferidos:", length(terminal_rows_check), "\n")
cat("Diferencas linha terminal vs estado observado:", length(terminal_mismatch), "\n")

if (length(terminal_mismatch) > 0L) {
  print(data.frame(
    species = tree$tip.label[terminal_mismatch],
    observed = terminal_observed[terminal_mismatch],
    reconstructed_endpoint = terminal_modal[terminal_mismatch]
  ))
  stop(
    "Ainda existem ramos terminais inconsistentes; FigS3 e Fig5 nao serao geradas.",
    call. = FALSE
  )
} else {
  cat("OK: todos os ramos terminais terminam com a mesma cor do estado observado.\n")
}

# ------------------------------------------------------------------------------
# RESULTADOS DA ANALISE 03A - PRINT
# ------------------------------------------------------------------------------
# Primeiro mostramos os tres resultados principais no Console.
cat("\n--- ESTADO PROVAVEL DA RAIZ ---\n")
print(simmap$summary$root)
cat("\n--- TRANSICOES ESTIMADAS ---\n")
print(simmap$summary$transitions)
cat("\n--- TEMPO PROPORCIONAL EM CADA ESTADO ---\n")
print(simmap$summary$dwell)

# ------------------------------------------------------------------------------
# TABELAS DA ANALISE 03A
# ------------------------------------------------------------------------------
console_step(4, 6, "Salvando tabelas de raiz, transicoes e tempo nos estados")
write_csv_utf8(
  simmap$summary$root,
  file.path(PATHS$tables_supp, "TableS5. Estado da raiz no SIMMAP.csv")
)
write_csv_utf8(
  simmap$summary$transitions,
  file.path(PATHS$tables_supp, "TableS6. Transicoes do SIMMAP.csv")
)
write_csv_utf8(
  simmap$summary$dwell,
  file.path(PATHS$tables_supp, "TableS7. Tempo nos estados do SIMMAP.csv")
)

# Tabelas reduzidas selecionadas para o texto principal.
write_csv_utf8(
  simmap$summary$root,
  file.path(PATHS$tables_main, "Table2. Estado provavel da raiz.csv")
)
main_transitions <- simmap$summary$transitions %>%
  dplyr::arrange(dplyr::desc(Media)) %>%
  dplyr::slice_head(n = 8)
write_csv_utf8(
  main_transitions,
  file.path(PATHS$tables_main, "Table3. Transicoes principais do SIMMAP.csv")
)

# ==============================================================================
# FIGURA DA ANALISE 03A.1 - FigS3. Reconstrucao filogenetica do dimorfismo
# ==============================================================================
# ESTE E O BLOCO QUE VOCE PODE ALTERAR SE QUISER MUDAR O TAMANHO DE FigS3.
# Alterar estas configuracoes NAO recalcula o SIMMAP.
FIGS3_NOME <- "FigS3. Reconstrucao filogenetica do dimorfismo"
FIGS3_LARGURA <- 10
FIGS3_ALTURA <- 10

plot_phylogeny_summary(
  tree = tree,
  edge_probabilities = simmap$summary$edge_probabilities,
  trait_vector = trait_vector,
  state_labels = STATE_LABELS_FIG,
  palette = PALETTE_DIMORPHISM,
  scheme_name = "tres estados",
  stem = FIGS3_NOME,
  width = FIGS3_LARGURA,
  height = FIGS3_ALTURA
)

# ==============================================================================
# FIGURAS S3B-S3E
# MESMO OBJETO DA FigS3, MUDANDO APENAS O CARATER COLORIDO NAS PONTAS
# ==============================================================================
plot_phylogeny_trait_colored <- function(
    tree,
    trait_vector,
    state_labels,
    palette,
    scheme_name,
    stem,
    width = 10,
    height = 10
) {
  
  # ============================================================================
  # 1. PRESERVA OS NOMES DAS ESPECIES
  # ============================================================================
  
  species_names <- names(trait_vector)
  
  trait_vector <- as.character(trait_vector)
  
  # IMPORTANTE:
  # recoloca os nomes das especies depois da conversao para character
  names(trait_vector) <- species_names
  
  
  # ============================================================================
  # 2. ALINHA O CARATER COM AS PONTAS DA ARVORE
  # ============================================================================
  
  tip_states <- trait_vector[tree$tip.label]
  
  # Confere se o alinhamento funcionou
  if (all(is.na(tip_states))) {
    stop(
      "Nenhuma especie da arvore foi encontrada no vetor do carater. ",
      "Verifique os nomes em tree$tip.label e data$species."
    )
  }
  
  
  # ============================================================================
  # 3. CORES DAS PONTAS
  # ============================================================================
  
  tip_colors <- unname(palette[tip_states])
  
  # NA = cinza claro
  tip_colors[is.na(tip_colors)] <- "#D9D9D9"
  
  
  # ============================================================================
  # 4. IDENTIFICA AS PONTAS DESCENDENTES DE CADA NO
  # ============================================================================
  
  n_tip <- ape::Ntip(tree)
  
  children <- split(
    tree$edge[, 2],
    tree$edge[, 1]
  )
  
  descendant_cache <- new.env(parent = emptyenv())
  
  
  get_descendant_tips <- function(node) {
    
    key <- as.character(node)
    
    if (exists(
      key,
      envir = descendant_cache,
      inherits = FALSE
    )) {
      
      return(
        get(
          key,
          envir = descendant_cache,
          inherits = FALSE
        )
      )
    }
    
    
    if (node <= n_tip) {
      
      result <- node
      
    } else {
      
      kids <- children[[key]]
      
      result <- unlist(
        lapply(
          kids,
          get_descendant_tips
        ),
        use.names = FALSE
      )
    }
    
    
    assign(
      key,
      result,
      envir = descendant_cache
    )
    
    result
  }
  
  
  # ============================================================================
  # 5. CORES DOS RAMOS
  #
  # Cada ramo recebe a cor do estado mais frequente entre suas especies
  # descendentes.
  #
  # Quanto maior a predominancia desse estado, mais intensa fica a cor.
  # ============================================================================
  
  edge_colors <- rep(
    "#D9D9D9",
    nrow(tree$edge)
  )
  
  
  for (i in seq_len(nrow(tree$edge))) {
    
    child_node <- tree$edge[i, 2]
    
    # --------------------------------------------------
    # Se for ramo terminal, usa EXATAMENTE a cor da ponta
    # --------------------------------------------------
    if (child_node <= n_tip) {
      
      tip_state <- tip_states[child_node]
      
      if (!is.na(tip_state) && tip_state %in% names(palette)) {
        edge_colors[i] <- unname(palette[tip_state])
      } else {
        edge_colors[i] <- "#D9D9D9"
      }
      
    } else {
      
      descendant_tips <- get_descendant_tips(child_node)
      
      states <- tip_states[descendant_tips]
      
      states <- states[
        !is.na(states) &
          states %in% names(palette)
      ]
      
      if (length(states) > 0L) {
        
        state_table <- table(states)
        
        modal_state <- names(state_table)[which.max(state_table)]
        
        dominance <- max(state_table) / sum(state_table)
        
        edge_colors[i] <- grDevices::adjustcolor(
          unname(palette[modal_state]),
          alpha.f = 0.35 + 0.65 * dominance
        )
        
      } else {
        edge_colors[i] <- "#D9D9D9"
      }
    }
  }
  
  
  # ============================================================================
  # 6. ESTADOS OBSERVADOS
  # ============================================================================
  
  valid_tip_states <- tip_states[
    !is.na(tip_states) &
      tip_states %in% names(state_labels)
  ]
  
  
  observed_states <- names(state_labels)[
    names(state_labels) %in%
      unique(valid_tip_states)
  ]
  
  
  # Checagem para evitar legenda vazia
  if (length(observed_states) == 0L) {
    
    stop(
      paste0(
        "Nenhum estado valido encontrado para ",
        scheme_name,
        ". Estados encontrados nas pontas: ",
        paste(
          unique(stats::na.omit(tip_states)),
          collapse = ", "
        )
      )
    )
  }
  
  
  # ============================================================================
  # 7. DESENHO DA ARVORE
  # ============================================================================
  
  draw_tree <- function() {
    
    legend_height <- max(1.6, 0.42 * length(observed_states))
    graphics::layout(
      matrix(c(1, 2), ncol = 1),
      heights = c(5.2, legend_height)
    )
    graphics::par(oma = c(0, 0, 0, 0), xpd = NA)
    
    graphics::par(
      mar = c(0.4, 1, 3.4, 1),
      xpd = NA
    )
    
    
    ape::plot.phylo(
      tree,
      type = "fan",
      show.tip.label = FALSE,
      
      # Ramos coloridos
      edge.color = edge_colors,
      
      edge.width = 1.5,
      no.margin = FALSE,
      
      main = scheme_name
    )
    
    
    # --------------------------------------------------------------------------
    # Pontas coloridas
    # --------------------------------------------------------------------------
    
    ape::tiplabels(
      pch = 21,
      bg = tip_colors,
      col = NA,
      cex = 0.24
    )
    
    
    # --------------------------------------------------------------------------
    # Legenda
    # --------------------------------------------------------------------------
    
    graphics::par(mar = c(0.1, 0.1, 0.1, 0.1), xpd = NA)
    graphics::plot.new()
    graphics::legend(
      "center",
      
      legend = unname(
        state_labels[observed_states]
      ),
      
      fill = unname(
        palette[observed_states]
      ),
      
      border = NA,
      bty = "n",
      cex = 0.80,
      title = "State",
      ncol = 1,
      x.intersp = 0.7,
      y.intersp = 0.95
    )
  }
  
  
  # ============================================================================
  # 8. SALVA PNG + PDF
  # ============================================================================
  
  save_base_plot_dual(
    stem,
    width,
    height,
    draw_tree
  )
}

trait_arch <- setNames(
  as.character(data$Arquitetura_Lamina),
  data$species
)

plot_phylogeny_trait_colored(
  tree = tree,
  trait_vector = trait_arch,
  state_labels = ARCHITECTURE_LABELS_FIG,
  palette = architecture_palette,
  scheme_name = "Fertile frond architecture",
  stem = "FigS3B. Fertile frond architecture"
)

# ==============================================================================
# FigS3B - FERTILE FROND ARCHITECTURE
# ==============================================================================

trait_arch <- setNames(
  as.character(data$Arquitetura_Lamina),
  data$species
)

plot_phylogeny_trait_colored(
  tree = tree,
  trait_vector = trait_arch,
  state_labels = ARCHITECTURE_LABELS_FIG,
  palette = architecture_palette,
  scheme_name = "Fertile frond architecture",
  stem = "FigS3B. Fertile frond architecture"
)


# ==============================================================================
# FigS3C - MORPHOLOGICAL VARIATION
# ==============================================================================

trait_variation <- setNames(
  as.character(data$Variacao_Morfologica),
  data$species
)

plot_phylogeny_trait_colored(
  tree = tree,
  trait_vector = trait_variation,
  state_labels = VARIATION_LABELS,
  palette = variation_palette,
  scheme_name = "Morphological variation fertile",
  stem = "FigS3C. Morphological variation fertile"
)


# ==============================================================================
# FigS3D - SIZE RATIO
# ==============================================================================

trait_size <- setNames(
  as.character(data$Relacao_Tamanho),
  data$species
)

plot_phylogeny_trait_colored(
  tree = tree,
  trait_vector = trait_size,
  state_labels = SIZE_LABELS,
  palette = size_palette,
  scheme_name = "Size ratio",
  stem = "FigS3D. Size ratio"
)


# ==============================================================================
# FigS3E - HABIT
# ==============================================================================

trait_habit <- setNames(
  as.character(data$Habito),
  data$species
)

plot_phylogeny_trait_colored(
  tree = tree,
  trait_vector = trait_habit,
  state_labels = HABIT_LABELS_FIG,
  palette = habit_palette,
  scheme_name = "Habit",
  stem = "FigS3E. Habit"
)

console_output(
  "FigS3B, FigS3C, FigS3D e FigS3E geradas."
)

# ==============================================================================
# FIGURA DA ANALISE 03A.2 - Fig5. Filogenia circular com aneis de caracteres
# ==============================================================================
# ESTA E A FIGURA PRINCIPAL associada ao SIMMAP.
# Alteracoes deste bloco mudam SOMENTE a aparencia da Fig5.

FIG5_NOME <- "Fig5. Filogenia circular com aneis de caracteres"
FIG5_LARGURA <- 19.0
FIG5_ALTURA <- 11.2

# ------------------------------------------------------------------------------
# CONTROLE VISUAL DA ARVORE E DOS ANEIS
# ------------------------------------------------------------------------------

# Pequeno espaco branco ENTRE um anel e o seguinte.
FIG5_ESPACO_ENTRE_ANEIS <- 0.006

# Espessura de cada faixa de caracteres.
FIG5_ESPESSURA_ANEIS <- 0.031

# Folga entre as pontas da arvore e o primeiro anel.
# Antes era 1.002; agora os ramos terminais nao ficam grudados no anel.
FIG5_DISTANCIA_DA_ARVORE <- 1.028

# Espessura dos ramos da arvore.
FIG5_ESPESSURA_RAMOS <- 0.95

# Folga entre o anel externo de familias e as linhas que levam aos codigos.
FIG5_FOLGA_LINHAS_FAMILIAS <- 0.012

# Espessura das linhas externas das familias.
FIG5_ESPESSURA_LINHAS_FAMILIAS <- 1.05

# Tamanho dos codigos das familias que aparecem ligados por linhas/setas.
# 0.82 e maior que a versao anterior. Se quiser ainda maior, use 0.90 ou 1.00.
FIG5_TAMANHO_ROTULOS_FAMILIAS <- 1.40

# ------------------------------------------------------------------------------
# GUIA VISUAL DOS ANEIS
# ------------------------------------------------------------------------------
# FALSE = remove as setas/guias dos aneis.
# Mantemos a tabela/painel lateral por familia, que ficou mais limpo e informativo.
FIG5_MOSTRAR_GUIA_ANEIS <- FALSE

# Tamanho dos nomes no guia: Dimorfismo, Arquitetura, Variacao, Tamanho, Habito e Familias.
FIG5_TAMANHO_TEXTO_GUIA_ANEIS <- 0.92

# ------------------------------------------------------------------------------
# PAINEL LATERAL - DIMORFISMO POR FAMILIA
# ------------------------------------------------------------------------------
# Nesta versao retiramos as pizzas dos nos para deixar a arvore mais limpa.
# Ao lado da arvore mostramos a proporcao observada de Mon/Hem/Hol em cada familia.
FIG5_MOSTRAR_PAINEL_FAMILIAS <- FALSE

# Tamanho dos codigos das familias e dos valores n no painel lateral.
# Aumente para 0.80-0.90 se quiser tudo ainda mais visivel.
# Se houver muitas familias, mantenha entre 0.45 e 0.60.
FIG5_TAMANHO_TEXTO_PAINEL_FAMILIAS <- 1.02

# ------------------------------------------------------------------------------
# CHECAGEM DA FUNCAO DA Fig5
# ------------------------------------------------------------------------------
# Esta verificacao evita o erro "argumentos nao utilizados" caso o RStudio ainda
# esteja com uma versao antiga da funcao carregada na memoria.
fig1_argumentos_necessarios <- c(
  "family_label_cex",
  "show_ring_guides",
  "ring_guide_cex",
  "show_family_panel",
  "family_panel_cex"
)
fig1_argumentos_disponiveis <- names(formals(plot_circular_phylogeny_rings))
fig1_argumentos_faltando <- setdiff(
  fig1_argumentos_necessarios,
  fig1_argumentos_disponiveis
)
if (length(fig1_argumentos_faltando) > 0L) {
  stop(
    paste0(
      "A funcao plot_circular_phylogeny_rings carregada no R e antiga. ",
      "Reinicie o R (Session > Restart R) e execute ESTE Script 03 inteiro com Source. ",
      "Argumentos ausentes: ", paste(fig1_argumentos_faltando, collapse = ", ")
    ),
    call. = FALSE
  )
}

console_step(5, 6, "Gerando as figuras da analise SIMMAP")

plot_circular_phylogeny_rings(
  tree = tree,
  edge_probabilities = simmap$summary$edge_probabilities,
  analysis_data = data,
  stem = FIG5_NOME,
  width = FIG5_LARGURA,
  height = FIG5_ALTURA,
  ring_gap_fraction = FIG5_ESPACO_ENTRE_ANEIS,
  ring_width_fraction = FIG5_ESPESSURA_ANEIS,
  ring_start_multiplier = FIG5_DISTANCIA_DA_ARVORE,
  branch_width = FIG5_ESPESSURA_RAMOS,
  family_connector_gap_fraction = FIG5_FOLGA_LINHAS_FAMILIAS,
  family_connector_width = FIG5_ESPESSURA_LINHAS_FAMILIAS,
  family_label_cex = FIG5_TAMANHO_ROTULOS_FAMILIAS,
  show_ring_guides = FIG5_MOSTRAR_GUIA_ANEIS,
  ring_guide_cex = FIG5_TAMANHO_TEXTO_GUIA_ANEIS,
  show_family_panel = FIG5_MOSTRAR_PAINEL_FAMILIAS,
  family_panel_cex = FIG5_TAMANHO_TEXTO_PAINEL_FAMILIAS
)

# ============================================================================
# FUNCAO EXCLUSIVA DA Fig5B - ESQUEMA DE IDENTIFICACAO DOS ANEIS
# ============================================================================
plot_ring_identifier_scheme <- function(stem,
                                        width = 9.2,
                                        height = 5.2,
                                        ring_lwd = 2.5,
                                        number_cex = 1.38,
                                        label_cex = 1.18) {
  
  draw_scheme <- function() {
    graphics::par(mar = c(0.4, 0.4, 0.4, 0.4), xpd = NA)
    graphics::plot.new()
    
    # AUMENTADO para caber os nomes inteiros sem cortar
    graphics::plot.window(
      xlim = c(0, 2.05),
      ylim = c(0, 1),
      asp = 1
    )
    
    # Centro dos aneis um pouco mais para a esquerda
    cx <- 0.34
    cy <- 0.52
    radii <- c(0.105, 0.155, 0.205, 0.255, 0.305, 0.355)
    
    ring_ids <- c("1", "2", "3", "4", "5", "F")
    ring_names <- c(
      "Pattern type",
      "Fertile frond Architecture",
      "Morphological variation fertile",
      "Size ratio",
      "Habit",
      "Families"
    )
    
    theta <- seq(0, 2 * pi, length.out = 500)
    
    # Aneis neutros
    for (r in radii) {
      graphics::lines(
        cx + r * cos(theta),
        cy + r * sin(theta),
        col = "#66727C",
        lwd = ring_lwd
      )
    }
    
    # Numeros dentro dos aneis
    id_angle <- 145 * pi / 180
    radii_mid <- c(
      radii[1] * 0.58,
      (radii[1] + radii[2]) / 2,
      (radii[2] + radii[3]) / 2,
      (radii[3] + radii[4]) / 2,
      (radii[4] + radii[5]) / 2,
      (radii[5] + radii[6]) / 2
    )
    
    for (i in seq_along(radii_mid)) {
      graphics::text(
        x = cx + radii_mid[i] * cos(id_angle),
        y = cy + radii_mid[i] * sin(id_angle),
        labels = ring_ids[i],
        cex = number_cex,
        font = 2,
        col = "#17212B"
      )
    }
    
    # Lista lateral mais afastada e com mais espaco
    y_positions <- seq(0.79, 0.25, length.out = length(ring_ids))
    x_num <- 1.02
    x_label <- 1.16
    
    for (i in seq_along(ring_ids)) {
      graphics::text(
        x = x_num,
        y = y_positions[i],
        labels = ring_ids[i],
        cex = number_cex,
        font = 2,
        adj = c(0.5, 0.5),
        col = "#17212B"
      )
      
      graphics::text(
        x = x_label,
        y = y_positions[i],
        labels = ring_names[i],
        cex = label_cex,
        adj = c(0, 0.5),
        col = "#263238"
      )
    }
  }
  
  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    plot_function = draw_scheme,
    directory = PATHS$figures_supp,
    fit_to_a4 = FALSE
  )
}

# ============================================================================
# FIGURA AUXILIAR DA Fig5 - Esquema sem cor para identificar os aneis
# ============================================================================
# Esta figura e apenas um guia visual para ser colocada junto da Fig5.
# Ela nao mexe na arvore principal: desenha somente aneis neutros e informa,
# com numero + nome, o que cada anel representa.
FIG5B_NOME <- "Fig5B. Esquema de identificacao dos aneis"
FIG5B_LARGURA <- 9.2
FIG5B_ALTURA <- 5.2

plot_ring_identifier_scheme(
  stem = FIG5B_NOME,
  width = FIG5B_LARGURA,
  height = FIG5B_ALTURA
)

# ============================================================================
# FIGURA SEPARADA - PROPORCAO DE DIMORFISMO POR FAMILIA
# ============================================================================
# A pedido, este resumo por familia passa a ser gerado separadamente,
# e nao mais junto da figura da filogenia.
plot_family_dimorphism_summary <- function(analysis_data, stem, width = 10.8, height = 6.6) {
  local_data <- analysis_data
  local_data$family <- as.character(local_data$family)
  local_data$Padrao_Dimorfismo <- as.character(local_data$Padrao_Dimorfismo)
  
  # A figura usa a paleta canonica; apenas as abreviacoes ficam locais.
  dimorphism_palette_local <- PALETTE_DIMORPHISM
  
  make_family_codes_local <- function(family_names) {
    stems <- toupper(sub("ACEAE$", "", family_names, ignore.case = TRUE))
    codes <- substr(stems, 1L, 3L)
    repeat {
      duplicated_codes <- duplicated(codes) | duplicated(codes, fromLast = TRUE)
      if (!any(duplicated_codes)) break
      previous <- codes
      codes[duplicated_codes] <- vapply(
        which(duplicated_codes),
        function(i) {
          width_i <- min(nchar(stems[i]), nchar(codes[i]) + 1L)
          substr(stems[i], 1L, width_i)
        },
        character(1)
      )
      if (identical(previous, codes)) {
        codes <- make.unique(codes, sep = "")
        break
      }
    }
    setNames(codes, family_names)
  }
  
  family_order <- unique(local_data$family)
  family_counts <- table(
    factor(local_data$family, levels = family_order),
    factor(local_data$Padrao_Dimorfismo, levels = c("0", "1", "2"))
  )
  
  family_prop <- sweep(family_counts, 1, pmax(rowSums(family_counts), 1), "/")
  
  family_codes <- make_family_codes_local(family_order)
  
  state_labels_family <- paste0(
    unname(STATE_LABELS[c("0", "1", "2")]),
    " (", c("Mon", "Hem", "Hol"), ")"
  )

  df_plot <- data.frame(
    Familia = rep(unname(family_codes[family_order]), each = 3),
    Estado = factor(
      rep(state_labels_family, times = nrow(family_prop)),
      levels = state_labels_family
    ),
    Proporcao = as.vector(t(family_prop)),
    stringsAsFactors = FALSE
  )
  
  pal <- stats::setNames(
    unname(dimorphism_palette_local[c("0", "1", "2")]),
    state_labels_family
  )
  
  plot <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Familia, y = Proporcao, fill = Estado)) +
    ggplot2::geom_col(width = 0.86, color = NA) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1.00),
      labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      x = "Family",
      y = "Proportion",
      fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 18, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 18, face = "bold"),
      axis.text.x = ggplot2::element_text(size = 11.5, angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 13),
      legend.position = "top",
      legend.text = ggplot2::element_text(size = 13),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
  
  save_plot_dual(plot, stem, width, height, directory = PATHS$figures_main)
}

# Gera o resumo por familia em figura separada.
FIG_PROP_NOME <- "Fig4. Proporcao de dimorfismo por familia"
FIG_PROP_LARGURA <- 10.8
FIG_PROP_ALTURA <- 6.6

plot_family_dimorphism_summary(
  analysis_data = data,
  stem = FIG_PROP_NOME,
  width = FIG_PROP_LARGURA,
  height = FIG_PROP_ALTURA
)

# ============================================================================
# BLOCO DA PRANCHA FINAL REMOVIDO
# ============================================================================
# A pedido, nao montamos mais a prancha combinando A + B + C + D neste script.
# A filogenia e o resumo de proporcoes agora sao gerados em figuras separadas.

# ============================================================================
# FIGURA SEPARADA - FILOGENIAS ASCENDENTES DOS CARACTERES
# ============================================================================
# Esta figura mostra apenas as especies hemidimorfas e holodimorfas
# (codigos 1 e 2 em Padrao_Dimorfismo). Para cada carater, desenhamos a mesma
# filogenia em orientacao ascendente (ramos terminais para cima), com as pontas
# coloridas segundo o estado do carater.
#
# A figura final empilha quatro paineis, um embaixo do outro:
# 1) Arquitetura da lamina
# 2) Variacao morfologica
# 3) Relacao de tamanho
# 4) Habito
# ============================================================================

console_step(7, 7, "Gerando a figura das filogenias ascendentes dos caracteres")

plot_upward_character_phylogenies_legacy <- function(tree, analysis_data, stem,
                                                     width = 16.5, height = 17.5,
                                                     directory = PATHS$figures_supp) {
  # Confere se as quatro paletas compartilhadas estao disponiveis.
  required_palettes <- c(
    "architecture_palette", "variation_palette", "size_palette", "habit_palette"
  )
  missing_palettes <- required_palettes[!vapply(
    required_palettes, exists, logical(1), inherits = TRUE
  )]
  if (length(missing_palettes) > 0L) {
    stop(
      "Paletas ausentes para a FigS4: ", paste(missing_palettes, collapse = ", "),
      ". Execute ESTE Script 03 inteiro com Source.",
      call. = FALSE
    )
  }
  
  # --------------------------------------------------------------------------
  # 1. FILTRA APENAS HEMI + HOLO
  # --------------------------------------------------------------------------
  plot_data <- analysis_data[analysis_data$Padrao_Dimorfismo %in% c(1, 2), , drop = FALSE]
  plot_data$species <- as.character(plot_data$species)
  
  common_species <- intersect(tree$tip.label, plot_data$species)
  if (length(common_species) < 3) {
    stop("Ha poucas especies em comum entre a arvore e o subconjunto hemi/holo.")
  }
  
  tree_sub <- ape::drop.tip(tree, setdiff(tree$tip.label, common_species))
  tree_sub <- ape::ladderize(tree_sub, right = FALSE)
  plot_data <- plot_data[match(tree_sub$tip.label, plot_data$species), , drop = FALSE]
  
  # Rotulos abreviados das especies: G. epiteto
  make_abbrev_species <- function(x) {
    x <- gsub("_", " ", x)
    parts <- strsplit(x, " +")[[1]]
    if (length(parts) >= 2) {
      paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
    } else {
      x
    }
  }
  tip_labels_abbrev <- vapply(tree_sub$tip.label, make_abbrev_species, character(1))
  
  # --------------------------------------------------------------------------
  # 2. INFORMACOES DOS PAINEIS
  # --------------------------------------------------------------------------
  trait_info <- list(
    list(
      var = "Arquitetura_Lamina",
      title = "Fertile frond Architecture",
      labels = ARCHITECTURE_LABELS_FIG,
      palette = architecture_palette
    ),
    list(
      var = "Variacao_Morfologica",
      title = "Morphological variation fertile",
      labels = VARIATION_LABELS,
      palette = variation_palette
    ),
    list(
      var = "Relacao_Tamanho",
      title = "Size ratio",
      labels = SIZE_LABELS,
      palette = size_palette
    ),
    list(
      var = "Habito",
      title = "Habit",
      labels = HABIT_LABELS_FIG,
      palette = habit_palette
    )
  )
  
  # --------------------------------------------------------------------------
  # 3. FUNCOES AUXILIARES
  # --------------------------------------------------------------------------
  edge_matrix <- tree_sub$edge
  n_tip <- ape::Ntip(tree_sub)
  children_of <- split(edge_matrix[, 2], edge_matrix[, 1])
  desc_cache <- new.env(parent = emptyenv())
  
  get_desc_tips <- function(node) {
    key <- as.character(node)
    if (exists(key, envir = desc_cache, inherits = FALSE)) {
      return(get(key, envir = desc_cache, inherits = FALSE))
    }
    if (node <= n_tip) {
      result <- node
    } else {
      kids <- children_of[[key]]
      result <- unlist(lapply(kids, get_desc_tips), use.names = FALSE)
    }
    assign(key, result, envir = desc_cache)
    result
  }
  
  compute_edge_styles <- function(values, palette) {
    values <- as.character(values)
    edge_cols <- rep(grDevices::adjustcolor("#93A1AF", alpha.f = 0.88), nrow(edge_matrix))
    edge_lwd <- rep(1.8, nrow(edge_matrix))
    
    for (i in seq_len(nrow(edge_matrix))) {
      child <- edge_matrix[i, 2]
      desc_tips <- get_desc_tips(child)
      states <- unique(stats::na.omit(values[desc_tips]))
      
      if (child <= n_tip) {
        st <- values[child]
        if (!is.na(st) && st %in% names(palette)) {
          edge_cols[i] <- palette[[st]]
        }
        edge_lwd[i] <- 2.8
      } else if (length(states) == 1 && states %in% names(palette)) {
        edge_cols[i] <- grDevices::adjustcolor(palette[[states]], alpha.f = 0.72)
        edge_lwd[i] <- 2.2
      }
    }
    
    list(col = edge_cols, lwd = edge_lwd)
  }
  
  draw_one_panel <- function(info, panel_index = 1L) {
    values <- as.character(plot_data[[info$var]])
    tip_cols <- unname(info$palette[values])
    tip_cols[is.na(tip_cols)] <- "#D9D9D9"
    edge_styles <- compute_edge_styles(values, info$palette)
    
    ape::plot.phylo(
      tree_sub,
      type = "cladogram",
      direction = "upwards",
      use.edge.length = FALSE,
      show.tip.label = FALSE,
      no.margin = TRUE,
      edge.width = edge_styles$lwd,
      edge.color = edge_styles$col,
      label.offset = 0,
      cex = 0.45
    )
    
    pp <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    tip_x <- pp$xx[seq_len(n_tip)]
    tip_y <- pp$yy[seq_len(n_tip)]
    y_rng <- range(pp$yy, finite = TRUE)
    y_span <- diff(y_rng)
    
    # Barra terminal colorida e ponto de destaque acima de cada ponta.
    seg0 <- 0.014 * y_span
    seg1 <- 0.062 * y_span
    graphics::segments(
      x0 = tip_x,
      y0 = tip_y + seg0,
      x1 = tip_x,
      y1 = tip_y + seg1,
      col = tip_cols,
      lwd = 4.0,
      xpd = NA
    )
    graphics::points(
      x = tip_x,
      y = tip_y + seg1 + 0.010 * y_span,
      pch = 21,
      bg = tip_cols,
      col = "#23313E",
      cex = 0.62,
      lwd = 0.8,
      xpd = NA
    )
    
    # Mostra os nomes apenas no primeiro painel, para evitar repeticao e poluicao.
    if (panel_index == 1L) {
      graphics::text(
        x = tip_x,
        y = tip_y + seg1 + 0.058 * y_span,
        labels = tip_labels_abbrev,
        srt = 90,
        adj = c(0, 0.5),
        cex = 0.46,
        col = "#314252",
        xpd = NA
      )
    }
    
    # Titulo do painel com uma pequena barra colorida a esquerda.
    usr <- graphics::par("usr")
    title_y <- usr[4] - 0.06 * (usr[4] - usr[3])
    graphics::segments(
      x0 = usr[1], x1 = usr[1] + 0.06 * (usr[2] - usr[1]),
      y0 = title_y, y1 = title_y,
      lwd = 6, col = unname(info$palette[[setdiff(names(info$palette), "0")[1]]]), xpd = NA
    )
    graphics::text(
      x = usr[1] + 0.075 * (usr[2] - usr[1]),
      y = title_y,
      labels = info$title,
      adj = c(0, 0.5),
      cex = 1.18,
      font = 2,
      col = "#1F2933",
      xpd = NA
    )
  }
  
  draw_bottom_legends <- function() {
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    
    graphics::text(
      x = 0.01, y = 0.95,
      labels = "Subconjunto mostrado: especies hemidimorfas e holodimorfas | Rotulos abreviados no painel superior",
      adj = c(0, 1), cex = 0.88, col = "#59636E"
    )
    
    x_positions <- c(0.02, 0.28, 0.53, 0.78)
    for (k in seq_along(trait_info)) {
      info <- trait_info[[k]]
      values <- as.character(plot_data[[info$var]])
      states_present <- sort(unique(values))
      states_present <- states_present[states_present %in% names(info$labels)]
      legend_labels <- unname(info$labels[states_present])
      legend_cols <- unname(info$palette[states_present])
      
      graphics::legend(
        x = x_positions[k],
        y = 0.78,
        legend = legend_labels,
        title = info$title,
        pt.bg = legend_cols,
        pch = 21,
        pt.cex = 1.0,
        col = "#22313F",
        bty = "n",
        cex = 0.76,
        y.intersp = 0.95,
        x.intersp = 0.55,
        xjust = 0,
        yjust = 1,
        ncol = if (length(legend_labels) >= 7) 2 else 1,
        text.font = 1,
        title.adj = 0
      )
    }
  }
  
  # --------------------------------------------------------------------------
  # 4. SALVA A FIGURA COMPLETA
  # --------------------------------------------------------------------------
  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    directory = directory,
    fit_to_a4 = FALSE,
    plot_function = function() {
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      
      graphics::layout(matrix(c(1, 2, 3, 4, 5), ncol = 1), heights = c(1.12, 1.00, 1.00, 1.00, 0.75))
      graphics::par(oma = c(0.4, 0.7, 1.2, 0.4), xpd = NA)
      
      for (i in seq_along(trait_info)) {
        if (i == 1) {
          graphics::par(mar = c(0.6, 0.5, 6.4, 0.5), xpd = NA)
        } else {
          graphics::par(mar = c(0.6, 0.5, 2.0, 0.5), xpd = NA)
        }
        draw_one_panel(trait_info[[i]], panel_index = i)
      }
      
      graphics::par(mar = c(0.3, 0.3, 0.3, 0.3), xpd = NA)
      draw_bottom_legends()
      
      graphics::mtext(
        "Filogenias ascendentes dos caracteres em especies hemi- e holodimorfas",
        side = 3,
        outer = TRUE,
        line = 0.1,
        cex = 1.20,
        font = 2
      )
    }
  )
}

# Versao editorial revisada da FigS4. Mantem os textos definidos acima no
# script, mas restaura a composicao A4 aprovada: uma unica filogenia, matriz de
# caracteres, faixa de familias e perfis comparativos alinhados.
plot_upward_character_phylogenies <- function(tree, analysis_data, stem,
                                              width = 11.69, height = 8.27,
                                              directory = PATHS$figures_supp) {
  required_palettes <- c(
    "architecture_palette", "variation_palette", "size_palette", "habit_palette"
  )
  missing_palettes <- required_palettes[!vapply(
    required_palettes, exists, logical(1), inherits = TRUE
  )]
  if (length(missing_palettes) > 0L) {
    stop(
      "Paletas ausentes para a FigS4: ", paste(missing_palettes, collapse = ", "),
      ". Execute ESTE Script 03 inteiro com Source.",
      call. = FALSE
    )
  }
  
  required_columns <- c(
    "species", "family", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho", "Habito"
  )
  missing_columns <- setdiff(required_columns, names(analysis_data))
  if (length(missing_columns) > 0L) {
    stop(
      "Colunas ausentes para a FigS4: ", paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  plot_data <- analysis_data[
    as.character(analysis_data$Padrao_Dimorfismo) %in% c("1", "2"),
    , drop = FALSE
  ]
  plot_data$species <- as.character(plot_data$species)
  plot_data$family <- as.character(plot_data$family)
  
  common_species <- intersect(tree$tip.label, plot_data$species)
  if (length(common_species) < 3L) {
    stop("Ha poucas especies em comum entre a arvore e o subconjunto hemi/holo.")
  }
  
  tree_sub <- ape::drop.tip(tree, setdiff(tree$tip.label, common_species))
  tree_sub <- ape::ladderize(tree_sub, right = FALSE)
  plot_data <- plot_data[match(tree_sub$tip.label, plot_data$species), , drop = FALSE]
  if (anyNA(plot_data$species) || anyNA(plot_data$family)) {
    stop("Falha ao alinhar especies, familias e filogenia na FigS4.")
  }
  
  n_tip <- ape::Ntip(tree_sub)
  n_hemi <- sum(as.character(plot_data$Padrao_Dimorfismo) == "1")
  n_holo <- sum(as.character(plot_data$Padrao_Dimorfismo) == "2")
  tip_labels_scientific <- gsub("_", " ", tree_sub$tip.label, fixed = TRUE)
  
  dimorphism_palette <- PALETTE_DIMORPHISM[c("1", "2")]
  track_info <- list(
    list(
      code = "1", short = "Dimorphism", var = "Padrao_Dimorfismo",
      labels = STATE_LABELS_FIG, palette = dimorphism_palette
    ),
    list(
      code = "2", short = "Architecture", var = "Arquitetura_Lamina",
      labels = ARCHITECTURE_LABELS_FIG, palette = architecture_palette
    ),
    list(
      code = "3", short = "Variation", var = "Variacao_Morfologica",
      labels = VARIATION_LABELS, palette = variation_palette
    ),
    list(
      code = "4", short = "Size", var = "Relacao_Tamanho",
      labels = SIZE_LABELS, palette = size_palette
    ),
    list(
      code = "5", short = "Habit", var = "Habito",
      labels = HABIT_LABELS_FIG, palette = habit_palette
    )
  )
  
  observed_states <- lapply(track_info, function(info) {
    values <- unique(as.character(plot_data[[info$var]]))
    codes <- names(info$labels)
    codes[codes %in% values]
  })
  
  shared_panel_bounds <- c(0.05, 0.94)
  family_bar_colors <- c("#DFE4E8", "#BBC6CD")
  family_order_display <- character(0)
  family_codes_display <- integer(0)
  
  draw_main_panel <- function() {
    tree_depths <- ape::node.depth.edgelength(tree_sub)
    crown_height <- max(tree_depths[seq_len(n_tip)], na.rm = TRUE)
    if (!is.finite(crown_height) || crown_height <= 0) {
      crown_height <- 1
    }
    
    x_padding <- 0.068 * n_tip
    graphics::par(mar = c(0.08, 1.2, 0.06, 0.25), xpd = NA)
    ape::plot.phylo(
      tree_sub,
      type = "phylogram",
      direction = "upwards",
      use.edge.length = TRUE,
      show.tip.label = FALSE,
      no.margin = TRUE,
      edge.color = "#7A8E9F",
      edge.width = 0.82,
      x.lim = c(1 - x_padding, n_tip + x_padding),
      y.lim = c(0, crown_height * 2.10)
    )
    
    pp <- get("last_plot.phylo", envir = .PlotPhyloEnv)
    tip_x <- pp$xx[seq_len(n_tip)]
    tip_y <- pp$yy[seq_len(n_tip)]
    tree_y_span <- max(tip_y, na.rm = TRUE) - min(pp$yy, na.rm = TRUE)
    if (!is.finite(tree_y_span) || tree_y_span <= 0) {
      tree_y_span <- crown_height
    }
    
    tip_spacing <- stats::median(diff(sort(tip_x)))
    if (!is.finite(tip_spacing) || tip_spacing <= 0) {
      tip_spacing <- 1
    }
    tile_width <- 0.82 * tip_spacing
    row_step <- 0.060 * tree_y_span
    tile_height <- 0.35 * row_step
    row_y <- max(tip_y, na.rm = TRUE) + seq_along(track_info) * row_step
    
    for (k in seq_along(track_info)) {
      info <- track_info[[k]]
      values <- as.character(plot_data[[info$var]])
      colors <- unname(info$palette[values])
      colors[is.na(colors)] <- "#D9D9D9"
      graphics::rect(
        xleft = tip_x - tile_width / 2,
        ybottom = row_y[k] - tile_height / 2,
        xright = tip_x + tile_width / 2,
        ytop = row_y[k] + tile_height / 2,
        col = colors,
        border = "white",
        lwd = 0.28
      )
      graphics::text(
        x = min(tip_x) - 0.55 * tip_spacing,
        y = row_y[k],
        labels = info$code,
        adj = c(1, 0.5),
        cex = 0.82,
        font = 2,
        col = "#263746"
      )
    }
    
    label_y <- row_y[length(row_y)] + 0.78 * row_step
    graphics::text(
      x = tip_x,
      y = label_y,
      labels = tip_labels_scientific,
      srt = 90,
      adj = c(0, 0.5),
      cex = 0.70,
      font = 3,
      col = "#263746"
    )
    
    name_height_inches <- max(graphics::strwidth(
      tip_labels_scientific, units = "inches", cex = 0.70, font = 3
    ))
    y_units_per_inch <- diff(graphics::par("usr")[3:4]) / graphics::par("pin")[2]
    family_row_y <- label_y + name_height_inches * y_units_per_inch + 1.15 * row_step
    family_bar_height <- 1.65 * row_step
    
    family_by_tip <- as.character(plot_data$family)
    tip_order <- order(tip_x)
    family_plot_order <- family_by_tip[tip_order]
    family_order_display <<- unique(family_plot_order)
    family_codes_display <<- stats::setNames(
      seq_along(family_order_display), family_order_display
    )
    
    family_runs <- rle(family_plot_order)
    run_end <- cumsum(family_runs$lengths)
    run_start <- run_end - family_runs$lengths + 1L
    for (i in seq_along(family_runs$values)) {
      family_name <- family_runs$values[i]
      family_code <- unname(family_codes_display[family_name])
      indices <- tip_order[run_start[i]:run_end[i]]
      fill_color <- family_bar_colors[(family_code - 1L) %% 2L + 1L]
      graphics::rect(
        xleft = min(tip_x[indices]) - tile_width / 2,
        ybottom = family_row_y - family_bar_height / 2,
        xright = max(tip_x[indices]) + tile_width / 2,
        ytop = family_row_y + family_bar_height / 2,
        col = fill_color,
        border = "white",
        lwd = 0.35
      )
      graphics::text(
        x = mean(range(tip_x[indices])),
        y = family_row_y,
        labels = family_code,
        cex = 0.68,
        srt = 90,
        font = 2,
        col = "#263746"
      )
    }
    
    graphics::text(
      x = min(tip_x) - 0.55 * tip_spacing,
      y = family_row_y,
      labels = "6",
      adj = c(1, 0.5),
      cex = 0.92,
      font = 2,
      col = "#263746"
    )
    
    usr <- graphics::par("usr")
    graphics::text(
      x = min(tip_x),
      y = usr[4] - 0.010 * diff(usr[3:4]),
      labels = "A  Phylogeny and character matrix",
      adj = c(0, 1),
      font = 2,
      cex = 1.14,
      col = "#172633"
    )
  }
  
  trait_profile <- function(info, states) {
    dimorphism <- factor(
      as.character(plot_data$Padrao_Dimorfismo), levels = c("1", "2")
    )
    trait <- factor(as.character(plot_data[[info$var]]), levels = states)
    counts <- table(dimorphism, trait)
    sweep(counts, 1, pmax(rowSums(counts), 1), "/")
  }
  
  compact_labels_for <- function(info, states) {
    overrides <- switch(
      info$var,
      Variacao_Morfologica = c(
        "0" = unname(VARIATION_LABELS["0"]),
        "1" = "Frond shape",
        "2" = "Frond size",
        "3" = "Petiole size",
        "4" = "Frond shape + size",
        "5" = "Frond shape + petiole",
        "6" = "Frond + petiole size",
        "7" = unname(VARIATION_LABELS["7"])
      ),
      Relacao_Tamanho = c(
        "0" = unname(SIZE_LABELS["0"]),
        "1" = "Fertile larger",
        "2" = "Fertile smaller",
        "3" = "Similar length",
        "4" = "Intra-individual"
      ),
      Habito = c(
        "0" = unname(HABIT_LABELS["0"]),
        "1" = "Rupicolous s.l.",
        "2" = "Epiphyte s.l.",
        "3" = "Aquatic s.l.",
        "4" = unname(HABIT_LABELS["4"]),
        "5" = unname(HABIT_LABELS["5"]),
        "6" = "Two or more habits"
      ),
      NULL
    )
    labels <- unname(info$labels[states])
    if (!is.null(overrides)) {
      replacement <- unname(overrides[states])
      labels[!is.na(replacement)] <- replacement[!is.na(replacement)]
    }
    labels
  }
  
  draw_profile_bars <- function(info, states, x, width, title_y, bar_y,
                                height = 0.052) {
    profile <- trait_profile(info, states)
    labels_dimorphism <- c("Hem", "Hol")
    x_start <- x + 0.052
    bar_width <- width - 0.062
    
    graphics::text(
      x, title_y,
      labels = paste0(info$code, "  ", info$short),
      adj = c(0, 0.5), cex = 1.10, font = 2, col = "#22313F"
    )
    for (i in seq_len(nrow(profile))) {
      graphics::text(
        x, bar_y[i], labels = labels_dimorphism[i],
        adj = c(0, 0.5), cex = 0.96, font = 2, col = "#526474"
      )
      cursor <- x_start
      for (j in seq_along(states)) {
        proportion <- profile[i, j]
        if (!is.finite(proportion) || proportion <= 0) {
          next
        }
        segment_width <- bar_width * proportion
        graphics::rect(
          cursor, bar_y[i] - height / 2,
          cursor + segment_width, bar_y[i] + height / 2,
          col = unname(info$palette[states[j]]), border = NA
        )
        cursor <- cursor + segment_width
      }
    }
  }
  
  draw_state_key <- function(info, states, x, width, y, y_step,
                             columns = 2L, cex = 0.91) {
    display_labels <- compact_labels_for(info, states)
    profile <- trait_profile(info, states)
    display_labels <- vapply(seq_along(states), function(j) {
      percentages <- round(100 * profile[, j])
      paste0(display_labels[j], "  ", percentages[1], "/", percentages[2], "%")
    }, character(1))
    
    n_rows <- ceiling(length(states) / columns)
    column_width <- width / columns
    for (j in seq_along(states)) {
      column_j <- (j - 1L) %/% n_rows
      row_j <- (j - 1L) %% n_rows
      xx <- x + column_j * column_width
      yy <- y - row_j * y_step
      graphics::points(
        xx, yy, pch = 22, bg = unname(info$palette[states[j]]),
        col = NA, cex = 1.08
      )
      graphics::text(
        xx + 0.014, yy, labels = display_labels[j],
        adj = c(0, 0.5), cex = cex, col = "#273746"
      )
    }
  }
  
  draw_legends <- function() {
    graphics::par(mar = c(0.18, 1.2, 0.12, 0.25), xpd = NA)
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    
    graphics::text(
      shared_panel_bounds[1], 0.99,
      labels = "B  Comparative profiles and legend of observed states",
      adj = c(0, 1), font = 2, cex = 1.18, col = "#172633"
    )
    graphics::text(
      shared_panel_bounds[2], 0.99,
      labels = "Values in legends: Hem/Hol (%)",
      adj = c(1, 1), cex = 0.82, col = "#5A6670"
    )
    
    dim_info <- track_info[[1]]
    dim_states <- observed_states[[1]]
    shared_width <- diff(shared_panel_bounds)
    graphics::text(
      shared_panel_bounds[1] + 0.27 * shared_width, 0.895,
      labels = paste0(dim_info$code, "  ", dim_info$short),
      adj = c(0, 0.5), font = 2, cex = 1.06, col = "#22313F"
    )
    dim_x <- shared_panel_bounds[1] + c(0.42, 0.69) * shared_width
    for (j in seq_along(dim_states)) {
      graphics::points(
        dim_x[j], 0.895, pch = 22,
        bg = unname(dim_info$palette[dim_states[j]]), col = NA, cex = 1.16
      )
      graphics::text(
        dim_x[j] + 0.016, 0.895,
        labels = paste0(
          unname(dim_info$labels[dim_states[j]]), " (n = ",
          sum(as.character(plot_data$Padrao_Dimorfismo) == dim_states[j]), ")"
        ),
        adj = c(0, 0.5), cex = 0.98, col = "#273746"
      )
    }
    
    panel_gap <- 0.035
    panel_width <- (shared_width - panel_gap) / 2
    panel_x <- c(
      shared_panel_bounds[1],
      shared_panel_bounds[1] + panel_width + panel_gap
    )
    
    draw_profile_bars(
      track_info[[2]], observed_states[[2]], panel_x[1], panel_width,
      title_y = 0.80, bar_y = c(0.73, 0.66)
    )
    draw_profile_bars(
      track_info[[3]], observed_states[[3]], panel_x[2], panel_width,
      title_y = 0.80, bar_y = c(0.73, 0.66)
    )
    draw_state_key(
      track_info[[2]], observed_states[[2]], panel_x[1], panel_width,
      y = 0.56, y_step = 0.06, columns = 2L
    )
    draw_state_key(
      track_info[[3]], observed_states[[3]], panel_x[2], panel_width,
      y = 0.56, y_step = 0.06, columns = 2L
    )
    
    draw_profile_bars(
      track_info[[4]], observed_states[[4]], panel_x[1], panel_width,
      title_y = 0.36, bar_y = c(0.29, 0.22)
    )
    draw_profile_bars(
      track_info[[5]], observed_states[[5]], panel_x[2], panel_width,
      title_y = 0.36, bar_y = c(0.29, 0.22)
    )
    draw_state_key(
      track_info[[4]], observed_states[[4]], panel_x[1], panel_width,
      y = 0.12, y_step = 0.06, columns = 2L
    )
    draw_state_key(
      track_info[[5]], observed_states[[5]], panel_x[2], panel_width,
      y = 0.15, y_step = 0.045, columns = 2L
    )
  }
  
  draw_family_legend <- function() {
    graphics::par(mar = c(0.10, 1.2, 0.08, 0.25), xpd = NA)
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    graphics::text(
      shared_panel_bounds[1], 0.98,
      labels = "C  Band 6: families - numbers positioned above species names",
      adj = c(0, 1), font = 2, cex = 1.08, col = "#172633"
    )
    
    n_columns <- 5L
    n_rows <- ceiling(length(family_order_display) / n_columns)
    x_columns <- seq(shared_panel_bounds[1], 0.82, length.out = n_columns)
    y_top <- 0.70
    y_step <- 0.145
    for (i in seq_along(family_order_display)) {
      column_i <- (i - 1L) %/% n_rows + 1L
      row_i <- (i - 1L) %% n_rows
      xx <- x_columns[column_i]
      yy <- y_top - row_i * y_step
      fill_color <- family_bar_colors[(i - 1L) %% 2L + 1L]
      graphics::points(xx, yy, pch = 22, bg = fill_color, col = NA, cex = 1.10)
      graphics::text(
        xx + 0.014, yy,
        labels = paste0(i, "  ", family_order_display[i]),
        adj = c(0, 0.5), cex = 0.94, col = "#273746"
      )
    }
  }
  
  save_base_plot_dual(
    stem = stem,
    width = width,
    height = height,
    directory = directory,
    fit_to_a4 = FALSE,
    plot_function = function() {
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::layout(
        matrix(1:3, ncol = 1), heights = c(4.60, 3.00, 1.05)
      )
      graphics::par(oma = c(0, 0, 0, 0), xpd = NA)
      draw_main_panel()
      draw_legends()
      draw_family_legend()
    }
  )
}

FIG_UPWARD_TRAITS_NOME <- "FigS4. Filogenias ascendentes dos caracteres em hemi e holodimorfas"
FIG_UPWARD_TRAITS_LARGURA <- 11.69
FIG_UPWARD_TRAITS_ALTURA <- 8.27

plot_upward_character_phylogenies(
  tree = tree,
  analysis_data = data,
  stem = FIG_UPWARD_TRAITS_NOME,
  width = FIG_UPWARD_TRAITS_LARGURA,
  height = FIG_UPWARD_TRAITS_ALTURA,
  directory = PATHS$figures_supp
)

console_output("Nova figura adicional salva: FigS4")

# ------------------------------------------------------------------------------
# OBJETO SALVO DA ANALISE 03A
# ------------------------------------------------------------------------------
console_step(6, 6, "Salvando o objeto SIMMAP para reutilizacao")
save_pipeline_rds(
  list(simmap = simmap, trait_vector = trait_vector,
    selected_model = evolution$models$best_model, Q = q_dimorphism$matrix,
    selection_policy = MK_POLICY_VERSION, input_signature = evolution$input_signature),
  "03_simmap_dimorfismo.rds"
)

console_output("03_simmap_dimorfismo.rds")
console_output("Table2, Table3, TableS5-S7, Fig5, Fig5B, Fig4, FigS3, FigS4 e objetos RDS do SIMMAP")
log_message("SCRIPT 03 concluido.")
console_done("SCRIPT 03", "10_SENSIBILIDADE_FILOGENETICA.R (sequencia V3)")

# ==============================================================================
# FIGURA ADICIONAL - FigS4B. DISTANCIAS FILOGENETICAS DENTRO DOS TIPOS DE DIMORFISMO
# ==============================================================================
# Esta etapa e apenas descritiva. Calcula as distancias filogeneticas entre todos
# os pares de especies pertencentes ao mesmo estado de dimorfismo, normaliza as
# distancias pelo maior valor observado na arvore e compara suas distribuicoes.
# As linhas tracejadas indicam as medianas de cada estado.
# ==============================================================================

plot_within_state_phylogenetic_distances <- function(
    tree,
    analysis_data,
    stem,
    width = 11.69,
    height = 8.27,
    directory = PATHS$figures_supp
) {
  
  required_columns <- c("species", "Estado")
  
  missing_columns <- setdiff(
    required_columns,
    names(analysis_data)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "Colunas ausentes para a figura de distancias filogeneticas: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  # --------------------------------------------------------------------------
  # Alinha o estado de dimorfismo com as pontas da arvore
  # --------------------------------------------------------------------------
  
  state_by_species <- setNames(
    as.character(analysis_data$Estado),
    as.character(analysis_data$species)
  )
  
  tip_states <- state_by_species[
    tree$tip.label
  ]
  
  valid_states <- c("0", "1", "2")
  
  keep <- !is.na(tip_states) &
    tip_states %in% valid_states
  
  if (sum(keep) < 3L) {
    stop(
      "Ha poucas especies com estado de dimorfismo valido para gerar a figura."
    )
  }
  
  tree_plot <- if (all(keep)) {
    
    tree
    
  } else {
    
    ape::drop.tip(
      tree,
      tree$tip.label[!keep]
    )
  }
  
  tip_states <- tip_states[
    tree_plot$tip.label
  ]
  
  
  # --------------------------------------------------------------------------
  # Distancias filogeneticas
  # --------------------------------------------------------------------------
  
  distance_matrix <- ape::cophenetic.phylo(
    tree_plot
  )
  
  max_distance <- max(
    distance_matrix,
    na.rm = TRUE
  )
  
  if (!is.finite(max_distance) ||
      max_distance <= 0) {
    
    stop(
      "Nao foi possivel normalizar as distancias filogeneticas da arvore."
    )
  }
  
  distance_matrix <- distance_matrix /
    max_distance
  
  
  # --------------------------------------------------------------------------
  # Extrai pares de especies pertencentes ao MESMO estado
  # --------------------------------------------------------------------------
  
  distance_rows <- lapply(
    valid_states,
    function(state) {
      
      species_state <- tree_plot$tip.label[
        tip_states == state
      ]
      
      if (length(species_state) < 2L) {
        return(NULL)
      }
      
      local_matrix <- distance_matrix[
        species_state,
        species_state,
        drop = FALSE
      ]
      
      values <- local_matrix[
        upper.tri(local_matrix)
      ]
      
      data.frame(
        Estado = state,
        Distancia = as.numeric(values),
        stringsAsFactors = FALSE
      )
    }
  )
  
  distance_data <- dplyr::bind_rows(
    distance_rows
  )
  
  if (nrow(distance_data) == 0L) {
    stop(
      "Nenhum par de especies do mesmo estado foi encontrado."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Nomes em ingles
  # --------------------------------------------------------------------------
  
  state_names <- STATE_LABELS
  
  
  # --------------------------------------------------------------------------
  # Paleta
  # --------------------------------------------------------------------------
  
  distance_palette <- PALETTE_DIMORPHISM
  
  
  # --------------------------------------------------------------------------
  # Resumo por estado
  # --------------------------------------------------------------------------
  
  summary_rows <- lapply(
    valid_states,
    function(state) {
      
      values <- distance_data$Distancia[
        distance_data$Estado == state
      ]
      
      n_species <- sum(
        tip_states == state,
        na.rm = TRUE
      )
      
      if (length(values) == 0L) {
        return(NULL)
      }
      
      data.frame(
        Estado = state,
        n = n_species,
        pares = length(values),
        mediana = stats::median(
          values,
          na.rm = TRUE
        ),
        stringsAsFactors = FALSE
      )
    }
  )
  
  summary_data <- dplyr::bind_rows(
    summary_rows
  )
  
  
  # --------------------------------------------------------------------------
  # Texto da legenda
  # --------------------------------------------------------------------------
  
  legend_labels <- setNames(
    vapply(
      valid_states,
      function(state) {
        
        row <- summary_data[
          summary_data$Estado == state,
          ,
          drop = FALSE
        ]
        
        if (nrow(row) == 0L) {
          return(
            unname(state_names[state])
          )
        }
        
        paste0(
          unname(state_names[state]),
          "  |  n = ", row$n,
          "  |  pairs = ", row$pares,
          "  |  median = ",
          sprintf("%.2f", row$mediana)
        )
      },
      character(1)
    ),
    valid_states
  )
  
  
  distance_data$Estado <- factor(
    distance_data$Estado,
    levels = valid_states
  )
  
  summary_data$Estado <- factor(
    summary_data$Estado,
    levels = valid_states
  )
  
  
  # --------------------------------------------------------------------------
  # Grafico
  # --------------------------------------------------------------------------
  
  plot <- ggplot2::ggplot(
    distance_data,
    ggplot2::aes(
      x = Distancia,
      colour = Estado,
      fill = Estado
    )
  ) +
    
    ggplot2::geom_density(
      alpha = 0.16,
      linewidth = 1.05,
      adjust = 0.85,
      na.rm = TRUE
    ) +
    
    ggplot2::geom_vline(
      data = summary_data,
      ggplot2::aes(
        xintercept = mediana,
        colour = Estado
      ),
      linewidth = 0.70,
      linetype = "dashed",
      show.legend = FALSE
    ) +
    
    ggplot2::scale_colour_manual(
      values = distance_palette,
      breaks = valid_states,
      labels = legend_labels,
      drop = FALSE,
      name = NULL
    ) +
    
    ggplot2::scale_fill_manual(
      values = distance_palette,
      breaks = valid_states,
      labels = legend_labels,
      drop = FALSE,
      guide = "none"
    ) +
    
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(
        0,
        1,
        by = 0.2
      ),
      labels = scales::label_number(
        accuracy = 0.1
      ),
      expand = ggplot2::expansion(
        mult = c(0, 0)
      )
    ) +
    
    ggplot2::labs(
      x = "Normalized phylogenetic distance",
      y = "Density",
      caption = paste0(
        "Each curve has area = 1 and compares distribution shape, not the number of species. ",
        "Descriptive analysis: species pairs are not independent."
      )
    ) +
    
    ggplot2::theme_minimal(
      base_size = 15
    ) +
    
    ggplot2::theme(
      
      panel.grid.minor =
        ggplot2::element_blank(),
      
      panel.grid.major.x =
        ggplot2::element_blank(),
      
      axis.title =
        ggplot2::element_text(
          size = 16
        ),
      
      axis.text =
        ggplot2::element_text(
          size = 12
        ),
      
      legend.position =
        c(0.03, 0.95),
      
      legend.justification =
        c(0, 1),
      
      legend.text =
        ggplot2::element_text(
          size = 12.5
        ),
      
      legend.background =
        ggplot2::element_rect(
          fill = grDevices::adjustcolor(
            "white",
            alpha.f = 0.82
          ),
          colour = NA
        ),
      
      plot.caption =
        ggplot2::element_text(
          size = 10.5,
          hjust = 0.5,
          colour = "#65727E",
          margin = ggplot2::margin(
            t = 10
          )
        ),
      
      plot.margin =
        ggplot2::margin(
          12,
          14,
          10,
          12
        )
    )
  
  
  # --------------------------------------------------------------------------
  # Salva PNG + PDF
  # --------------------------------------------------------------------------
  
  save_plot_dual(
    plot,
    stem,
    width,
    height,
    directory = directory
  )
  
  
  invisible(
    list(
      distances = distance_data,
      summary = summary_data,
      plot = plot
    )
  )
}


# ==============================================================================
# GERAR A FIGURA
# ==============================================================================

FIG_PHYLO_DISTANCE_NOME <-
  "FigS4B. Distancias filogeneticas dentro dos tipos de dimorfismo"

FIG_PHYLO_DISTANCE_LARGURA <- 11.69
FIG_PHYLO_DISTANCE_ALTURA <- 8.27


phylogenetic_distance_figure <-
  plot_within_state_phylogenetic_distances(
    
    tree = tree,
    
    analysis_data = data,
    
    stem = FIG_PHYLO_DISTANCE_NOME,
    
    width = FIG_PHYLO_DISTANCE_LARGURA,
    
    height = FIG_PHYLO_DISTANCE_ALTURA,
    
    directory = PATHS$figures_supp
  )


console_output(
  "Nova figura adicional salva: FigS4B"
)

# ============================================================================== 
# ANALISE COMPLEMENTAR - ROTAS DE ENTRADA EM HOLODIMORFIA
# ============================================================================== 
# Esta etapa NAO recalcula o SIMMAP. Ela usa os mesmos mapas gerados acima para
# responder uma pergunta mais especifica:
#
# Quando uma linhagem ENTRA no estado Holodimorphic, qual era o estado imediatamente
# anterior no ramo?
#   - Monomorphic -> Holodimorphic  = entrada direta
#   - Hemidimorphic -> Holodimorphic = entrada a partir de Hemidimorphic
#
# Esta formulacao e mais rigorosa do que afirmar automaticamente que toda
# transicao 1 -> 2 representa uma sequencia completa 0 -> 1 -> 2, porque uma
# historia SIMMAP pode conter reversoes. Portanto, a tabela descreve o
# PRECURSOR IMEDIATO das entradas em Holodimorphic.
# ============================================================================== 

summarize_holodimorphy_entry_routes <- function(simmap_result) {
  maps <- ensure_multisimmap(simmap_result$maps)
  states <- c("0", "1", "2")
  
  per_map <- dplyr::bind_rows(lapply(seq_along(maps), function(i) {
    counted <- phytools::countSimmap(
      maps[[i]],
      states = states,
      message = FALSE
    )
    Tr <- counted$Tr[states, states, drop = FALSE]
    
    direct <- as.numeric(Tr["0", "2"])
    from_hemi <- as.numeric(Tr["1", "2"])
    total <- direct + from_hemi
    
    data.frame(
      SIMMAP = i,
      Monomorphic_para_Holodimorphic = direct,
      Hemidimorphic_para_Holodimorphic = from_hemi,
      Total_entradas_Holodimorphic = total,
      stringsAsFactors = FALSE
    )
  }))
  
  route_values <- list(
    "Monomorphic -> Holodimorphic" = per_map$Monomorphic_para_Holodimorphic,
    "Hemidimorphic -> Holodimorphic" = per_map$Hemidimorphic_para_Holodimorphic
  )
  
  total_mean <- mean(per_map$Total_entradas_Holodimorphic)
  
  summary_rows <- lapply(names(route_values), function(route) {
    x <- route_values[[route]]
    global_fraction <- if (is.finite(total_mean) && total_mean > 0) {
      mean(x) / total_mean
    } else {
      NA_real_
    }
    
    data.frame(
      Rota = route,
      Media_transicoes = mean(x),
      Mediana_transicoes = stats::median(x),
      IC95_inferior = unname(stats::quantile(x, 0.025, names = FALSE)),
      IC95_superior = unname(stats::quantile(x, 0.975, names = FALSE)),
      Probabilidade_de_ocorrer = mean(x > 0),
      Fracao_media_das_entradas_em_Holodimorphic = global_fraction,
      N_SIMMAP = length(x),
      stringsAsFactors = FALSE
    )
  })
  
  summary_table <- dplyr::bind_rows(summary_rows)
  
  # Complemento: em quantos mapas cada rota teve mais eventos que a outra?
  summary_table$Probabilidade_de_ser_rota_mais_frequente <- c(
    mean(
      per_map$Monomorphic_para_Holodimorphic >
        per_map$Hemidimorphic_para_Holodimorphic
    ),
    mean(
      per_map$Hemidimorphic_para_Holodimorphic >
        per_map$Monomorphic_para_Holodimorphic
    )
  )
  
  list(summary = summary_table, per_map = per_map)
}

holodimorphy_routes <- summarize_holodimorphy_entry_routes(simmap)

cat("\n--- ROTAS DE ENTRADA EM HOLODIMORFIA ---\n")
print(holodimorphy_routes$summary)

write_csv_utf8(
  holodimorphy_routes$summary,
  file.path(
    PATHS$tables_supp,
    "TableS28. Rotas de entrada em holodimorfia.csv"
  )
)

saveRDS(
  holodimorphy_routes,
  windows_safe_path(
    file.path(PATHS$objects, "03_rotas_entrada_holodimorfia.rds")
  )
)

console_output("TableS28 e 03_rotas_entrada_holodimorfia.rds")
log_message("Analise complementar das rotas de entrada em holodimorfia concluida.")
