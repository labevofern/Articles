# ==============================================================================
# 07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R
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
#   Os modelos usam a mesma arvore completa do Script 01, incluindo as especies
#   monomorficas como estado 0. A FigS4, gerada no Script 03, continua sendo uma
#   visualizacao recortada apenas para as especies dimorficas.
#
# QUANDO TERMINAR:
#   Depois, rode 08_CORRELACOES_E_SINAL_MORFOLOGICO.R.
# ------------------------------------------------------------------------------
# PERGUNTA:
# Como a monomorfia e as diferentes solucoes morfologicas das frondes ferteis se
# transformam ao longo da filogenia completa?
#
# CARACTERES:
# Arquitetura da lamina fertil; variacao morfologica comparativa; e relacao de
# tamanho total.
#
# ANALISES:
# Modelos Mk + matriz Q + SIMMAP para cada caracter.
#
# PARA QUE SERVE:
# Estima como cada componente morfologico se transforma na mesma arvore que
# inclui especies monomorficas, hemidimorficas e holodimorficas.
#
# O QUE OLHAR:
# Frequencia dos estados, AICc, Delta AICc, pesos de Akaike, matrizes Q,
# transicoes SIMMAP, tempo nos estados e estados raros.
#
# DEPENDENCIAS:
# 01_PREPARAR_DADOS_E_FILOGENIA.R
#
# OBJETO SALVO:
# Scripts_/_Objetos_R/resultados_centrais_morfologia.rds
#
# TABELAS GERADAS NESTE SCRIPT:
# Table5. Modelos das solucoes morfologicas.csv
# TableS12. Frequencias dos estados morfologicos.csv
# TableS13. Comparacao dos modelos morfologicos.csv
# TableS14. Matrizes Q dos caracteres morfologicos.csv            
# TableS15_Transicoes_SIMMAP.csv
# TableS16. Tempo nos estados morfologicos.csv
# TableS17. Estado da raiz das solucoes morfologicas.csv
# TableS18_Estados_Raros.csv
# TableS27_Origens_Independentes.csv
#
# FIGURAS GERADAS NESTE SCRIPT:
# FigS6. Frequencias dos estados morfologicos
# FigS7. Comparacao dos modelos morfologicos
# FigS8. Rede de transicoes da arquitetura
# FigS9. Rede de transicoes da variacao morfologica
# FigS10. Rede de transicoes da relacao de tamanho
# FigS11. Matriz Q da arquitetura
# FigS12. Matriz Q da variacao morfologica
# FigS13. Matriz Q da relacao de tamanho
# FigS24. Independent origins of morphological solutions
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
required_packages <- c("ape", "corHMM", "dplyr", "ggplot2", "openxlsx", "phytools", "scales", "tidyr", "viridisLite")
check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# CONFIGURACAO E FUNCOES EXCLUSIVAS DO SCRIPT 07
# ============================================================================== 
# Todo o codigo abaixo ate "FUNCOES ESPECIFICAS DESTA ANALISE" pertence apenas
# aos modelos morfologicos: arvore completa, Mk, matriz Q,
# SIMMAP, resumos, workbook e utilidades graficas desta etapa.
# Os caminhos desta analise partem diretamente dos caminhos gerais do projeto.
MORPH_PROJECT_DIR <- project_io_dir

# A morfologia usa exatamente a mesma estrutura do restante do artigo.
# Tabelas publicas sao gravadas diretamente em Tabelas_ e figuras em Imagens_.
# Objetos e logs permanecem em Scripts_ porque nao fazem parte do manuscrito.
MORPH_ROOT <- MORPH_PROJECT_DIR
MORPH_PATHS <- list(
  root = MORPH_ROOT,
  logs = PATHS$logs,
  data = PATHS$data,
  models = PATHS$models,
  tables = PATHS$tables_supp,
  figures = PATHS$figures_supp,
  objects = PATHS$objects,
  processed_data = file.path(
    PATHS$data,
    "dados_processados_alinhados.csv"
  ),
  processed_tree = file.path(
    PATHS$data,
    "arvore_ftol_subset_ultrametrica.tre"
  )
)

for (p in MORPH_PATHS[c("root", "logs", "data", "models", "tables", "figures", "objects")]) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

MORPH_LOG <- file.path(MORPH_PATHS$logs, "execucao_morfologia.log")

# ------------------------------------------------------------------------------
# 2. CONFIGURACAO
# ------------------------------------------------------------------------------

MORPH_QUICK <- is_true(Sys.getenv("LEAF_DIMORPHISM_QUICK", "false"))
MORPH_REUSE <- is_true(Sys.getenv("LEAF_DIMORPHISM_REUSE_MORPH_MODELS", "false"))

# Estes parametros controlam APENAS Mk + SIMMAP dos tres caracteres morfologicos.
MORPH_CONFIG <- list(
  seed = 20260804L,
  quick_mode = MORPH_QUICK,
  reuse_models = MORPH_REUSE,
  n_starts = if (MORPH_QUICK) 0L else 100L,
  n_simmap = if (MORPH_QUICK) 10L else 500L,
  n_cores = 1L,
  lower_rate_bound = 1e-10,
  upper_rate_bound = 100,
  overwrite_outputs = TRUE
)

set.seed(MORPH_CONFIG$seed)

# ------------------------------------------------------------------------------
# 3. ROTULOS DOS CARACTERES
# ------------------------------------------------------------------------------

MORPH_LABELS <- list(
  Arquitetura_Lamina = ARCHITECTURE_LABELS,
  Variacao_Morfologica = VARIATION_LABELS,
  Relacao_Tamanho = SIZE_LABELS
)

MORPH_TRAITS <- c("Arquitetura_Lamina", "Variacao_Morfologica", "Relacao_Tamanho")

# ------------------------------------------------------------------------------
# 4. LOG E EXPORTACAO LOCAL
# ------------------------------------------------------------------------------


morph_log <- function(..., level = "INFO") {
  txt <- paste(..., collapse = "")
  line <- sprintf("[%s] [%s] %s",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, txt)
  cat(line, "\n")
  cat(
    line,
    "\n",
    file = windows_safe_path(MORPH_LOG),
    append = TRUE
  )
}

morph_write_csv <- function(x, filename) {
  path <- file.path(MORPH_PATHS$tables, filename)
  write_csv_utf8(x, path)
  invisible(path)
}

morph_save_plot <- function(plot, stem, width = 10, height = 7,
                            directory = MORPH_PATHS$figures) {
  dimensions <- fit_a4_dimensions(width, height)
  width <- unname(dimensions["width"])
  height <- unname(dimensions["height"])
  
  # Mostra primeiro no painel Plots e depois salva em PDF e PNG de 300 dpi.
  preview_ggplot(plot)
  
  ggplot2::ggsave(
    windows_safe_path(
      file.path(directory, paste0(stem, ".pdf"))
    ),
    plot = plot, width = width, height = height, units = "in",
    device = grDevices::cairo_pdf, bg = "white"
  )
  save_ggplot_png(
    plot = plot,
    path = file.path(directory, paste0(stem, ".png")),
    width = width,
    height = height,
    dpi = 300
  )
}

morph_save_base_plot <- function(stem, width = 10, height = 10, plot_fun,
                                 directory = MORPH_PATHS$figures) {
  dimensions <- fit_a4_dimensions(width, height)
  width <- unname(dimensions["width"])
  height <- unname(dimensions["height"])
  
  if (interactive()) plot_fun()
  
  grDevices::pdf(
    windows_safe_path(
      file.path(directory, paste0(stem, ".pdf"))
    ),
    width = width, height = height, useDingbats = FALSE
  )
  plot_fun()
  grDevices::dev.off()
  
  grDevices::png(
    windows_safe_path(
      file.path(directory, paste0(stem, ".png"))
    ),
    width = width, height = height, units = "in", res = 300, bg = "white"
  )
  plot_fun()
  grDevices::dev.off()
}

# ------------------------------------------------------------------------------
# 6. LEITURA DOS DADOS PROCESSADOS
# ------------------------------------------------------------------------------

# Carrega dados/arvore processados para os modulos morfologicos.
# Mantemos essa leitura centralizada para garantir que 07 e 08 usem a mesma entrada.
morph_load_processed <- function() {
  prepared01 <- require_saved_rds(
    "01_dados_e_filogenia.rds",
    "01_PREPARAR_DADOS_E_FILOGENIA.R"
  )

  dat <- prepared01$data
  tr <- prepared01$tree

  required <- c(
    "species", "Padrao_Dimorfismo", "Arquitetura_Lamina",
    "Variacao_Morfologica", "Relacao_Tamanho"
  )
  missing <- setdiff(required, names(dat))
  if (length(missing) > 0L) {
    stop(
      "Colunas ausentes no objeto oficial do Script 01: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  for (v in c("Padrao_Dimorfismo", MORPH_TRAITS)) {
    dat[[v]] <- suppressWarnings(as.integer(dat[[v]]))
  }

  dat <- dat[match(tr$tip.label, dat$species), , drop = FALSE]
  rownames(dat) <- dat$species

  if (!identical(as.character(tr$tip.label), as.character(dat$species))) {
    stop("Falha ao alinhar dados e arvore recebidos do Script 01.", call. = FALSE)
  }

  list(
    data = dat,
    tree = tr,
    source_signature = list(
      tip.label = as.character(tr$tip.label),
      edge = unclass(tr$edge),
      edge.length = as.numeric(tr$edge.length),
      Nnode = as.integer(tr$Nnode),
      species = as.character(dat$species),
      dimorphism = as.integer(dat$Padrao_Dimorfismo)
    )
  )
}

# ------------------------------------------------------------------------------
# 7. DADOS MORFOLOGICOS NA ARVORE COMPLETA
# ------------------------------------------------------------------------------

morph_prepare_full_tree <- function(data, tree) {
  analysis_data <- data[match(tree$tip.label, data$species), , drop = FALSE]
  rownames(analysis_data) <- analysis_data$species
  
  if (!identical(tree$tip.label, analysis_data$species)) {
    stop("Falha ao alinhar os dados morfologicos com a arvore completa.", call. = FALSE)
  }
  
  # Controle essencial para usar 0 como estado biologico no modelo:
  #   - toda especie monomorfica deve ter codigo 0;
  #   - nenhuma especie dimorfica deve ter codigo 0.
  # O script interrompe a analise se essa correspondencia for violada.
  qc_coding <- dplyr::bind_rows(lapply(MORPH_TRAITS, function(trait) {
    monomorphic <- analysis_data$Padrao_Dimorfismo == 0L
    dimorphic <- analysis_data$Padrao_Dimorfismo > 0L
    observed <- !is.na(analysis_data[[trait]])
    monomorphic_nonzero <- monomorphic & observed & analysis_data[[trait]] != 0L
    dimorphic_zero <- dimorphic & observed & analysis_data[[trait]] == 0L
    data.frame(
      Caracter = trait,
      N_total = nrow(analysis_data),
      N_monomorficas = sum(monomorphic, na.rm = TRUE),
      N_dimorficas = sum(dimorphic, na.rm = TRUE),
      N_ausentes = sum(!observed),
      N_monomorficas_codigo_nao_zero = sum(monomorphic_nonzero, na.rm = TRUE),
      N_dimorficas_codigo_zero = sum(dimorphic_zero, na.rm = TRUE),
      Codificacao_consistente = !any(monomorphic_nonzero | dimorphic_zero, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  
  write_csv_utf8(
    qc_coding,
    file.path(MORPH_PATHS$data, "QC_codificacao_monomorficas_morfologia.csv")
  )
  
  if (any(!qc_coding$Codificacao_consistente)) {
    stop(
      "A codificacao morfologica nao coincide com Padrao_Dimorfismo. ",
      "Confira QC_codificacao_monomorficas_morfologia.csv antes de modelar.",
      call. = FALSE
    )
  }
  
  write_csv_utf8(
    analysis_data,
    file.path(MORPH_PATHS$data, "dados_analise_morfologica_arvore_completa.csv")
  )
  
  list(data = analysis_data, tree = tree, qc_coding = qc_coding)
}

# ------------------------------------------------------------------------------
# 8. FUNCOES PARA MODELOS Mk
# ------------------------------------------------------------------------------

morph_safe_corhmm <- function(tree, trait_data, model_name) {
  warnings_found <- character(0)
  
  fit <- tryCatch(
    withCallingHandlers(
      corHMM::corHMM(
        phy = tree,
        data = trait_data,
        rate.cat = 1L,
        model = model_name,
        node.states = "none",
        root.p = "yang",
        nstarts = MORPH_CONFIG$n_starts,
        n.cores = MORPH_CONFIG$n_cores,
        lower.bound = MORPH_CONFIG$lower_rate_bound,
        upper.bound = MORPH_CONFIG$upper_rate_bound
      ),
      warning = function(w) {
        warnings_found <<- c(warnings_found, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(list(fit = NULL, error = conditionMessage(fit),
                warnings = unique(warnings_found)))
  }
  
  list(fit = fit, error = NA_character_, warnings = unique(warnings_found))
}

# Versao generica dos modelos Mk para cada carater morfologico.
morph_fit_model_set <- function(tree, data, trait) {
  # O estado 0 (Monomorphic) e mantido explicitamente.
  keep <- !is.na(data[[trait]])
  d <- data[keep, c("species", trait), drop = FALSE]
  names(d) <- c("species", "state")

  missing_monomorphic <- data$species[
    data$Padrao_Dimorfismo == 0L & !data$species %in% d$species
  ]
  if (length(missing_monomorphic) > 0L) {
    stop(
      "O caracter ", trait, " excluiria especies monomorficas por dado ausente: ",
      paste(missing_monomorphic, collapse = ", "),
      call. = FALSE
    )
  }

  tr <- ape::keep.tip(tree, d$species)
  d <- d[match(tr$tip.label, d$species), , drop = FALSE]
  d$state <- as.integer(d$state)

  if (!identical(as.character(tr$tip.label), as.character(d$species))) {
    stop("Falha ao alinhar arvore e dados para ", trait, ".", call. = FALSE)
  }

  state_counts <- as.data.frame(table(d$state), stringsAsFactors = FALSE)
  names(state_counts) <- c("Estado", "N")
  state_counts$Estado <- as.character(state_counts$Estado)
  state_counts$Estado_nome <- unname(MORPH_LABELS[[trait]][state_counts$Estado])
  state_counts$Caracter <- trait

  if (length(unique(d$state)) < 2L) {
    stop("O caracter ", trait, " possui menos de dois estados observados.", call. = FALSE)
  }

  cache_path <- file.path(MORPH_PATHS$objects, paste0("modelos_", trait, ".rds"))
  cache_signature <- list(
    policy = MK_POLICY_VERSION,
    trait = trait,
    tip.label = as.character(tr$tip.label),
    edge = unclass(tr$edge),
    edge.length = as.numeric(tr$edge.length),
    Nnode = as.integer(tr$Nnode),
    species = as.character(d$species),
    states = as.integer(d$state),
    n_starts = MORPH_CONFIG$n_starts,
    lower = MORPH_CONFIG$lower_rate_bound,
    upper = MORPH_CONFIG$upper_rate_bound,
    corHMM = as.character(utils::packageVersion("corHMM"))
  )

  if (MORPH_CONFIG$reuse_models && file.exists(cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cached) && identical(cached$signature, cache_signature)) {
      morph_log("Reutilizando modelos morfologicos em cache para ", trait, ".")
      return(cached$result)
    }
    morph_log(
      "Cache ignorado para ", trait,
      " porque arvore, dados, configuracao ou politica de selecao mudaram.",
      level = "AVISO"
    )
  }

  models <- c("ER", "SYM", "ARD")
  fits <- list()
  rows <- list()

  for (m in models) {
    morph_log("Ajustando ", trait, " - modelo ", m, "...")
    ans <- morph_safe_corhmm(tr, d, m)
    fits[[m]] <- ans

    row <- mk_model_row(
      ans,
      group = trait,
      model = m,
      lower = MORPH_CONFIG$lower_rate_bound,
      upper = MORPH_CONFIG$upper_rate_bound,
      language = "pt"
    )
    row$Caracter <- row$Esquema
    row$Esquema <- NULL
    rows[[m]] <- row
  }

  comparison <- dplyr::bind_rows(rows)
  comparison <- mk_select(comparison, "Modelo", "Peso_Akaike")
  comparison$Selecionado_reconstrucao <- comparison$Selecionado

  best <- comparison$Modelo[comparison$Selecionado]
  if (length(best) != 1L) {
    stop("Nao foi possivel identificar um unico melhor modelo para ", trait, ".", call. = FALSE)
  }
  best_fit <- fits[[best]]$fit

  result <- list(
    tree = tr,
    data = d,
    state_counts = state_counts,
    fits = fits,
    comparison = comparison,
    best_model = best,
    best_fit = best_fit
  )

  saveRDS(
    list(signature = cache_signature, result = result),
    windows_safe_path(cache_path),
    compress = "xz"
  )

  result
}

morph_extract_q <- function(fit, trait, state_codes = NULL) {
  if (is.null(state_codes)) {
    stop("state_codes deve ser informado para extrair a matriz Q com seguranca.", call. = FALSE)
  }
  state_codes <- as.character(state_codes)

  # Usa a funcao compartilhada do Script 00, que recupera explicitamente a
  # correspondencia entre os estados originais e os indices internos do corHMM.
  Q <- mk_q_matrix(fit, state_codes)

  states <- rownames(Q)
  labels <- MORPH_LABELS[[trait]]
  long <- expand.grid(
    Origem = states,
    Destino = states,
    stringsAsFactors = FALSE
  )
  long$Taxa_por_Ma <- mapply(function(a, b) Q[a, b], long$Origem, long$Destino)
  long <- long[long$Origem != long$Destino, , drop = FALSE]
  long$Origem_nome <- unname(labels[long$Origem])
  long$Destino_nome <- unname(labels[long$Destino])
  long$Caracter <- trait
  long <- long[, c(
    "Caracter", "Origem", "Origem_nome", "Destino",
    "Destino_nome", "Taxa_por_Ma"
  )]

  list(matrix = Q, tidy = long)
}

# ------------------------------------------------------------------------------
# 9. SIMMAP E RESUMO DAS HISTORIAS
# ------------------------------------------------------------------------------

morph_ensure_multisimmap <- function(maps) {
  if (inherits(maps, "simmap") && !inherits(maps, "multiSimmap")) {
    maps <- list(maps)
    class(maps) <- c("multiSimmap", "multiPhylo")
  }
  maps
}

morph_mapped_edge_complete <- function(simmap_tree, states) {
  out <- matrix(
    0, nrow = nrow(simmap_tree$edge), ncol = length(states),
    dimnames = list(NULL, states)
  )
  present <- intersect(colnames(simmap_tree$mapped.edge), states)
  out[, present] <- simmap_tree$mapped.edge[, present, drop = FALSE]
  out
}

morph_summarize_simmap <- function(maps, tree, trait, labels) {
  maps <- morph_ensure_multisimmap(maps)
  states <- colnames(maps[[1]]$mapped.edge)
  n_maps <- length(maps)
  
  trans_array <- array(
    0, dim = c(n_maps, length(states), length(states)),
    dimnames = list(NULL, states, states)
  )
  dwell <- matrix(
    0, nrow = n_maps, ncol = length(states),
    dimnames = list(NULL, states)
  )
  mapped <- vector("list", n_maps)
  
  for (i in seq_along(maps)) {
    counted <- phytools::countSimmap(maps[[i]], states = states, message = FALSE)
    trans_array[i, , ] <- counted$Tr[states, states, drop = FALSE]
    mapped[[i]] <- morph_mapped_edge_complete(maps[[i]], states)
    dwell[i, ] <- colSums(mapped[[i]])
  }
  
  trans_rows <- list()
  z <- 1L
  for (a in states) {
    for (b in states) {
      if (a == b) next
      x <- trans_array[, a, b]
      trans_rows[[z]] <- data.frame(
        Caracter = trait,
        Origem = a,
        Origem_nome = unname(labels[a]),
        Destino = b,
        Destino_nome = unname(labels[b]),
        Media = mean(x),
        Mediana = stats::median(x),
        IC95_inferior = unname(stats::quantile(x, 0.025, names = FALSE)),
        IC95_superior = unname(stats::quantile(x, 0.975, names = FALSE))
      )
      z <- z + 1L
    }
  }
  
  total_len <- sum(tree$edge.length)
  dwell_summary <- dplyr::bind_rows(lapply(states, function(s) {
    x <- dwell[, s] / total_len
    data.frame(
      Caracter = trait,
      Estado = s,
      Estado_nome = unname(labels[s]),
      Proporcao_media = mean(x),
      Proporcao_mediana = stats::median(x),
      IC95_inferior = unname(stats::quantile(x, 0.025, names = FALSE)),
      IC95_superior = unname(stats::quantile(x, 0.975, names = FALSE))
    )
  }))
  
  desc <- phytools::describe.simmap(maps, plot = FALSE)
  root_probs <- simmap_root_probabilities(desc, maps, tree, states)
  root <- data.frame(
    Caracter = trait,
    Estado = states,
    Estado_nome = unname(labels[states]),
    Probabilidade_raiz = root_probs,
    Nota = "Raiz da arvore completa, incluindo o estado monomorfico (0)."
  )
  
  edge_time <- Reduce(`+`, mapped) / n_maps
  edge_prob <- edge_time / rowSums(edge_time)
  
  list(
    transitions = dplyr::bind_rows(trans_rows),
    dwell = dwell_summary,
    root = root,
    edge_probabilities = edge_prob,
    description = desc
  )
}

# SIMMAP dos caracteres morfologicos na arvore completa.
morph_run_simmap <- function(model_result, q_result, trait) {
  tr <- model_result$tree
  d <- model_result$data
  Q <- q_result$matrix
  
  x <- setNames(as.character(d$state), d$species)[tr$tip.label]
  states <- rownames(Q)
  
  # Garante que Q e vetor possuam os mesmos estados.
  observed <- sort(unique(as.character(x)))
  if (!all(observed %in% states)) {
    stop("Estados do vetor e da matriz Q nao coincidem para ", trait, ".", call. = FALSE)
  }
  
  morph_log(
    "Gerando ", MORPH_CONFIG$n_simmap,
    " mapas estocasticos para ", trait, "..."
  )
  
  maps <- phytools::make.simmap(
    tree = tr,
    x = x,
    model = "ARD",
    nsim = MORPH_CONFIG$n_simmap,
    Q = Q,
    pi = "estimated",
    message = FALSE
  )
  maps <- morph_ensure_multisimmap(maps)
  
  summary <- morph_summarize_simmap(
    maps, tr, trait, MORPH_LABELS[[trait]]
  )
  
  saveRDS(
    maps,
    windows_safe_path(
      file.path(MORPH_PATHS$objects, paste0("simmap_", trait, ".rds"))
    ),
    compress = "xz"
  )
  saveRDS(
    summary,
    windows_safe_path(
      file.path(MORPH_PATHS$objects, paste0("simmap_resumo_", trait, ".rds"))
    )
  )
  
  list(maps = maps, summary = summary)
}


# ------------------------------------------------------------------------------
# 9B. ORIGENS INDEPENDENTES DAS SOLUCOES MORFOLOGICAS
# ------------------------------------------------------------------------------
# Para cada historia SIMMAP, o numero de origens de um estado e calculado como:
#   - todas as transicoes que ENTRAM naquele estado a partir de outro estado;
#   - +1 quando aquele estado ja esta presente na raiz da arvore daquele mapa.
#
# Isso permite responder quantas vezes uma mesma solucao morfologica apareceu
# independentemente na arvore completa. O estado 0 participa das historias como
# monomorfia, mas nao e contado como uma "solucao morfologica" na TableS27.
# ------------------------------------------------------------------------------
morph_root_state_from_map <- function(simmap_tree) {
  root_node <- ape::Ntip(simmap_tree) + 1L
  root_edges <- which(simmap_tree$edge[, 1] == root_node)
  if (length(root_edges) == 0L) return(NA_character_)
  
  root_states <- vapply(
    simmap_tree$maps[root_edges],
    function(segments) {
      nm <- names(segments)
      if (length(nm) == 0L) return(NA_character_)
      nm[1L]
    },
    character(1)
  )
  root_states <- root_states[!is.na(root_states)]
  if (length(root_states) == 0L) return(NA_character_)
  
  # Em um SIMMAP consistente os ramos que saem da raiz compartilham o mesmo
  # estado ancestral. Se houver discrepancia numerica rara, usamos o modal.
  tab <- sort(table(root_states), decreasing = TRUE)
  names(tab)[1L]
}

morph_independent_origins <- function(simmap_result, trait, labels) {
  maps <- morph_ensure_multisimmap(simmap_result$maps)
  
  states <- sort(unique(unlist(lapply(
    maps,
    function(m) colnames(m$mapped.edge)
  ))))
  all_states <- states[!is.na(states)]
  states <- setdiff(all_states, "0")
  
  if (length(states) == 0L) {
    return(data.frame())
  }
  
  origin_matrix <- matrix(
    0,
    nrow = length(maps),
    ncol = length(states),
    dimnames = list(NULL, states)
  )
  
  for (i in seq_along(maps)) {
    counted <- phytools::countSimmap(
      maps[[i]],
      states = all_states,
      message = FALSE
    )
    Tr <- counted$Tr[all_states, all_states, drop = FALSE]
    root_state <- morph_root_state_from_map(maps[[i]])
    
    for (state in states) {
      other_states <- setdiff(all_states, state)
      n_entries <- if (length(other_states) == 0L) {
        0
      } else {
        sum(Tr[other_states, state, drop = FALSE])
      }
      origin_matrix[i, state] <- n_entries + as.integer(identical(root_state, state))
    }
  }
  
  rows <- lapply(states, function(state) {
    x <- origin_matrix[, state]
    data.frame(
      Caracter = trait,
      Estado = state,
      Estado_nome = unname(labels[state]),
      Media_origens = mean(x),
      Mediana_origens = stats::median(x),
      IC95_inferior = unname(stats::quantile(x, 0.025, names = FALSE)),
      IC95_superior = unname(stats::quantile(x, 0.975, names = FALSE)),
      Minimo = min(x),
      Maximo = max(x),
      Probabilidade_2_ou_mais_origens = mean(x >= 2L),
      N_SIMMAP = length(x),
      stringsAsFactors = FALSE
    )
  })
  
  dplyr::bind_rows(rows)
}

plot_independent_origins_morph <- function(
    origins_table,
    stem = "FigS24. Independent origins of morphological solutions",
    width = 12.5,
    height = 10.5) {
  
  d <- origins_table[
    is.finite(origins_table$Media_origens) &
      is.finite(origins_table$IC95_inferior) &
      is.finite(origins_table$IC95_superior) &
      is.finite(origins_table$Probabilidade_2_ou_mais_origens),
    , drop = FALSE
  ]
  
  if (nrow(d) == 0L) {
    morph_log("FigS24 nao gerada: tabela de origens vazia.", level = "AVISO")
    return(invisible(NULL))
  }
  
  trait_names <- c(
    Arquitetura_Lamina = "Fertile-frond architecture",
    Variacao_Morfologica = "Morphological variation",
    Relacao_Tamanho = "Size ratio"
  )
  d$Caracter_nome <- unname(trait_names[d$Caracter])
  d$Estado_nome <- factor(d$Estado_nome, levels = rev(unique(d$Estado_nome)))
  
  # A FigS24 continua mostrando media + IC95%, mas agora tambem mostra a
  # probabilidade de cada estado ter pelo menos duas origens independentes.
  # Essa probabilidade ja e calculada na TableS27; nao e um novo teste.
  d$Rotulo_figura <- paste0(
    "Mean = ", sprintf("%.2f", d$Media_origens),
    "   |   >=2 origins = ",
    sprintf("%.0f%%", 100 * d$Probabilidade_2_ou_mais_origens)
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = Media_origens, y = Estado_nome)
  ) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.55,
      color = "#8A949E"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = IC95_inferior,
        xend = IC95_superior,
        y = Estado_nome,
        yend = Estado_nome
      ),
      linewidth = 0.85,
      color = "#5B6770",
      lineend = "round"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = Probabilidade_2_ou_mais_origens),
      shape = 21,
      fill = "white",
      color = "#263238",
      stroke = 0.9
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = IC95_superior,
        label = Rotulo_figura
      ),
      hjust = -0.10,
      size = 3.15,
      color = "#263238",
      family = "sans",
      show.legend = FALSE
    ) +
    ggplot2::facet_wrap(
      ~Caracter_nome,
      scales = "free_y",
      ncol = 1
    ) +
    ggplot2::scale_size_continuous(
      range = c(2.8, 5.2),
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      name = "Probability of >=2\nindependent origins"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.05, 0.42))
    ) +
    ggplot2::labs(
      x = "Estimated number of independent origins (mean and 95% interval)",
      y = NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 12.5) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      axis.text.y = ggplot2::element_text(size = 10.5),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9.5),
      plot.margin = ggplot2::margin(10, 190, 10, 10)
    )
  
  morph_save_plot(
    p,
    stem = stem,
    width = width,
    height = height,
    directory = MORPH_PATHS$figures
  )
  
  invisible(p)
}

# ------------------------------------------------------------------------------
# 10. COMPONENTES BINARIOS DA VARIACAO MORFOLOGICA

morph_palette <- function(n, option = "D") {
  viridisLite::viridis(n, option = option, begin = 0.05, end = 0.95)
}

morph_trait_label <- function(x) {
  dplyr::recode(
    x,
    Arquitetura_Lamina = "Fertile-frond architecture",
    Variacao_Morfologica = "Morphological variation",
    Relacao_Tamanho = "Size ratio"
  )
}

morph_pretty_binary <- function(x) {
  map <- c(
    "Mudanca_forma" = "Change in frond shape",
    "Mudanca_lamina" = "Change in lamina size",
    "Mudanca_peciolo" = "Change in petiole size",
    "Holodimorfia" = unname(STATE_LABELS["2"])
  )
  out <- as.character(x)
  idx <- out %in% names(map)
  out[idx] <- unname(map[out[idx]])
  out
}

# ------------------------------------------------------------------------------
# 14. WORKBOOK MORFOLOGICO
# ------------------------------------------------------------------------------

morph_create_workbook <- function(tables, output_name) {
  wb <- openxlsx::createWorkbook(creator = "Pipeline morfologia evolutiva")
  used <- character(0)
  
  for (nm in names(tables)) {
    x <- tables[[nm]]
    if (is.null(x) || !is.data.frame(x) || ncol(x) == 0L) next
    
    safe <- gsub("[\\\\/:*?\\[\\]]", "_", nm)
    safe <- substr(safe, 1, 31)
    if (safe %in% used) safe <- substr(paste0(safe, "_2"), 1, 31)
    used <- c(used, safe)
    
    openxlsx::addWorksheet(wb, safe, gridLines = FALSE)
    openxlsx::writeDataTable(
      wb, safe, x,
      tableStyle = "TableStyleMedium2",
      withFilter = TRUE
    )
    openxlsx::freezePane(wb, safe, firstActiveRow = 2)
    openxlsx::setColWidths(wb, safe, cols = seq_len(ncol(x)), widths = "auto")
  }
  
  path <- file.path(MORPH_PATHS$tables, output_name)
  openxlsx::saveWorkbook(
    wb,
    windows_safe_path(path),
    overwrite = TRUE
  )
  path
}



# ==============================================================================
# FUNCAO DA ANALISE 6 - Mk + MATRIZ Q + SIMMAP DOS CARACTERES MORFOLOGICOS
# ==============================================================================
# LEIA ASSIM:
#   - melhor modelo = estrutura de taxas mais apoiada para aquele caracter;
#   - matriz Q = quais mudancas tem maiores/menores taxas;
#   - SIMMAP = quantas vezes as mudancas provavelmente ocorreram e por quanto
#     tempo a arvore permaneceu em cada estado.
# ==============================================================================


# ==============================================================================
# FUNCAO DA ANALISE 7 - PAGEL + SINAL FILOGENETICO DA MORFOLOGIA
# ==============================================================================
# TESTE DE PAGEL:
#   Modelo independente: X e Y mudam sem depender um do outro.
#   Modelo dependente: a taxa de mudanca de X pode depender do estado de Y e
#   vice-versa. Melhor suporte ao dependente sugere evolucao correlacionada.
#
# ESTATISTICA D:
#   Avalia se caracteres binarios estao agrupados na filogenia.
# ==============================================================================


# ==============================================================================
# FUNCOES DE VISUALIZACAO DA MORFOLOGIA EVOLUTIVA
# ==============================================================================
# Todas as figuras sao apenas representacoes das analises; nao constituem
# testes estatisticos adicionais. O diagrama aluvial e DESCRITIVO e nao deve ser
# interpretado como uma sequencia temporal de transformacoes.
# ==============================================================================

# ------------------------------------------------------------------------------
# FUNCAO AUXILIAR: TEMA DO MANUSCRITO
# ------------------------------------------------------------------------------

theme_morph <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(color = "#4B5563"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "top"
    )
}

# ------------------------------------------------------------------------------
# VISUALIZAR E SALVAR A ANALISE Mk + MATRIZ Q + SIMMAP
# ------------------------------------------------------------------------------

# ==============================================================================


# ==============================================================================
# FUNCOES ESPECIFICAS DESTA ANALISE
# Estas funcoes ficam aqui para que a estrutura de tabelas/figuras desta etapa
# seja visivel no proprio script e nao fique escondida no script 00.
# ==============================================================================

# FUNCAO CENTRAL DESTA ETAPA.
# Ela executa a analise evolutiva separadamente para Arquitetura, Variacao Morfologica e Relacao de Tamanho.


# ==============================================================================
# FUNCOES DE FIGURA DESTA ANALISE
# ==============================================================================
# As funcoes abaixo NAO ajustam modelos. Elas apenas desenham resultados que ja
# foram calculados. Por isso, mudar titulo/tamanho/cores aqui nao muda a analise.

plot_morph_frequency <- function(freq, stem, width = 11, height = 11) {
  d <- freq
  d$Caracter_nome <- vapply(d$Caracter, morph_trait_label, character(1))
  d$Estado_nome <- ifelse(is.na(d$Estado_nome), paste("State", d$Estado), d$Estado_nome)
  d$Estado_nome_plot <- factor(
    d$Estado_nome,
    levels = unique(d$Estado_nome[order(d$Proporcao)])
  )
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = Proporcao, y = Estado_nome_plot, fill = Caracter_nome)
  ) +
    ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("n = ", N)),
      hjust = -0.08,
      size = 3.1
    ) +
    ggplot2::facet_wrap(~Caracter_nome, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::scale_fill_manual(
      values = setNames(
        morph_palette(length(unique(d$Caracter_nome)), option = "D"),
        unique(d$Caracter_nome)
      )
    ) +
    ggplot2::labs(
      title = "Morphological states across the complete phylogeny",
      subtitle = "Monomorphic species are retained as state 0 in the evolutionary models",
      x = "Proportion of all sampled species",
      y = NULL
    ) +
    theme_morph()
  
  morph_save_plot(p, stem, width, height)
}

plot_morph_model_comparison <- function(cmp, stem, width = 10, height = 9) {
  d <- cmp[is.finite(cmp$AICc), , drop = FALSE]
  d$Caracter_nome <- vapply(d$Caracter, morph_trait_label, character(1))
  
  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = Peso_Akaike, y = Modelo, fill = Caracter_nome)
  ) +
    ggplot2::geom_col(width = 0.68, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          "AICc = ", sprintf("%.1f", AICc),
          ifelse(Selecionado_reconstrucao, "  *", "")
        )
      ),
      hjust = -0.04,
      size = 3.1
    ) +
    ggplot2::facet_wrap(~Caracter_nome, ncol = 1) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.18),
      labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(
      values = setNames(
        morph_palette(length(unique(d$Caracter_nome)), option = "C"),
        unique(d$Caracter_nome)
      )
    ) +
    ggplot2::labs(
      title = "Support for evolutionary models of morphological solutions",
      subtitle = "ER = equal rates; SYM = symmetric rates; ARD = all rates different; * model used in SIMMAP",
      x = "Akaike weight",
      y = NULL
    ) +
    theme_morph()
  
  morph_save_plot(p, stem, width, height)
}

plot_transition_network_morph <- function(result, trait, stem, width = 9, height = 9) {
  Q <- result$Q$matrix
  states <- rownames(Q)
  labels <- MORPH_LABELS[[trait]][states]
  count_tab <- table(as.character(result$model$data$state))
  counts <- as.numeric(count_tab[states]); counts[is.na(counts)] <- 0
  n <- length(states)
  angles <- seq(pi / 2, pi / 2 - 2 * pi, length.out = n + 1L)[seq_len(n)]
  palette <- setNames(morph_palette(n, option = "D"), states)
  
  nodes <- data.frame(
    state = states,
    label = ifelse(is.na(labels), states, unname(labels)),
    x = cos(angles), y = sin(angles), n = counts,
    color = unname(palette[states]), stringsAsFactors = FALSE
  )
  edges <- expand.grid(from = states, to = states, stringsAsFactors = FALSE)
  edges$rate <- mapply(function(a, b) Q[a, b], edges$from, edges$to)
  edges <- edges[edges$from != edges$to & is.finite(edges$rate) & edges$rate > 0, , drop = FALSE]
  idx <- setNames(seq_along(states), states)
  edges$grupo_curva <- ifelse(idx[edges$from] < idx[edges$to], "A", "B")
  edges <- dplyr::left_join(edges, nodes[, c("state", "x", "y")], by = c("from" = "state"))
  target <- nodes[, c("state", "x", "y")]; names(target) <- c("to", "xend", "yend")
  edges <- dplyr::left_join(edges, target, by = "to")
  
  p <- ggplot2::ggplot()
  if (any(edges$grupo_curva == "A")) {
    p <- p + ggplot2::geom_curve(
      data = edges[edges$grupo_curva == "A", ],
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linewidth = rate, alpha = rate),
      curvature = 0.18, color = "#374151",
      arrow = grid::arrow(length = grid::unit(0.14, "inches"), type = "closed"),
      lineend = "round", show.legend = FALSE
    )
  }
  if (any(edges$grupo_curva == "B")) {
    p <- p + ggplot2::geom_curve(
      data = edges[edges$grupo_curva == "B", ],
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linewidth = rate, alpha = rate),
      curvature = -0.18, color = "#374151",
      arrow = grid::arrow(length = grid::unit(0.14, "inches"), type = "closed"),
      lineend = "round", show.legend = FALSE
    )
  }
  p <- p +
    ggplot2::geom_point(
      data = nodes, ggplot2::aes(x = x, y = y, size = n, fill = color),
      shape = 21, color = "#111827", stroke = 0.9, show.legend = FALSE
    ) +
    ggplot2::geom_label(
      data = nodes,
      ggplot2::aes(x = x, y = y, label = paste0(label, "\nn=", n)),
      size = if (n > 6) 2.7 else 3.1, lineheight = 0.9,
      fill = scales::alpha("white", 0.72), linewidth = 0, fontface = "bold"
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_size_continuous(range = c(12, 27)) +
    ggplot2::scale_linewidth_continuous(range = c(0.45, 4.5)) +
    ggplot2::scale_alpha_continuous(range = c(0.28, 0.95)) +
    ggplot2::coord_equal(xlim = c(-1.55, 1.55), ylim = c(-1.5, 1.5), clip = "off") +
    ggplot2::labs(
      title = paste("Transition rates -", morph_trait_label(trait)),
      subtitle = paste("Best model:", result$model$best_model, "| arrow width is proportional to the estimated rate")
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "#4B5563"),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )
  
  morph_save_plot(p, stem, width, height)
}

format_rate_morph_final <- function(x) {
  x <- as.numeric(x)
  ifelse(!is.finite(x), "",
    ifelse(x <= 1e-8, "≈0",
      ifelse(x < 1e-4, formatC(x, digits = 1, format = "e"),
        ifelse(x < 0.01, formatC(x, digits = 4, format = "f"), formatC(x, digits = 3, format = "f")))))
}

format_rate_legend_morph_final <- function(x) {
  x <- as.numeric(x)
  ifelse(!is.finite(x), "",
    ifelse(x <= 1e-8, "0",
      ifelse(x < 0.001, formatC(x, digits = 1, format = "e"), formatC(x, digits = 3, format = "fg"))))
}

# No maximo tres marcas na barra de cores para evitar sobreposicao dos rotulos.
rate_breaks_3_morph_final <- function(limits) {
  limits <- as.numeric(limits)
  limits <- limits[is.finite(limits)]
  if (length(limits) < 2L) return(NULL)
  lo <- min(limits)
  hi <- max(limits)
  if (!is.finite(hi) || hi <= 0) return(NULL)

  b <- pretty(c(max(0, lo), hi), n = 4)
  b <- b[is.finite(b) & b > 0 & b <= hi * (1 + 1e-10)]
  if (length(b) <= 3L) return(b)

  idx <- unique(round(seq(1, length(b), length.out = 3)))
  b[idx]
}

plot_q_heatmap_morph <- function(result, trait, stem, width = 11, height = 8) {
  d <- result$Q$tidy
  d$Origem_nome <- ifelse(is.na(d$Origem_nome), d$Origem, d$Origem_nome)
  d$Destino_nome <- ifelse(is.na(d$Destino_nome), d$Destino, d$Destino_nome)
  best_model <- as.character(result$model$best_model)
  rates <- d$Taxa_por_Ma[is.finite(d$Taxa_por_Ma) & d$Taxa_por_Ma > 0]
  unique_rates <- unique(signif(rates, 10))
  is_equal_rate <- length(unique_rates) <= 1L

  if (identical(best_model, "ER")) {
    shared_rate <- if (length(rates)) rates[1] else NA_real_
    subtitle_text <- paste0("ER model: one shared transition rate for all state changes (q = ",
                            formatC(shared_rate, digits = 7, format = "fg"), " per Ma)")
    p <- ggplot2::ggplot(d, ggplot2::aes(x = Destino_nome, y = Origem_nome)) +
      ggplot2::geom_tile(fill = "#C94478", color = "white", linewidth = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(Taxa_por_Ma > 0, format_rate_morph_final(Taxa_por_Ma), "0")), size = 3.0)
  } else {
    subtitle_text <- if (identical(best_model, "SYM")) {
      "SYM model: reciprocal transitions share rates within state pairs"
    } else if (identical(best_model, "ARD")) {
      "ARD model: each directional state change has its own estimated rate"
    } else paste("Transition rates; model", best_model)

    p <- ggplot2::ggplot(d, ggplot2::aes(x = Destino_nome, y = Origem_nome, fill = Taxa_por_Ma)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(Taxa_por_Ma > 0, format_rate_morph_final(Taxa_por_Ma), "0")), size = 3.0) +
      ggplot2::scale_fill_viridis_c(
        option = "C", trans = "sqrt", name = "Rate per Ma",
        breaks = rate_breaks_3_morph_final, labels = format_rate_legend_morph_final,
        guide = ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5,
                                        barwidth = grid::unit(4.8, "cm"), barheight = grid::unit(0.34, "cm"), ticks = TRUE)
      )
  }

  p <- p +
    ggplot2::labs(title = paste("Q matrix -", morph_trait_label(trait)), subtitle = subtitle_text,
                  x = "Destination state", y = "Origin state") +
    theme_morph() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
                   panel.grid = ggplot2::element_blank(), legend.position = "top",
                   legend.direction = "horizontal", legend.title = ggplot2::element_text(size = 9.5, face = "bold"),
                   legend.text = ggplot2::element_text(size = 8.5), legend.margin = ggplot2::margin(b = 4))

  if (!identical(best_model, "ER") && is_equal_rate && length(rates) > 1L) {
    morph_log("All off-diagonal Q rates are equal for ", trait,
              " even though the selected model is ", best_model,
              ". Inspect model convergence and the Q matrix.", level = "AVISO")
  }
  morph_save_plot(p, stem, width, height)
  invisible(p)
}

plot_tree_reconstruction_morph <- function(result, trait, stem, width = 10, height = 10) {
  tr <- result$model$tree
  edge_prob <- result$simmap$summary$edge_probabilities
  states <- colnames(edge_prob)
  pal <- setNames(morph_palette(length(states), option = "D"), states)
  dominant_idx <- max.col(edge_prob, ties.method = "first")
  dominant_state <- states[dominant_idx]
  confidence <- edge_prob[cbind(seq_len(nrow(edge_prob)), dominant_idx)]
  edge_col <- mapply(
    function(cl, cf) grDevices::adjustcolor(cl, alpha.f = 0.20 + 0.80 * cf),
    unname(pal[dominant_state]), confidence, USE.NAMES = FALSE
  )
  trait_vector <- setNames(as.character(result$model$data$state), result$model$data$species)
  tip_col <- unname(pal[trait_vector[tr$tip.label]])
  
  draw <- function() {
    graphics::par(mar = c(1, 1, 3.2, 1), xpd = NA)
    ape::plot.phylo(
      tr, type = "fan", show.tip.label = FALSE,
      edge.color = edge_col, edge.width = 0.7,
      main = paste("Reconstrucao:", morph_trait_label(trait))
    )
    ape::tiplabels(pch = 21, bg = tip_col, col = NA, cex = 0.24)
    leg_labels <- MORPH_LABELS[[trait]][states]
    leg_labels <- ifelse(is.na(leg_labels), states, leg_labels)
    graphics::legend(
      "topleft", legend = leg_labels, fill = unname(pal[states]), border = NA,
      bty = "n", cex = if (length(states) > 6) 0.65 else 0.75, title = "Estado"
    )
    graphics::mtext(
      "Cor do ramo = estado modal entre SIMMAPs; transparencia = incerteza",
      side = 1, line = -1, cex = 0.65, col = "#4B5563"
    )
  }
  morph_save_base_plot(stem, width, height, draw)
}

analisar_caracter_morfologico <- function(morph_tree, morph_data, trait) {
  model <- morph_fit_model_set(morph_tree, morph_data, trait)
  observed_states <- sort(unique(as.character(model$data$state)))
  q <- morph_extract_q(model$best_fit, trait, state_codes = observed_states)
  
  offdiag_rates <- q$tidy$Taxa_por_Ma[
    is.finite(q$tidy$Taxa_por_Ma) & q$tidy$Taxa_por_Ma > 0
  ]
  n_unique_rates <- length(unique(signif(offdiag_rates, 10)))
  
  if (identical(model$best_model, "ER")) {
    morph_log(
      trait, ": model ER selected. The Q matrix is expected to have one shared ",
      "off-diagonal transition rate; equal values are correct by model definition."
    )
  } else if (n_unique_rates <= 1L && length(offdiag_rates) > 1L) {
    morph_log(
      trait, ": selected model = ", model$best_model,
      ", but Q has only one unique positive off-diagonal rate. Check convergence.",
      level = "AVISO"
    )
  }
  
  simmap <- morph_run_simmap(model, q, trait)
  list(model = model, Q = q, simmap = simmap)
}

# ==============================================================================
# EXECUCAO DA ANALISE 07
# ==============================================================================
console_banner("SCRIPT 07", "Evolucao das solucoes morfologicas")
console_note("Os modelos usam a arvore completa; monomorficas entram como estado 0.")
if (file.exists(MORPH_LOG)) file.remove(MORPH_LOG)

# ------------------------------------------------------------------------------
# PREPARACAO DA ANALISE 07
# ------------------------------------------------------------------------------
console_step(1, 8, "Carregando dados na mesma arvore completa do Script 01")
loaded <- morph_load_processed()
prepared <- morph_prepare_full_tree(loaded$data, loaded$tree)
morph_data <- prepared$data
morph_tree <- prepared$tree
source_signature01 <- loaded$source_signature

if (!identical(as.character(morph_tree$tip.label), as.character(morph_data$species))) {
  stop("A amostra morfologica final nao coincide com a arvore recebida do Script 01.", call. = FALSE)
}

# ==============================================================================
# ANALISE 07A - FREQUENCIAS DOS ESTADOS MORFOLOGICOS
# ==============================================================================
console_step(2, 8, "Contando os estados morfologicos")
frequency_tables <- lapply(MORPH_TRAITS, function(trait) {
  values <- morph_data[[trait]]
  values <- values[!is.na(values)]
  x <- as.data.frame(table(values), stringsAsFactors = FALSE)
  names(x) <- c("Estado", "N")
  x$Estado <- as.character(x$Estado)
  x$Estado_nome <- unname(MORPH_LABELS[[trait]][x$Estado])
  x$Caracter <- trait
  x$Proporcao <- x$N / sum(x$N)
  x[, c("Caracter", "Estado", "Estado_nome", "N", "Proporcao")]
})
frequency_table <- dplyr::bind_rows(frequency_tables)
cat("\n--- FREQUENCIAS DOS ESTADOS MORFOLOGICOS ---\n")
print(frequency_table)
morph_write_csv(frequency_table, "TableS12. Frequencias dos estados morfologicos.csv")

# FIGURA DA ANALISE 07A - FigS6
FIGS6_NOME <- "FigS6. Frequencies of morphological states"
FIGS6_LARGURA <- 11
FIGS6_ALTURA <- 11
plot_morph_frequency(frequency_table, FIGS6_NOME, FIGS6_LARGURA, FIGS6_ALTURA)

# ==============================================================================
# ANALISE 07B - ARQUITETURA DA LAMINA
# ==============================================================================
console_step(3, 8, "Arquitetura: Mk -> Q -> SIMMAP -> figuras")
res_arch <- analisar_caracter_morfologico(morph_tree, morph_data, "Arquitetura_Lamina")
cat("\n--- ARQUITETURA: COMPARACAO DE MODELOS ---\n"); print(res_arch$model$comparison)
cat("\n--- ARQUITETURA: MATRIZ Q ---\n"); print(res_arch$Q$tidy)
cat("\n--- ARQUITETURA: TRANSICOES SIMMAP ---\n"); print(res_arch$simmap$summary$transitions)

# FIGURAS DA ANALISE 07B - arquitetura
# Ajuste nome/tamanho aqui se quiser modificar somente as figuras de arquitetura.
FIGS8_NOME <- "FigS8. Rede de transicoes da arquitetura"
FIGS8_LARGURA <- 9
FIGS8_ALTURA <- 9
FIGS11_NOME <- "FigS11. Matriz Q da arquitetura"
FIGS11_LARGURA <- 11
FIGS11_ALTURA <- 8
plot_transition_network_morph(res_arch, "Arquitetura_Lamina", FIGS8_NOME, FIGS8_LARGURA, FIGS8_ALTURA)
plot_q_heatmap_morph(res_arch, "Arquitetura_Lamina", FIGS11_NOME, FIGS11_LARGURA, FIGS11_ALTURA)

# ==============================================================================
# ANALISE 07C - VARIACAO MORFOLOGICA COMPARATIVA
# ==============================================================================
console_step(4, 8, "Variacao morfologica: Mk -> Q -> SIMMAP -> figuras")
res_var <- analisar_caracter_morfologico(morph_tree, morph_data, "Variacao_Morfologica")
cat("\n--- VARIACAO: COMPARACAO DE MODELOS ---\n"); print(res_var$model$comparison)
cat("\n--- VARIACAO: MATRIZ Q ---\n"); print(res_var$Q$tidy)
cat("\n--- VARIACAO: TRANSICOES SIMMAP ---\n"); print(res_var$simmap$summary$transitions)

# FIGURAS DA ANALISE 07C - variacao morfologica
FIGS9_NOME <- "FigS9. Rede de transicoes da variacao morfologica"
FIGS9_LARGURA <- 9
FIGS9_ALTURA <- 9
FIGS12_NOME <- "FigS12. Matriz Q da variacao morfologica"
FIGS12_LARGURA <- 11
FIGS12_ALTURA <- 8
plot_transition_network_morph(res_var, "Variacao_Morfologica", FIGS9_NOME, FIGS9_LARGURA, FIGS9_ALTURA)
plot_q_heatmap_morph(res_var, "Variacao_Morfologica", FIGS12_NOME, FIGS12_LARGURA, FIGS12_ALTURA)

# ==============================================================================
# ANALISE 07D - RELACAO DE TAMANHO
# ==============================================================================
console_step(5, 8, "Relacao de tamanho: Mk -> Q -> SIMMAP -> figuras")
res_size <- analisar_caracter_morfologico(morph_tree, morph_data, "Relacao_Tamanho")
cat("\n--- TAMANHO: COMPARACAO DE MODELOS ---\n"); print(res_size$model$comparison)
cat("\n--- TAMANHO: MATRIZ Q ---\n"); print(res_size$Q$tidy)
cat("\n--- TAMANHO: TRANSICOES SIMMAP ---\n"); print(res_size$simmap$summary$transitions)

# FIGURAS DA ANALISE 07D - relacao de tamanho
FIGS10_NOME <- "FigS10. Rede de transicoes da relacao de tamanho"
FIGS10_LARGURA <- 9
FIGS10_ALTURA <- 9
FIGS13_NOME <- "FigS13. Matriz Q da relacao de tamanho"
FIGS13_LARGURA <- 11
FIGS13_ALTURA <- 8
plot_transition_network_morph(res_size, "Relacao_Tamanho", FIGS10_NOME, FIGS10_LARGURA, FIGS10_ALTURA)
plot_q_heatmap_morph(res_size, "Relacao_Tamanho", FIGS13_NOME, FIGS13_LARGURA, FIGS13_ALTURA)

# ==============================================================================
# ANALISE 07E - CONSOLIDACAO DOS TRES COMPONENTES
# ==============================================================================
console_step(6, 8, "Consolidando tabelas e comparacao geral dos modelos")
results_by_trait <- list(
  Arquitetura_Lamina = res_arch,
  Variacao_Morfologica = res_var,
  Relacao_Tamanho = res_size
)
model_results <- lapply(results_by_trait, `[[`, "model")
q_results <- lapply(results_by_trait, `[[`, "Q")
simmap_results <- lapply(results_by_trait, `[[`, "simmap")

model_comparison <- dplyr::bind_rows(lapply(model_results, function(x) x$comparison))
q_table <- dplyr::bind_rows(lapply(q_results, function(x) x$tidy))
simmap_transitions <- dplyr::bind_rows(lapply(simmap_results, function(x) x$summary$transitions))
simmap_dwell <- dplyr::bind_rows(lapply(simmap_results, function(x) x$summary$dwell))
simmap_root <- dplyr::bind_rows(lapply(simmap_results, function(x) x$summary$root))

cat("\n--- COMPARACAO CONSOLIDADA DOS MODELOS ---\n"); print(model_comparison)
cat("\n--- MATRIZES Q CONSOLIDADAS ---\n"); print(q_table)

main_morph_models <- model_comparison[
  model_comparison$Selecionado_reconstrucao %in% TRUE,
  , drop = FALSE
]
write_csv_utf8(main_morph_models, file.path(PATHS$tables_main, "Table5. Modelos das solucoes morfologicas.csv"))
morph_write_csv(model_comparison, "TableS13. Comparacao dos modelos morfologicos.csv")
morph_write_csv(q_table, "TableS14. Matrizes Q dos caracteres morfologicos.csv")
morph_write_csv(simmap_transitions, "TableS15_Transicoes_SIMMAP.csv")
morph_write_csv(simmap_dwell, "TableS16. Tempo nos estados morfologicos.csv")
morph_write_csv(simmap_root, "TableS17. Estado da raiz das solucoes morfologicas.csv")

# FIGURA DA ANALISE 07E - comparacao geral dos modelos
FIGS7_NOME <- "FigS7. Comparison of morphological models"
FIGS7_LARGURA <- 10
FIGS7_ALTURA <- 9
plot_morph_model_comparison(model_comparison, FIGS7_NOME, FIGS7_LARGURA, FIGS7_ALTURA)

# ==============================================================================
# ANALISE 07F - DIAGNOSTICO DE ESTADOS RAROS
# ==============================================================================
rare_states <- frequency_table %>%
  dplyr::mutate(
    Estado_raro_N_menor_5 = N < 5,
    Observacao = dplyr::if_else(
      N < 5,
      "Interpretar taxas envolvendo este estado com cautela.",
      ""
    )
  )
cat("\n--- DIAGNOSTICO DE ESTADOS RAROS ---\n")
print(rare_states)
morph_write_csv(rare_states, "TableS18_Estados_Raros.csv")


# ============================================================================== 
# ANALISE 07G - ORIGENS INDEPENDENTES / CONVERGENCIA DAS SOLUCOES MORFOLOGICAS
# ============================================================================== 
# Esta etapa usa os MESMOS 500 SIMMAPs ja calculados acima. Nenhum modelo Mk e
# reajustado. Apenas contamos, em cada historia evolutiva, quantas vezes cada
# estado apareceu independentemente.
console_step(7, 8, "Contando origens independentes das solucoes morfologicas")

independent_origins <- dplyr::bind_rows(
  morph_independent_origins(
    res_arch$simmap,
    "Arquitetura_Lamina",
    MORPH_LABELS$Arquitetura_Lamina
  ),
  morph_independent_origins(
    res_var$simmap,
    "Variacao_Morfologica",
    MORPH_LABELS$Variacao_Morfologica
  ),
  morph_independent_origins(
    res_size$simmap,
    "Relacao_Tamanho",
    MORPH_LABELS$Relacao_Tamanho
  )
)

cat("\n--- ORIGENS INDEPENDENTES DAS SOLUCOES MORFOLOGICAS ---\n")
print(independent_origins)

morph_write_csv(
  independent_origins,
  "TableS27_Origens_Independentes.csv"
)

FIGS24_NOME <- "FigS24. Independent origins of morphological solutions"
FIGS24_LARGURA <- 12.5
FIGS24_ALTURA <- 10.5

plot_independent_origins_morph(
  independent_origins,
  stem = FIGS24_NOME,
  width = FIGS24_LARGURA,
  height = FIGS24_ALTURA
)

# ==============================================================================
# OBJETO FINAL DO SCRIPT 07
# ==============================================================================
console_step(8, 8, "Salvando o objeto central para o Script 08")
core_results <- list(
  data = morph_data,
  tree = morph_tree,
  source_signature01 = source_signature01,
  selection_policy = MK_POLICY_VERSION,
  qc_coding = prepared$qc_coding,
  frequencies = frequency_table,
  models = model_results,
  Q = q_results,
  simmap = simmap_results,
  comparison = model_comparison,
  transitions = simmap_transitions,
  dwell = simmap_dwell,
  root = simmap_root,
  independent_origins = independent_origins
)

saveRDS(
  core_results,
  windows_safe_path(file.path(MORPH_PATHS$objects, "resultados_centrais_morfologia.rds")),
  compress = "xz"
)
save_pipeline_rds(core_results, "07_evolucao_solucoes_morfologicas.rds")

# ------------------------------------------------------------------------------
# WORKBOOK GERAL - OPCIONAL
# ------------------------------------------------------------------------------
# As mesmas informacoes ja sao salvas separadamente em TableS12-TableS18 e
# TableS27. Por isso, o workbook com varias abas fica DESATIVADO por padrao.
# Nenhuma analise foi removida. Se quiser gerar novamente o arquivo Excel com
# todas as abas, basta trocar FALSE por TRUE abaixo.
GERAR_WORKBOOK_MORFOLOGIA <- FALSE

if (isTRUE(GERAR_WORKBOOK_MORFOLOGIA)) {
  morph_create_workbook(
    list(
      Frequencias = frequency_table,
      Modelos = model_comparison,
      Matriz_Q = q_table,
      Transicoes_SIMMAP = simmap_transitions,
      Tempo_estados = simmap_dwell,
      Raiz = simmap_root,
      Estados_raros = rare_states,
      Origens_independentes = independent_origins
    ),
    output_name = "TablesS. Morfologia evolutiva.xlsx"
  )
}

console_output("07_evolucao_solucoes_morfologicas.rds e resultados_centrais_morfologia.rds")
console_output("Table5, TableS12-S18, TableS27, FigS6-S13 e FigS24")
log_message("SCRIPT 07 concluido.")
console_done("SCRIPT 07", "08_CORRELACOES_E_SINAL_MORFOLOGICO.R")
