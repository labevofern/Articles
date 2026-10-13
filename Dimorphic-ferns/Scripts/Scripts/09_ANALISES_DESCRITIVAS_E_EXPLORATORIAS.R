# ==============================================================================
# VERSAO CORRIGIDA V5 - correcoes de position_stack e slice_head
# 09_ANALISES_DESCRITIVAS_E_EXPLORATORIAS.R
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
#   Esta analise e descritiva.
#   Ela nao substitui os testes filogeneticos formais.
#
# QUANDO TERMINAR:
#   Fim das analises.
#   O Script 12 e opcional para editar/refazer figuras a partir dos .rds.
# ------------------------------------------------------------------------------
# OBJETIVO:
# Gerar um resumo descritivo das combinacoes morfologicas observadas entre todas
# as especies, incluindo as monomorficas.
#
# PARA QUE SERVE:
# A tabela mantem todas as especies. A Fig3 e uma visualizacao descritiva das
# sindromes entre hemidimorfia e holodimorfia.
#
# CADA SINDROME E DEFINIDA PELA COMBINACAO DE:
#   - arquitetura da lamina;
#   - componente da variacao morfologica;
#   - relacao de tamanho entre frondes.
#
# O QUE OLHAR:
#   - sindromes mais frequentes;
#   - sindromes raras;
#   - diferencas entre hemidimorfia e holodimorfia na Fig3;
#   - combinacoes compartilhadas entre os dois padroes;
#   - frequencia absoluta e relativa de cada combinacao.
#
# DEPENDENCIAS:
#   01_PREPARAR_DADOS_E_FILOGENIA.R
#
# LE:
#   Scripts_/_Objetos_R/01_dados_e_filogenia.rds
#
# OBJETO SALVO:
#   Scripts_/_Objetos_R/09_exploratorias.rds
#
# TABELA GERADA:
#   TableS11. Observed morphological routes.csv
#
# FIGURA GERADA:
#   Fig3. Morphological syndromes
#
# IMPORTANTE:
#   Todo o texto apresentado dentro da figura esta em ingles.
# ==============================================================================

# ==============================================================================
# LOCALIZAR A PASTA DOS SCRIPTS
# ==============================================================================

get_scripts_dir <- function() {
  configured_dir <- Sys.getenv("MORPH_SCRIPTS_DIR", unset = "")
  
  if (nzchar(configured_dir)) {
    # Mantem o caminho fornecido sem resolver junctions/symlinks. Isso tambem
    # evita problemas de locale em caminhos Windows que contenham acentos.
    return(gsub("\\\\", "/", configured_dir))
  }
  
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  
  if (length(file_arg) > 0L) {
    return(
      dirname(
        normalizePath(
          sub("^--file=", "", file_arg[1]),
          winslash = "/"
        )
      )
    )
  }
  
  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    active_path <- rstudioapi::getActiveDocumentContext()$path
    
    if (nzchar(active_path)) {
      return(dirname(normalizePath(active_path, winslash = "/")))
    }
  }
  
  normalizePath(getwd(), winslash = "/")
}


SCRIPTS_DIR <- get_scripts_dir()


# ==============================================================================
# CONFIGURACAO E PACOTES
# ==============================================================================

source(
  file.path(SCRIPTS_DIR, "00_CONFIGURACAO_E_FUNCOES.R"),
  chdir = TRUE
)


required_packages <- c(
  "dplyr",
  "ggplot2",
  "stringr",
  "tidyr",
  "patchwork"
)

check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# FUNCOES AUXILIARES
# ==============================================================================

get_figure_labels <- function(fig_name, fallback_name) {
  labels_fig <- get0(fig_name, inherits = TRUE, ifnotfound = NULL)
  
  if (!is.null(labels_fig)) {
    return(labels_fig)
  }
  
  labels_base <- get0(fallback_name, inherits = TRUE, ifnotfound = NULL)
  
  if (is.null(labels_base)) {
    stop(
      paste0(
        "Nao foi encontrado nenhum dos objetos de rotulos: ",
        fig_name,
        " ou ",
        fallback_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  labels_base
}


morph_save_plot <- function(
    plot,
    stem,
    width = 11.5,
    height = 8.2
) {
  save_plot_dual(
    plot,
    stem,
    width,
    height,
    directory = PATHS$figures_main
  )
}


# Encurta apenas os textos usados na figura. Os nomes completos permanecem
# preservados na tabela suplementar e no objeto salvo.
short_morph_label <- function(x) {
  short_map <- c(
    stats::setNames(
      c("Contracted", "Spiciform", "Paniculiform", "Digitiform", "Sporocarp", "Radiciform"),
      unname(ARCHITECTURE_LABELS[as.character(1:6)])
    ),
    stats::setNames(
      c(
        "Shape variation", "Size variation", "Petiole variation",
        "Shape + size", "Shape + petiole", "Frond + petiole size",
        unname(VARIATION_LABELS["7"])
      ),
      unname(VARIATION_LABELS[as.character(1:7)])
    ),
    stats::setNames(
      c("Larger fertile", "Smaller fertile", "Similar length", "Intra-individual"),
      unname(SIZE_LABELS[as.character(1:4)])
    )
  )
  
  x_chr <- as.character(x)
  out <- unname(short_map[x_chr])
  out[is.na(out)] <- x_chr[is.na(out)]
  out
}


# ==============================================================================
# PREPARAR OS DADOS DAS SINDROMES MORFOLOGICAS
# ==============================================================================

prepare_route_data <- function(analysis_data) {
  architecture_labels_use <- get_figure_labels(
    "ARCHITECTURE_LABELS_FIG",
    "ARCHITECTURE_LABELS"
  )
  
  variation_labels_use <- get_figure_labels(
    "VARIATION_LABELS_FIG",
    "VARIATION_LABELS"
  )
  
  size_labels_use <- get_figure_labels(
    "SIZE_LABELS_FIG",
    "SIZE_LABELS"
  )
  
  analysis_data %>%
    dplyr::mutate(
      Pattern = unname(STATE_LABELS[as.character(Padrao_Dimorfismo)]),
      Architecture = unname(
        architecture_labels_use[as.character(Arquitetura_Lamina)]
      ),
      Variation = unname(
        variation_labels_use[as.character(Variacao_Morfologica)]
      ),
      Size_Relation = unname(
        size_labels_use[as.character(Relacao_Tamanho)]
      )
    ) %>%
    dplyr::filter(
      !is.na(Pattern),
      !is.na(Architecture),
      !is.na(Variation),
      !is.na(Size_Relation)
    ) %>%
    dplyr::count(
      Pattern,
      Architecture,
      Variation,
      Size_Relation,
      name = "N"
    ) %>%
    dplyr::arrange(dplyr::desc(N))
}


# ==============================================================================
# FIG3 - MORPHOLOGICAL SYNDROMES
# ==============================================================================

plot_morphological_syndromes <- function(
    route_data,
    stem = "Fig3. Morphological syndromes",
    width = 11.5,
    height = 8.2,
    top_n = 10
) {
  # A entrada permanece completa. O filtro e aplicado somente a copia usada na
  # figura, sem alterar a tabela nem qualquer inferencia filogenetica.
  pattern_levels <- unname(STATE_LABELS[c("1", "2")])
  route_data <- route_data[
    route_data$Pattern %in% pattern_levels,
    ,
    drop = FALSE
  ]
  character_levels <- c(
    "Architecture",
    "Morphological variation",
    "Size relationship"
  )
  
  syndrome_data <- route_data %>%
    dplyr::mutate(
      Syndrome_raw = paste(
        Architecture,
        Variation,
        Size_Relation,
        sep = " | "
      )
    ) %>%
    dplyr::group_by(
      Pattern,
      Architecture,
      Variation,
      Size_Relation,
      Syndrome_raw
    ) %>%
    dplyr::summarise(N = sum(N), .groups = "drop")
  
  pattern_totals <- syndrome_data %>%
    dplyr::group_by(Pattern) %>%
    dplyr::summarise(Pattern_total = sum(N), .groups = "drop")
  
  pattern_palette <- stats::setNames(
    unname(PALETTE_DIMORPHISM[c("1", "2")]),
    unname(STATE_LABELS[c("1", "2")])
  )
  
  pattern_n <- stats::setNames(
    pattern_totals$Pattern_total,
    pattern_totals$Pattern
  )
  
  pattern_legend_labels <- stats::setNames(
    paste0(pattern_levels, " (n = ", pattern_n[pattern_levels], ")"),
    pattern_levels
  )
  
  
  # ---------------------------------------------------------------------------
  # PAINEL A - FREQUENCIAS DOS ESTADOS
  # ---------------------------------------------------------------------------
  
  state_data <- dplyr::bind_rows(
    syndrome_data %>%
      dplyr::group_by(Pattern, State = Architecture) %>%
      dplyr::summarise(N = sum(N), .groups = "drop") %>%
      dplyr::mutate(Character = "Architecture"),
    syndrome_data %>%
      dplyr::group_by(Pattern, State = Variation) %>%
      dplyr::summarise(N = sum(N), .groups = "drop") %>%
      dplyr::mutate(Character = "Morphological variation"),
    syndrome_data %>%
      dplyr::group_by(Pattern, State = Size_Relation) %>%
      dplyr::summarise(N = sum(N), .groups = "drop") %>%
      dplyr::mutate(Character = "Size relationship")
  ) %>%
    dplyr::group_by(Character, State) %>%
    tidyr::complete(
      Pattern = pattern_levels,
      fill = list(N = 0L)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(pattern_totals, by = "Pattern") %>%
    dplyr::mutate(
      Percentage = 100 * N / Pattern_total,
      Percentage_label = paste0(sprintf("%.0f", Percentage), "%"),
      State_short = short_morph_label(State)
    )
  
  preferred_state_orders <- list(
    "Architecture" = short_morph_label(
      unname(ARCHITECTURE_LABELS[as.character(1:6)])
    ),
    "Morphological variation" = short_morph_label(
      unname(VARIATION_LABELS[c("7", "1", "2", "3", "4", "5", "6")])
    ),
    "Size relationship" = short_morph_label(
      unname(SIZE_LABELS[c("4", "1", "3", "2")])
    )
  )
  
  state_orders <- lapply(
    character_levels,
    function(character_name) {
      present_states <- unique(
        state_data$State_short[state_data$Character == character_name]
      )
      preferred <- preferred_state_orders[[character_name]]
      
      c(
        preferred[preferred %in% present_states],
        sort(setdiff(present_states, preferred))
      )
    }
  )
  names(state_orders) <- character_levels
  
  state_axis_levels <- unlist(
    lapply(
      character_levels,
      function(character_name) {
        paste(
          character_name,
          rev(state_orders[[character_name]]),
          sep = "___"
        )
      }
    ),
    use.names = FALSE
  )
  
  state_data <- state_data %>%
    dplyr::mutate(
      Pattern = factor(Pattern, levels = pattern_levels),
      Character = factor(Character, levels = character_levels),
      State_axis = factor(
        paste(Character, State_short, sep = "___"),
        levels = state_axis_levels
      )
    )
  
  p_heat <- ggplot2::ggplot(
    state_data,
    ggplot2::aes(x = Pattern, y = State_axis, fill = Percentage)
  ) +
    ggplot2::geom_tile(
      color = "#FFFFFF",
      linewidth = 0.9,
      width = 0.96,
      height = 0.86
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = Percentage_label),
      color = "#111827",
      fontface = "bold",
      size = 3.0
    ) +
    ggplot2::facet_grid(
      Character ~ .,
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    ggplot2::scale_x_discrete(
      position = "top",
      drop = FALSE
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) sub("^.*___", "", x),
      drop = TRUE
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c(
        "#F4F7F5",
        "#DCEAE3",
        "#B9D3C6",
        "#8DB8A3",
        "#6F9F88"
      ),
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      name = "% within pattern"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(
        direction = "vertical",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(0.34, "cm"),
        barheight = grid::unit(2.8, "cm")
      )
    ) +
    ggplot2::labs(
      title = "A   Component frequencies",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 12.5,
        color = "#111827",
        margin = ggplot2::margin(b = 4)
      ),
      panel.grid = ggplot2::element_blank(),
      panel.spacing.y = grid::unit(0.12, "cm"),
      axis.text.x.top = ggplot2::element_text(
        face = "bold",
        size = 10.2,
        color = "#111827",
        margin = ggplot2::margin(b = 2)
      ),
      axis.text.y = ggplot2::element_text(
        size = 9.0,
        color = "#111827",
        lineheight = 0.95
      ),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        face = "bold",
        size = 9.7,
        color = "#111827",
        hjust = 1,
        margin = ggplot2::margin(r = 10)
      ),
      legend.position = "right",
      legend.title = ggplot2::element_text(
        face = "bold",
        size = 9.2,
        color = "#111827"
      ),
      legend.text = ggplot2::element_text(
        size = 8.6,
        color = "#111827"
      ),
      plot.margin = ggplot2::margin(2, 5, 3, 2)
    )
  
  
  # ---------------------------------------------------------------------------
  # PAINEL B - DEZ SINDROMES MAIS FREQUENTES
  # ---------------------------------------------------------------------------
  
  syndrome_totals <- syndrome_data %>%
    dplyr::group_by(
      Syndrome_raw,
      Architecture,
      Variation,
      Size_Relation
    ) %>%
    dplyr::summarise(Total = sum(N), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(Total), Syndrome_raw)
  
  n_top <- min(top_n, nrow(syndrome_totals))
  
  top_syndromes <- syndrome_totals %>%
    dplyr::slice_head(n = n_top) %>%
    dplyr::mutate(
      Code = paste0("S", seq_len(dplyr::n())),
      Architecture_short = short_morph_label(Architecture),
      Variation_short = short_morph_label(Variation),
      Size_short = short_morph_label(Size_Relation),
      Syndrome_label = paste0(
        Code,
        "  ",
        Architecture_short,
        "  |  ",
        Variation_short,
        "  |  ",
        Size_short
      )
    )
  
  bar_data <- tidyr::expand_grid(
    Pattern = pattern_levels,
    Syndrome_raw = top_syndromes$Syndrome_raw
  ) %>%
    dplyr::left_join(
      syndrome_data %>% dplyr::select(Pattern, Syndrome_raw, N),
      by = c("Pattern", "Syndrome_raw")
    ) %>%
    dplyr::left_join(pattern_totals, by = "Pattern") %>%
    dplyr::left_join(
      top_syndromes %>%
        dplyr::select(Syndrome_raw, Syndrome_label),
      by = "Syndrome_raw"
    ) %>%
    dplyr::mutate(
      N = dplyr::coalesce(N, 0L),
      Percentage = 100 * N / Pattern_total,
      Value_label = dplyr::if_else(
        N > 0L,
        paste0(N, "  (", sprintf("%.1f", Percentage), "%)"),
        ""
      ),
      Pattern = factor(Pattern, levels = pattern_levels),
      Syndrome_label = factor(
        Syndrome_label,
        levels = rev(top_syndromes$Syndrome_label)
      )
    )
  
  max_pct <- max(bar_data$Percentage, na.rm = TRUE)
  label_room <- max(4, 0.38 * max_pct)
  
  p_bar <- ggplot2::ggplot(
    bar_data,
    ggplot2::aes(x = Percentage, y = Syndrome_label, fill = Pattern)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.86),
      width = 0.50,
      alpha = 0.96
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = Value_label),
      position = ggplot2::position_dodge(width = 0.86),
      hjust = -0.08,
      size = 2.9,
      color = "#111827",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = pattern_palette,
      breaks = pattern_levels,
      labels = pattern_legend_labels,
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, max_pct + label_room),
      expand = ggplot2::expansion(mult = c(0, 0)),
      breaks = pretty(c(0, max_pct), n = 5),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::labs(
      title = "B   Most frequent syndrome combinations",
      subtitle = "Labels show species count and within-pattern percentage.",
      x = "Share within dimorphism pattern",
      y = NULL,
      fill = NULL
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        direction = "horizontal",
        nrow = 1,
        byrow = TRUE
      )
    ) +
    ggplot2::theme_minimal(base_size = 10.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 12.5,
        color = "#111827",
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 9.2,
        color = "#374151",
        margin = ggplot2::margin(b = 7)
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = "#E5E7EB",
        linewidth = 0.35
      ),
      axis.text.y = ggplot2::element_text(
        face = "plain",
        size = 7.8,
        color = "#111827",
        margin = ggplot2::margin(r = 1)
      ),
      axis.text.x = ggplot2::element_text(
        size = 8.8,
        color = "#111827"
      ),
      axis.title.x = ggplot2::element_text(
        face = "bold",
        size = 10,
        color = "#111827",
        margin = ggplot2::margin(t = 7)
      ),
      legend.position = "top",
      legend.justification = "left",
      legend.text = ggplot2::element_text(
        size = 9.2,
        color = "#111827"
      ),
      legend.key.height = grid::unit(0.36, "cm"),
      legend.key.width = grid::unit(0.70, "cm"),
      plot.margin = ggplot2::margin(3, 12, 4, 0)
    )
  
  
  # ---------------------------------------------------------------------------
  # COMPOSICAO FINAL E EXPORTACAO
  # ---------------------------------------------------------------------------
  
  # O painel B fica livre do alinhamento lateral do facet do painel A. Assim,
  # os nomes das sindromes permanecem junto ao inicio das barras.
  # Compatibilidade com versoes novas e antigas do patchwork.
  # Nas versoes recentes, free() aceita os argumentos type e side.
  # Em versoes antigas esses argumentos nao existem; nesse caso usamos
  # patchwork::free(p_bar), preservando a composicao sem interromper o script.
  free_args <- names(formals(patchwork::free))
  
  if (all(c("type", "side") %in% free_args)) {
    p_bar_compact <- patchwork::free(
      p_bar,
      type = "label",
      side = "l"
    )
  } else {
    warning(
      paste0(
        "A versao instalada de patchwork nao suporta os argumentos ",
        "type/side em free(). Usando patchwork::free(p_bar)."
      ),
      call. = FALSE
    )
    
    p_bar_compact <- patchwork::free(p_bar)
  }
  
  p <- p_heat / p_bar_compact +
    patchwork::plot_layout(heights = c(1.02, 1.08)) +
    patchwork::plot_annotation(
      title = "Morphological syndromes in dimorphic species",
      subtitle = paste0(
        "Component frequencies and the most frequent combinations, compared ",
        "between hemidimorphic and holodimorphic species."
      ),
      caption = paste0(
        "Displayed subset: hemidimorphic and holodimorphic species. ",
        "The analysis input retains monomorphic species."
      ),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          size = 17,
          color = "#111827",
          hjust = 0
        ),
        plot.subtitle = ggplot2::element_text(
          size = 10,
          color = "#374151",
          hjust = 0,
          margin = ggplot2::margin(b = 3)
        ),
        plot.caption = ggplot2::element_text(
          size = 8.5,
          color = "#4B5563",
          hjust = 0,
          margin = ggplot2::margin(t = 4)
        ),
        plot.margin = ggplot2::margin(8, 10, 6, 10)
      )
    )
  
  morph_save_plot(
    p,
    stem,
    width,
    height
  )
  
  invisible(p)
}


# ==============================================================================
# EXECUCAO DA ANALISE 09
# ==============================================================================

run_analysis_09 <- function() {
  console_banner(
    "SCRIPT 09",
    "Analises descritivas e exploratorias"
  )
  
  console_step(
    1,
    3,
    "Carregando os dados preparados no Script 01"
  )
  
  prepared <- require_saved_rds(
    "01_dados_e_filogenia.rds",
    "01_PREPARAR_DADOS_E_FILOGENIA.R"
  )
  
  console_step(
    2,
    3,
    "Contando as sindromes morfologicas observadas"
  )
  
  route_table <- prepare_route_data(prepared$data)
  
  cat("\n--- OBSERVED MORPHOLOGICAL SYNDROMES ---\n")
  print(route_table)
  
  write_csv_utf8(
    route_table,
    file.path(
      PATHS$tables_supp,
      "TableS11. Observed morphological routes.csv"
    )
  )
  
  FIG3_NOME <- "Fig3. Morphological syndromes"
  FIG3_LARGURA <- 11.5
  FIG3_ALTURA <- 8.2
  
  plot_morphological_syndromes(
    route_table,
    stem = FIG3_NOME,
    width = FIG3_LARGURA,
    height = FIG3_ALTURA,
    top_n = 10
  )
  
  console_step(
    3,
    3,
    "Salvando o resumo para refazer a figura depois"
  )
  
  save_pipeline_rds(
    list(rotas_morfologicas = route_table),
    "09_exploratorias.rds"
  )
  
  console_output(
    "TableS11 -> complete descriptive table of observed morphological combinations"
  )
  console_output("Fig3 -> most frequent morphological syndromes")
  console_output("09_exploratorias.rds")
  
  log_message("SCRIPT 09 concluido.")
  
  console_done(
    "SCRIPT 09",
    "Analises finalizadas. Use o Script 10 somente se quiser refazer figuras."
  )
  
  invisible(route_table)
}


if (!identical(Sys.getenv("MORPH_SKIP_RUN", unset = "0"), "1")) {
  run_analysis_09()
}
 

