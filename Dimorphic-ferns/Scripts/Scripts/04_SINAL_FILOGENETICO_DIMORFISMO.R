# ==============================================================================
# 04_SINAL_FILOGENETICO_DIMORFISMO.R
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
#   Este script calcula a estatistica D de Fritz & Purvis para avaliar
#   sinal filogenetico na presenca/ausencia de dimorfismo foliar.
#
#   Dimorfismo_binario:
#     0 = dimorfismo ausente
#     1 = dimorfismo presente
#
# INTERPRETACAO GERAL:
#   D ~ 0  = padrao compativel com limiar browniano
#   D ~ 1  = padrao aproximadamente aleatorio
#   D < 0  = agrupamento filogenetico mais forte que o browniano
#   0<D<1  = padrao intermediario
#   D > 1  = maior dispersao filogenetica que o esperado ao acaso
#
# IMPORTANTE:
#   A interpretacao final deve considerar simultaneamente:
#     - valor observado de D;
#     - P contra D = 0;
#     - P contra D = 1.
#
# QUANDO TERMINAR:
#   Depois, rode 05_HABITO_E_DIMORFISMO.R.
# ------------------------------------------------------------------------------
# PERGUNTA:
# Especies aparentadas tendem a compartilhar a presenca/ausencia de dimorfismo?
#
# ANALISE:
# Estatistica D de Fritz & Purvis para dimorfismo binario.
#
# PARA QUE SERVE:
# Avalia se a distribuicao de presenca/ausencia de dimorfismo apresenta
# estrutura filogenetica.
#
# O QUE OLHAR:
# D, P contra D=1 (aleatorio) e P contra D=0 (limiar browniano).
#
# DEPENDENCIAS:
# 01_PREPARAR_DADOS_E_FILOGENIA.R
#
# LE:
# Scripts_/_Objetos_R/01_dados_e_filogenia.rds
#
# OBJETOS SALVOS:
# Scripts_/_Objetos_R/estatistica_D.rds
# Scripts_/_Objetos_R/04_sinal_filogenetico_dimorfismo.rds
#
# TABELAS GERADAS:
# TableS8. Sinal filogenetico D do dimorfismo.csv
#
# FIGURAS GERADAS:
# Sinal filogenetico D do dimorfismo.pdf
# Sinal filogenetico D do dimorfismo.tiff
# ==============================================================================


# ==============================================================================
# LOCALIZACAO DO DIRETORIO DE SCRIPTS
# ==============================================================================

get_scripts_dir <- function() {
  
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
    
    p <- rstudioapi::getActiveDocumentContext()$path
    
    if (nzchar(p)) {
      return(
        dirname(
          normalizePath(
            p,
            winslash = "/"
          )
        )
      )
    }
  }
  
  normalizePath(
    getwd(),
    winslash = "/"
  )
}


SCRIPTS_DIR <- get_scripts_dir()

source(
  file.path(
    SCRIPTS_DIR,
    "00_CONFIGURACAO_E_FUNCOES.R"
  ),
  chdir = TRUE
)


# ==============================================================================
# PACOTES
# ==============================================================================

required_packages <- c(
  "ape",
  "caper",
  "ggplot2",
  "dplyr"
)

check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# CONFIGURACAO EXCLUSIVA DO SCRIPT 04
# ==============================================================================

quick_mode <- is_true(
  Sys.getenv(
    "LEAF_DIMORPHISM_QUICK",
    "false"
  )
)

CONFIG <- list(
  
  # Numero de permutacoes.
  # O modo rapido serve apenas para testar o funcionamento do script.
  n_permutations_D = if (quick_mode) 100L else 5000L,
  
  # Semente fixa para tornar as permutacoes reproduziveis.
  seed_D = 1234L,
  
  # Nivel de significancia utilizado apenas na interpretacao automatica.
  alpha = 0.05
)


if (quick_mode) {
  
  warning(
    paste0(
      "MODO RAPIDO ATIVADO: a estatistica D sera calculada com apenas ",
      CONFIG$n_permutations_D,
      " permutacoes. Use o modo normal para os resultados finais."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# FUNCOES EXCLUSIVAS DO SCRIPT 04
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. VALIDACAO DOS DADOS
# ------------------------------------------------------------------------------

validate_D_input <- function(tree, analysis_data) {
  
  required_columns <- c(
    "species",
    "Dimorfismo_binario"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(analysis_data)
  )
  
  if (length(missing_columns) > 0L) {
    
    stop(
      paste0(
        "Colunas obrigatorias ausentes: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Verifica especies vazias
  # ---------------------------------------------------------------------------
  
  if (
    any(is.na(analysis_data$species)) ||
    any(!nzchar(trimws(as.character(analysis_data$species))))
  ) {
    
    stop(
      "Existem especies ausentes ou vazias nos dados.",
      call. = FALSE
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Verifica duplicatas
  # ---------------------------------------------------------------------------
  
  duplicated_species <- unique(
    analysis_data$species[
      duplicated(analysis_data$species)
    ]
  )
  
  if (length(duplicated_species) > 0L) {
    
    stop(
      paste0(
        "Existem especies duplicadas nos dados: ",
        paste(
          duplicated_species,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Verifica NA no carater
  # ---------------------------------------------------------------------------
  
  if (any(is.na(analysis_data$Dimorfismo_binario))) {
    
    stop(
      paste0(
        "Existem valores NA em Dimorfismo_binario. ",
        "Revise os dados antes de calcular a estatistica D."
      ),
      call. = FALSE
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Verifica se o carater realmente e binario
  # ---------------------------------------------------------------------------
  
  observed_states <- sort(
    unique(
      as.numeric(
        analysis_data$Dimorfismo_binario
      )
    )
  )
  
  if (!all(observed_states %in% c(0, 1))) {
    
    stop(
      paste0(
        "Dimorfismo_binario deve conter somente 0 e 1. ",
        "Estados encontrados: ",
        paste(
          observed_states,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Os dois estados precisam estar representados
  # ---------------------------------------------------------------------------
  
  if (length(observed_states) < 2L) {
    
    stop(
      paste0(
        "A estatistica D nao pode ser calculada porque apenas um ",
        "estado do carater esta presente nos dados."
      ),
      call. = FALSE
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Confere correspondencia entre arvore e dados
  # ---------------------------------------------------------------------------
  
  species_not_tree <- setdiff(
    analysis_data$species,
    tree$tip.label
  )
  
  tree_not_data <- setdiff(
    tree$tip.label,
    analysis_data$species
  )
  
  if (
    length(species_not_tree) > 0L ||
    length(tree_not_data) > 0L
  ) {
    
    message("")
    
    if (length(species_not_tree) > 0L) {
      
      message(
        "Especies nos dados mas ausentes da arvore:"
      )
      
      message(
        paste(
          species_not_tree,
          collapse = ", "
        )
      )
    }
    
    if (length(tree_not_data) > 0L) {
      
      message(
        "Especies na arvore mas ausentes dos dados:"
      )
      
      message(
        paste(
          tree_not_data,
          collapse = ", "
        )
      )
    }
    
    stop(
      paste0(
        "Arvore e dados nao possuem exatamente o mesmo conjunto de especies. ",
        "Revise o Script 01."
      ),
      call. = FALSE
    )
  }
  
  
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# 2. INTERPRETACAO AUTOMATICA DA ESTATISTICA D
# ------------------------------------------------------------------------------

interpret_phylogenetic_D <- function(
    D,
    p_random,
    p_brownian,
    alpha = 0.05) {
  
  # ---------------------------------------------------------------------------
  # Diferente do aleatorio, mas compativel com Browniano
  # ---------------------------------------------------------------------------
  
  if (
    p_random < alpha &&
    p_brownian >= alpha
  ) {
    
    return(
      paste0(
        "Sinal filogenetico; padrao compativel com ",
        "o modelo de limiar browniano"
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Compativel com aleatoriedade, mas diferente do Browniano
  # ---------------------------------------------------------------------------
  
  if (
    p_random >= alpha &&
    p_brownian < alpha
  ) {
    
    return(
      paste0(
        "Padrao compativel com distribuicao aleatoria ",
        "em relacao a filogenia"
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Diferente tanto de D=0 quanto de D=1
  # ---------------------------------------------------------------------------
  
  if (
    p_random < alpha &&
    p_brownian < alpha
  ) {
    
    if (D < 0) {
      
      return(
        paste0(
          "Agrupamento filogenetico mais forte que ",
          "o esperado pelo modelo de limiar browniano"
        )
      )
    }
    
    
    if (D >= 0 && D <= 1) {
      
      return(
        paste0(
          "Sinal filogenetico intermediario entre ",
          "o limiar browniano e a distribuicao aleatoria"
        )
      )
    }
    
    
    if (D > 1) {
      
      return(
        paste0(
          "Padrao mais filogeneticamente disperso ",
          "que o esperado sob distribuicao aleatoria"
        )
      )
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Nao difere de nenhum dos dois modelos
  # ---------------------------------------------------------------------------
  
  if (
    p_random >= alpha &&
    p_brownian >= alpha
  ) {
    
    return(
      paste0(
        "Resultado inconclusivo entre as referencias D=0 e D=1; ",
        "o padrao nao diferiu significativamente de nenhum dos dois modelos"
      )
    )
  }
  
  
  return(
    "Interpretacao nao classificada automaticamente"
  )
}


# ------------------------------------------------------------------------------
# 3. CALCULO DA ESTATISTICA D
# ------------------------------------------------------------------------------

run_phylogenetic_D <- function(
    tree,
    analysis_data) {
  
  # ---------------------------------------------------------------------------
  # Validacao
  # ---------------------------------------------------------------------------
  
  validate_D_input(
    tree,
    analysis_data
  )
  
  
  # ---------------------------------------------------------------------------
  # Ordena os dados exatamente na mesma ordem das pontas da arvore
  # ---------------------------------------------------------------------------
  
  analysis_data <- analysis_data[
    match(
      tree$tip.label,
      analysis_data$species
    ),
    ,
    drop = FALSE
  ]
  
  
  # ---------------------------------------------------------------------------
  # Prepara arvore
  # ---------------------------------------------------------------------------
  
  signal_tree <- tree
  
  # Labels internos nao sao necessarios para esta analise.
  signal_tree$node.label <- NULL
  
  
  # ---------------------------------------------------------------------------
  # Prepara dados
  # ---------------------------------------------------------------------------
  
  signal_data <- data.frame(
    species = as.character(
      analysis_data$species
    ),
    dimorphism = as.numeric(
      analysis_data$Dimorfismo_binario
    ),
    stringsAsFactors = FALSE
  )
  
  
  # ---------------------------------------------------------------------------
  # Torna as permutacoes reproduziveis
  # ---------------------------------------------------------------------------
  
  set.seed(
    CONFIG$seed_D
  )
  
  
  # ---------------------------------------------------------------------------
  # Estatistica D
  # ---------------------------------------------------------------------------
  
  result <- caper::phylo.d(
    data = signal_data,
    phy = signal_tree,
    names.col = species,
    binvar = dimorphism,
    permut = CONFIG$n_permutations_D
  )
  
  
  # ---------------------------------------------------------------------------
  # Interpretacao automatica
  # ---------------------------------------------------------------------------
  
  interpretation <- interpret_phylogenetic_D(
    D = as.numeric(result$DEstimate),
    p_random = as.numeric(result$Pval1),
    p_brownian = as.numeric(result$Pval0),
    alpha = CONFIG$alpha
  )
  
  
  # ---------------------------------------------------------------------------
  # Tabela resumida
  # ---------------------------------------------------------------------------
  
  output <- data.frame(
    
    Caracter = "Dimorfismo presente versus ausente",
    
    D = as.numeric(
      result$DEstimate
    ),
    
    P_contra_aleatorio_D1 = as.numeric(
      result$Pval1
    ),
    
    P_contra_browniano_D0 = as.numeric(
      result$Pval0
    ),
    
    Permutacoes = CONFIG$n_permutations_D,
    
    Semente = CONFIG$seed_D,
    
    Interpretacao = interpretation,
    
    stringsAsFactors = FALSE
  )
  
  
  # ---------------------------------------------------------------------------
  # Salva o objeto completo retornado pelo caper
  # ---------------------------------------------------------------------------
  
  saveRDS(
    result,
    windows_safe_path(
      file.path(
        PATHS$objects,
        "estatistica_D.rds"
      )
    )
  )
  
  
  list(
    raw = result,
    summary = output
  )
}


# ------------------------------------------------------------------------------
# 4. FORMATACAO DE P PARA A FIGURA
# ------------------------------------------------------------------------------

format_p_D <- function(p) {
  
  if (is.na(p)) {
    return("NA")
  }
  
  if (p < 0.001) {
    return("< 0.001")
  }
  
  sprintf(
    "%.3f",
    p
  )
}


# ------------------------------------------------------------------------------
# 5. FIGURA DA ESTATISTICA D
# ------------------------------------------------------------------------------

plot_phylogenetic_D <- function(
    raw_D,
    summary_D) {
  
  # ---------------------------------------------------------------------------
  # Valores utilizados pelo caper para padronizar D
  # ---------------------------------------------------------------------------
  
  mean_random <- as.numeric(
    raw_D$Parameters$MeanRandom
  )
  
  mean_brownian <- as.numeric(
    raw_D$Parameters$MeanBrownian
  )
  
  denominator <- mean_random - mean_brownian
  
  
  if (
    !is.finite(denominator) ||
    abs(denominator) < .Machine$double.eps
  ) {
    
    stop(
      "Nao foi possivel padronizar as simulacoes na escala de D.",
      call. = FALSE
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Converte as simulacoes para a escala padronizada de D
  # ---------------------------------------------------------------------------
  
  scale_to_D <- function(x) {
    
    (
      as.numeric(x) -
        mean_brownian
    ) /
      denominator
  }
  
  
  # ---------------------------------------------------------------------------
  # Junta as duas distribuicoes simuladas
  # ---------------------------------------------------------------------------
  
  simulated_D <- rbind(
    
    data.frame(
      D = scale_to_D(
        raw_D$Permutations$brownian
      ),
      Model = "Brownian threshold"
    ),
    
    data.frame(
      D = scale_to_D(
        raw_D$Permutations$random
      ),
      Model = "Random"
    )
  )
  
  
  # Ordem fixa da legenda
  simulated_D$Model <- factor(
    simulated_D$Model,
    levels = c(
      "Brownian threshold",
      "Random"
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Valores observados
  # ---------------------------------------------------------------------------
  
  observed_D <- as.numeric(
    raw_D$DEstimate
  )
  
  p_brownian <- as.numeric(
    summary_D$P_contra_browniano_D0[1]
  )
  
  p_random <- as.numeric(
    summary_D$P_contra_aleatorio_D1[1]
  )
  
  
  annotation_text <- paste0(
    "atop(",
    "italic(D) == ", sprintf("%.3f", observed_D), ",",
    "atop(",
    "italic(P)~'vs.'~italic(D)==0~':'~'", format_p_D(p_brownian), "',",
    "italic(P)~'vs.'~italic(D)==1~':'~'", format_p_D(p_random), "'",
    ")",
    ")"
  )
  
  
  # ---------------------------------------------------------------------------
  # Figura
  # ---------------------------------------------------------------------------
  
  p <- ggplot2::ggplot(
    simulated_D,
    ggplot2::aes(
      x = D,
      fill = Model,
      colour = Model
    )
  ) +
    
    # Distribuicoes simuladas
    ggplot2::geom_density(
      alpha = 0.20,
      linewidth = 1.2,
      adjust = 1.05
    ) +
    
    # D = 0
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.65,
      colour = "grey40"
    ) +
    
    # D = 1
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = "dashed",
      linewidth = 0.65,
      colour = "grey40"
    ) +
    
    # D observado
    ggplot2::geom_vline(
      xintercept = observed_D,
      linewidth = 1.3,
      colour = "black"
    ) +
    
    # Valores estatisticos
    ggplot2::annotate(
      geom = "label",
      x = -Inf,
      y = Inf,
      label = annotation_text,
      parse = TRUE,
      hjust = -0.08,
      vjust = 1.15,
      size = 3.5,
      linewidth = 0.25,
      fill = "white"
    ) +
    
    # Paleta
    ggplot2::scale_fill_manual(
      values = c(
        "Brownian threshold" = "#2A9D8F",
        "Random" = "#2F6690"
      )
    ) +
    
    ggplot2::scale_colour_manual(
      values = c(
        "Brownian threshold" = "#2A9D8F",
        "Random" = "#2F6690"
      )
    ) +
    
    # Eixos
    ggplot2::labs(
      x = "Phylogenetic D",
      y = "Density",
      fill = NULL,
      colour = NULL
    ) +
    
    # Um pouco de espaco acima das densidades
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(
        mult = c(
          0,
          0.12
        )
      )
    ) +
    
    # Tema
    ggplot2::theme_classic(
      base_size = 12
    ) +
    
    ggplot2::theme(
      
      legend.position = "bottom",
      
      legend.justification = "center",
      
      legend.text = ggplot2::element_text(
        size = 10
      ),
      
      axis.title = ggplot2::element_text(
        size = 11,
        face = "bold"
      ),
      
      axis.text = ggplot2::element_text(
        size = 10,
        colour = "black"
      ),
      
      axis.line = ggplot2::element_line(
        linewidth = 0.65,
        colour = "black"
      ),
      
      axis.ticks = ggplot2::element_line(
        linewidth = 0.55,
        colour = "black"
      ),
      
      plot.margin = ggplot2::margin(
        t = 8,
        r = 12,
        b = 8,
        l = 8
      )
    )
  
  
  return(p)
}


# ==============================================================================
# EXECUCAO DA ANALISE 04
# ==============================================================================

console_banner(
  "SCRIPT 04",
  "Sinal filogenetico do dimorfismo"
)


# ------------------------------------------------------------------------------
# PASSO 1
# ------------------------------------------------------------------------------

console_step(
  1,
  3,
  "Carregando dados e arvore do Script 01"
)


prepared <- require_saved_rds(
  "01_dados_e_filogenia.rds",
  "01_PREPARAR_DADOS_E_FILOGENIA.R"
)


# ------------------------------------------------------------------------------
# PASSO 2
# ------------------------------------------------------------------------------

console_step(
  2,
  3,
  "Calculando a estatistica D"
)


D_analysis <- run_phylogenetic_D(
  prepared$tree,
  prepared$data
)


# Resultado completo do caper
raw_D <- D_analysis$raw


# Tabela resumida
result_D <- D_analysis$summary


# Mostra no Console
print(
  result_D
)


# ------------------------------------------------------------------------------
# GERA A FIGURA
# ------------------------------------------------------------------------------

fig_D <- plot_phylogenetic_D(
  raw_D = raw_D,
  summary_D = result_D
)


print(
  fig_D
)


# ------------------------------------------------------------------------------
# PASSO 3
# ------------------------------------------------------------------------------

console_step(
  3,
  3,
  "Salvando tabela, figura e objetos da analise"
)


# ==============================================================================
# TABELA
# ==============================================================================

write_csv_utf8(
  result_D,
  file.path(
    PATHS$tables_supp,
    "TableS8. Sinal filogenetico D do dimorfismo.csv"
  )
)


# ==============================================================================
# FIGURA PDF
# ==============================================================================

ggplot2::ggsave(
  filename = windows_safe_path(
    file.path(
      PATHS$figures_supp,
      "Sinal filogenetico D do dimorfismo.pdf"
    )
  ),
  plot = fig_D,
  width = 7.2,
  height = 4.8,
  units = "in"
)


# ==============================================================================
# FIGURA TIFF
# ==============================================================================

ggplot2::ggsave(
  filename = windows_safe_path(
    file.path(
      PATHS$figures_supp,
      "Sinal filogenetico D do dimorfismo.tiff"
    )
  ),
  plot = fig_D,
  width = 7.2,
  height = 4.8,
  units = "in",
  device = "tiff",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)


# ==============================================================================
# OBJETO DO PIPELINE
# ==============================================================================

# Mantemos o mesmo formato do pipeline anterior:
# o arquivo 04 contem a tabela resumida.
# O objeto completo de phylo.d fica separado em estatistica_D.rds.

save_pipeline_rds(
  result_D,
  "04_sinal_filogenetico_dimorfismo.rds"
)


# ==============================================================================
# FINALIZACAO
# ==============================================================================

console_output(
  c(
    "TableS8. Sinal filogenetico D do dimorfismo.csv",
    "Sinal filogenetico D do dimorfismo.pdf",
    "Sinal filogenetico D do dimorfismo.tiff",
    "estatistica_D.rds",
    "04_sinal_filogenetico_dimorfismo.rds"
  )
)


log_message(
  "SCRIPT 04 concluido."
)


console_done(
  "SCRIPT 04",
  "05_HABITO_E_DIMORFISMO.R"
)