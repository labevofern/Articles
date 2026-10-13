# ==============================================================================
# 10B_CONGRUENCIA_TOPOLOGICA.R
# ==============================================================================
# OBJETIVO
# ------------------------------------------------------------------------------
# Quantificar a congruencia topologica entre:
#   1. FTOL consensus tree
#   2. FTOL maximum-likelihood tree
#
# Calcula:
#   - Robinson-Foulds / split distance absoluta
#   - split distance normalizada
#   - numero de splits compartilhados
#   - numero de clados enraizados compartilhados
#   - proporcao de clados compartilhados
#   - indice de Jaccard dos clados
#
# IMPORTANTE:
# Este script NAO refaz nenhuma analise evolutiva.
# Ele usa as duas arvores ja salvas pelo Script 10.
# ==============================================================================


# ==============================================================================
# 0. LOCALIZAR A PASTA DOS SCRIPTS E CARREGAR FUNCOES
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
  
  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(
        dirname(
          normalizePath(
            frame$ofile,
            winslash = "/"
          )
        )
      )
    }
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
  
  normalizePath(getwd(), winslash = "/")
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
# 1. CARREGAR RESULTADOS DO SCRIPT 10
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat(" CONGRUENCIA TOPOLOGICA: FTOL CONSENSUS vs FTOL ML\n")
cat("============================================================\n\n")


sensitivity_results <- require_saved_rds(
  "10_sensibilidade_filogenetica.rds",
  "10_SENSIBILIDADE_FILOGENETICA.R"
)


main_tree <- sensitivity_results$main_tree
alternative_tree <- sensitivity_results$alternative_tree


if (!inherits(main_tree, "phylo")) {
  stop("A arvore principal nao e um objeto phylo.")
}

if (!inherits(alternative_tree, "phylo")) {
  stop("A arvore alternativa nao e um objeto phylo.")
}


if (!setequal(
  main_tree$tip.label,
  alternative_tree$tip.label
)) {
  stop(
    "As duas arvores nao possuem exatamente o mesmo conjunto de especies."
  )
}


cat(
  "Numero de especies:",
  ape::Ntip(main_tree),
  "\n\n"
)


# ==============================================================================
# 2. EXTRAIR SPLITS E CLADOS
# ==============================================================================

# Usa a funcao que ja existe no seu Script 00.
main_keys <- sens_topology_keys(main_tree)
alternative_keys <- sens_topology_keys(alternative_tree)


# ==============================================================================
# 3. SPLITS NAO ENRAIZADOS
# ==============================================================================

splits_main <- main_keys$unrooted
splits_alt  <- alternative_keys$unrooted

shared_splits <- intersect(
  splits_main,
  splits_alt
)

unique_main_splits <- setdiff(
  splits_main,
  splits_alt
)

unique_alt_splits <- setdiff(
  splits_alt,
  splits_main
)


n_splits_main <- length(splits_main)
n_splits_alt  <- length(splits_alt)
n_shared_splits <- length(shared_splits)


# Distancia Robinson-Foulds / symmetric difference
RF_absolute <-
  length(unique_main_splits) +
  length(unique_alt_splits)


# Normalizacao simetrica.
#
# RF = 0  -> topologias identicas
# RF = 1  -> nenhuma biparticao nao trivial compartilhada
#
RF_denominator <-
  n_splits_main +
  n_splits_alt

RF_normalized <- if (
  RF_denominator > 0
) {
  RF_absolute / RF_denominator
} else {
  NA_real_
}


# Proporcao de splits da arvore principal recuperados na ML
prop_splits_main_shared <- if (
  n_splits_main > 0
) {
  n_shared_splits / n_splits_main
} else {
  NA_real_
}


# Proporcao de splits da ML recuperados na principal
prop_splits_alt_shared <- if (
  n_splits_alt > 0
) {
  n_shared_splits / n_splits_alt
} else {
  NA_real_
}


# ==============================================================================
# 4. CLADOS ENRAIZADOS
# ==============================================================================

clades_main <- main_keys$rooted
clades_alt  <- alternative_keys$rooted


shared_clades <- intersect(
  clades_main,
  clades_alt
)

unique_main_clades <- setdiff(
  clades_main,
  clades_alt
)

unique_alt_clades <- setdiff(
  clades_alt,
  clades_main
)


n_clades_main <- length(clades_main)
n_clades_alt  <- length(clades_alt)
n_shared_clades <- length(shared_clades)


rooted_clade_distance <-
  length(unique_main_clades) +
  length(unique_alt_clades)


prop_main_clades_shared <- if (
  n_clades_main > 0
) {
  n_shared_clades / n_clades_main
} else {
  NA_real_
}


prop_alt_clades_shared <- if (
  n_clades_alt > 0
) {
  n_shared_clades / n_clades_alt
} else {
  NA_real_
}


# Medida simetrica de sobreposicao
clade_union <- union(
  clades_main,
  clades_alt
)

clade_jaccard <- if (
  length(clade_union) > 0
) {
  n_shared_clades / length(clade_union)
} else {
  NA_real_
}


rooted_identical <- identical(
  clades_main,
  clades_alt
)


# ==============================================================================
# 5. TABELA FINAL
# ==============================================================================

congruence_table <- data.frame(
  
  Metric = c(
    
    "Number of tips",
    
    "Unrooted splits - consensus",
    "Unrooted splits - ML",
    "Shared unrooted splits",
    
    "Unrooted RF distance",
    "Normalized unrooted RF distance",
    
    "Proportion of consensus splits shared",
    "Proportion of ML splits shared",
    
    "Rooted clades - consensus",
    "Rooted clades - ML",
    "Shared rooted clades",
    
    "Rooted clade distance",
    
    "Proportion of consensus rooted clades shared",
    "Proportion of ML rooted clades shared",
    
    "Rooted-clade Jaccard similarity",
    
    "Rooted topologies identical"
    
  ),
  
  Value = c(
    
    ape::Ntip(main_tree),
    
    n_splits_main,
    n_splits_alt,
    n_shared_splits,
    
    RF_absolute,
    RF_normalized,
    
    prop_splits_main_shared,
    prop_splits_alt_shared,
    
    n_clades_main,
    n_clades_alt,
    n_shared_clades,
    
    rooted_clade_distance,
    
    prop_main_clades_shared,
    prop_alt_clades_shared,
    
    clade_jaccard,
    
    rooted_identical
    
  ),
  
  stringsAsFactors = FALSE
)


print(
  congruence_table,
  row.names = FALSE
)


# ==============================================================================
# 6. SALVAR
# ==============================================================================

output_file <- file.path(
  PATHS$tables_supp,
  "Tree_topological_congruence.csv"
)


write_csv_utf8(
  congruence_table,
  output_file
)


cat("\n")
cat("Tabela salva em:\n")
cat(output_file, "\n")


# ==============================================================================
# 7. RESUMO NO CONSOLE
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("RESUMO\n")
cat("------------------------------------------------------------\n")

cat(
  "Tips:",
  ape::Ntip(main_tree),
  "\n"
)

cat(
  "RF absoluta:",
  RF_absolute,
  "\n"
)

cat(
  "RF normalizada:",
  sprintf("%.4f", RF_normalized),
  "\n"
)

cat(
  "Clados enraizados compartilhados:",
  n_shared_clades,
  "/",
  n_clades_main,
  "da arvore consenso (",
  sprintf(
    "%.2f%%",
    100 * prop_main_clades_shared
  ),
  ")\n",
  sep = ""
)

cat(
  "Clados enraizados compartilhados:",
  n_shared_clades,
  "/",
  n_clades_alt,
  "da arvore ML (",
  sprintf(
    "%.2f%%",
    100 * prop_alt_clades_shared
  ),
  ")\n",
  sep = ""
)

cat(
  "Jaccard dos clados:",
  sprintf("%.4f", clade_jaccard),
  "\n"
)

cat(
  "Topologias enraizadas identicas:",
  rooted_identical,
  "\n"
)

cat("------------------------------------------------------------\n")
cat("FIM\n")