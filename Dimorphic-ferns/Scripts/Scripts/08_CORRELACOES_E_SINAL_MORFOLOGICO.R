# ==============================================================================
# 08_CORRELACOES_E_SINAL_MORFOLOGICO.R
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
#   07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R
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
#   A estatistica D usa os contrastes de dimorfismo definidos na amostra apropriada.
#   Nos testes de Pagel entre padrao de dimorfismo e componentes morfologicos,
#   comparamos apenas Hemidimorphic vs Holodimorphic; as monomorficas sao excluidas
#   desse contraste porque, por definicao, nao podem apresentar os componentes de
#   diferenciacao entre frondes.
#
# QUANDO TERMINAR:
#   Depois, rode 09_ANALISES_DESCRITIVAS_E_EXPLORATORIAS.R.
# ------------------------------------------------------------------------------
# PERGUNTAS:
# Os componentes morfologicos evoluem de forma correlacionada? Entre as especies
# dimorficas, a diferenca entre hemidimorfia e holodimorfia esta historicamente
# associada a certos componentes? Essas solucoes apresentam sinal filogenetico?
#
# ANALISES:
# Testes de Pagel para evolucao correlacionada + estatistica D.
#
# PARA QUE SERVE:
# Testa associacao historica entre caracteres e avalia o grau de agrupamento
# filogenetico dos componentes morfologicos.
#
# CUIDADO:
# Evolucao correlacionada indica associacao historica, nao causalidade.
#
# O QUE OLHAR:
# Delta AIC em favor do modelo dependente, P, P_FDR e estatistica D.
#
# DEPENDENCIAS:
# 07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R
#
# LE:
# Scripts_/_Objetos_R/resultados_centrais_morfologia.rds
#
# OBJETO SALVO:
# Scripts_/_Objetos_R/resultados_evolucao_correlacionada.rds
#
# TABELAS GERADAS NESTE SCRIPT:
# TableS19_Pagel_Correlacionada.csv         x
# TableS20. Sinal filogenetico D da morfologia.csv
#
# FIGURAS GERADAS NESTE SCRIPT:
# FigS17. Evolucao correlacionada - teste de Pagel
# FigS18. Sinal filogenetico D da morfologia
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
required_packages <- c("ape", "caper", "dplyr", "ggplot2", "openxlsx", "phytools", "scales", "tidyr", "viridisLite")
check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# CONFIGURACAO E FUNCOES EXCLUSIVAS DO SCRIPT 08
# ============================================================================== 
# Este script NAO contem Mk nem SIMMAP. Ele recebe os dados morfologicos prontos
# do Script 07 e executa apenas Pagel + estatistica D dos componentes derivados.

MORPH_PATHS <- list(
  logs = PATHS$logs,
  data = PATHS$data,
  tables = PATHS$tables_supp,
  figures = PATHS$figures_supp,
  objects = PATHS$objects
)
MORPH_LOG <- file.path(MORPH_PATHS$logs, "execucao_morfologia_correlacoes.log")
quick_mode <- is_true(Sys.getenv("LEAF_DIMORPHISM_QUICK", "false"))
MORPH_CONFIG <- list(
  seed = 20260804L,
  quick_mode = quick_mode,
  reuse_pagel = is_true(Sys.getenv("LEAF_DIMORPHISM_REUSE_PAGEL", "false")),
  pagel_method = "fitDiscrete",
  n_permutations_D = if (quick_mode) 100L else 5000L,
  min_binary_group = 5L
)
set.seed(MORPH_CONFIG$seed)

# Rotulos morfologicos usados apenas para traduzir os componentes testados.
MORPH_LABELS <- list(
  Arquitetura_Lamina = ARCHITECTURE_LABELS,
  Variacao_Morfologica = VARIATION_LABELS,
  Relacao_Tamanho = SIZE_LABELS
)

# As figuras herdam os mesmos rotulos canonicos definidos no Script 00.
# Eventuais abreviacoes devem ser aplicadas apenas no momento da plotagem.
ARCHITECTURE_LABELS_FIG08 <- ARCHITECTURE_LABELS
VARIATION_LABELS_FIG08 <- VARIATION_LABELS
SIZE_LABELS_FIG08 <- SIZE_LABELS

MORPH_LABELS_FIG08 <- list(
  Arquitetura_Lamina = ARCHITECTURE_LABELS_FIG08,
  Variacao_Morfologica = VARIATION_LABELS_FIG08,
  Relacao_Tamanho = SIZE_LABELS_FIG08
)

morph_log <- function(..., level = "INFO") {
  txt <- paste(..., collapse = "")
  line <- sprintf("[%s] [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, txt)
  cat(line, "\n")
  cat(line, "\n", file = windows_safe_path(MORPH_LOG), append = TRUE)
}

morph_write_csv <- function(x, filename) {
  path <- file.path(MORPH_PATHS$tables, filename)
  write_csv_utf8(x, path)
  invisible(path)
}

morph_save_plot <- function(plot, stem, width = 10, height = 7,
                            directory = MORPH_PATHS$figures) {
  save_plot_dual(plot, stem, width, height, directory = directory)
}

morph_add_binary_components <- function(data) {
  out <- data
  
  # Codigo composto:
  # 1 Forma
  # 2 Lamina
  # 3 Peciolo
  # 4 Forma + Lamina
  # 5 Forma + Peciolo
  # 6 Lamina + Peciolo
  # 7 Forma + Lamina + Peciolo
  v <- out$Variacao_Morfologica
  
  # Monomorficas possuem Variacao_Morfologica = 0 e entram como ausencia (0)
  # dos tres componentes. Apenas dados realmente ausentes permanecem como NA.
  out$Mudanca_forma <- ifelse(
    is.na(v), NA_integer_,
    as.integer(v %in% c(1L, 4L, 5L, 7L))
  )
  out$Mudanca_lamina <- ifelse(
    is.na(v), NA_integer_,
    as.integer(v %in% c(2L, 4L, 6L, 7L))
  )
  out$Mudanca_peciolo <- ifelse(
    is.na(v), NA_integer_,
    as.integer(v %in% c(3L, 5L, 6L, 7L))
  )
  
  # Contrastes binarios do padrao de dimorfismo.
  # Os tres primeiros permanecem disponiveis para a estatistica D e outras
  # analises univariadas. O contraste Hemi_vs_Holo e usado especificamente
  # nos testes de Pagel com os componentes morfologicos.
  p <- out$Padrao_Dimorfismo
  
  # Qualquer dimorfismo vs monomorfia:
  # 0 = monomorfa; 1 = hemidimorfa + holodimorfa.
  out$Dimorfismo_qualquer <- ifelse(
    is.na(p),
    NA_integer_,
    as.integer(p %in% c(1L, 2L))
  )
  
  # Hemidimorfia vs monomorfia:
  # 0 = monomorfa; 1 = hemidimorfa; holodimorfas = NA.
  out$Hemidimorfia_vs_Monomorfia <- ifelse(
    is.na(p) | p == 2L,
    NA_integer_,
    as.integer(p == 1L)
  )
  
  # Holodimorfia vs monomorfia:
  # 0 = monomorfa; 1 = holodimorfa; hemidimorfas = NA.
  out$Holodimorfia_vs_Monomorfia <- ifelse(
    is.na(p) | p == 1L,
    NA_integer_,
    as.integer(p == 2L)
  )
  
  # Hemidimorfia vs holodimorfia, usado no Pagel:
  # 0 = hemidimorfa; 1 = holodimorfa; monomorfas = NA.
  out$Hemidimorfia_vs_Holodimorfia <- ifelse(
    is.na(p) | p == 0L,
    NA_integer_,
    as.integer(p == 2L)
  )
  
  out
}

# ------------------------------------------------------------------------------
# 11. TESTE DE PAGEL PARA EVOLUCAO CORRELACIONADA
# ------------------------------------------------------------------------------

morph_pagel_one <- function(tree, data, x_name, y_name, family_name) {
  d <- data[, c("species", x_name, y_name), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  
  if (nrow(d) < 10L) {
    return(data.frame(
      Familia = family_name, Metodo = MORPH_CONFIG$pagel_method,
      X = x_name, Y = y_name, N = nrow(d),
      AIC_independente = NA_real_, AIC_dependente = NA_real_,
      Delta_AIC_favor_dependente = NA_real_, LR = NA_real_, P = NA_real_,
      Status = "Ignorado: N insuficiente"
    ))
  }
  
  tx <- table(d[[x_name]])
  ty <- table(d[[y_name]])
  
  if (length(tx) < 2L || length(ty) < 2L ||
      min(tx) < MORPH_CONFIG$min_binary_group ||
      min(ty) < MORPH_CONFIG$min_binary_group) {
    return(data.frame(
      Familia = family_name, Metodo = MORPH_CONFIG$pagel_method,
      X = x_name, Y = y_name, N = nrow(d),
      AIC_independente = NA_real_, AIC_dependente = NA_real_,
      Delta_AIC_favor_dependente = NA_real_, LR = NA_real_, P = NA_real_,
      Status = "Ignorado: estado binario raro"
    ))
  }
  
  joint <- table(d[[x_name]], d[[y_name]])
  if (sum(joint > 0) < 4L) {
    return(data.frame(
      Familia = family_name, Metodo = MORPH_CONFIG$pagel_method,
      X = x_name, Y = y_name, N = nrow(d),
      AIC_independente = NA_real_, AIC_dependente = NA_real_,
      Delta_AIC_favor_dependente = NA_real_, LR = NA_real_, P = NA_real_,
      Status = "Ignorado: nem todas as combinacoes 0/1 estao representadas"
    ))
  }
  
  tr <- ape::keep.tip(tree, d$species)
  d <- d[match(tr$tip.label, d$species), ]
  
  x <- setNames(as.integer(d[[x_name]]), d$species)
  y <- setNames(as.integer(d[[y_name]]), d$species)
  
  cache_path <- file.path(
    MORPH_PATHS$objects,
    paste0(
      "Pagel_", toupper(MORPH_CONFIG$pagel_method), "_",
      family_name, "_", x_name, "_", y_name, ".rds"
    )
  )
  
  cache_signature <- list(
    method = MORPH_CONFIG$pagel_method,
    model = "ARD",
    dep.var = "xy",
    tip.label = as.character(tr$tip.label),
    edge = unclass(tr$edge),
    edge.length = as.numeric(tr$edge.length),
    Nnode = as.integer(tr$Nnode),
    X = as.integer(x[tr$tip.label]),
    Y = as.integer(y[tr$tip.label]),
    phytools = as.character(utils::packageVersion("phytools"))
  )

  fit <- NULL
  if (MORPH_CONFIG$reuse_pagel && file.exists(cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cached) &&
        !is.null(cached$signature) &&
        identical(cached$signature, cache_signature) &&
        !is.null(cached$fit)) {
      morph_log("Reutilizando teste de Pagel em cache validado: ", x_name, " x ", y_name)
      fit <- cached$fit
    } else {
      morph_log("Cache de Pagel ignorado por incompatibilidade: ", x_name, " x ", y_name, level = "AVISO")
    }
  }

  if (is.null(fit)) {
    fit <- tryCatch(
      phytools::fitPagel(
        tr, x, y,
        method = MORPH_CONFIG$pagel_method,
        model = "ARD",
        dep.var = "xy"
      ),
      error = function(e) e
    )
  }
  
  if (inherits(fit, "error")) {
    return(data.frame(
      Familia = family_name, Metodo = MORPH_CONFIG$pagel_method,
      X = x_name, Y = y_name, N = nrow(d),
      AIC_independente = NA_real_, AIC_dependente = NA_real_,
      Delta_AIC_favor_dependente = NA_real_, LR = NA_real_, P = NA_real_,
      Status = paste0("Falhou: ", conditionMessage(fit))
    ))
  }
  
  saveRDS(
    list(signature = cache_signature, fit = fit),
    windows_safe_path(cache_path),
    compress = "xz"
  )
  
  data.frame(
    Familia = family_name,
    Metodo = MORPH_CONFIG$pagel_method,
    X = x_name,
    Y = y_name,
    N = nrow(d),
    AIC_independente = fit$independent.AIC,
    AIC_dependente = fit$dependent.AIC,
    Delta_AIC_favor_dependente = fit$independent.AIC - fit$dependent.AIC,
    LR = fit$lik.ratio,
    P = fit$P,
    Status = "OK"
  )
}

# Executa uma familia de testes de Pagel para pares binarios definidos pelo Script 08.
morph_run_pagel_family <- function(tree, data, pairs, family_name) {
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    morph_log(
      "Pagel: ", family_name, " | ",
      pairs$X[i], " x ", pairs$Y[i], "..."
    )
    morph_pagel_one(
      tree, data, pairs$X[i], pairs$Y[i], family_name
    )
  })
  out <- dplyr::bind_rows(rows)
  out$P_FDR <- NA_real_
  ok <- out$Status == "OK" & is.finite(out$P)
  if (any(ok)) out$P_FDR[ok] <- stats::p.adjust(out$P[ok], method = "BH")
  out$Evidencia_FDR <- !is.na(out$P_FDR) & out$P_FDR < 0.05
  out
}

# ------------------------------------------------------------------------------
# 12. ESTATISTICA D PARA CARACTERES BINARIOS
# ------------------------------------------------------------------------------

morph_phylo_D_one <- function(tree, data, trait) {
  d <- data[, c("species", trait), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  tab <- table(d[[trait]])
  
  if (length(tab) < 2L || min(tab) < MORPH_CONFIG$min_binary_group) {
    return(data.frame(
      Caracter = trait, N = nrow(d), D = NA_real_,
      P_contra_aleatorio_D1 = NA_real_,
      P_contra_browniano_D0 = NA_real_,
      Permutacoes = MORPH_CONFIG$n_permutations_D,
      Status = "Ignorado: estado raro"
    ))
  }
  
  tr <- ape::keep.tip(tree, d$species)
  tr$node.label <- NULL
  d <- d[match(tr$tip.label, d$species), ]
  
  signal_data <- data.frame(
    species = d$species,
    trait = as.integer(d[[trait]])
  )
  
  ans <- tryCatch(
    caper::phylo.d(
      data = signal_data,
      phy = tr,
      names.col = species,
      binvar = trait,
      permut = MORPH_CONFIG$n_permutations_D
    ),
    error = function(e) e
  )
  
  if (inherits(ans, "error")) {
    return(data.frame(
      Caracter = trait, N = nrow(d), D = NA_real_,
      P_contra_aleatorio_D1 = NA_real_,
      P_contra_browniano_D0 = NA_real_,
      Permutacoes = MORPH_CONFIG$n_permutations_D,
      Status = paste0("Falhou: ", conditionMessage(ans))
    ))
  }
  
  saveRDS(
    ans,
    windows_safe_path(
      file.path(MORPH_PATHS$objects, paste0("D_", trait, ".rds"))
    )
  )
  
  data.frame(
    Caracter = trait,
    N = nrow(d),
    D = ans$DEstimate,
    P_contra_aleatorio_D1 = ans$Pval1,
    P_contra_browniano_D0 = ans$Pval0,
    Permutacoes = MORPH_CONFIG$n_permutations_D,
    Status = "OK"
  )
}

# ------------------------------------------------------------------------------
# 13. PALETAS E AUXILIARES DE FIGURA
# ------------------------------------------------------------------------------



# Utilidades de nomes, workbook e tema usadas apenas para apresentar os resultados
# desta analise. Elas ficam aqui para o Script 08 poder ser lido isoladamente.
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
    "Hemidimorfia_vs_Monomorfia" = "Hemidimorphic vs monomorphic",
    "Holodimorfia_vs_Monomorfia" = "Holodimorphic vs monomorphic",
    "Dimorfismo_qualquer" = "Any dimorphism vs monomorphism",
    "Hemidimorfia_vs_Holodimorfia" = "Hemidimorphic vs holodimorphic"
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

# FUNCAO CENTRAL: testa se componentes morfologicos evoluem de forma correlacionada
# e calcula o sinal filogenetico dos caracteres binarios derivados.


# ==============================================================================
# FUNCOES DE ROTULOS E FIGURAS DA ANALISE 08
# ==============================================================================
pretty_name <- function(x) {
  out <- morph_pretty_binary(x)
  
  arch_idx <- grepl("^Arquitetura_", x)
  if (any(arch_idx)) {
    codes <- sub("^Arquitetura_", "", x[arch_idx])
    labels <- unname(MORPH_LABELS_FIG08$Arquitetura_Lamina[codes])
    labels[is.na(labels)] <- codes[is.na(labels)]
    out[arch_idx] <- paste0("Architecture: ", labels)
  }
  
  size_idx <- grepl("^Tamanho_", x)
  if (any(size_idx)) {
    codes <- sub("^Tamanho_", "", x[size_idx])
    labels <- unname(MORPH_LABELS_FIG08$Relacao_Tamanho[codes])
    labels[is.na(labels)] <- codes[is.na(labels)]
    out[size_idx] <- paste0("Size ratio: ", labels)
  }
  
  var_idx <- grepl("^Variacao_", x)
  if (any(var_idx)) {
    codes <- sub("^Variacao_", "", x[var_idx])
    labels <- unname(MORPH_LABELS_FIG08$Variacao_Morfologica[codes])
    labels[is.na(labels)] <- codes[is.na(labels)]
    out[var_idx] <- paste0("Variation: ", labels)
  }
  
  out
}

status_to_english <- function(x) {
  out <- as.character(x)
  out[out == "OK"] <- "OK"
  out[grepl("N insuficiente", out, fixed = TRUE)] <- "Not estimated: insufficient sample size"
  out[grepl("estado binario raro", out, fixed = TRUE)] <- "Not estimated: rare binary state"
  out[grepl("nem todas as combinacoes 0/1", out, fixed = TRUE)] <- "Not estimated: incomplete 0/1 combinations"
  out[grepl("estado raro", out, fixed = TRUE)] <- "Not estimated: rare state"
  fail_idx <- grepl("^Falhou:", out)
  out[fail_idx] <- sub("^Falhou:", "Failed:", out[fail_idx])
  out
}

format_p_fig08 <- function(x) {
  ifelse(
    is.na(x),
    "NA",
    ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )
}

plotmath_p_pair_fig08 <- function(p_raw, p_fdr) {
  one_value <- function(x) {
    if (is.na(x)) return("== NA")
    if (x < 0.001) return("< 0.001")
    paste0("== ", sprintf("%.3f", x))
  }
  mapply(
    function(a, b) {
      paste0(
        "italic(P) ", one_value(a),
        "~~~italic(P)[FDR] ", one_value(b)
      )
    },
    p_raw,
    p_fdr,
    USE.NAMES = FALSE
  )
}


plotmath_p_single_fig08 <- function(p_raw) {
  one_value <- function(x) {
    if (is.na(x)) return("== NA")
    if (x < 0.001) return("< 0.001")
    paste0("== ", sprintf("%.3f", x))
  }
  vapply(
    p_raw,
    function(a) paste0("italic(P) ", one_value(a)),
    character(1)
  )
}

plotmath_D_p_pair_fig08 <- function(p_d0, p_d1) {
  one_value <- function(x) {
    if (is.na(x)) return("== NA")
    if (x < 0.001) return("< 0.001")
    paste0("== ", sprintf("%.3f", x))
  }
  mapply(
    function(a, b) {
      paste0(
        "italic(P)[D==0] ", one_value(a),
        "~~~italic(P)[D==1] ", one_value(b)
      )
    },
    p_d0,
    p_d1,
    USE.NAMES = FALSE
  )
}

pagel_test_label_fig08 <- function(x_name, y_name) {
  paste(pretty_name(x_name), "×", pretty_name(y_name))
}


compact_name_fig08 <- function(x) {
  out <- pretty_name(x)
  
  # The facet already identifies the character group, so repeated prefixes are
  # removed to keep Figure S18 compact and publication-friendly.
  out <- sub("^Architecture: ", "", out)
  out <- sub("^Variation: ", "", out)
  out <- sub("^Size ratio: ", "", out)
  
  out <- sub("^Change in frond shape$", "Frond shape", out)
  out <- sub("^Change in lamina size$", "Lamina size", out)
  out <- sub("^Change in petiole size$", "Petiole size", out)
  
  out
}

sig_code_fig08 <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(
      p < 0.001,
      "***",
      ifelse(
        p < 0.01,
        "**",
        ifelse(p < 0.05, "*", "ns")
      )
    )
  )
}

# ------------------------------------------------------------------------------
# FIGURE S17 - PAGEL TESTS
# ------------------------------------------------------------------------------
# IMPORTANT:
# For Pagel tests involving the dimorphism pattern, only dimorphic species are
# compared: 0 = Hemidimorphic; 1 = Holodimorphic; Monomorphic = NA.
# Monomorphic species remain in the broader project and in the planned D contrasts,
# but are not included in this particular correlated-evolution contrast.
plot_pagel_morph <- function(
    pagel,
    stem = "FigS17. Evolucao correlacionada - teste de Pagel",
    width = 13,
    height = 10.5
) {
  if (nrow(pagel) == 0L) return(invisible(NULL))
  
  d <- pagel
  d$Teste <- pagel_test_label_fig08(d$X, d$Y)
  
  d$Familia_nome <- dplyr::recode(
    d$Familia,
    Componentes_morfologicos = "Coevolution among morphological components",
    Hemi_vs_Holo_componentes = "Hemidimorphy vs holodimorphy × morphological components",
    .default = d$Familia
  )
  
  d$Estimable <- d$Status == "OK" & is.finite(d$Delta_AIC_favor_dependente)
  d$Status_en <- status_to_english(d$Status)
  d$P_label <- plotmath_p_pair_fig08(d$P, d$P_FDR)
  d$P_bruto_label <- plotmath_p_single_fig08(d$P)
  
  family_order <- c(
    "Coevolution among morphological components",
    "Hemidimorphy vs holodimorphy × morphological components"
  )
  d$Familia_nome <- factor(d$Familia_nome, levels = family_order)
  
  test_order <- c(
    "Change in frond shape × Change in lamina size",
    "Change in frond shape × Change in petiole size",
    "Change in lamina size × Change in petiole size",
    "Hemidimorphic vs holodimorphic × Change in frond shape",
    "Hemidimorphic vs holodimorphic × Change in lamina size",
    "Hemidimorphic vs holodimorphic × Change in petiole size"
  )
  d$Teste <- factor(d$Teste, levels = rev(test_order[test_order %in% d$Teste]))
  
  d_ok <- d[d$Estimable, , drop = FALSE]
  d_bad <- d[!d$Estimable, , drop = FALSE]
  
  p <- ggplot2::ggplot(d, ggplot2::aes(y = Teste)) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = 2,
      linewidth = 0.65,
      color = "#6B7280"
    ) +
    ggplot2::geom_vline(
      xintercept = 2,
      linetype = 3,
      linewidth = 0.65,
      color = "#9CA3AF"
    ) +
    ggplot2::geom_segment(
      data = d_ok,
      ggplot2::aes(
        x = 0,
        xend = Delta_AIC_favor_dependente,
        yend = Teste,
        color = Evidencia_FDR
      ),
      linewidth = 0.95,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = d_ok,
      ggplot2::aes(
        x = Delta_AIC_favor_dependente,
        color = Evidencia_FDR
      ),
      size = 3.8
    ) +
    ggplot2::geom_text(
      data = d_ok,
      ggplot2::aes(
        x = Delta_AIC_favor_dependente,
        label = P_bruto_label
      ),
      nudge_y = 0.25,
      vjust = 0,
      size = 3.15,
      color = "#263238",
      family = "sans",
      parse = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = d_ok,
      ggplot2::aes(
        x = Inf,
        label = P_label
      ),
      hjust = -0.05,
      size = 3.20,
      color = "#263238",
      family = "sans",
      parse = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = d_bad,
      ggplot2::aes(
        x = 0,
        label = Status_en
      ),
      hjust = -0.04,
      size = 3.10,
      color = "#7A8791",
      fontface = "italic",
      show.legend = FALSE
    ) +
    ggplot2::facet_wrap(
      ~Familia_nome,
      scales = "free_y",
      ncol = 1
    ) +
    ggplot2::scale_color_manual(
      values = c("FALSE" = "#6B7280", "TRUE" = "#B2182B"),
      labels = c("FALSE" = "No", "TRUE" = "Yes"),
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.06, 0.33))
    ) +
    ggplot2::labs(
      title = "Evidence for correlated evolution",
      subtitle = "Positive ΔAIC values favor the dependent model; ΔAIC = 2 indicates initial support",
      x = "ΔAIC = AIC independent − AIC dependent",
      y = NULL,
      color = expression(italic(P)[FDR] < 0.05)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#4B5563"),
      strip.text = ggplot2::element_text(face = "bold", size = 11.5),
      axis.text.y = ggplot2::element_text(size = 10.2, color = "#1F2933"),
      axis.text.x = ggplot2::element_text(size = 10.2),
      axis.title.x = ggplot2::element_text(size = 11.3, face = "bold"),
      legend.position = "bottom",
      panel.spacing.y = grid::unit(0.45, "cm"),
      plot.margin = ggplot2::margin(10, 205, 10, 10)
    )
  
  morph_save_plot(p, stem, width, height)
}

# ------------------------------------------------------------------------------
# FIGURE S18 - PHYLOGENETIC SIGNAL D
# ------------------------------------------------------------------------------
plot_D_morph <- function(
    signal,
    stem = "FigS18. Sinal filogenetico D da morfologia",
    width = 15.0,
    height = 12.5
) {
  if (nrow(signal) == 0L) return(invisible(NULL))
  
  d <- signal

  # Limites dinamicos do eixo D. A estatistica D pode assumir valores < -1 ou > 1;
  # portanto, limites fixos removeriam pontos/segmentos validos da figura.
  d_values_scale <- d$D[is.finite(d$D)]
  if (length(d_values_scale) == 0L) d_values_scale <- c(0, 1)
  raw_min_D <- min(c(0, 1, d_values_scale))
  raw_max_D <- max(c(0, 1, d_values_scale))
  span_D <- max(raw_max_D - raw_min_D, 1)
  x_lower_D <- min(-0.10, raw_min_D - 0.08 * span_D)
  x_upper_D <- max(1.05, raw_max_D + 0.08 * span_D)
  
  # ---------------------------------------------------------------------------
  # ROTULOS DOS CARACTERES
  # ---------------------------------------------------------------------------
  d$Caracter_nome <- compact_name_fig08(d$Caracter)
  
  # Os tres contrastes do padrao de dimorfismo permanecem nomeados individualmente.
  d$Estimable <- d$Status == "OK" & is.finite(d$D)
  d$Status_en <- status_to_english(d$Status)
  
  # ---------------------------------------------------------------------------
  # SIGNIFICANCIA DOS TESTES CONTRA D = 0 E D = 1
  # ---------------------------------------------------------------------------
  d$Sig_D0 <- sig_code_fig08(d$P_contra_browniano_D0)
  d$Sig_D1 <- sig_code_fig08(d$P_contra_aleatorio_D1)
  
  # D e P em italico.
  d$Right_label <- ifelse(
    d$Estimable,
    paste0(
      "italic(D) == ", sprintf("%.2f", d$D),
      "~~~italic(P)[italic(D)==0]~'='~'", d$Sig_D0, "'",
      "~~~italic(P)[italic(D)==1]~'='~'", d$Sig_D1, "'"
    ),
    NA_character_
  )
  
  # ---------------------------------------------------------------------------
  # GRUPOS
  # ---------------------------------------------------------------------------
  d$Group <- dplyr::case_when(
    d$Caracter %in% c(
      "Hemidimorfia_vs_Monomorfia",
      "Holodimorfia_vs_Monomorfia",
      "Dimorfismo_qualquer"
    ) ~ "Dimorphism contrasts",
    d$Caracter %in% c("Mudanca_forma", "Mudanca_lamina", "Mudanca_peciolo") ~
      "Morphological components",
    grepl("^Arquitetura_", d$Caracter) ~ "Fertile-frond architecture",
    grepl("^Variacao_", d$Caracter) ~ "Morphological variation",
    grepl("^Tamanho_", d$Caracter) ~ "Size ratio",
    TRUE ~ "Other"
  )
  
  group_order <- c(
    "Dimorphism contrasts",
    "Morphological components",
    "Fertile-frond architecture",
    "Morphological variation",
    "Size ratio",
    "Other"
  )
  
  group_order <- group_order[group_order %in% unique(d$Group)]
  d$Group <- factor(d$Group, levels = group_order)
  
  # ==========================================================================
  # POSICOES VERTICAIS MANUAIS
  # ==========================================================================
  # Nao usamos facets. Cada caracter recebe exatamente o mesmo espacamento.
  # O titulo de cada grupo ocupa sua propria linha acima do conjunto.
  # ==========================================================================
  d$Y_plot <- NA_real_
  title_rows <- list()
  
  cursor <- 0
  title_to_first_row <- 0.85
  row_step <- 1.00
  group_gap <- 0.75
  
  for (g in group_order) {
    
    idx <- which(as.character(d$Group) == g)
    if (length(idx) == 0L) next
    
    bad_idx <- idx[!d$Estimable[idx]]
    good_idx <- idx[d$Estimable[idx]]
    
    if (length(good_idx) > 0L) {
      good_idx <- good_idx[order(d$D[good_idx], na.last = TRUE)]
    }
    
    display_idx <- c(bad_idx, good_idx)
    
    # Linha reservada ao titulo do grupo.
    title_rows[[length(title_rows) + 1L]] <- data.frame(
      Group_title = g,
      Y_raw = cursor,
      stringsAsFactors = FALSE
    )
    
    cursor <- cursor + title_to_first_row
    
    # Linhas dos caracteres, todas com a mesma distancia.
    for (j in seq_along(display_idx)) {
      d$Y_plot[display_idx[j]] <- cursor
      cursor <- cursor + row_step
    }
    
    cursor <- cursor + group_gap
  }
  
  title_data <- dplyr::bind_rows(title_rows)
  
  max_position <- max(
    c(d$Y_plot, title_data$Y_raw),
    na.rm = TRUE
  )
  
  # Inverte o eixo para o primeiro grupo ficar no topo.
  d$Y_plot <- max_position - d$Y_plot
  title_data$Y_plot <- max_position - title_data$Y_raw
  
  # ---------------------------------------------------------------------------
  # DADOS PARA AS CAMADAS
  # ---------------------------------------------------------------------------
  axis_data <- d[order(d$Y_plot), , drop = FALSE]
  d_ok <- d[d$Estimable, , drop = FALSE]
  d_bad <- d[!d$Estimable, , drop = FALSE]
  
  # ==========================================================================
  # FIGURA
  # ==========================================================================
  p <- ggplot2::ggplot() +
    
    # Referencia D = 0.
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = 2,
      linewidth = 0.70,
      color = "#2563EB"
    ) +
    
    # Referencia D = 1.
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = 2,
      linewidth = 0.70,
      color = "#B91C1C"
    ) +
    
    # Linha entre D = 0 e o valor observado.
    ggplot2::geom_segment(
      data = d_ok,
      ggplot2::aes(
        x = 0,
        xend = D,
        y = Y_plot,
        yend = Y_plot
      ),
      linewidth = 0.95,
      color = "#6B7280",
      lineend = "round"
    ) +
    
    # Pontos.
    ggplot2::geom_point(
      data = d_ok,
      ggplot2::aes(
        x = D,
        y = Y_plot
      ),
      size = 4.0,
      color = "#111827"
    ) +
    
    # D + P(D=0) + P(D=1) alinhados com o caracter.
    ggplot2::geom_text(
      data = d_ok,
      ggplot2::aes(
        x = Inf,
        y = Y_plot,
        label = Right_label
      ),
      hjust = -0.04,
      vjust = 0.5,
      size = 3.05,
      color = "#263238",
      family = "sans",
      parse = TRUE,
      show.legend = FALSE
    ) +
    
    # Estados nao estimaveis.
    ggplot2::geom_text(
      data = d_bad,
      ggplot2::aes(
        x = Inf,
        y = Y_plot,
        label = Status_en
      ),
      hjust = -0.04,
      vjust = 0.5,
      size = 3.05,
      color = "#7A8791",
      fontface = "italic",
      show.legend = FALSE
    ) +
    
    # Titulos horizontais acima de cada grupo.
    ggplot2::geom_text(
      data = title_data,
      ggplot2::aes(
        x = -Inf,
        y = Y_plot,
        label = Group_title
      ),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 4.15,
      hjust = 1.05,
      vjust = 0.5,
      color = "#111827"
    ) +
    
    # Eixo Y.
    ggplot2::scale_y_continuous(
      breaks = axis_data$Y_plot,
      labels = axis_data$Caracter_nome,
      expand = ggplot2::expansion(add = c(0.25, 0.45))
    ) +
    
    # Eixo X.
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 6),
      limits = c(x_lower_D, x_upper_D),
      expand = ggplot2::expansion(mult = c(0.03, 0.22))
    ) +
    
    # Titulos e eixos.
    ggplot2::labs(
      title = "Phylogenetic signal of morphological components",
      subtitle = expression(
        "Dashed lines mark " * italic(D) * " = 0 and " * italic(D) * " = 1"
      ),
      x = expression(italic(D) * " statistic"),
      y = NULL,
      caption = paste0(
        "Significance codes: ns >= 0.05; * < 0.05; ** < 0.01; *** < 0.001. ",
        "Exact values are reported in Table S20."
      )
    ) +
    
    ggplot2::coord_cartesian(clip = "off") +
    
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      
      # Titulo geral centralizado.
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 15.5,
        hjust = 0.5
      ),
      
      # Subtitulo geral centralizado.
      plot.subtitle = ggplot2::element_text(
        size = 10.8,
        color = "#4B5563",
        hjust = 0.5,
        margin = ggplot2::margin(b = 12)
      ),
      
      plot.caption = ggplot2::element_text(
        size = 8.9,
        color = "#5E6A73",
        hjust = 0.5,
        margin = ggplot2::margin(t = 10)
      ),
      
      # Nomes dos caracteres.
      axis.text.y = ggplot2::element_text(
        size = 9.8,
        color = "#1F2933",
        margin = ggplot2::margin(r = 8)
      ),
      
      axis.text.x = ggplot2::element_text(size = 10.4),
      
      axis.title.x = ggplot2::element_text(
        size = 11.6,
        face = "bold",
        margin = ggplot2::margin(t = 8)
      ),
      
      plot.margin = ggplot2::margin(
        14,
        175,
        18,
        185
      )
    )
  
  morph_save_plot(
    p,
    stem,
    width,
    height
  )
}

# ==============================================================================
# EXECUCAO DA ANALISE 08
# ==============================================================================
console_banner("SCRIPT 08", "Correlacoes evolutivas e sinal filogenetico da morfologia")

# ------------------------------------------------------------------------------
# PREPARACAO
# ------------------------------------------------------------------------------
console_step(1, 5, "Carregando os resultados morfologicos do Script 07")
core <- require_saved_rds(
  "resultados_centrais_morfologia.rds",
  "07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R"
)
prepared01 <- require_saved_rds(
  "01_dados_e_filogenia.rds",
  "01_PREPARAR_DADOS_E_FILOGENIA.R"
)

# O Script 08 so prossegue se o objeto do 07 vier exatamente da amostra/filogenia
# atualmente salva pelo Script 01. Isso impede usar resultados morfologicos antigos.
same_tree_01_07 <-
  identical(as.character(core$tree$tip.label), as.character(prepared01$tree$tip.label)) &&
  identical(unclass(core$tree$edge), unclass(prepared01$tree$edge)) &&
  isTRUE(all.equal(as.numeric(core$tree$edge.length), as.numeric(prepared01$tree$edge.length), tolerance = 0)) &&
  identical(as.integer(core$tree$Nnode), as.integer(prepared01$tree$Nnode))
same_data_01_07 <-
  identical(as.character(core$data$species), as.character(prepared01$data$species)) &&
  identical(as.integer(core$data$Padrao_Dimorfismo), as.integer(prepared01$data$Padrao_Dimorfismo))

if (!same_tree_01_07 || !same_data_01_07) {
  stop(
    "O resultado do Script 07 nao corresponde ao objeto atual do Script 01. ",
    "Rode novamente o Script 07 antes do Script 08.",
    call. = FALSE
  )
}

if (!any(core$data$Padrao_Dimorfismo == 0L, na.rm = TRUE)) {
  stop(
    "O objeto do Script 07 ainda e da versao antiga sem monomorficas. ",
    "Rode novamente 07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R antes do Script 08.",
    call. = FALSE
  )
}

# Mantemos exatamente os mesmos terminais e a mesma arvore do Script 01/07.
# Monomorficas sao codificadas como ausencia (0), nao como dado ausente.
tr <- core$tree
dat <- core$data[match(tr$tip.label, core$data$species), , drop = FALSE]
if (!identical(tr$tip.label, dat$species)) {
  stop("Falha ao alinhar a arvore completa e os dados no Script 08.", call. = FALSE)
}
dat <- morph_add_binary_components(dat)

# Criamos variaveis binarias um-estado-contra-os-demais para diagnosticos e D.
architecture_states <- sort(unique(dat$Arquitetura_Lamina)); architecture_states <- architecture_states[architecture_states != 0L]
for (s in architecture_states) {
  nm <- paste0("Arquitetura_", s)
  dat[[nm]] <- ifelse(
    is.na(dat$Arquitetura_Lamina),
    NA_integer_,
    as.integer(dat$Arquitetura_Lamina == s)
  )
}
size_states <- sort(unique(dat$Relacao_Tamanho)); size_states <- size_states[size_states != 0L]
for (s in size_states) {
  nm <- paste0("Tamanho_", s)
  dat[[nm]] <- ifelse(
    is.na(dat$Relacao_Tamanho),
    NA_integer_,
    as.integer(dat$Relacao_Tamanho == s)
  )
}
variation_states <- sort(unique(dat$Variacao_Morfologica)); variation_states <- variation_states[variation_states != 0L]
for (s in variation_states) {
  nm <- paste0("Variacao_", s)
  dat[[nm]] <- ifelse(
    is.na(dat$Variacao_Morfologica),
    NA_integer_,
    as.integer(dat$Variacao_Morfologica == s)
  )
}
write_csv_utf8(dat, file.path(MORPH_PATHS$data, "dados_todas_especies_componentes_binarios.csv"))

# ==============================================================================
# ANALISE 08A - TESTE DE PAGEL: EVOLUCAO CORRELACIONADA
# ==============================================================================
console_step(2, 5, "Executando os testes planejados de Pagel")

# 08A.1 - coevolucao entre forma, lamina e peciolo.
pairs_components <- data.frame(
  X = c("Mudanca_forma", "Mudanca_forma", "Mudanca_lamina"),
  Y = c("Mudanca_lamina", "Mudanca_peciolo", "Mudanca_peciolo"),
  stringsAsFactors = FALSE
)
pagel_components <- morph_run_pagel_family(
  tr, dat, pairs_components, "Componentes_morfologicos"
)

# 08A.2 - hemidimorfia vs holodimorfia em relacao a cada componente.
# Monomorficas recebem NA neste contraste e sao excluidas apenas destes tres testes.
pairs_hemi_holo_components <- data.frame(
  X = rep("Hemidimorfia_vs_Holodimorfia", 3),
  Y = c("Mudanca_forma", "Mudanca_lamina", "Mudanca_peciolo"),
  stringsAsFactors = FALSE
)
pagel_hemi_holo_components <- morph_run_pagel_family(
  tr, dat, pairs_hemi_holo_components, "Hemi_vs_Holo_componentes"
)

# Correcao FDR conjunta sobre todos os 6 testes planejados deste script.
pagel_all <- dplyr::bind_rows(
  pagel_components,
  pagel_hemi_holo_components
)
pagel_all$P_FDR <- NA_real_
ok <- pagel_all$Status == "OK" & is.finite(pagel_all$P)
if (any(ok)) pagel_all$P_FDR[ok] <- stats::p.adjust(pagel_all$P[ok], method = "BH")
pagel_all$Evidencia_FDR <- !is.na(pagel_all$P_FDR) & pagel_all$P_FDR < 0.05

if (nrow(pagel_all) > 0L) {
  pagel_all$X_nome <- pretty_name(pagel_all$X)
  pagel_all$Y_nome <- pretty_name(pagel_all$Y)
  pagel_all$Interpretacao_AIC <- dplyr::case_when(
    !is.finite(pagel_all$Delta_AIC_favor_dependente) ~ "Not estimated",
    pagel_all$Delta_AIC_favor_dependente >= 10 ~ "Strong support for the dependent model",
    pagel_all$Delta_AIC_favor_dependente >= 4 ~ "Moderate support for the dependent model",
    pagel_all$Delta_AIC_favor_dependente >= 2 ~ "Some support for the dependent model",
    TRUE ~ "Little or no additional support for the dependent model"
  )
}

# PRINT + TABELA DA ANALISE 08A
cat("\n--- RESULTADOS DOS TESTES DE PAGEL ---\n")
print(pagel_all)
morph_write_csv(pagel_all, "TableS19_Pagel_Correlacionada.csv")

# FIGURA DA ANALISE 08A - FigS17
FIGS17_NOME <- "FigS17. Evolucao correlacionada - teste de Pagel"
FIGS17_LARGURA <- 13
FIGS17_ALTURA <- 10.5
plot_pagel_morph(pagel_all, FIGS17_NOME, FIGS17_LARGURA, FIGS17_ALTURA)

# ==============================================================================
# ANALISE 08B - ESTATISTICA D DOS COMPONENTES MORFOLOGICOS
# ==============================================================================
console_step(3, 5, "Calculando a estatistica D dos componentes")
architecture_columns <- paste0("Arquitetura_", architecture_states)
size_columns <- paste0("Tamanho_", size_states)
variation_columns <- paste0("Variacao_", variation_states)

# IMPORTANT FIX:
# The previous version created binary variables for architecture, size ratio,
# and morphological variation, but only architecture states were included in D.
# We now test all three state sets. Rare states remain in the table/figure with
# an explicit non-estimable status rather than disappearing silently.
signal_traits <- c(
  "Hemidimorfia_vs_Monomorfia",
  "Holodimorfia_vs_Monomorfia",
  "Dimorfismo_qualquer",
  "Mudanca_forma",
  "Mudanca_lamina",
  "Mudanca_peciolo",
  architecture_columns,
  variation_columns,
  size_columns
)
signal_rows <- lapply(signal_traits, function(v) {
  morph_log("Estatistica D: ", v, "...")
  morph_phylo_D_one(tr, dat, v)
})
signal_D <- dplyr::bind_rows(signal_rows)
signal_D$Caracter_nome <- pretty_name(signal_D$Caracter)
signal_D$Interpretacao <- dplyr::case_when(
  signal_D$Status != "OK" ~ status_to_english(signal_D$Status),
  signal_D$P_contra_aleatorio_D1 < 0.05 & signal_D$P_contra_browniano_D0 >= 0.05 ~
    "Clustered; compatible with a Brownian threshold model",
  signal_D$P_contra_aleatorio_D1 < 0.05 & signal_D$P_contra_browniano_D0 < 0.05 & signal_D$D < 0 ~
    "More clustered than expected under a Brownian threshold model",
  signal_D$P_contra_aleatorio_D1 < 0.05 & signal_D$P_contra_browniano_D0 < 0.05 ~
    "Intermediate structure; different from both D = 0 and D = 1",
  TRUE ~ "Not different from an approximately random distribution"
)

# PRINT + TABELA DA ANALISE 08B
cat("\n--- RESULTADOS DA ESTATISTICA D MORFOLOGICA ---\n")
print(signal_D)
morph_write_csv(signal_D, "TableS20. Sinal filogenetico D da morfologia.csv")

# FIGURA DA ANALISE 08B - FigS18
FIGS18_NOME <- "FigS18. Sinal filogenetico D da morfologia"
FIGS18_LARGURA <- 16.5
FIGS18_ALTURA <- 14.5
plot_D_morph(signal_D, FIGS18_NOME, FIGS18_LARGURA, FIGS18_ALTURA)

# ==============================================================================
# SALVAMENTO FINAL DO SCRIPT 08
# ==============================================================================
console_step(4, 5, "Salvando o objeto completo desta analise")
correlated_results <- list(
  data = dat,
  tree = tr,
  source_signature01 = core$source_signature01,
  selection_policy = core$selection_policy,
  Pagel = pagel_all,
  D = signal_D,
  architecture_states = architecture_states,
  size_states = size_states,
  variation_states = variation_states
)
saveRDS(
  correlated_results,
  windows_safe_path(file.path(MORPH_PATHS$objects, "resultados_evolucao_correlacionada.rds")),
  compress = "xz"
)
save_pipeline_rds(correlated_results, "08_correlacoes_sinal_morfologico.rds")

console_step(5, 5, "Gerando workbook desta analise")
morph_create_workbook(
  list(Pagel = pagel_all, Sinal_D = signal_D),
  output_name = "TablesS. Correlacoes e sinal morfologico.xlsx"
)

console_output("resultados_evolucao_correlacionada.rds e 08_correlacoes_sinal_morfologico.rds")
console_output("TableS19, FigS17, TableS20 e FigS18")
log_message("SCRIPT 08 concluido.")
console_done("SCRIPT 08", "09_ANALISES_DESCRITIVAS_E_EXPLORATORIAS.R")
