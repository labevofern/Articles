# ==============================================================================
# 01_PREPARAR_DADOS_E_FILOGENIA.R
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
#   Nenhuma analise anterior.
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
#   Este script deve ser executado primeiro sempre que a matriz mudar.
#
# QUANDO TERMINAR:
#   Depois, rode 02_EVOLUCAO_DIMORFISMO.R.
# ------------------------------------------------------------------------------
# OBJETIVO:
# Preparar a matriz e a FTOL para que todas as analises seguintes usem as mesmas
# especies, os mesmos codigos e a mesma ordem entre dados e arvore.
#
# PARA QUE SERVE:
# Esta etapa nao testa uma hipotese biologica. Ela faz o controle de qualidade,
# valida os codigos, alinha os nomes com a FTOL e salva os objetos processados.
#
# O QUE CONFERIR:
# Numero de especies mantidas/excluidas, codigos invalidos, correcoes de nomes,
# enraizamento e ultrametricidade da arvore.
#
# DEPENDENCIAS:
# Nenhuma analise anterior. Este e o primeiro script a executar.
#
# ENTRADA:
# Tabelas_/Matriz_final_completa.xlsx
#
# OBJETO SALVO:
# Scripts_/_Objetos_R/01_dados_e_filogenia.rds
#
# ARQUIVOS DE CONTROLE GERADOS NESTE SCRIPT:
# Controle de qualidade e amostragem.csv
# Frequencia dos estados de dimorfismo.csv
#
# IMPORTANTE: Table S1 e Table S2 sao reservadas a material externo ao pipeline
# analitico e, portanto, NAO sao criadas por este script.
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
  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) return(dirname(normalizePath(frame$ofile,winslash="/")))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p, winslash = "/")))
  }
  normalizePath(getwd(), winslash = "/")
}
# Descobre automaticamente onde esta a pasta Scripts_.
# Isso permite usar Source no RStudio sem precisar escrever o caminho deste arquivo manualmente.
SCRIPTS_DIR <- get_scripts_dir()
# Carrega as configuracoes e funcoes gerais compartilhadas pelo pipeline.
# IMPORTANTE: o 00 nao executa uma analise; ele apenas fornece ferramentas que este script usa.
source(file.path(SCRIPTS_DIR, "00_CONFIGURACAO_E_FUNCOES.R"), chdir = TRUE)
# Antes de iniciar, conferimos se todos os pacotes necessarios estao instalados.
# Se algum estiver faltando, o script para aqui em vez de falhar no meio da analise.
required_packages <- c("ape", "dplyr", "ftolr", "openxlsx")
check_packages(required_packages)
load_packages(required_packages)


# ==============================================================================
# CONFIGURACAO EXCLUSIVA DO SCRIPT 01
# ============================================================================== 
# Estes parametros pertencem somente a preparacao dos dados/filogenia.
# Mude aqui apenas se quiser alterar o modo rapido ou a tolerancia da arvore.
quick_mode <- is_true(Sys.getenv("LEAF_DIMORPHISM_QUICK", "false"))
CONFIG <- list(
  seed = 20260803L,
  quick_mode = quick_mode,
  quick_tips_per_common_state = 40L,
  ultrametric_tolerance = 1e-4
)

# Grupo externo usado SOMENTE para auditoria da FTOL nesta etapa.
grupo_externo <- c(
  "Anthoceros_angustus", "Marchantia_polymorpha", "Physcomitrium_patens",
  "Amborella_trichopoda", "Acorus_calamus", "Liquidambar_formosana",
  "Torminalis_glaberrima", "Plumbago_auriculata", "Nicotiana_tabacum",
  "Lamprocapnos_spectabilis", "Magnolia_tripetala", "Kadsura_longipedunculata",
  "Trithuria_inconspicua", "Ginkgo_biloba", "Sciadopitys_verticillata"
)

# Antes de qualquer leitura, verifica se a UNICA entrada deste script existe.
# Esta verificacao fica aqui (e nao no 00) porque apenas o Script 01 le a matriz.
if (!file.exists(PATHS$data_file)) {
  stop_pipeline(
    "Nao encontrei Matriz_final_completa.xlsx. O arquivo deve estar em: ",
    PATHS$data_file
  )
}

# Funcoes abaixo pertencem a preparacao/controle de qualidade deste script.
validate_codes <- function(data, column, allowed) {
  observed <- sort(unique(data[[column]]))
  invalid <- setdiff(observed, allowed)
  if (length(invalid) > 0L) {
    stop_pipeline(
      "A coluna ", column, " contem codigos invalidos: ",
      paste(invalid, collapse = ", ")
    )
  }
}

# Converte uma coluna em inteiros e informa valores problematicos.
safe_integer <- function(x, column_name) {
  out <- suppressWarnings(as.integer(as.character(x)))
  bad <- is.na(out) & !is.na(x) & trimws(as.character(x)) != ""
  if (any(bad)) {
    stop_pipeline(
      "Valores nao numericos em ", column_name, ": ",
      paste(unique(as.character(x[bad])), collapse = ", ")
    )
  }
  out
}

# A FTOL temporal instalada apresenta diferenca maxima de poucos milionésimos
# de Ma entre as profundidades das pontas. Esta funcao corrige apenas esse
# arredondamento numerico, estendendo ramos terminais por no maximo a tolerancia.
correct_near_ultrametric <- function(tree, tolerance = 1e-4) {
  tip_depths <- ape::node.depth.edgelength(tree)[seq_len(ape::Ntip(tree))]
  spread <- diff(range(tip_depths))
  
  if (spread == 0) {
    attr(tree, "ultrametric_correction") <- 0
    return(tree)
  }
  
  if (spread > tolerance) {
    stop_pipeline(
      "A arvore nao e ultrametrica por ", signif(spread, 6),
      " Ma, acima da tolerancia de ", tolerance,
      ". Nao foi aplicada correcao automatica."
    )
  }
  
  target_depth <- max(tip_depths)
  terminal_rows <- match(seq_len(ape::Ntip(tree)), tree$edge[, 2])
  adjustments <- target_depth - tip_depths
  tree$edge.length[terminal_rows] <- tree$edge.length[terminal_rows] + adjustments
  attr(tree, "ultrametric_correction") <- max(adjustments)
  tree
}



# Define uma semente fixa para tornar qualquer etapa aleatoria reproduzivel.
# Mantendo este valor, a mesma entrada deve produzir a mesma subamostra no modo rapido.
set.seed(CONFIG$seed)
console_banner("SCRIPT 01", "Preparacao dos dados e da filogenia")
log_message("SCRIPT 01 - preparacao dos dados e da filogenia.")

console_step(1, 6, "Lendo Matriz_final_completa.xlsx e conferindo as colunas")
# Aqui a planilha original e lida SEM alterar os nomes das colunas.
# raw_data e a copia bruta; nao modificamos o arquivo Excel original.
raw_data <- openxlsx::read.xlsx(PATHS$data_file, check.names = FALSE)
# Remove apenas espacos acidentais no inicio/fim dos nomes das colunas.
# Isso evita erros como "Habito " ser interpretado como uma coluna diferente de "Habito".
names(raw_data) <- trimws(names(raw_data))
if ("Habito_Substrato" %in% names(raw_data) && !"Habito" %in% names(raw_data)) {
  names(raw_data)[names(raw_data) == "Habito_Substrato"] <- "Habito"
}

# Lista das colunas que obrigatoriamente precisam existir NA PLANILHA (nomes em ingles).
# Se uma delas faltar, e melhor parar agora do que produzir uma analise incompleta.
required_columns <- c(
  "family", "genus", "species", "order", "Pattern_type", "Fertile_frond_architecture",
  "Morphological_variation_fertile", "Size_ratio", "Habit"
)
# Compara o que o script espera com o que realmente existe no Excel.
missing_columns <- setdiff(required_columns, names(raw_data))
if (length(missing_columns) > 0L) {
  stop_pipeline("Colunas ausentes: ", paste(missing_columns, collapse = ", "))
}

console_step(2, 6, "Limpando nomes, convertendo codigos e validando a matriz")
# A partir daqui trabalhamos apenas com as colunas necessarias.
# Isso deixa o objeto mais simples e evita que colunas extras interfiram sem querer.
data <- raw_data[, required_columns]
# Guardamos os nomes originais antes da limpeza para poder auditar qualquer alteracao.
original_species <- as.character(data$species)
# Padroniza os nomes das especies para o formato usado pela arvore: Genus_species.
# Exemplo: "Adiantum latifolium" vira "Adiantum_latifolium".
data$species <- gsub("\\s+", "_", trimws(original_species))
taxon_corrections <- data.frame(
  original = original_species,
  corrigido = data$species,
  alterado = original_species != data$species
)
taxon_corrections <- taxon_corrections[taxon_corrections$alterado, , drop = FALSE]

# Duplicatas seriam perigosas porque uma ponta da arvore precisa corresponder a uma unica linha.
# Por isso o script interrompe imediatamente se duas linhas terminarem com o mesmo nome.
if (anyDuplicated(data$species)) {
  duplicates <- unique(data$species[duplicated(data$species)])
  stop_pipeline("Especies duplicadas apos limpeza: ", paste(duplicates, collapse = ", "))
}

# Estas colunas representam codigos numericos de categorias.
# Elas sao convertidas explicitamente para inteiro antes de validar os valores permitidos.
numeric_columns <- c(
  "Pattern_type", "Fertile_frond_architecture",
  "Morphological_variation_fertile", "Size_ratio", "Habit"
)
for (column in numeric_columns) data[[column]] <- safe_integer(data[[column]], column)

# Valores ausentes podem alterar o numero de especies entre analises.
# Nesta versao preferimos parar e avisar, em vez de excluir silenciosamente linhas incompletas.
if (anyNA(data[, required_columns])) {
  missing_by_column <- colSums(is.na(data[, required_columns]))
  missing_by_column <- missing_by_column[missing_by_column > 0]
  stop_pipeline(
    "Existem valores ausentes: ",
    paste(names(missing_by_column), missing_by_column, sep = "=", collapse = ", ")
  )
}

# Agora conferimos cada sistema de codificacao.
# Se aparecer um codigo fora desses intervalos, provavelmente a matriz mudou ou ha erro de digitacao.
validate_codes(data, "Pattern_type", 0:2)
validate_codes(data, "Fertile_frond_architecture", 0:6)
validate_codes(data, "Morphological_variation_fertile", 0:7)
validate_codes(data, "Size_ratio", 0:4)
validate_codes(data, "Habit", 0:6)

# ------------------------------------------------------------------------------
# PADRONIZACAO INTERNA DOS NOMES DAS COLUNAS
# ------------------------------------------------------------------------------
# A planilha pode permanecer com os nomes em ingles. A partir daqui o pipeline
# trabalha com nomes internos padronizados, porque os Scripts 02-09 procuram
# exatamente estas colunas.
column_name_map <- c(
  "Pattern_type" = "Padrao_Dimorfismo",
  "Fertile_frond_architecture" = "Arquitetura_Lamina",
  "Morphological_variation_fertile" = "Variacao_Morfologica",
  "Size_ratio" = "Relacao_Tamanho",
  "Habit" = "Habito"
)

for (old_name in names(column_name_map)) {
  new_name <- unname(column_name_map[[old_name]])
  if (old_name %in% names(data)) {
    names(data)[names(data) == old_name] <- new_name
  }
}

# Confere se a padronizacao produziu todas as colunas exigidas pelos scripts
# seguintes. Se alguma faltar, o script para aqui com uma mensagem clara.
internal_required_columns <- c(
  "Padrao_Dimorfismo", "Arquitetura_Lamina", "Variacao_Morfologica",
  "Relacao_Tamanho", "Habito"
)
missing_internal_columns <- setdiff(internal_required_columns, names(data))
if (length(missing_internal_columns) > 0L) {
  stop_pipeline(
    "Falha ao padronizar os nomes internos das colunas: ",
    paste(missing_internal_columns, collapse = ", ")
  )
}

# Cria variaveis derivadas que serao reutilizadas por varios scripts.
# Estado mantem os 3 estados; Dimorfismo_binario resume 0 = monomorfa e 1 = qualquer dimorfismo.
data$Estado <- as.character(data$Padrao_Dimorfismo)
data$Dimorfismo_binario <- as.integer(data$Padrao_Dimorfismo > 0L)
data$Habito_nome <- unname(HABIT_LABELS[as.character(data$Habito)])

# IMPORTANTE: as frequencias finais dos estados NAO sao calculadas aqui.
# Primeiro a matriz precisa ser filtrada para as especies realmente presentes na FTOL.
# Assim a frequencia descritiva, state_counts e o objeto salvo usam exatamente a mesma amostra das analises.
write_csv_utf8(taxon_corrections, file.path(PATHS$data, "correcoes_taxonomicas.csv"))

console_step(3, 6, "Carregando a FTOL e conferindo o grupo externo")
log_message("Carregando FTOL temporal e enraizada...")
# Carrega duas versoes da FTOL: uma com os grupos externos e outra somente com as samambaias analisadas.
# A primeira serve para auditoria; a segunda e a arvore que entra nas analises.
tree_with_outgroups <- ftolr::ft_tree(branch_len = "ultra", consensus = TRUE, rooted = TRUE, drop_og = FALSE)
full_tree <- ftolr::ft_tree(branch_len = "ultra", consensus = TRUE, rooted = TRUE, drop_og = TRUE)
# Identifica quais terminais a propria FTOL reconhece como grupo externo.
ftol_outgroups <- setdiff(tree_with_outgroups$tip.label, full_tree$tip.label)
informed_outgroups_present <- intersect(grupo_externo, tree_with_outgroups$tip.label)
informed_outgroups_missing <- setdiff(grupo_externo, tree_with_outgroups$tip.label)
outgroup_species <- sort(unique(c(grupo_externo, ftol_outgroups)))
# Monta uma tabela de controle para comparar o grupo externo informado com o que existe na FTOL.
# Esta tabela nao muda a arvore; serve apenas para documentar a verificacao.
outgroup_qc <- data.frame(
  Especie = outgroup_species,
  Informada = outgroup_species %in% grupo_externo,
  Outgroup_oficial_FTOL = outgroup_species %in% ftol_outgroups,
  Presente_na_FTOL = outgroup_species %in% tree_with_outgroups$tip.label
)
write_csv_utf8(outgroup_qc, file.path(PATHS$data, "controle_grupo_externo.csv"))
if (length(informed_outgroups_missing) > 0L) {
  log_message("Grupo externo ausente na FTOL: ", paste(informed_outgroups_missing, collapse = ", "), level = "AVISO")
}

console_step(4, 6, "Alinhando especies da matriz com as pontas da filogenia")
# Encontra somente as especies presentes AO MESMO TEMPO na matriz e na FTOL.
# Estas sao as especies que podem entrar nas analises filogeneticas.
common_species <- intersect(data$species, full_tree$tip.label)
# Guarda as especies do Excel que nao foram encontradas na FTOL.
# NAO apagamos essa informacao: ela e exportada para voce revisar depois.
unmatched_data <- setdiff(data$species, full_tree$tip.label)
unmatched_tree <- setdiff(full_tree$tip.label, data$species)
if (length(common_species) < 2L) stop_pipeline("Menos de duas especies compativeis com a FTOL.")

# Poda a FTOL para manter apenas as especies efetivamente analisadas.
tree <- ape::keep.tip(full_tree, common_species)
# Reordena as linhas da matriz para ficarem exatamente na mesma ordem das pontas da arvore.
# Essa etapa e essencial: modelos filogeneticos assumem correspondencia correta entre dados e tips.
data <- data[match(tree$tip.label, data$species), , drop = FALSE]
rownames(data) <- data$species
if (!identical(tree$tip.label, data$species)) stop_pipeline("Falha ao alinhar dados e arvore.")

# Corrige apenas pequenas diferencas numericas de ultrametricidade dentro da tolerancia definida.
# Se a diferenca for grande, a funcao para em vez de alterar a arvore silenciosamente.
tree <- correct_near_ultrametric(tree, CONFIG$ultrametric_tolerance)
if (!ape::is.rooted(tree)) stop_pipeline("A arvore resultante nao esta enraizada.")
if (any(tree$edge.length <= 0)) stop_pipeline("A arvore contem ramos nao positivos.")

ultrametric_correction <- attr(tree, "ultrametric_correction")
# A seguir salvamos arquivos de auditoria.
# Eles permitem saber exatamente o que entrou, o que ficou de fora e qual arvore foi usada.
write_csv_utf8(data.frame(species = unmatched_data), file.path(PATHS$data, "especies_excel_sem_ftol.csv"))
write_csv_utf8(data.frame(species = unmatched_tree), file.path(PATHS$data, "especies_ftol_sem_dados.csv"))
write_csv_utf8(data, file.path(PATHS$data, "dados_processados_alinhados.csv"))
ape::write.tree(tree, file = windows_safe_path(file.path(PATHS$data, "arvore_ftol_subset_ultrametrica.tre")))

console_step(5, 6, "Gerando as tabelas de controle de qualidade")
# Resume o controle de qualidade da execucao: tamanho da amostra, exclusoes, versoes e correcao da arvore.
qc_table <- data.frame(
  Item = c(
    "Linhas no Excel", "Especies na analise", "Especies excluidas",
    "Correcoes de nomes", "FTOL data version", "ftolr package version",
    "Outgroups informados presentes", "Outgroups oficiais FTOL",
    "Correcao ultrametrica maxima (Ma)", "Modo rapido", "Seed"
  ),
  Valor = c(
    nrow(raw_data), ape::Ntip(tree), length(unmatched_data), nrow(taxon_corrections),
    as.character(ftolr::ft_data_ver()), as.character(utils::packageVersion("ftolr")),
    paste0(length(informed_outgroups_present), "/", length(grupo_externo)),
    length(ftol_outgroups), ultrametric_correction, CONFIG$quick_mode, CONFIG$seed
  )
)
print(qc_table)
write_csv_utf8(qc_table, file.path(PATHS$data, "Controle de qualidade e amostragem.csv"))

# MODO RAPIDO: usado apenas para testar se o codigo funciona.
# No modo completo este bloco e ignorado e TODAS as especies compativeis sao mantidas.
if (CONFIG$quick_mode) {
  quick_species <- unlist(lapply(split(data$species, data$Estado), function(species) {
    if (length(species) <= CONFIG$quick_tips_per_common_state) return(species)
    sample(species, CONFIG$quick_tips_per_common_state)
  }), use.names = FALSE)
  tree <- ape::keep.tip(tree, quick_species)
  data <- data[match(tree$tip.label, data$species), , drop = FALSE]
  rownames(data) <- data$species
}

# Frequencias calculadas APOS toda a filtragem da amostra.
# Portanto N e proporcoes correspondem exatamente aos terminais usados nas analises.
state_counts <- table(factor(data$Estado, levels = names(STATE_LABELS)))
state_counts_table <- data.frame(
  Estado = names(STATE_LABELS),
  Estado_nome = unname(STATE_LABELS),
  N = as.integer(state_counts),
  Proporcao = as.integer(state_counts) / sum(state_counts),
  stringsAsFactors = FALSE
)
print(state_counts_table)
write_csv_utf8(
  state_counts_table,
  file.path(PATHS$data, "Frequencia dos estados de dimorfismo.csv")
)

# Reune em um unico objeto tudo o que os scripts seguintes precisam.
# Pense neste objeto como o "ponto de partida oficial" das analises 02-09.
prepared <- list(
  data = data,
  tree = tree,
  state_counts = state_counts,
  state_counts_table = state_counts_table,
  qc = qc_table,
  taxon_corrections = taxon_corrections,
  unmatched_data = unmatched_data,
  unmatched_tree = unmatched_tree
)
console_step(6, 6, "Salvando o objeto que sera usado pelas analises seguintes")
# Salva o objeto preparado em .rds.
# NAO apague este arquivo enquanto estiver trabalhando: os scripts 02-09 o leem para evitar repetir esta preparacao.
save_pipeline_rds(prepared, "01_dados_e_filogenia.rds")
console_output("Scripts_/_Objetos_R/01_dados_e_filogenia.rds")
console_output("Arquivos de controle sem numeracao em Dados_processados_/; Table S1 e Table S2 permanecem reservadas ao material externo.")
log_message("SCRIPT 01 concluido.")
console_done("SCRIPT 01", "02_EVOLUCAO_DIMORFISMO.R")