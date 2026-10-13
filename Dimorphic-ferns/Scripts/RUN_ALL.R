# ==============================================================================
# RUN_ALL.R
# ==============================================================================

# ------------------------------------------------------------------------------
# QUANDO USAR O RUN_ALL
# ------------------------------------------------------------------------------
# Use este arquivo quando quiser executar TODO o projeto desde o inicio.
# Se voce esta aprendendo ou tentando localizar um erro, e melhor rodar os
# scripts 00, 01, 02, 03... um por um. Assim voce sabe exatamente onde parou.
#
# COMO EXECUTAR:
#   1. Abra RUN_ALL.R no RStudio.
#   2. Clique em Source.
#   3. Nao selecione trechos.
#   4. Acompanhe no Console qual script esta sendo executado.
#
# ------------------------------------------------------------------------------
# RODA TODO O PIPELINE NA ORDEM CORRETA.
#
# ORDEM:
# 00 Configuracao e funcoes
# 01 Preparacao dos dados e filogenia
# 02 Evolucao do dimorfismo
# 03 SIMMAP
# 04 Sinal filogenetico
# 05 Habito e dimorfismo
# 06 Taxas ocultas
# 07 Evolucao das solucoes morfologicas
# 08 Correlacoes/sinal morfologico
# 09 Descritivas/exploratorias
# 10 Sensibilidade filogenetica
# 11 Sensibilidade Pagel / hidden rates
# ==============================================================================

# ==============================================================================
# INSTALAR E CARREGAR TODOS OS PACOTES DO PROJETO
# ==============================================================================

options(
  repos = c(
    CRAN = "https://cloud.r-project.org"
  )
)


# ------------------------------------------------------------------------------
# 1. Todos os pacotes utilizados nas analises
# ------------------------------------------------------------------------------

pacotes <- c(
  "ape",
  "caper",
  "corHMM",
  "dplyr",
  "ftolr",
  "ggplot2",
  "openxlsx",
  "patchwork",
  "phylolm",
  "phytools",
  "rstudioapi",
  "rgbif",
  "scales",
  "stringr",
  "tidyr",
  "viridisLite"
)


# ------------------------------------------------------------------------------
# 2. Instalar pacotes do CRAN SOMENTE se estiverem faltando
# ------------------------------------------------------------------------------

pacotes_cran <- setdiff(
  pacotes,
  "ftolr"
)

faltando <- pacotes_cran[
  !vapply(
    pacotes_cran,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if (length(faltando) > 0L) {
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "INSTALANDO PACOTES QUE ESTAO FALTANDO\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    paste(
      faltando,
      collapse = ", "
    ),
    "\n\n"
  )
  
  
  install.packages(
    faltando,
    dependencies = NA
  )
  
} else {
  
  cat(
    "\nTodos os pacotes do CRAN ja estao instalados.\n"
  )
}


# ------------------------------------------------------------------------------
# 2B. CONFERIR RGBIF PARA A RECONCILIACAO TAXONOMICA DO SCRIPT 01
# ------------------------------------------------------------------------------
# O Script 01 usa rgbif::name_backbone() com a API taxonomica v2 para resolver
# sinonimos, mudancas de genero, novas combinacoes e grafias inequivocas.
#
# IMPORTANTE:
# - rgbif 3.8.5 e suficiente para esta rotina;
# - nao tentamos atualizar rgbif aqui, porque requireNamespace() ja pode ter
#   carregado o namespace e o Windows nao permite substituir com seguranca um
#   pacote carregado na mesma sessao;
# - se a versao for antiga, o pipeline para ANTES das analises e informa como
#   atualizar em uma sessao R reiniciada.

if (!requireNamespace("rgbif", quietly = TRUE)) {
  stop(
    paste0(
      "ERRO: o pacote rgbif nao esta instalado.\n",
      "Reinicie o R, execute install.packages('rgbif') e rode novamente."
    ),
    call. = FALSE
  )
}

if (utils::packageVersion("rgbif") < "3.8.5") {
  stop(
    paste0(
      "ERRO: e necessario rgbif >= 3.8.5. Versao instalada: ",
      as.character(utils::packageVersion("rgbif")),
      ".\nReinicie o R, execute install.packages('rgbif') e rode novamente."
    ),
    call. = FALSE
  )
}

cat(
  "Versao do rgbif: ",
  as.character(utils::packageVersion("rgbif")),
  " - OK para reconciliacao taxonomica.\n",
  sep = ""
)


# ==============================================================================
# 3. VERIFICAR A VERSAO DO PATCHWORK
# ==============================================================================
#
# A Fig3 do Script 09 utiliza:
#
# patchwork::free(
#   p_bar,
#   type = "label",
#   side = "l"
# )
#
# Portanto, precisamos de uma versao de patchwork que suporte
# os argumentos "type" e "side".
#
# O RUN_ALL verifica isso ANTES de iniciar as analises.
# ==============================================================================


if (!requireNamespace("patchwork", quietly = TRUE)) {
  
  cat(
    "\npatchwork nao encontrado. Instalando...\n"
  )
  
  install.packages(
    "patchwork",
    dependencies = NA
  )
}


# Verifica diretamente se a funcao free() possui os argumentos necessarios.
patchwork_free_args <- names(
  formals(
    patchwork::free
  )
)


if (!all(c("type", "side") %in% patchwork_free_args)) {
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "ATUALIZANDO PATCHWORK\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "A versao instalada de patchwork nao suporta os argumentos\n"
  )
  
  cat(
    "type/side usados na Fig3 do Script 09.\n"
  )
  
  cat(
    "Tentando atualizar patchwork antes de iniciar o pipeline...\n\n"
  )
  
  
  install.packages(
    "patchwork",
    dependencies = NA
  )
  
  
  # Conferir novamente depois da instalacao.
  patchwork_free_args <- names(
    formals(
      patchwork::free
    )
  )
  
  
  if (!all(c("type", "side") %in% patchwork_free_args)) {
    
    warning(
      paste0(
        "\nA versao disponivel de patchwork ainda nao suporta ",
        "type/side em free().\n",
        "O Script 09 possui uma rotina de compatibilidade e ",
        "utilizara patchwork::free(p_bar) automaticamente.\n"
      ),
      call. = FALSE
    )
    
  } else {
    
    cat(
      "\npatchwork atualizado com sucesso.\n"
    )
  }
  
} else {
  
  cat(
    "\npatchwork compativel com a Fig3.\n"
  )
}


cat(
  "Versao do patchwork: ",
  as.character(
    packageVersion("patchwork")
  ),
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 4. Instalar ftolr SOMENTE se estiver faltando
# ------------------------------------------------------------------------------

if (!requireNamespace("ftolr", quietly = TRUE)) {
  
  cat(
    "\nInstalando o pacote ftolr...\n"
  )
  
  
  if (!requireNamespace("remotes", quietly = TRUE)) {
    
    install.packages(
      "remotes"
    )
  }
  
  
  remotes::install_github(
    "fernphy/ftolr",
    dependencies = NA,
    upgrade = "never"
  )
  
} else {
  
  cat(
    "\nftolr ja esta instalado.\n"
  )
}


# ------------------------------------------------------------------------------
# 5. Conferencia final dos pacotes
# ------------------------------------------------------------------------------

ainda_faltando <- pacotes[
  !vapply(
    pacotes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if (length(ainda_faltando) > 0L) {
  
  stop(
    "\nERRO: estes pacotes ainda nao foram instalados corretamente:\n",
    paste(
      ainda_faltando,
      collapse = ", "
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 6. Carregar TODOS os pacotes
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  
  library(ape)
  library(caper)
  library(corHMM)
  library(dplyr)
  library(ftolr)
  library(ggplot2)
  library(openxlsx)
  library(patchwork)
  library(phylolm)
  library(phytools)
  library(rstudioapi)
  library(rgbif)
  library(scales)
  library(stringr)
  library(tidyr)
  library(viridisLite)
  
})


cat(
  "\n============================================================\n"
)

cat(
  "TODOS OS PACOTES ESTAO INSTALADOS E CARREGADOS.\n"
)

cat(
  "Pode executar o pipeline.\n"
)

cat(
  "============================================================\n"
)


# ==============================================================================
# INICIAR AS ANALISES
# ==============================================================================


get_scripts_dir <- function() {
  
  args <- commandArgs(
    trailingOnly = FALSE
  )
  
  file_arg <- grep(
    "^--file=",
    args,
    value = TRUE
  )
  
  
  if (length(file_arg) > 0L) {
    
    return(
      dirname(
        normalizePath(
          sub(
            "^--file=",
            "",
            file_arg[1]
          ),
          winslash = "/"
        )
      )
    )
  }
  
  
  if (
    requireNamespace(
      "rstudioapi",
      quietly = TRUE
    ) &&
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


# ==============================================================================
# ORDEM OFICIAL DO PIPELINE
# ==============================================================================

scripts <- c(
  "00_CONFIGURACAO_E_FUNCOES.R",
  "01_PREPARAR_DADOS_E_FILOGENIA.R",
  "02_EVOLUCAO_DIMORFISMO.R",
  "03_SIMMAP_DIMORFISMO.R",
  "04_SINAL_FILOGENETICO_DIMORFISMO.R",
  "05_HABITO_E_DIMORFISMO.R",
  "06_TAXAS_OCULTAS.R",
  "07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R",
  "08_CORRELACOES_E_SINAL_MORFOLOGICO.R",
  "09_ANALISES_DESCRITIVAS_E_EXPLORATORIAS.R",
  "10_SENSIBILIDADE_FILOGENETICA.R",
  "11_SENSIBILIDADE_PAGEL_HIDDEN_RATES.R"
)


# ==============================================================================
# CONFERIR SE TODOS OS SCRIPTS EXISTEM
# ==============================================================================

missing_scripts <- scripts[
  !file.exists(
    file.path(
      SCRIPTS_DIR,
      scripts
    )
  )
]


if (length(missing_scripts) > 0L) {
  
  stop(
    "Os seguintes scripts nao foram encontrados em Scripts_: ",
    paste(
      missing_scripts,
      collapse = ", "
    ),
    call. = FALSE
  )
}


# ==============================================================================
# EXECUTAR O PIPELINE
# ==============================================================================

for (script in scripts) {
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "EXECUTANDO:",
    script,
    "\n"
  )
  
  cat(
    "============================================================\n"
  )
  
  
  source(
    file.path(
      SCRIPTS_DIR,
      script
    ),
    chdir = TRUE
  )
}


# ==============================================================================
# FINAL
# ==============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "PIPELINE COMPLETO FINALIZADO COM SUCESSO.\n"
)

cat(
  "Os Scripts 00 a 11 foram executados na ordem correta.\n"
)

cat(
  "============================================================\n"
)