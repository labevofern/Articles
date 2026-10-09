# =============================================================================
# SNV
# =============================================================================
# Article: ........................... 
# Authors: Marise H.V. de Oliveira, Niksoney A. Mendonça and Thaís E. Almeida
# Script author: Niksoney Azevedo Mendonça
# E-mail: niksoneyazevedo2017@gmail.com
# =============================================================================
# Limpa o ambiente e fecha todas as visualizações
rm(list = ls()) 
# =============================================================================
# PACOTES PARA NECESSÁRIOS
# =============================================================================
install.packages(c("ggplot2", "tidyr", "dplyr", "plotly"))

library(ggplot2)  # Visualização de dados avançada
library(tidyr)    # Manipulação de dados (pivotagem, nesting, etc.)
library(dplyr)    # Manipulação de dados (filtros, seleção, agrupamento)
library(plotly)   # Para identificar os pontos no gráfico
# =============================================================================
# Standard Normal Variate (SNV)
# =============================================================================

# Definir diretório de trabalho
setwd("C:/Users/Jhonathan/Desktop/RESULTADOS_ANALISES/NIR/MORFOLOGICA")

# Importar a tabela NIR
Matriz_NIR <- read.table(
  file = "dado_morfologico_2026.csv",
  header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8",
  colClasses = "character"
)

# Função para aplicar SNV
aplicar_snv <- function(dados) {
  # Separar metadados e espectros
  dados_categoricos <- dados[, 1:3]
  valores_espectrais <- dados[, 4:ncol(dados)]
  
  # Converter para numérico
  valores_espectrais <- apply(valores_espectrais, 2, as.numeric)
  
  # Aplicar SNV em cada linha
  snv <- t(apply(valores_espectrais, 1, function(x) {
    (x - mean(x)) / sd(x)
  }))
  
  # Combinar de volta
  dados_snv <- cbind(dados_categoricos, snv)
  return(dados_snv)
}

# Aplicar o SNV
Matriz_NIR_SNV <- aplicar_snv(Matriz_NIR)

# Salvar o resultado
write.table(
  Matriz_NIR_SNV,
  file = "dado_morfologico_2026_snv.csv",
  row.names = FALSE,
  sep = ";", quote = FALSE, dec = ".", fileEncoding = "UTF-8"
)
