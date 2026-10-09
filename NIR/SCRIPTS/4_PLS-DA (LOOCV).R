# =============================================================================
# PLS-da (LOOCV)
# =============================================================================
# Article: ........................... 
# Authors: Marise H.V. de Oliveira, Niksoney A. Mendonça and Thaís E. Almeida
# Script author: Niksoney Azevedo Mendonça
# E-mail: niksoneyazevedo2017@gmail.com
# =============================================================================

rm(list=ls()) #Limpar a lista de arquivos
# =============================================================================
# PACOTES PARA NECESSÁRIOS
# =============================================================================
install.packages(c("dplyr", "caret", "MASS", "pls", "magrittr", "boot", "reshape2"))

library(dplyr)        # Para manipulação de dados
library(caret)        # Para machine learning
library(MASS)         # Para análises estatísticas
library(pls)          # Para regressão PLS
library(magrittr)     # Para usar o operador pipe %>% 
library(boot)         # Para métodos bootstrap
library(reshape2)     # Para transformar dados

# =============================================================================
# CRIE A TABELAS COM OS ARQUIVOS DE LEITURA
# =============================================================================

setwd("C:/Users/Jhonathan/Desktop/RESULTADOS_ANALISES/NIR/MORFOLOGICA/COMSNV")

#Carrega dado NIR
NIRdata = read.table ('DADOS MEDIOS_aba_morfologico_2026_snv.csv', header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8", colClasses = "character")
#NIRdata = read.table ('DADOS MEDIOS_ada_morfologico_2026_snv.csv', header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8", colClasses = "character")
#NIRdata = read.table ('DADOS MEDIOS_abaada_morfologico_2026_snv.csv', header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8", colClasses = "character")

#NIRdata = read.table ('DADOS MEDIOS_ada_filogenetico_2026_snv.csv', header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8", colClasses = "character")
#NIRdata = read.table ('DADOS MEDIOS_ada_filogenetico_2026_snv.csv', header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8", colClasses = "character")
#NIRdata = read.table ('DADOS MEDIOS_abaada_filogenetico_2026_snv.csv', header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8", colClasses = "character")

# ------------------------------------------------------
# AJUSTAR OS DADOS
# ------------------------------------------------------

# Transformar o nome da coluna 'especie' em nomes válidos (sem espaços)
NIRdata$especie <- make.names(NIRdata$especie)

# Converter as colunas espectrais (da 4 até a última) para numéricas
NIRdata[, 4:ncol(NIRdata)] <- lapply(NIRdata[, 4:ncol(NIRdata)], as.numeric)

# Remover linhas com valores ausentes (caso existam)
dados <- na.omit(NIRdata)

# ------------------------------------------------------
# CONFIGURAR A VALIDAÇÃO CRUZADA (LOOCV)
# ------------------------------------------------------
set.seed(123)

pls_ctrl <- trainControl(
  method = "LOOCV",         # Leave-One-Out Cross Validation
  verboseIter = TRUE,       # Exibe progresso
  classProbs = FALSE,       # Só TRUE se for classificação
  returnData = TRUE,
  savePredictions = "final" # Salva predições finais
)

# ------------------------------------------------------
# DEFINIR A GRADE DE PARÂMETROS
# ------------------------------------------------------
# Testar de 1 até o limite seguro de componentes (ex: 10)
pls_grid <- expand.grid(ncomp = 1:20)

# ------------------------------------------------------
# DEFINIR SE O MODELO É DE CLASSIFICAÇÃO OU REGRESSÃO
# ------------------------------------------------------
# Se a variável resposta (coluna 3) for categórica:
y <- as.factor(dados[, 2])

# ------------------------------------------------------
# TREINAR O MODELO PLS
# ------------------------------------------------------
pls_loocv <- train(
  x = dados[, 4:ncol(dados)],  # Preditores (espectros)
  y = y,                       # Variável resposta
  method = "pls",
  trControl = pls_ctrl,
  tuneGrid = pls_grid,
  preProcess = c("center", "scale"),  # Padroniza os dados
  maxit = 4000                        # Máximo de iterações
)

# Ver qual número de componentes foi escolhido pelo modelo
#pls_loocv$bestTune

# Ver a acurácia de todos os componentes testados
#pls_loocv$results

# Criando a matriz de confusão para os dados de teste
conf_matrix_teste <- confusionMatrix(pls_loocv$pred$pred, pls_loocv$pred$obs)

# Exibindo os resultados da matriz de confusão
model_accuracy_teste <- conf_matrix_teste
print(model_accuracy_teste)


# Criando a matriz de confusão original
conf_matrix_teste <- confusionMatrix(pls_loocv$pred$pred, pls_loocv$pred$obs)

# Extraindo a TABELAS de confusão
conf_table <- conf_matrix_teste$table

# Transpondo a TABELAS de confusão
conf_table_transposed <- t(conf_table)

# Criando uma nova matriz de confusão com a TABELAS transposta
conf_matrix_teste_transposed <- confusionMatrix(conf_table_transposed)

# Exibindo os resultados da nova matriz de confusão
print(conf_matrix_teste_transposed)

# Conferir a Balanced Accuracy por espécie
BalAcc_check <- round(conf_matrix_teste_transposed$byClass[,"Balanced Accuracy"]*100, 1)
BalAcc_check


# COM SNV
BalAcc_values <- c(71, 59, 62, 79, 55, 74) #aba_morfologico
#BalAcc_values <- c(66, 68, 62, 59, 66, 79) #ada_morfologico
#BalAcc_values <- c(81, 62, 62, 69, 60, 76) #abaada_morfologico

#BalAcc_values <- c(88, 55, 80, 63, 69) #aba_filogenetico
#BalAcc_values <- c(63, 56, 66, 65, 68) #ada_filogenetico
#BalAcc_values <- c(65, 59, 83, 61, 75) #abaada_filogenetico

# SEM SNV
#BalAcc_values <- c(68, 55, 69, 73, 55, 73) #aba_morfologica
#BalAcc_values <- c(66, 73, 67, 70, 59, 74) #ada_morfologica
#BalAcc_values <- c(76, 64, 60, 76, 59, 77) #abaada_morfologica

#BalAcc_values <- c(75, 53, 64, 72, 69) #aba_filogenetico
#BalAcc_values <- c(67, 55, 66, 63, 63) #ada_filogenetico
#BalAcc_values <- c(68, 53, 61, 72, 70) #abaada_filogenetico

# Adicionando os valores como uma nova coluna na matriz de confusão transposta
conf_table_transposed <- cbind(conf_table_transposed, "BalAcc%" = BalAcc_values)

# Criando um vetor com os nomes das classes
classes_values <- c("alb", "ano", "cra", "mor", "obl", "ruf")
 #classes_values <- c("cal", "can", "cme", "cmo", "cru")

# Adicionando a coluna "Classes" como a primeira coluna
conf_table_transposed <- cbind(Classes = classes_values, conf_table_transposed)

# Convertendo a matriz em data frame e adicionando colunas
conf_table_transposed <- conf_table_transposed %>%
  as.data.frame(stringsAsFactors = FALSE, check.names = FALSE) %>%  # evita alteração de nomes
  cbind("BalAcc%" = BalAcc_values) %>%                              # adiciona BalAcc% fixo
  cbind(Classes = classes_values, .)                                # adiciona Classes como primeira coluna

# Garantindo a ordem das colunas
conf_table_transposed <- conf_table_transposed[, c("Classes", setdiff(colnames(conf_table_transposed), "Classes"))]

# Exibindo a matriz de confusão transposta atualizada sem aspas
print(conf_table_transposed, quote = FALSE)

# Exportando os resultados da matriz de confusão por classe
write.table(conf_table_transposed, file='aba_matriz_loocv_morfologico_2026_snv.csv', sep="\t", row.names=TRUE)
#write.table(conf_table_transposed, file='ada_matriz_loocv_morfologico_2026_snv.csv', sep="\t", row.names=TRUE)
#write.table(conf_table_transposed, file='abaada_matriz_loocv_morfologico_snv.csv', sep="\t", row.names=TRUE)

#write.table(conf_table_transposed, file='aba_matriz_loocv_filogenetico_2026_snv.csv', sep="\t", row.names=TRUE)
#write.table(conf_table_transposed, file='ada_matriz_loocv_filogenetico_2026_snv.csv', sep="\t", row.names=TRUE)
#write.table(conf_table_transposed, file='abaada_matriz_loocv_filogenetico_snv.csv', sep="\t", row.names=TRUE)

#----------------------------------------------------------------------------------------------------------------------------
rm(list=ls()) #Limpar a lista de arquivos

library(ggplot2)
library(reshape2)
library(dplyr)
library(ggnewscale)

setwd("C:/Users/Jhonathan/Desktop/RESULTADOS_ANALISES/NIR/MORFOLOGICA/COMSNV")

# Lista de arquivos e nomes de saída por hipotese: morfologica ou filogenetica
arquivos <- list(
  list(path = "aba_matriz_loocv_morfologico_2026_snv.csv",
       nome = "aba_matrizconfusao_morfologico_2026_snv.png"))

#arquivos <- list(
  #list(path = "ada_matriz_loocv_filogenetico_2026_snv.csv",
       #nome = "ada_matrizconfusao_filogenetico_2026_snv.png"))

#arquivos <- list(
#list(path = "abaada_matriz_loocv_filogenetico_snv.csv",
#nome = "abaada_matrizconfusao_filogenetico_snv.png"))

# Função para gerar o gráfico
gerar_figura <- function(arquivo, nome_saida) {
  tab <- read.table(arquivo, sep = "\t", stringsAsFactors = TRUE,
                    header = TRUE, check.names = FALSE)
  
  # Ordem base = ordem das colunas do CSV (exceto a coluna "Classes")
  ord_x <- colnames(tab)[colnames(tab) != "Classes"]
  

  # Y deve espelhar X, mas sem "BalAcc%" (normalmente não é linha)
  ord_y <- ord_x[ord_x != "BalAcc%"]
  
  if ("Classes" %in% names(tab)) {
    extra_y <- setdiff(unique(as.character(tab$Classes)), ord_y)
    ord_y <- c(ord_y, extra_y)
    tab$Classes <- factor(tab$Classes, levels = ord_y)
  }
  
  # Converter para formato longo
  data_long <- reshape2::melt(tab, id.vars = "Classes")
  colnames(data_long) <- c("Classe_predita", "Classe_real", "Valor")
  
  # Aplicar níveis aos eixos
  data_long$Classe_real    <- factor(as.character(data_long$Classe_real), levels = ord_x)
  data_long$Classe_predita <- factor(as.character(data_long$Classe_predita), levels = ord_y)
  
  # ---- Lógica de rótulos ----
  data_long$Label <- with(data_long, ifelse(
    Classe_real == "BalAcc%" & (is.na(Valor) | Valor == 0), "NA",   # BalAcc%: mostra NA
    ifelse(is.na(Valor) | Valor == 0, "", Valor)                    # demais: 0 e NA ficam vazios
  ))
  # ---------------------------
  
  # Plot
  p <- ggplot() +
    geom_tile(
      data = dplyr::filter(data_long, Classe_real != "BalAcc%"),
      aes(x = Classe_real, y = Classe_predita, fill = Valor),
      color = "white", linewidth = 0.5
    ) +
    scale_fill_gradientn(
      colors = c("#2C5282", "#3182CE", "#90CDF4"),
      na.value = "grey90",   # fundo dos NAs claro
      guide = "none"
    ) +
    ggnewscale::new_scale_fill() +
    geom_tile(
      data = dplyr::filter(data_long, Classe_real == "BalAcc%"),
      aes(x = Classe_real, y = Classe_predita, fill = "highlight"),
      color = "white", linewidth = 0.5
    ) +
    scale_fill_manual(values = c("highlight" = "#1A365D"), guide = "none") +
    geom_text(
      data = dplyr::filter(data_long, Label != ""),
      aes(
        x = Classe_real,
        y = Classe_predita,
        label = Label,
        fontface = ifelse(Classe_real == "BalAcc%", "bold", "plain")
      ),
      color = "white",
      size = 5
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1),  # aumentei tamanho
      axis.text.y = element_text(size = 14),                                   # aumentei tamanho
      axis.title  = element_text(size = 15),
      panel.grid  = element_blank()
    ) +
    labs(x = "", y = "")
  
  ggsave(file.path("Figuras", nome_saida), plot = p, width = 7, height = 7, dpi = 300)
  cat("✅ Figura salva:", nome_saida, "\n")
}

# Loop para gerar todas
for (arq in arquivos) {
  gerar_figura(arq$path, arq$nome)
}


