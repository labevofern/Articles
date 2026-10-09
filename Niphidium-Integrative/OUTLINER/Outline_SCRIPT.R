# =============================================================================
# EFA
# =============================================================================
# Article: ........................... 
# Authors: Marise H.V. de Oliveira, Niksoney A. Mendonça and Thaís E. Almeida
# Script author: Niksoney Azevedo Mendonça
# E-mail: niksoneyazevedo2017@gmail.com
# ==============================================================================
# Limpar ambiente
rm(list = ls())
# =============================================================================
# PACOTES PARA NECESSÁRIOS
# =============================================================================
install.packages(c("readxl", "Momocs", "tidyverse", "ggplot2", "tidyr", "dplyr"))

library(readxl)     # Permite importar dados de planilhas do Excel (.xls e .xlsx) para o R
library(Momocs)     # Usado para análise de formas (morfometria geométrica), especialmente em biologia e morfologia
library(tidyverse)  # Conjunto de pacotes integrados para manipulação, transformação e visualização de dados
library(ggplot2)    # Pacote principal de visualização de dados; cria gráficos elegantes e personalizáveis
library(tidyr)      # Ferramentas para reorganizar (transformar) dados — deixar "longos" ou "largos"
library(dplyr)      # Ferramentas para manipulação eficiente de dados (selecionar, filtrar, agrupar, resumir, etc.)
# ==============================================================================
# DEFINIR AS CORES
# ==============================================================================

# Paleta de cores personalizada (Morfologica)
#my_colors <- c(
  #"#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2")

# Paleta de cores personalizada (filogenetica)
my_colors <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B")

# ==============================================================================
# PROCESSAMENTO INICIAL
# ==============================================================================

# Definir o diretório de trabalho para onde estão os arquivos
# Colocar o nome da pasta da espécie que to trabalhando
# Ler os arquivos de texto na pasta

# Diretório
setwd("C:/Users/Jhonathan/Desktop/OUTLINE/FILOGENETICA/EFA_FILOGENETICA")

# ------------------------------------------------------------------------------
# Listar arquivos de contornos (.txt)
# ------------------------------------------------------------------------------

lf <- list.files(
  pattern = "\\.txt$",
  full.names = TRUE
)

# Visualizar os nomes dos arquivos .txt encontrados
print(lf)

# Ler o arquivo Excel com os nomes das espécies e identificadores dos indivíduos
spp_names <- readxl::read_xlsx("TABELA_FILOGENETICA.xlsx")

# Exibir os nomes únicos das espécies na coluna 'spp' do arquivo Excel
unique(spp_names$especie)

# Importar as coordenadas dos arquivos de texto para um objeto de dados
lf_coord <- import_txt(lf)

# Verificar se o número de arquivos e linhas na planilha são iguais
stopifnot(length(lf) == nrow(spp_names))

names(lf_coord) <- spp_names$id

# Criar data frame com fatores
lf_fac <- data.frame(
  Type = spp_names$especie,
  Heb = spp_names$herbário
)

# Criar objeto do Momocs
lf_out <- Out(lf_coord, fac = lf_fac)

# Definir a ordem desejada das espécies na legenda
#ordem <- c("ALB", "ANO", "CAR", "CRA", "MOR", "OBL", "RUF") 
ordem <- c("CAL", "CAN", "CCA", "CME", "CMO", "CRU")

# Transformar em fator respeitando a ordem
lf_out$fac$Type <- factor(spp_names$especie, levels = ordem)

lf_out$fac$Type <- as.factor(lf_out$fac$Type)

# ==============================================================================
# FORMAS
# ==============================================================================

# Primeiro abre o dispositivo gráfico PNG
png("formas por espécie_FILOGENETICA.png", width = 10, height = 8, units = "in", res = 300)

# Depois gera o gráfico
panel(lf_out,
      fac = "Type",
      col = my_colors)

# Adiciona a legenda
legend("topright", 
       legend = levels(lf_out$fac$Type),
       col = my_colors[1:length(levels(lf_out$fac$Type))],
       pch = 16,
       pt.cex = 2.5,
       cex = 0.8,
       bty = "n",
       inset = c(0, 0),
       xpd = TRUE,
       y.intersp = 2) # Aumenta o espaçamento vertical entre os itens

# Finalmente fecha o dispositivo gráfico
dev.off()

# ==============================================================================
# VISUALIZAR CONTONOS EMPILHADOS
# ==============================================================================

stack(coo_center(lf_out),           #Centraliza os contornos antes de plotá-los
      palette = col_summer,         #Define a paleta de cores para o gráfico
      fac = lf_out$Type,            #Fator que define como os contornos são agrupados ou coloridos
      title = "Stack of outlines from all individuals",
      subtitle = paste("Relação entre Área e Comprimento dos Contornos das escamas"))  
                                    #Adiciona um subtítulo com informações extras

# ==============================================================================
# DEFINIR NÚMERO DE HARMONICOS
# ==============================================================================

# USANDO A VARIÂNCIA DA FORMA PARA ESCOLHER O NÚMERO IDEAL DE HARMONICOS
# Calibrar o número de harmônicos para explicar até 99% da variância

# Definir o nome do arquivo e a resolução (300 DPI)
png("harmonico.png", width = 10, height = 8, units = "in", res = 300)

harmonics_info <- calibrate_harmonicpower_efourier(lf_out, 
                                                   nb.h = 10, 
                                                   drop = 1,
                                                   thresh = c(80, 90, 95, 99, 99.9),
                                                   plot = TRUE) #Ver quantos harmonicos melhor explicam

# Verificar o conteúdo de harmonics_info
print(harmonics_info)


# Fechar o dispositivo gráfico e salvar o arquivo
dev.off()

# ==============================================================================
# EFOURIER
# ==============================================================================

# Realizando a análise de Fourier elíptica (EFA) nas formas armazenadas em lf_out
lf_fou <- lf_out %>% 
  coo_center %>%                   # Centraliza os contornos no centro geométrico.
  efourier(7, norm=TRUE) 

# variação de tamanho (alometria) norm = FALSE
# variação na forma               norm = TRUE
# ==============================================================================
# PCA e LDA com dados processados (coo_center, coo_scale, coo_align e coo_slidedirection) e adição das metricas_EFA
# ==============================================================================

# Adicionando variáveis escalares aos dados (cálculo de medidas geométricas)
scalars <- lf_out %>% measure(coo_area, coo_perim, coo_length) 

# coo_convexity, coo_elongation, coo_circularity
#'scalars' contém as áreas, perímetros e comprimentos dos contornos

# Combina os coeficientes de Fourier com as variáveis escalares em uma única tabela
lf_fou_and_scalars <- TraCoe(cbind(lf_fou$coe, scalars$coe), fac=lf_fou$fac) 

# Coeficientes de Fourier elíptico
lf_fou_and_scalars$coe

# Transformar lf_fou_and_scalars$coe em data frame
lf_coe_df <- as.data.frame(lf_fou_and_scalars$coe)

# Eliminar as colunas 1, 2 e 3
lf_coe_df_reduced <- lf_coe_df[, -c(1, 8, 15)]

# Restaurar o formato original (matriz) se necessário
lf_fou_and_scalars$coe <- as.matrix(lf_coe_df_reduced)

#----------------------PCA

# Realizar a PCA
lf_pca3 <- PCA(x = lf_fou_and_scalars,        #Objeto com os coeficientes de Fourier
               fac = lf_fou_and_scalars$fac)  #Variável categórica associada (fac)

criar_plot_pca_elipse_contorno <- function(pca_obj, out_obj, conf_level = 0.95) {
  pca_data <- data.frame(
    PC1 = pca_obj$x[, 1],
    PC2 = pca_obj$x[, 2],
    especie = out_obj$fac$Type
  )
  
  # % de variância
  variance_explained <- round((pca_obj$sdev^2 / sum(pca_obj$sdev^2)) * 100, 1)
  
  p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = especie)) +
    geom_point(size = 3, alpha = 0.7) +
    
    # ELLIPSES contornadas sem preenchimento
    stat_ellipse(aes(color = especie), level = conf_level, geom = "path", size = 1) +
    
    scale_color_manual(values = my_colors) +
    
    labs(
      x = paste0("PC1 (", variance_explained[1], "%)"),
      y = paste0("PC2 (", variance_explained[2], "%)"),
      color = "Species"
    ) +
    theme_bw() +
    theme(
      plot.title = element_blank(),
      legend.position = "right",
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold")
    )
  
  return(p)
}

#-----------------------------------------------------------------------------------------------------------------------
# Gerar gráfico
#-----------------------------------------------------------------------------------------------------------------------
p1 <- criar_plot_pca_elipse_contorno(lf_pca3, lf_out)

# Salvar
ggsave("pca (com processamento)_FILOGENETICA.png", p1,
       width = 10, height = 8, dpi = 300)

# Exibir
print(p1)

#----------------------LDA

# Dados já existentes (supondo que lf_fou já foi criado antes da PCA)
coefs2 <- lf_fou_and_scalars$coe           # Extrai coeficientes
fac_data <- lf_fou_and_scalars$fac        # Extrai fatores (Type e Heb)

# Checar variância
#vars <- apply(coefs2, 2, var)
#vars

# Remover as colunas com valores 0 e 1 diretamente dos coeficientes se necessário
 #coefs_filtered2 <- coefs2[, -c(1, 8, 15)]

# ------------------------------------------------------------------------------

# Definir o nome do arquivo e a resolução (300 DPI)
png("lda (com processamento)_FILOGENETICO.png", width = 10, height = 8, units = "in", res = 300)

# Converter para formato do Momocs e rodar LDA
lda_data2 <- LDA(
  x = coefs2,          # Usa os coeficientes filtrados
  fac = fac_data$Type)         # Usa Type como grupo

# Plotar LDA
plot_LDA(lda_data2, 
         palette = pal_manual(my_colors, transp = 0))
# ------------------------------------------------------------------------------
# Mostrar resultados (incluindo acurácia)
print(lda_data2)  # Mostra estatísticas gerais

# Acurácia específica:
cat("\nAcurácia da LDA:", mean(lda_data2$CV.correct) * 100, "%\n")

# Matriz de confusão
cat("\nMatriz de Confusão (Validação Cruzada):\n")
print(lda_data2$CV.tab)

# Fechar o dispositivo gráfico e salvar o arquivo
dev.off()

# ==============================================================================
# MATRIZ DE CONFUSÃO TIPO 1
# ==============================================================================

library(ggplot2)

# Order of species
#species_order <- c("ALB", "ANO", "CAR", "CRA", "MOR", "OBL", "RUF")  
species_order <- c("CAL", "CAN", "CCA", "CME", "CMO", "CRU")

# 1) Extract confusion matrix
cm <- as.matrix(lda_data2$CV.tab)

# 2) Remove rows/cols with only zeros
keep_rows <- rowSums(cm) > 0
keep_cols <- colSums(cm) > 0
cm <- cm[keep_rows, keep_cols, drop = FALSE]

# 3) Reorder according to species_order
species_present <- species_order[species_order %in% rownames(cm)]
cm <- cm[species_present, species_present, drop = FALSE]

# 4) Convert to data.frame
df <- as.data.frame(as.table(cm))
colnames(df) <- c("Actual", "Predicted", "Count")

# 5) Calculate percentages by row
row_totals <- tapply(df$Count, df$Actual, sum)
df$Percent <- 100 * df$Count / row_totals[as.character(df$Actual)]

# 6) Force factor order
df$Actual <- factor(df$Actual, levels = species_present)
df$Predicted <- factor(df$Predicted, levels = species_present)

# 7) Remove zeros from labels
df$Label <- ifelse(df$Percent == 0, "", sprintf("%.1f%%", df$Percent))

# 8) Plot with clean colors
p <- ggplot(df, aes(x = Predicted, y = Actual, fill = Percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), size = 4, color = "black") +
  scale_fill_gradient(low = "white", high = "#5C7AEA", limits = c(0,100), na.value = "white") +
  scale_y_discrete(limits = rev(species_present)) +
  labs(x = "Predicted", y = "Actual", title = "Confusion Matrix (Cross-Validation)", fill = "Percentage") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# 9) Save figure
ggsave("Imagens/cv (com processamento)_FILOGENETICO.png", plot = p, width = 10, height = 8, units = "in", dpi = 300)

dev.off()

# ==============================================================================
# MANOVA
# ==============================================================================

# Rodar a MANOVA para o modelo
MANOVA(lf_pca3, ~Type)

# Rodar a MANOVA par a par por especie
resultado_manova <- MANOVA_PW(lf_pca3, ~Type)  

# Salvar a tabela de significância ($stars.tab) em um arquivo CSV
write.csv(resultado_manova$stars.tab, file = "Imagens/manova (com processamento)_FILOGENETICO.csv", row.names = TRUE)

dados_manova <- read.csv("Imagens/manova (com processamento)_FILOGENETICO.csv", sep = ",", row.names = 1)

library(dplyr)
library(tidyr)
library(ggplot2)

# Definir a ordem desejada das espécies
#species_order <- c("ALB", "ANO", "CAR", "CRA", "MOR", "OBL", "RUF")  
species_order <- c("CAL", "CAN", "CCA", "CME", "CMO", "CRU")

# Forçar a ordem das espécies em ambos os eixos
dados_long <- dados_manova %>%
  mutate(Observação = row.names(dados_manova)) %>%
  pivot_longer(
    cols = -Observação,
    names_to = "Variável",
    values_to = "Significância"
  ) %>%
  mutate(
    Significância_desc = case_when(
      Significância == "***" ~ "italic('p') < 0.001",
      Significância == "**"  ~ "italic('p') < 0.01", 
      Significância == "*"   ~ "italic('p') < 0.05",
      Significância == "."   ~ "italic('p') < 0.1",
      Significância == "-"   ~ "italic('p') >= 0.05",
      Significância == "NA"  ~ "Não aplicável",
      TRUE ~ NA_character_
    ),
    # Forçar a ordem das espécies tanto em Observação quanto em Variável
    Observação = factor(Observação, levels = species_order),
    Variável   = factor(Variável, levels = species_order)
  )

# Salvar o gráfico
png("Imagens/MANOVA (com processamento)_FILOGENETICO.png", width = 10, height = 8, units = "in", res = 300)

ggplot(dados_long, aes(x = Variável, y = Observação, fill = Significância_desc)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    name = "Significance Level",
    values = c(
      "Não aplicável" = "grey90",
      "italic('p') >= 0.05" = "grey90",
      "italic('p') < 0.1" = "#E3F2FD",
      "italic('p') < 0.05" = "#90CAF9",
      "italic('p') < 0.01" = "#1E88E5",
      "italic('p') < 0.001" = "#0D47A1"
    ),
    labels = c(
      "Não aplicável" = "Não aplicável",
      "italic('p') >= 0.05" = expression(italic('p') >= 0.05),
      "italic('p') < 0.1" = expression(italic('p') < 0.1),
      "italic('p') < 0.05" = expression(italic('p') < 0.05),
      "italic('p') < 0.01" = expression(italic('p') < 0.01),
      "italic('p') < 0.001" = expression(italic('p') < 0.001)
    ),
    drop = FALSE,
    na.translate = FALSE
  ) +
  theme_minimal() +
  labs(x = "Variable", y = "Species") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.position = "right"
  ) +
  coord_fixed()

dev.off()
