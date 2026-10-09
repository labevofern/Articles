# =============================================================================
# Morfometria linear
# =============================================================================
# Artigo: TESE MARISE OLIVEIRA........................... 
# Autores: Marise H.V. de Oliveira, Niksoney A. Mendonça e Thaís E. Almeida
# Autor do script: Niksoney Azevedo Mendonça
# E-mail: niksoneyazevedo2017@gmail.com
# ==============================================================================

#======================================================================================
# Remove todos os objetos do ambiente para evitar interferência de análises anteriores
# =====================================================================================
rm(list = ls())

# =============================================================================
# PACOTES NECESSÁRIOS
# =============================================================================
install.packages(c("tidyverse", "FactoMineR", "factoextra", "MASS", "ggrepel", "ggthemes", "ggplot2"))

library(tidyverse)   # Conjunto de pacotes para manipulação, transformação e visualização de dados (inclui dplyr, ggplot2, tidyr, etc.)
library(FactoMineR)  # Usado para análises multivariadas: PCA (componentes principais), CA (análise de correspondência), MFA, etc.
library(factoextra)  # Facilita a visualização e interpretação dos resultados das análises feitas com FactoMineR ou stats
library(MASS)        # Fornece funções estatísticas avançadas e datasets; muito usado em regressão e análise discriminante
library(ggrepel)     # Melhora rótulos de gráficos no ggplot2, evitando sobreposição de textos
library(ggthemes)    # Oferece temas e paletas prontos para personalizar gráficos do ggplot2 (Economist, Wall Street Journal, etc.)
library(ggplot2)     # Pacote principal para visualização de dados — cria gráficos elegantes e personalizáveis


# ==========================================
# LEMBRETE
# ==========================================

# lembrar de rodar (clados) para hipóteses filogenéticas e (especie) para hipóteses morfológicas

# ==========================================
# 0. Preparação
# ==========================================

# Cria pasta para figuras, se não existir
if (!dir.exists("Figuras")) dir.create("Figuras")

# Paleta de cores personalizada (especies)
my_colors_especie <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2"
)

#Paleta de cores personalizada (clados)
my_colors_clados <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B")


# ==========================================
# 1. Ler dados
# ==========================================

# Definir diretório
setwd("C:/Users/Jhonathan/Desktop/LINEAR_COM_VIRGULA")

dados <- read_delim("DADOS_LINEAR_FILOGENETICA.csv",
                    delim = ";",
                    locale = locale(encoding = "Latin1")) |>
  janitor::clean_names()

# Define variável de grupo
#dados$especie <- as.factor(dados$especie) #morfológica
dados$clados <- as.factor(dados$clados) #filogenética


# Separa variáveis numéricas
num_vars <- dados[, sapply(dados, is.numeric)]

# Substitui NAs pela média de cada variável
num_vars <- as.data.frame(lapply(num_vars, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}))

# ==========================================
# 2. PCA
# ==========================================
pca <- prcomp(num_vars, scale. = TRUE)

# ---- Gráfico 1: Scree plot ----
p1 <- fviz_eig(pca, addlabels = TRUE, barfill = "#3bc9db", barcolor = "white") +
  theme_minimal(base_size = 14) +
  labs(title = "Variância explicada por componente") +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path("Figuras", "PCA_ScreePlot_filogenetica.png"), plot = p1, width = 7, height = 7, dpi = 300)
#ggsave(file.path("Figuras", "PCA_ScreePloT_morfologica.png"), plot = p1, width = 7, height = 7, dpi = 300)

# ---- Gráfico 2: Biplot estilizado ----
p2 <- fviz_pca_biplot(
  pca,
  geom.ind = "point",
  habillage = dados$clados,   # ou dados$especie
  addEllipses = FALSE,
  col.var = "gray30",
  palette = my_colors_clados, # ou my_colors_especie
  repel = TRUE,
  pointsize = 2
) +
  scale_shape_manual(values = c(1,2,0,3,8,4,6)) +
  theme_minimal(base_size = 14)

ggsave(file.path("Figuras", "PCA_Biplot_filogenetica.png"), plot = p2, width = 7, height = 7, dpi = 300)
#ggsave(file.path("Figuras", "PCA_Biplot_morfologica.png"), plot = p2, width = 7, height = 7, dpi = 300)

# Loadings (pesos das variáveis nas PCs)
loadings <- as.data.frame(pca$rotation[, 1:3])
round(loadings, 3)

# Instalar se ainda não tiver

library(writexl)

# Salvar em Excel
write_xlsx(loadings, "loadings_PCA_filogenetica.xlsx")
#write_xlsx(loadings, "loadings_PCA_morfologica.xlsx")

# O arquivo será salvo na pasta de trabalho atual
getwd()  # mostra onde ele foi salvo

# ==========================================
# 3. LDA (Análise Discriminante Linear)
# ==========================================

lda_fit <- lda(clados ~ ., data = cbind(clados = dados$clados, num_vars))
pred <- predict(lda_fit)

#lda_fit <- lda(especie ~ ., data = cbind(especie = dados$especie, num_vars))
#pred <- predict(lda_fit)

# Converte para fatores simples
pred_class <- as.factor(as.character(pred$class))
#real_class <- as.factor(as.character(dados$especie))
real_class <- as.factor(as.character(dados$clados))

# ---- Estatísticas ----
acuracia <- mean(pred_class == real_class)
cat("Acurácia geral:", round(acuracia * 100, 2), "%\n\n")

confusao <- table(Predito = pred_class, Real = real_class)
confusao <- confusao[levels(real_class), levels(real_class)]

cat("Matriz de confusão:\n")
print(confusao)

taxa_grupo <- diag(prop.table(confusao, 2))
cat("\nTaxa de acerto por grupo (%):\n")
print(round(100 * taxa_grupo, 2))

prop_var <- lda_fit$svd^2 / sum(lda_fit$svd^2)
cat("\nProporção de variância explicada (LDs):\n")
print(round(100 * prop_var, 2))

# ==========================================
# 4. MATRIZ DE CONFUSÃO VISUAL (HEATMAP)
# ==========================================

# ----------------------------
# ----------------------------
# Calcular CP corretamente (por coluna = classe real)
cp <- round(100 * diag(confusao) / colSums(confusao), 1)

# Matriz normal
conf_mat <- as.matrix(confusao)

# Converter matriz para longo
conf_long <- as.data.frame(as.table(conf_mat))
colnames(conf_long) <- c("Predito", "Real", "Frequencia")

# Dataframe do CP separado
cp_df <- data.frame(
  Predito = rownames(conf_mat),
  Real = "CP (%)",
  Frequencia = cp
)

# Juntar
conf_plot <- rbind(conf_long, cp_df)

# Criar variável para escala (separar matriz vs CP)
conf_plot$Escala <- ifelse(conf_plot$Real == "CP (%)",
                           conf_plot$Frequencia / 10,  # traz CP pra escala da matriz
                           conf_plot$Frequencia)

# ----------------------------
# Plot
p3 <- ggplot(conf_plot, aes(x = Real, y = Predito, fill = Escala)) +
  
  geom_tile(color = "white", linewidth = 0.3) +
  
  geom_text(aes(label = ifelse(Real == "CP (%)",
                               gsub("\\.", ",", Frequencia),
                               ifelse(Frequencia == 0, "",
                                      Frequencia))),
            size = 4, color = "black") +
  
  scale_fill_gradient(
    low = "#f0f4f8",   # bem claro
    high = "#08306d",  # azul escuro forte
    limits = c(0, 10),  # 🔥 fixa a escala
    breaks = c(0, 2, 4, 6,8, 10),
    oob = scales::squish
  )+
  
  labs(
    title = paste0("Matriz de Confusão — LDA (Acurácia: ",
                   gsub("\\.", ",", round(acuracia * 100, 1)), "%)"),
    x = "Classe Real",
    y = "Classe Predita",
    fill = "Frequência"
  ) +
  
  coord_equal() +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Mostrar
p3

# Salvar
ggsave(file.path("Figuras", "LDA_MatrizConfusao_filogeneticaj.png"),
       plot = p3, width = 7, height = 7, dpi = 300)

# ==========================================
# 5. IMPORTÂNCIA DAS VARIÁVEIS (LDA)
# ==========================================

coef_lda <- as.data.frame(lda_fit$scaling)
coef_lda$Importancia_Total <- apply(abs(coef_lda), 1, sum)
coef_lda_ord <- coef_lda[order(-coef_lda$Importancia_Total), ]

cat("\n=== Variáveis mais importantes para separar as espécies ===\n")
print(round(coef_lda_ord, 4))

top_vars <- head(coef_lda_ord, 10) |> 
  rownames_to_column("Variavel")

p4 <- ggplot(top_vars, aes(x = reorder(Variavel, Importancia_Total), 
                           y = Importancia_Total)) +
  geom_col(fill = "#08306d") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Variáveis mais explicativas",
    x = "Variável métrica",
    y = "Importância total (coeficientes absolutos)"
  ) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path("Figuras", "LDA_Top10_Variaveis_filogenetica.png"), plot = p4, width = 7, height = 7, dpi = 300)
#ggsave(file.path("Figuras", "LDA_Top10_Variaveis_morfológica.png"), plot = p4, width = 7, height = 7, dpi = 300)


