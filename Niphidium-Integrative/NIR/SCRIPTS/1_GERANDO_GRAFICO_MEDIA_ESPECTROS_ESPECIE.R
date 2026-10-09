###############################################################
# Article: ........................... 
# Authors: Marise H.V. de Oliveira, Niksoney A. Mendonça and Thaís E. Almeida
# Script author: Marise Oliveira
# E-mail: mariseohv@gmail.com
# Script: Grafico espectral médio por hipótese de espécie - NIR
# Descrição:Este script calcula o espectro médio NIR para cada espécie
# de Niphidium e gera um gráfico de absorbância por 
# comprimento de onda (900–1700 nm) NIR-S-G1 portátil.
###############################################################

# ============================================================
# 2. LIMPAR AMBIENTE DE TRABALHO
# Remove objetos previamente carregados na sessão do R
# ============================================================

rm(list = ls())
# ============================================================
# 1. CARREGAR PACOTES NECESSÁRIOS
# ============================================================

library(ggplot2)   # criação de gráficos
library(dplyr)     # manipulação de dados


# ============================================================
# 3. DEFINIR DIRETÓRIO DE TRABALHO
# Caminho onde estão os dados e onde a figura será salva
# ============================================================

setwd("C:/Users/Jhonathan/Desktop/RESULTADOS_ANALISES/NIR/MORFOLOGICA")


# ============================================================
# 4. IMPORTAR MATRIZ DE DADOS
# A matriz contém:
# - coluna de identificação da espécie
# - colunas espectrais (comprimentos de onda)
# ============================================================

Matriz_NIR <- read.table(
  file = "dado_morfologico_2026.csv",
  sep = ";",
  header = TRUE
)


# ============================================================
# 5. VERIFICAR NOMES DAS COLUNAS
# Importante para confirmar que os dados foram lidos corretamente
# ============================================================

print(colnames(Matriz_NIR))


# ============================================================
# 6. DEFINIR A VARIÁVEL DE AGRUPAMENTO
# Neste caso, os espectros serão agrupados por espécie
# ============================================================

categoria <- "especie"


# ============================================================
# 7. IDENTIFICAR COLUNAS COM DADOS ESPECTRAIS
# As colunas espectrais começam com "X" (ex: X901, X902...)
# ============================================================

cls <- grep("^X", colnames(Matriz_NIR), ignore.case = FALSE)


# ============================================================
# 8. SELECIONAR APENAS AS COLUNAS ESPECTRAIS
# ============================================================

dd <- Matriz_NIR[, cls]


# ============================================================
# 9. CALCULAR ESPECTRO MÉDIO POR ESPÉCIE
# ============================================================

dt <- aggregate(
  dd,
  by = list(Matriz_NIR[, categoria]),
  FUN = mean
)


# ============================================================
# 10. EXTRAIR OS COMPRIMENTOS DE ONDA
# Remove o "X" dos nomes das colunas e converte para número
# ============================================================

xx <- colnames(dt[, -1])
xx <- as.numeric(gsub("X", "", xx))
xx <- floor(xx)  # remove casas decimais

print(xx)


# ============================================================
# 11. VERIFICAR SE OS DADOS ESTÃO NO INTERVALO ESPERADO
# (900–1700 nm)
# ============================================================

if(all(xx >= 900 & xx <= 1700)) {
  
  cat("Os dados de comprimento de onda estão no intervalo correto (900–1700 nm)\n")
  
} else {
  
  stop("Erro: comprimentos de onda fora do intervalo esperado (900–1700 nm)")
  
}


# ============================================================
# 12. ORGANIZAR DADOS PARA O GGPLOT
# ============================================================

dados_plot <- data.frame(
  
  x = rep(xx, nrow(dt)),
  y = as.vector(t(dt[, -1])),
  group = rep(dt[, 1], each = length(xx))
  
)

print(head(dados_plot))


# ============================================================
# 13. GERAR GRÁFICO ESPECTRAL
# ============================================================

p1 <- ggplot(dados_plot, aes(x = x, y = y, group = group, color = as.factor(group))) +
  
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  
  scale_color_manual(values = c("red", "blue", "green", "purple", "brown", "pink")) +
  
  scale_x_continuous(
    limits = c(900, 1700),
    breaks = seq(900, 1700, by = 100),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    limits = c(0.12, 0.40),
    breaks = seq(0.10, 0.40, by = 0.05),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Comprimento de Onda (nm)",
    y = "Absorbância"
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.background = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )


# Exibir gráfico
print(p1)


# ============================================================
# 14. SALVAR FIGURA
# ============================================================

dir.create("Figuras", showWarnings = FALSE)

ggsave(
  "Figuras/Graficosespectro_morfologico_2026.png",
  plot = p1,
  width = 7,
  height = 4,
  dpi = 300,
  units = "in"
)
