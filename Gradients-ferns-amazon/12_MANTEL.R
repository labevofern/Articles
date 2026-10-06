#===========================================================
# INSTALAÇÃO E CARREGAMENTO DE PACOTES
#===========================================================
if (!require("vegan")) install.packages("vegan")
if (!require("indicspecies")) install.packages("indicspecies")

library(vegan)
library(indicspecies)

#===========================================================
# DEFINIÇÃO DOS GRUPOS E CRIAÇÃO DO FATOR DE HABITAT
#===========================================================
parcelas_riparia <- c(1, 2, 5, 6, 13, 18, 22)
parcelas_terra_firme <- c(3, 4, 7, 10, 11, 12, 14, 15, 16, 17, 19, 20, 21, 23, 26, 27)

# Cria a coluna Grupo no env_data com base nos numerais dos nomes das linhas (rownames)
id_numeric <- as.numeric(gsub("\\D", "", rownames(env_data)))

env_data$Grupo <- NA
env_data$Grupo[id_numeric %in% parcelas_riparia] <- "Ripária"
env_data$Grupo[id_numeric %in% parcelas_terra_firme] <- "Terra Firme"

env_data$Grupo <- factor(env_data$Grupo)

#===========================================================
# 1. PROPORÇÃO ENTRE SAMAMBAIAS E LICÓFITAS
#===========================================================
cat("\n=========================================\n")
cat("1. PROPORÇÃO LICÓFITAS X SAMAMBAIAS\n")
cat("=========================================\n")

lyco_sp <- grep("Selaginella", colnames(community_data), value = TRUE, ignore.case = TRUE)
monilo_sp <- setdiff(colnames(community_data), lyco_sp)

n_lyco <- length(lyco_sp)
n_monilo <- length(monilo_sp)
total_sp <- ncol(community_data)

abund_lyco <- sum(community_data[, lyco_sp, drop = FALSE])
abund_monilo <- sum(community_data[, monilo_sp, drop = FALSE])
abund_total <- sum(community_data)

cat("Riqueza Monilófitas (Samambaias):", n_monilo, "(", round(n_monilo/total_sp * 100, 1), "%)\n")
cat("Riqueza Licófitas:", n_lyco, "(", round(n_lyco/total_sp * 100, 1), "%)\n")
cat("Abundância Monilófitas:", abund_monilo, "(", round(abund_monilo/abund_total * 100, 1), "%)\n")
cat("Abundância Licófitas:", abund_lyco, "(", round(abund_lyco/abund_total * 100, 1), "%)\n")

#===========================================================
# 2. CURVA DE ACUMULAÇÃO E ESTIMADORES DE RIQUEZA
#===========================================================
cat("\n=========================================\n")
cat("2. ESTIMADORES DE RIQUEZA E RAREFAÇÃO\n")
cat("=========================================\n")

pool_res <- specpool(community_data)
print(pool_res)

sac <- specaccum(community_data, method = "random", permutations = 999)

dev.new()
plot(sac, ci.type = "polygon", ci.col = "lightgray", ci.lty = 0, col = "darkgreen",
     lwd = 2, xlab = "Número de Parcelas Amostradas", ylab = "Riqueza Acumulada de Espécies",
     main = "Curva de Acumulação de Espécies (REBIO Gurupi)")
abline(h = pool_res$chao, col = "red", lty = 2)

#===========================================================
# 3. ANÁLISE DE ESPÉCIES INDICADORAS (IndVal)
#===========================================================
cat("\n=========================================\n")
cat("3. ANÁLISE DE ESPÉCIES INDICADORAS (IndVal)\n")
cat("=========================================\n")

indval_res <- multipatt(community_data, env_data$Grupo, func = "IndVal.g", duleg = TRUE, control = how(nperm = 999))
summary(indval_res, alpha = 0.05)

#===========================================================
# 4. COMPARAÇÃO DIRETA ENTRE HABITATS (Ripária vs Terra Firme)
#===========================================================
cat("\n=========================================\n")
cat("4. COMPARAÇÃO ENTRE MATA RIPÁRIA E TERRA FIRME\n")
cat("=========================================\n")

riq_plot <- rowSums(community_data > 0)
abund_plot <- rowSums(community_data)
shannon_plot <- diversity(community_data, index = "shannon")

df_hab <- data.frame(
  Grupo = env_data$Grupo,
  Riqueza = riq_plot,
  Abundancia = abund_plot,
  Shannon = shannon_plot
)

cat("\n--- Médias e Desvios por Habitat ---\n")
print(aggregate(. ~ Grupo, data = df_hab, FUN = function(x) paste0(round(mean(x), 2), " ± ", round(sd(x), 2))))

cat("\n--- Teste de Mann-Whitney (Riqueza) ---\n")
print(wilcox.test(Riqueza ~ Grupo, data = df_hab))

cat("\n--- Teste de Mann-Whitney (Abundância) ---\n")
print(wilcox.test(Abundancia ~ Grupo, data = df_hab))

rip_mask <- env_data$Grupo == "Ripária"
tf_mask <- env_data$Grupo == "Terra Firme"

sp_riparia <- colnames(community_data)[colSums(community_data[rip_mask, , drop = FALSE]) > 0]
sp_tf <- colnames(community_data)[colSums(community_data[tf_mask, , drop = FALSE]) > 0]

exclusivas_riparia <- setdiff(sp_riparia, sp_tf)
exclusivas_tf <- setdiff(sp_tf, sp_riparia)
compartilhadas <- intersect(sp_riparia, sp_tf)

cat("\nEspécies exclusivas da Mata Ripária (", length(exclusivas_riparia), "): ", paste(exclusivas_riparia, collapse = ", "), "\n")
cat("Espécies exclusivas da Terra Firme (", length(exclusivas_tf), "): ", paste(exclusivas_tf, collapse = ", "), "\n")
cat("Espécies compartilhadas entre habitats (", length(compartilhadas), "): ", paste(compartilhadas, collapse = ", "), "\n")


#===========================================================
# INSTALAÇÃO DO PACOTE OPENXLSX
#===========================================================
if (!require("openxlsx")) install.packages("openxlsx")
library(openxlsx)

#===========================================================
# 1. PREPARAÇÃO DAS TABELAS
#===========================================================

# Aba 1: Proporção das Linhagens
df_proporcao <- data.frame(
  Grupo_Taxonomico = c("Monilófitas (Samambaias)", "Licófitas", "Total"),
  Riqueza = c(n_monilo, n_lyco, total_sp),
  Porcentagem_Riqueza = round(c(n_monilo/total_sp * 100, n_lyco/total_sp * 100, 100), 2),
  Abundancia = c(abund_monilo, abund_lyco, abund_total),
  Porcentagem_Abundancia = round(c(abund_monilo/abund_total * 100, abund_lyco/abund_total * 100, 100), 2)
)

# Aba 2: Estimadores de Riqueza (specpool)
df_estimadores <- data.frame(
  Metrica = c("Riqueza Observada (S_obs)", "Chao 1", "Jackknife 1", "Jackknife 2", "Bootstrap"),
  Valor_Estimado = c(pool_res$Species, pool_res$chao, pool_res$jack1, pool_res$jack2, pool_res$boot),
  Erro_Padrao = c(NA, pool_res$chao.se, pool_res$jack1.se, NA, pool_res$boot.se),
  Cobertura_Amostral_Pct = round(c(100, (pool_res$Species / pool_res$chao) * 100, 
                                   (pool_res$Species / pool_res$jack1) * 100, 
                                   (pool_res$Species / pool_res$jack2) * 100, 
                                   (pool_res$Species / pool_res$boot) * 100), 2)
)

# Aba 3: Espécies Indicadoras (IndVal)
df_indval <- data.frame(
  Especie = c("Triplophyllum_dicksonioides", "Lygodium_volubile", "Adiantum_terminatum"),
  Habitat = c("Mata Ripária", "Mata Ripária", "Mata Ripária"),
  IndVal = c(0.878, 0.741, 0.724),
  p_valor = c(0.001, 0.008, 0.030),
  Significancia = c("***", "**", "*")
)

# Aba 4: Comparação entre Habitats (Médias e Testes)
if (exists("df_hab")) {
  # Tabela de Médias ± SD
  df_comparacao_stats <- aggregate(. ~ Grupo, data = df_hab, FUN = function(x) paste0(round(mean(x), 2), " ± ", round(sd(x), 2)))
  
  # Resultados dos testes de Mann-Whitney
  w_riq <- wilcox.test(Riqueza ~ Grupo, data = df_hab)
  w_abund <- wilcox.test(Abundancia ~ Grupo, data = df_hab)
  
  df_testes <- data.frame(
    Variavel = c("Riqueza", "Abundância"),
    Estatistica_W = c(w_riq$statistic, w_abund$statistic),
    p_valor = c(w_riq$p.value, w_abund$p.value)
  )
}

# Aba 5: Espécies Exclusivas e Compartilhadas
if (exists("exclusivas_riparia")) {
  max_len <- max(length(exclusivas_riparia), length(exclusivas_tf), length(compartilhadas))
  
  df_distribuicao <- data.frame(
    Exclusivas_Riparia = c(exclusivas_riparia, rep(NA, max_len - length(exclusivas_riparia))),
    Exclusivas_Terra_Firme = c(exclusivas_tf, rep(NA, max_len - length(exclusivas_tf))),
    Compartilhadas = c(compartilhadas, rep(NA, max_len - length(compartilhadas)))
  )
}

#===========================================================
# 2. CRIAÇÃO E SALVAMENTO DA PLANILHA EXCEL
#===========================================================

wb <- createWorkbook()

addWorksheet(wb, "Linhagens")
writeData(wb, "Linhagens", df_proporcao)

addWorksheet(wb, "Estimadores_Riqueza")
writeData(wb, "Estimadores_Riqueza", df_estimadores)

addWorksheet(wb, "Especies_Indicadoras")
writeData(wb, "Especies_Indicadoras", df_indval)

if (exists("df_comparacao_stats")) {
  addWorksheet(wb, "Medias_Habitats")
  writeData(wb, "Medias_Habitats", df_comparacao_stats)
  
  addWorksheet(wb, "Teste_Mann_Whitney")
  writeData(wb, "Teste_Mann_Whitney", df_testes)
}

if (exists("df_distribuicao")) {
  addWorksheet(wb, "Distribuicao_Habitats")
  writeData(wb, "Distribuicao_Habitats", df_distribuicao)
}

# Salva a planilha na pasta de trabalho ativa
saveWorkbook(wb, "Resultados_Complementares_Ferns.xlsx", overwrite = TRUE)

cat("\nPlanilha 'Resultados_Complementares_Ferns.xlsx' gerada com sucesso!\n")

#===========================================================
# INSTALAÇÃO E CARREGAMENTO DE PACOTES
#===========================================================
if (!require("readxl")) install.packages("readxl")
if (!require("vegan")) install.packages("vegan")

library(readxl)
library(vegan)

library(vegan)

# 1. Tabela de coordenadas em graus decimais
coords_df <- data.frame(
  plot = c(1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 26, 27),
  Latitude = c(-3.279333, -3.285917, -3.280278, -3.283500, -3.285028, -3.287083, -3.287472, 
               -3.272556, -3.275972, -3.279222, -3.280861, -3.282278, -3.285611, -3.558639, 
               -3.558472, -3.558389, -3.558111, -3.558222, -3.557833, -3.557972, -3.568667, 
               -3.567833, -3.567722),
  Longitude = c(-46.714639, -46.687278, -46.720389, -46.711667, -46.708417, -46.704167, -46.703500, 
                -46.714583, -46.706333, -46.698222, -46.693722, -46.689833, -46.681500, -46.804111, 
                -46.795306, -46.788639, -46.786444, -46.777556, -46.768778, -46.768389, -46.803333, 
                -46.776778, -46.768000)
)

# 2. Ajusta as linhas para corresponderem exatamente à ordem do community_data
rownames(coords_df) <- coords_df$plot

# Se as linhas de community_data forem números/strings (ex: "1", "2"...):
coords_sinc <- coords_df[as.character(rownames(community_data)), c("Latitude", "Longitude")]

# 3. Verificação de segurança (deve retornar FALSE)
if (anyNA(coords_sinc)) {
  stop("Ainda existem NAs! Verifique se os nomes das linhas do community_data coincidem com a coluna 'plot'.")
}

# 4. Matrizes de Distância e Teste de Mantel
dist_geo <- dist(coords_sinc)
dist_flor <- vegdist(community_data, method = "bray")

mantel_res <- mantel(dist_flor, dist_geo, method = "pearson", permutations = 999)
print(mantel_res)



#===========================================================
# PACOTES NECESSÁRIOS
#===========================================================
if (!require("vegan")) install.packages("vegan")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("patchwork")) install.packages("patchwork")

library(vegan)
library(ggplot2)
library(patchwork)

#===========================================================
# 1. CÁLCULO DAS MATRIZES DE DISTÂNCIA
#===========================================================

# A) Distância Florística (Bray-Curtis)
dist_flor <- vegdist(community_data, method = "bray")
# Converte colunas de texto com vírgula para números
env_data[] <- lapply(env_data, function(x) {
  if (is.character(x)) {
    # Tenta converter vírgula para ponto e mudar para numérico
    num_x <- as.numeric(gsub(",", ".", x))
    if (!any(is.na(num_x))) return(num_x)
  }
  return(x)
})

# Em seguida, seleciona apenas as colunas numéricas
env_num <- env_data[, sapply(env_data, is.numeric)]
env_scaled <- decostand(env_num, method = "standardize")
dist_env <- dist(env_scaled)
# B) Distância Ambiental (Padronização + Distância Euclidiana)
# Substitua 'env_data' pelo seu data.frame com as variáveis ambientais
env_scaled <- decostand(env_data, method = "standardize") 
dist_env <- dist(env_scaled)

# C) Distância Geográfica (Euclidiana com coordenadas alinhadas)
coords_sinc <- coords_df[as.character(rownames(community_data)), c("Latitude", "Longitude")]
dist_geo <- dist(coords_sinc)

#===========================================================
# 2. EXECUÇÃO DOS TESTES DE MANTEL
#===========================================================
mantel_env <- mantel(dist_flor, dist_env, method = "pearson", permutations = 999)
mantel_geo <- mantel(dist_flor, dist_geo, method = "pearson", permutations = 999)

# Extração dos coeficientes e p-valores
r_env <- round(mantel_env$statistic, 4)
p_env <- mantel_env$signif

r_geo <- round(mantel_geo$statistic, 4)
p_geo <- mantel_geo$signif

#===========================================================
# 3. CONSTRUÇÃO DOS GRÁFICOS (GGPLOT2)
#===========================================================

# Data.frames para plotagem
df_env <- data.frame(dist_env = as.vector(dist_env), dist_flor = as.vector(dist_flor))
df_geo <- data.frame(dist_geo = as.vector(dist_geo), dist_flor = as.vector(dist_flor))

# Estilo visual padronizado
tema_publicacao <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Painel Esquerdo: Florística x Ambiental
p1 <- ggplot(df_env, aes(x = dist_env, y = dist_flor)) +
  geom_point(color = "#66a182", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "#1e4d3a", fill = "#d9d9d9", se = TRUE, linewidth = 1) +
  labs(x = "Environmental Distance", y = "Floristic distance") +
  annotate(
    "text", x = max(df_env$dist_env), y = min(df_env$dist_flor),
    label = paste0("r = ", sprintf("%.4f", r_env), "\np = ", sprintf("%.3f", p_env)),
    hjust = 1, vjust = 0, size = 4, fontface = "italic"
  ) +
  tema_publicacao

# Painel Direito: Florística x Geográfica
p2 <- ggplot(df_geo, aes(x = dist_geo, y = dist_flor)) +
  geom_point(color = "#66a182", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "#1e4d3a", fill = "#d9d9d9", se = TRUE, linewidth = 1) +
  labs(x = "Geographical distance", y = "Floristic distance") +
  annotate(
    "text", x = max(df_geo$dist_geo), y = min(df_geo$dist_flor),
    label = paste0("r = ", sprintf("%.4f", r_geo), "\np = ", sprintf("%.3f", p_geo)),
    hjust = 1, vjust = 0, size = 4, fontface = "italic"
  ) +
  tema_publicacao

#===========================================================
# 4. COMBINAÇÃO E EXPORTAÇÃO
#===========================================================
p_duplo <- p1 + p2

ggsave(
  filename = "Figura_Mantel_Ambiental_Geografica.png",
  plot = p_duplo,
  width = 10,
  height = 4.8,
  dpi = 300
)

# 1. Cria a tabela com os resultados dos dois testes
tabela_mantel <- data.frame(
  Matriz_Preditora = c("Distancia Ambiental", "Distancia Geografica"),
  Coeficiente_r = c(round(mantel_env$statistic, 4), round(mantel_geo$statistic, 4)),
  p_valor = c(mantel_env$signif, mantel_geo$signif),
  Significancia = ifelse(c(mantel_env$signif, mantel_geo$signif) <= 0.01, "**", "*")
)

# 2. Mostra a tabela no console do RStudio
print(tabela_mantel)

# 3. Salva em CSV (formato padrão do Excel brasileiro com separador ';')
write.csv2(tabela_mantel, "Tabela_Resultados_Mantel.csv", row.names = FALSE)