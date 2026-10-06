library(vegan)
library(ggplot2)

# 1. Carregar dados e rodar acúmulo
dados <- read.csv2("ferns_analise.csv", row.names = 1)
dados <- as.matrix(dados)

set.seed(123)
pool <- poolaccum(dados, permutations = 1000)

# 2. Extrair dados
obs   <- summary(pool)$S
chao  <- summary(pool)$chao
jack1 <- summary(pool)$jack1
boot  <- summary(pool)$boot

df_plot <- data.frame(
  Parcelas   = obs[, 1],
  Observado  = obs[, 2],
  Obs_lower  = obs[, 3],
  Obs_upper  = obs[, 4],
  Chao2      = chao[, 2],
  Jackknife1 = jack1[, 2],
  Bootstrap  = boot[, 2]
)

# 3. Gerar gráfico otimizado com tema_dissertacao()
g_acumulo <- ggplot(df_plot, aes(x = Parcelas)) +
  # Intervalo de Confiança
  geom_ribbon(aes(ymin = Obs_lower, ymax = Obs_upper), fill = "grey85", alpha = 0.6) +
  
  # Linhas dos Estimadores
  geom_line(aes(y = Observado, color = "Observado", linetype = "Observado"), linewidth = 1.1) +
  geom_line(aes(y = Chao2, color = "Chao 2", linetype = "Chao 2"), linewidth = 0.9) +
  geom_line(aes(y = Jackknife1, color = "Jackknife 1", linetype = "Jackknife 1"), linewidth = 0.9) +
  geom_line(aes(y = Bootstrap, color = "Bootstrap", linetype = "Bootstrap"), linewidth = 0.9) +
  
  # Cores da Paleta Oficial da Dissertação
  scale_color_manual(
    name = "Estimadores",
    limits = c("Observado", "Chao 2", "Jackknife 1", "Bootstrap"),
    values = c(
      "Observado"   = "black",
      "Chao 2"      = cores$agua,
      "Jackknife 1" = cores$destaque,
      "Bootstrap"   = cores$solo
    )
  ) +
  
  # Tipos de Linha
  scale_linetype_manual(
    name = "Estimadores",
    limits = c("Observado", "Chao 2", "Jackknife 1", "Bootstrap"),
    values = c(
      "Observado"   = "solid",
      "Chao 2"      = "dashed",
      "Jackknife 1" = "dotdash",
      "Bootstrap"   = "dotted"
    )
  ) +
  
  # Eixos e Rótulos
  labs(
    x = "Número de parcelas",
    y = "Número de espécies (Riqueza)",
    title = "Curva de Acúmulo e Estimadores de Riqueza"
  ) +
  scale_x_continuous(breaks = seq(1, 23, by = 3), expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(1, 23), ylim = c(0, 35)) +
  
  # Aplicação do Tema Padrão
  tema_dissertacao() +
  theme(
    legend.position = "right",
    legend.key.width = unit(1.2, "cm")
  )

print(g_acumulo)

# 4. Salvar
salvar_figura(g_acumulo, nome = "curva_acumulo_especies", tipo = "simples", idioma = "PT")

library(ggplot2)
library(dplyr)

# 1. Dados de preferência de habitat (%) já calculados a partir da sua matriz
df_preferencia <- data.frame(
  Especie = c(
    "Triplophyllum glabrum", "Adiantum glaucescens", "Adiantum lucidum",
    "Lygodium venustum", "Trichomanes pinnatum", "Lomariopsis japurensis",
    "Triplophyllum funestum", "Adiantum dolosum", "Trichomanes vittaria",
    "Adiantum cajennense", "Nephrolepis biserrata", "Triplophyllum dicksonioides",
    "Adiantum terminatum", "Lygodium volubile", "Asplenium serratum",
    "Meniscium serratum", "Pleopeltis desvauxii", "Polytaenium guayanense",
    "Selaginella radiata", "Telmatoblechnum serrulatum", "Adiantum tetraphyllum",
    "Cyathea microdonta"
  ),
  Ambiente = c(rep("Terra Firme", 6), rep("Floresta Ripária", 16)),
  # Valores negativos espelham a Terra Firme para a esquerda do eixo 0
  Valor = c(-100, -100, -92.3, -61.5, -58.3, -50.0,
            71.0, 72.7, 75.0, 75.0, 80.0, 82.6, 
            83.0, 91.7, 100, 100, 100, 100, 100, 100, 100, 100),
  # Rótulos absolutos para exibição
  Label = c("100.0%", "100.0%", "92.3%", "61.5%", "58.3%", "50.0%",
            "71.0%", "72.7%", "75.0%", "75.0%", "80.0%", "82.6%", 
            "83.0%", "91.7%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%", "100.0%")
)

# Ordenar as espécies com base no Valor para criar a curva contínua do Lollipop
df_preferencia$Especie <- factor(df_preferencia$Especie, levels = df_preferencia$Especie[order(df_preferencia$Valor)])
tema_dissertacao() +
  theme(
    text = element_text(face = "plain"),           # Remove o negrito de todos os textos base
    plot.title = element_text(face = "plain"),     # Remove o negrito do título principal
    axis.title = element_text(face = "plain"),     # Remove o negrito dos títulos dos eixos (X e Y)
    axis.text.x = element_text(face = "plain"),    # Remove o negrito dos números do eixo X
    axis.text.y = element_text(face = "italic"),   # Mantém as espécies em itálico
    
    # ... (resto das configurações de margem/grid)
  )
# 2. Construção do Gráfico
g_lollipop <- ggplot(df_preferencia, aes(x = Valor, y = Especie, color = Ambiente)) +
  
  # Linhas de grade horizontais finas (simulando o painel de fundo da sua referência)
  geom_hline(yintercept = seq_along(df_preferencia$Especie), color = "grey90", linewidth = 0.4) +
  
  # Eixo vertical central (Marco Zero / 50-50)
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
  
  # Segmentos e pontos do Lollipop
  geom_segment(aes(x = 0, xend = Valor, y = Especie, yend = Especie), linewidth = 1) +
  geom_point(size = 3.3) +
  
  # Rótulos dinâmicos (posicionados à esquerda ou à direita dependendo da direção)
  geom_text(aes(label = Label, 
                hjust = ifelse(Valor < 0, 1.2, -0.2)), 
            size = 3.0, color = "black", family = "sans") +
  
  # Integração com a paleta oficial da dissertação
  scale_color_manual(values = c("Terra Firme" = cores$vegetacao, "Floresta Ripária" = cores$agua)) +
  
  # Conversão do eixo X contínuo para rótulos espelhados
  scale_x_continuous(
    limits = c(-120, 120), # Margem extra para não cortar os textos das porcentagens
    breaks = c(-100, -50, 0, 50, 100),
    labels = c("100%", "50%", "Preference", "50%", "100%")
  ) +
  
  # Textos
  labs(
    title = "Habitat Preference by Species",
    subtitle = "← Terra Firme Forest                 Riparian Forest →",
    x = "",
    y = NULL
  ) +
  
  # Aplicação do Tema Padrão
  tema_dissertacao() +
  theme(
    axis.text.y = element_text(face = "italic"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#444444", margin = margin(b = 15)),
    legend.position = "none", # Removida pois o subtítulo e o eixo já diferenciam os ambientes
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
  )

print(g_lollipop)

# 3. Salvar usando a função padronizada
salvar_figura(g_lollipop, nome = "lollipop_preferencia_habitat", tipo = "simples", idioma = "EN")
