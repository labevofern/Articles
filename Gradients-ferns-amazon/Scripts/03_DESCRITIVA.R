#===============================================================================
# DESCRIÇÃO DA COMUNIDADE
#===============================================================================

library(vegan)
library(psych)

cat("============================================================\n")
cat("DESCRIÇÃO DA COMUNIDADE\n")
cat("============================================================\n\n")

#------------------------------------------------------------------
# Índices por parcela
#------------------------------------------------------------------

riqueza <- specnumber(community_data)

abundancia <- rowSums(community_data)

shannon <- diversity(community_data, index = "shannon")

ocorrencia <- colSums(community_data > 0)

#------------------------------------------------------------------
# Resumo da comunidade
#------------------------------------------------------------------

cat("Número de parcelas:", nrow(community_data), "\n")
cat("Número de espécies:", ncol(community_data), "\n")
cat("Número total de indivíduos:", sum(community_data), "\n\n")

#------------------------------------------------------------------
# Estatísticas descritivas
#------------------------------------------------------------------

resumo_comunidade <- data.frame(
  
  Parcela = rownames(community_data),
  
  Riqueza = riqueza,
  
  Abundancia = abundancia,
  
  Shannon = shannon
  
)

cat("------------------------------------------------------------\n")
cat("Riqueza\n")
cat("------------------------------------------------------------\n")

print(psych::describe(resumo_comunidade$Riqueza))

cat("\n")

cat("------------------------------------------------------------\n")
cat("Abundância\n")
cat("------------------------------------------------------------\n")

print(psych::describe(resumo_comunidade$Abundancia))

cat("\n")

cat("------------------------------------------------------------\n")
cat("Shannon\n")
cat("------------------------------------------------------------\n")

print(psych::describe(resumo_comunidade$Shannon))

cat("\n")

cat("------------------------------------------------------------\n")
cat("Variáveis ambientais\n")
cat("------------------------------------------------------------\n")

print(psych::describe(env_data))

cat("\n")

#------------------------------------------------------------------
# Espécies mais frequentes
#------------------------------------------------------------------

freq <- data.frame(
  
  Especie = names(ocorrencia),
  
  Parcelas = ocorrencia,
  
  Frequencia = round(ocorrencia/nrow(community_data)*100,1)
  
)

freq <- freq[order(-freq$Parcelas),]

cat("------------------------------------------------------------\n")
cat("Espécies mais frequentes\n")
cat("------------------------------------------------------------\n")

print(head(freq,10))

cat("\n")

#------------------------------------------------------------------
# Espécies raras
#------------------------------------------------------------------

raras <- subset(freq, Parcelas == 1)

cat("Número de espécies registradas em apenas uma parcela:",
    nrow(raras), "\n\n")

#------------------------------------------------------------------
# Exportação
#------------------------------------------------------------------

write.csv(
  
  resumo_comunidade,
  
  "Resultados/Resumo_Comunidade.csv",
  
  row.names = FALSE
  
)

write.csv(
  
  freq,
  
  "Resultados/Frequencia_Especies.csv",
  
  row.names = FALSE
  
)

gt::gt(resumo_comunidade)

library(ggplot2)

ggplot(resumo_comunidade,
       aes(Riqueza))+
  
  geom_histogram(
    bins=8,
    fill="forestgreen",
    color="black")+
  
  theme_bw()
ggplot(resumo_comunidade,
       aes(Abundancia))+
  
  geom_histogram(
    bins=8,
    fill="steelblue",
    color="black")+
  
  theme_bw()

#===========================================================
# PAINEL DAS VARIÁVEIS AMBIENTAIS
#===========================================================

library(tidyverse)

env_long <- env_data %>%
  tibble::rownames_to_column("Parcela") %>%
  pivot_longer(
    cols = -Parcela,
    names_to = "Variavel",
    values_to = "Valor"
  )

ggplot(env_long,
       aes(x = "", y = Valor)) +
  
  geom_boxplot(
    fill = "forestgreen",
    alpha = 0.7,
    outlier.color = "red"
  ) +
  
  facet_wrap(~Variavel,
             scales = "free_y",
             ncol = 3) +
  
  labs(
    x = "",
    y = "Valor"
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    
    strip.background = element_rect(fill = "grey90"),
    
    strip.text = element_text(face = "bold"),
    
    axis.text.x = element_blank(),
    
    axis.ticks.x = element_blank())

ggsave(
  "Figuras/PT/Boxplots_Ambientais_PT.png",
  width = 9,
  height = 7,
  dpi = 300
)

ggsave(
  "Figuras/PT/Boxplots_Ambientais_PT.pdf",
  width = 9,
  height = 7
)

nomes_en <- c(
  
  canoppy = "Canopy cover",
  pH_solo = "Soil pH",
  carbono_solo = "Soil carbon",
  umidade_solo = "Soil moisture",
  inclinacao = "Slope",
  nivel_agua = "Water table",
  altitude = "Elevation",
  argila = "Clay"
  
)
env_long_en <- env_long

env_long_en$Variavel <-
  nomes_en[env_long_en$Variavel]
ggplot(env_long_en,
       aes(x="",y=Valor))+
  
  geom_boxplot(
    fill="steelblue",
    alpha=.7)+
  
  facet_wrap(~Variavel,
             scales="free_y",
             ncol=3)+
  
  labs(
    x="",
    y="Value"
  )+
  
  theme_bw()
ggsave(
  "Figuras/EN/Environmental_Boxplots_EN.png",
  width=9,
  height=7,
  dpi=300
)
nomes_pt <- c(
  
  canoppy = "Cobertura do dossel",
  pH_solo = "pH",
  carbono_solo = "Carbono",
  umidade_solo = "Umidade",
  inclinacao = "Inclinação",
  nivel_agua = "Nível da água",
  altitude = "Altitude",
  argila = "Argila"

)

env_long <- env_data |>
  tibble::rownames_to_column("Parcela") |>
  tidyr::pivot_longer(
    cols = -Parcela,
    names_to = "Variavel",
    values_to = "Valor"
  )

env_long$Variavel <-
  nomes_pt[env_long$Variavel]
grafico_PT <- ggplot(
  env_long,
  aes(x = "", y = Valor)
) +
  
  geom_violin(
    fill = "#7FC97F",
    color = "grey30",
    alpha = 0.7,
    trim = FALSE
  ) +
  
  geom_boxplot(
    width = 0.15,
    fill = "white",
    outlier.shape = NA,
    linewidth = 0.4
  ) +
  
  geom_jitter(
    width = 0.08,
    size = 1.8,
    alpha = 0.8
  ) +
  
  facet_wrap(
    ~Variavel,
    scales = "free_y",
    ncol = 3
  ) +
  
  labs(
    
    x = "",
    
    y = "Valor"
    
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    
    strip.background = element_rect(fill = "grey90"),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    axis.text.x = element_blank(),
    
    axis.ticks.x = element_blank(),
    
    panel.grid = element_blank()
    
  )
grafico_PT
ggsave(
  
  "Figuras/PT/Variaveis_Ambientais_PT.png",
  
  grafico_PT,
  
  width = 10,
  
  height = 8,
  
  dpi = 600
  
)

