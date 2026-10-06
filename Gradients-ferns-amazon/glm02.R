#=========================================================
# 08 - GENERALIZED LINEAR MODELS
#=========================================================

#=========================================================
# PACOTES
#=========================================================

library(glmmTMB)
library(MuMIn)
library(DHARMa)
library(ggeffects)
library(broom.mixed)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(patchwork)


#=========================================================
# 1. MÉTRICAS DA COMUNIDADE
#=========================================================

# Riqueza de espécies
S <- vegan::specnumber(community_data)

# Abundância total
N <- rowSums(community_data)

# Diversidade de Shannon
H <- vegan::diversity(
  community_data,
  index = "shannon"
)


#=========================================================
# 2. DATAFRAME DOS GLMs
#=========================================================

df_taxonomico <- env_data

df_taxonomico$S <- S
df_taxonomico$N <- N
df_taxonomico$Shannon <- H


#=========================================================
# 3. PREDITORES
#=========================================================

preditores <- c(
  "canoppy",
  "pH_solo",
  "carbono_solo",
  "umidade_solo",
  "inclinacao",
  "nivel_agua",
  "altitude",
  "argila"
)


#=========================================================
# 4. REMOVER NAs
#=========================================================

df_glm <- na.omit(
  df_taxonomico[
    c(
      "S",
      "N",
      "Shannon",
      preditores
    )
  ]
)


# Conferir número de observações
cat("\nNúmero de observações utilizadas nos GLMs:", nrow(df_glm), "\n")


#=========================================================
# 5. PADRONIZAÇÃO DOS PREDITORES
#=========================================================

df_glm_scaled <- df_glm

df_glm_scaled[preditores] <-
  scale(df_glm_scaled[preditores])


# Conferir os dados
print(df_glm_scaled)


#=========================================================
# 6. MODELO COMPLETO - RIQUEZA
#=========================================================

modelo_S <- glmmTMB(
  
  S ~
    canoppy +
    pH_solo +
    carbono_solo +
    umidade_solo +
    inclinacao +
    nivel_agua +
    altitude +
    argila,
  
  data = df_glm_scaled,
  
  family = nbinom2
)


#=========================================================
# 7. MODELO COMPLETO - ABUNDÂNCIA
#=========================================================

modelo_N <- glmmTMB(
  
  N ~
    canoppy +
    pH_solo +
    carbono_solo +
    umidade_solo +
    inclinacao +
    nivel_agua +
    altitude +
    argila,
  
  data = df_glm_scaled,
  
  family = nbinom2
)


#=========================================================
# 8. MODELO COMPLETO - SHANNON
#=========================================================

modelo_H <- glmmTMB(
  
  Shannon ~
    canoppy +
    pH_solo +
    carbono_solo +
    umidade_solo +
    inclinacao +
    nivel_agua +
    altitude +
    argila,
  
  data = df_glm_scaled,
  
  family = gaussian()
)


#=========================================================
# 9. RESUMO DOS MODELOS COMPLETOS
#=========================================================

cat("\n========================================\n")
cat("MODELO - SPECIES RICHNESS\n")
cat("========================================\n")

summary(modelo_S)


cat("\n========================================\n")
cat("MODELO - ABUNDANCE\n")
cat("========================================\n")

summary(modelo_N)


cat("\n========================================\n")
cat("MODELO - SHANNON DIVERSITY\n")
cat("========================================\n")

summary(modelo_H)


#=========================================================
# 10. SELEÇÃO DE MODELOS POR AICc
#=========================================================

options(na.action = "na.fail")


#---------------------------------------------------------
# Riqueza
#---------------------------------------------------------

dredge_S <- dredge(modelo_S)

best_S <- get.models(
  dredge_S,
  1
)[[1]]


#---------------------------------------------------------
# Abundância
#---------------------------------------------------------

dredge_N <- dredge(modelo_N)

best_N <- get.models(
  dredge_N,
  1
)[[1]]


#---------------------------------------------------------
# Shannon
#---------------------------------------------------------

dredge_H <- dredge(modelo_H)

best_H <- get.models(
  dredge_H,
  1
)[[1]]


#=========================================================
# 11. RESUMO DOS MODELOS SELECIONADOS
#=========================================================

cat("\n========================================\n")
cat("BEST MODEL - SPECIES RICHNESS\n")
cat("========================================\n")

summary(best_S)


cat("\n========================================\n")
cat("BEST MODEL - ABUNDANCE\n")
cat("========================================\n")

summary(best_N)


cat("\n========================================\n")
cat("BEST MODEL - SHANNON\n")
cat("========================================\n")

summary(best_H)


#=========================================================
# 12. SALVAR TABELAS DE SELEÇÃO
#=========================================================

salvar_tabela(
  as.data.frame(dredge_S),
  "GLM_Richness_Selection"
)

salvar_tabela(
  as.data.frame(dredge_N),
  "GLM_Abundance_Selection"
)

salvar_tabela(
  as.data.frame(dredge_H),
  "GLM_Shannon_Selection"
)


#=========================================================
# 13. NOMES DAS VARIÁVEIS EM INGLÊS
#=========================================================

nomes_en <- c(
  
  canoppy = "Canopy openness",
  
  pH_solo = "Soil pH",
  
  carbono_solo = "Soil carbon",
  
  umidade_solo = "Soil moisture",
  
  inclinacao = "Slope",
  
  nivel_agua = "Water table depth",
  
  altitude = "Elevation",
  
  argila = "Clay content"
)


#=========================================================
# 14. NOMES DAS RESPOSTAS
#=========================================================

nomes_respostas <- c(
  
  S = "Species richness",
  
  N = "Abundance",
  
  Shannon = "Shannon diversity"
)


#=========================================================
# 15. FUNÇÃO PARA EXTRAIR COEFICIENTES
#=========================================================

extrair_glm <- function(
    modelo,
    resposta
){
  
  coef <- as.data.frame(
    summary(modelo)$coefficients$cond
  )
  
  coef$Variavel <- rownames(coef)
  
  coef <- coef %>%
    filter(
      Variavel != "(Intercept)"
    )
  
  names(coef) <- c(
    "Estimate",
    "SE",
    "z",
    "p",
    "Variable"
  )
  
  coef <- coef %>%
    mutate(
      
      Response = resposta,
      
      CI_low =
        Estimate -
        1.96 * SE,
      
      CI_high =
        Estimate +
        1.96 * SE
    )
  
  return(coef)
}


#=========================================================
# 16. EXTRAIR RESULTADOS DOS MODELOS COMPLETOS
#=========================================================
#
# IMPORTANTE:
# Aqui usamos modelo_S, modelo_N e modelo_H,
# e NÃO best_S, best_N e best_H.
#
# Assim, todas as 8 variáveis aparecem para
# todas as 3 respostas.
#=========================================================

coeficientes <- bind_rows(
  
  extrair_glm(
    modelo_S,
    "Species richness"
  ),
  
  extrair_glm(
    modelo_N,
    "Abundance"
  ),
  
  extrair_glm(
    modelo_H,
    "Shannon diversity"
  )
)


#=========================================================
# 17. ADICIONAR NOMES EM INGLÊS
#=========================================================

coeficientes$Variable <-
  nomes_en[coeficientes$Variable]


#=========================================================
# 18. TABELA COMPLETA DOS RESULTADOS
#=========================================================

tabela_glm <- coeficientes %>%
  
  mutate(
    
    Significance = case_when(
      
      p < 0.001 ~ "***",
      
      p < 0.01 ~ "**",
      
      p < 0.05 ~ "*",
      
      p < 0.10 ~ ".",
      
      TRUE ~ ""
    ),
    
    p_value =
      p,
    
    CI_95 =
      paste0(
        sprintf("%.2f", CI_low),
        " to ",
        sprintf("%.2f", CI_high)
      )
  ) %>%
  
  select(
    
    Response,
    
    Variable,
    
    Estimate,
    
    SE,
    
    z,
    
    CI_95,
    
    p_value,
    
    Significance
  )


#=========================================================
# 19. TABELA PARA APRESENTAÇÃO
#=========================================================

tabela_glm_apresentacao <- tabela_glm %>%
  
  mutate(
    
    Estimate = round(
      Estimate,
      3
    ),
    
    SE = round(
      SE,
      3
    ),
    
    z = round(
      z,
      3
    ),
    
    p_value = signif(
      p_value,
      3
    )
  )


# Visualizar
print(
  tabela_glm_apresentacao,
  row.names = FALSE
)


#=========================================================
# 20. TABELA FINAL COM p-VALOR FORMATADO
#=========================================================

tabela_glm_final <- tabela_glm %>%
  
  mutate(
    
    Estimate = round(
      Estimate,
      3
    ),
    
    SE = round(
      SE,
      3
    ),
    
    z = round(
      z,
      3
    ),
    
    p_value = ifelse(
      
      p_value < 0.001,
      
      format(
        p_value,
        scientific = TRUE,
        digits = 3
      ),
      
      sprintf(
        "%.4f",
        p_value
      )
    )
  ) %>%
  
  select(
    
    Response,
    
    Variable,
    
    Estimate,
    
    SE,
    
    z,
    
    CI_95,
    
    p_value,
    
    Significance
  )


# Visualizar
print(
  tabela_glm_final,
  row.names = FALSE
)


#=========================================================
# 21. SALVAR TABELAS
#=========================================================

salvar_tabela(
  tabela_glm_apresentacao,
  "GLM_Results_All_Variables"
)

salvar_tabela(
  tabela_glm_final,
  "GLM_Results_Final"
)
#=========================================================
# 22. FOREST PLOT
#=========================================================

grafico_forest <- function(
    dados,
    titulo,
    cor
){
  
  dados <- dados %>%
    arrange(
      Estimate
    )
  
  dados$Variable <-
    factor(
      dados$Variable,
      levels = dados$Variable
    )
  
  
  ggplot(
    dados,
    aes(
      x = Estimate,
      y = Variable
    )
  ) +
    
    geom_vline(
      xintercept = 0,
      colour = "grey80",
      linewidth = .5,
      linetype = 2
    ) +
    
    geom_errorbar(
      aes(
        xmin = CI_low,
        xmax = CI_high
      ),
      orientation = "y",
      width = .18,
      linewidth = .8,
      colour = "grey45"
    ) +
    
    geom_point(
      size = 3.8,
      shape = 21,
      fill = cor,
      colour = "black",
      stroke = .4
    ) +
    
    labs(
      title = titulo,
      x = "Standardized coefficient",
      y = NULL
    ) +
    
    tema_dissertacao() +
    
    theme(
      
      plot.title =
        element_text(
          hjust = .5,
          size = 12
        ),
      
      panel.grid.major.y =
        element_blank(),
      
      panel.grid.minor =
        element_blank(),
      
      axis.text.y =
        element_text(
          size = 10
        ),
      
      legend.position =
        "none"
    )
}


#=========================================================
# 23. FOREST - SPECIES RICHNESS
#=========================================================

g_S <- grafico_forest(
  
  filter(
    coeficientes,
    Response == "Species richness"
  ),
  
  "Species richness",
  
  "#1b9e77"
)


#=========================================================
# 24. FOREST - ABUNDANCE
#=========================================================

g_N <- grafico_forest(
  
  filter(
    coeficientes,
    Response == "Abundance"
  ),
  
  "Abundance",
  
  "#d95f02"
)


#=========================================================
# 25. FOREST - SHANNON
#=========================================================

g_H <- grafico_forest(
  
  filter(
    coeficientes,
    Response == "Shannon diversity"
  ),
  
  "Shannon diversity",
  
  "#3182bd"
)


#=========================================================
# 26. PAINEL FOREST
#=========================================================

forest_glm <-
  
  g_S +
  g_N +
  g_H +
  
  plot_layout(
    ncol = 3
  )


print(
  forest_glm
)


#=========================================================
# 27. SALVAR FOREST PLOT
#=========================================================

salvar_figura(
  
  forest_glm,
  
  "GLM_Forest_All_Variables",
  
  tipo = "painel"
)


#=========================================================
# 28. FUNÇÃO PARA GRÁFICOS DE PREDIÇÃO
#=========================================================

grafico_predicao <- function(
    modelo,
    variavel,
    resposta,
    mostrar_y = FALSE
){
  
  
  #-------------------------------------------------------
  # Predições diretamente do modelo glmmTMB
  #-------------------------------------------------------
  
  pred <- ggpredict(
    modelo,
    terms = variavel
  )
  
  
  #-------------------------------------------------------
  # Nome da variável
  #-------------------------------------------------------
  
  nome_x <-
    nomes_en[variavel]
  
  
  #-------------------------------------------------------
  # Nome do eixo Y
  #-------------------------------------------------------
  
  if(
    mostrar_y
  ){
    
    nome_y <-
      
      case_when(
        
        resposta == "S" ~
          "Species richness",
        
        resposta == "N" ~
          "Abundance",
        
        resposta == "Shannon" ~
          "Shannon diversity",
        
        TRUE ~
          resposta
      )
    
  } else {
    
    nome_y <- NULL
    
  }
  
  
  #-------------------------------------------------------
  # Gráfico
  #-------------------------------------------------------
  
  p <-
    
    ggplot() +
    
    
    # Pontos observados
    geom_point(
      
      data =
        df_glm_scaled,
      
      aes(
        
        x =
          .data[[variavel]],
        
        y =
          .data[[resposta]]
      ),
      
      colour = "grey60",
      
      alpha = .50,
      
      size = 2
    ) +
    
    
    # Intervalo de confiança
    geom_ribbon(
      
      data =
        pred,
      
      aes(
        
        x = x,
        
        ymin = conf.low,
        
        ymax = conf.high
      ),
      
      fill = "#9ecae1",
      
      alpha = .30
    ) +
    
    
    # Linha de predição
    geom_line(
      
      data =
        pred,
      
      aes(
        
        x = x,
        
        y = predicted
      ),
      
      colour = "#2171b5",
      
      linewidth = 1.2
    ) +
    
    
    labs(
      
      x = nome_x,
      
      y = nome_y
    ) +
    
    
    tema_dissertacao() +
    
    
    theme(
      
      panel.border =
        element_blank(),
      
      panel.grid.minor =
        element_blank(),
      
      panel.grid.major.x =
        element_blank(),
      
      panel.grid.major.y =
        element_line(
          colour = "grey90",
          linewidth = .3
        ),
      
      axis.title.x =
        element_text(
          size = 10
        ),
      
      axis.title.y =
        element_text(
          size = 10
        ),
      
      axis.text =
        element_text(
          size = 9
        ),
      
      plot.margin =
        margin(
          4,
          4,
          4,
          4
        )
    )
  
  
  #-------------------------------------------------------
  # Retirar eixo Y dos painéis secundários
  #-------------------------------------------------------
  
  if(
    !mostrar_y
  ){
    
    p <-
      p +
      
      theme(
        
        axis.text.y =
          element_blank(),
        
        axis.ticks.y =
          element_blank()
      )
  }
  
  
  return(p)
}


#=========================================================
# 29. GRÁFICOS DE RIQUEZA
#=========================================================

S_g1 <-
  grafico_predicao(
    modelo_S,
    "canoppy",
    "S",
    TRUE
  )

S_g2 <-
  grafico_predicao(
    modelo_S,
    "pH_solo",
    "S"
  )

S_g3 <-
  grafico_predicao(
    modelo_S,
    "carbono_solo",
    "S"
  )

S_g4 <-
  grafico_predicao(
    modelo_S,
    "umidade_solo",
    "S"
  )

S_g5 <-
  grafico_predicao(
    modelo_S,
    "inclinacao",
    "S",
    TRUE
  )

S_g6 <-
  grafico_predicao(
    modelo_S,
    "nivel_agua",
    "S"
  )

S_g7 <-
  grafico_predicao(
    modelo_S,
    "altitude",
    "S"
  )

S_g8 <-
  grafico_predicao(
    modelo_S,
    "argila",
    "S"
  )


#=========================================================
# 30. PAINEL DE RIQUEZA
#=========================================================

painel_riqueza <-
  
  (S_g1 + S_g2 + S_g3 + S_g4) /
  (S_g5 + S_g6 + S_g7 + S_g8)


print(
  painel_riqueza
)


salvar_figura(
  
  painel_riqueza,
  
  "GLM_Species_Richness_Panel",
  
  tipo = "painel"
)


#=========================================================
# 31. GRÁFICOS DE ABUNDÂNCIA
#=========================================================

N_g1 <-
  grafico_predicao(
    modelo_N,
    "canoppy",
    "N",
    TRUE
  )

N_g2 <-
  grafico_predicao(
    modelo_N,
    "pH_solo",
    "N"
  )

N_g3 <-
  grafico_predicao(
    modelo_N,
    "carbono_solo",
    "N"
  )

N_g4 <-
  grafico_predicao(
    modelo_N,
    "umidade_solo",
    "N"
  )

N_g5 <-
  grafico_predicao(
    modelo_N,
    "inclinacao",
    "N",
    TRUE
  )

N_g6 <-
  grafico_predicao(
    modelo_N,
    "nivel_agua",
    "N"
  )

N_g7 <-
  grafico_predicao(
    modelo_N,
    "altitude",
    "N"
  )

N_g8 <-
  grafico_predicao(
    modelo_N,
    "argila",
    "N"
  )


#=========================================================
# 32. PAINEL DE ABUNDÂNCIA
#=========================================================

painel_abundancia <-
  
  (N_g1 + N_g2 + N_g3 + N_g4) /
  (N_g5 + N_g6 + N_g7 + N_g8)


print(
  painel_abundancia
)


salvar_figura(
  
  painel_abundancia,
  
  "GLM_Abundance_Panel",
  
  tipo = "painel"
)


#=========================================================
# 33. GRÁFICOS DE SHANNON
#=========================================================

H_g1 <-
  grafico_predicao(
    modelo_H,
    "canoppy",
    "Shannon",
    TRUE
  )

H_g2 <-
  grafico_predicao(
    modelo_H,
    "pH_solo",
    "Shannon"
  )

H_g3 <-
  grafico_predicao(
    modelo_H,
    "carbono_solo",
    "Shannon"
  )

H_g4 <-
  grafico_predicao(
    modelo_H,
    "umidade_solo",
    "Shannon"
  )

H_g5 <-
  grafico_predicao(
    modelo_H,
    "inclinacao",
    "Shannon",
    TRUE
  )

H_g6 <-
  grafico_predicao(
    modelo_H,
    "nivel_agua",
    "Shannon"
  )

H_g7 <-
  grafico_predicao(
    modelo_H,
    "altitude",
    "Shannon"
  )

H_g8 <-
  grafico_predicao(
    modelo_H,
    "argila",
    "Shannon"
  )


#=========================================================
# 34. PAINEL DE SHANNON
#=========================================================

painel_shannon <-
  
  (H_g1 + H_g2 + H_g3 + H_g4) /
  (H_g5 + H_g6 + H_g7 + H_g8)


print(
  painel_shannon
)


salvar_figura(
  
  painel_shannon,
  
  "GLM_Shannon_Diversity_Panel",
  
  tipo = "painel"
)


#=========================================================
# 35. DIAGNÓSTICO DOS MODELOS - DHARMa
#=========================================================

# Riqueza
sim_S <-
  simulateResiduals(
    modelo_S
  )

plot(
  sim_S
)


# Abundância
sim_N <-
  simulateResiduals(
    modelo_N
  )

plot(
  sim_N
)


# Shannon
sim_H <-
  simulateResiduals(
    modelo_H
  )

plot(
  sim_H
)


#=========================================================
# FIM
#=========================================================