#=========================================================
# 08 - GENERALIZED LINEAR MODELS
#=========================================================

library(glmmTMB)
library(MuMIn)
library(DHARMa)
library(ggeffects)
library(broom.mixed)
library(dplyr)
library(ggplot2)
library(vegan)

#=========================================================
# MÉTRICAS DA COMUNIDADE
#=========================================================

S <- vegan::specnumber(community_data)

N <- rowSums(community_data)

H <- vegan::diversity(
  community_data,
  index = "shannon"
)

#=========================================================
# DATAFRAME DOS GLMs
#=========================================================

df_taxonomico <- env_data

df_taxonomico$S <- S
df_taxonomico$N <- N
df_taxonomico$Shannon <- H

#=========================================================
# PREDITORES
#=========================================================

preditores <- c("canoppy","pH_solo","carbono_solo",
  
  "umidade_solo", "inclinacao", "nivel_agua",  "altitude","argila")

#=========================================================
# REMOVER NAs
#=========================================================

df_glm <- na.omit(df_taxonomico[ c(  "S",    "N",     "Shannon",  preditores  )])
  
#=========================================================
# PADRONIZAÇÃO
#=========================================================

df_glm_scaled <- df_glm

df_glm_scaled[ preditores] <-  scale(df_glm_scaled[ preditores]    )
df_glm_scaled

#=========================================================
# MODELO COMPLETO - RIQUEZA
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
# MODELO COMPLETO - ABUNDÂNCIA
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
# MODELO - SHANNON
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
# SELEÇÃO DE MODELOS
#=========================================================

options(na.action = "na.fail")

dredge_S <- dredge(modelo_S)

dredge_N <- dredge(modelo_N)

dredge_H <- dredge(modelo_H)

best_S <- get.models(dredge_S, 1)[[1]]

best_N <- get.models(dredge_N, 1)[[1]]

best_H <- get.models(dredge_H, 1)[[1]]

summary(best_S)

summary(best_N)

summary(best_H)

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
#==============GRÁFICOS=================#
library(ggplot2)
library(dplyr)
library(patchwork)

#=========================================================
# EXTRAÇÃO DOS COEFICIENTES
#=========================================================

extrair_glm <- function(modelo, resposta){
  
  coef <- as.data.frame(
    summary(modelo)$coefficients$cond
  )
  
  coef$Variavel <- rownames(coef)
  
  coef <- coef %>%
    filter(Variavel != "(Intercept)")
  
  names(coef) <- c(
    
    "Estimativa",
    
    "Erro",
    
    "z",
    
    "p",
    
    "Variavel"
    
  )
  
  coef$IC_inf <- coef$Estimativa - 1.96*coef$Erro
  
  coef$IC_sup <- coef$Estimativa + 1.96*coef$Erro
  
  coef$Modelo <- resposta
  
  coef
  
}
coeficientes <-
  
  bind_rows(
    
    extrair_glm(best_S,"Richness"),
    
    extrair_glm(best_N,"Abundance"),
    
    extrair_glm(best_H,"Shannon")
    
  )
nomes_pt <- c(
  
  canoppy="Cobertura do dossel",
  
  pH_solo="pH",
  
  carbono_solo="Carbono",
  
  umidade_solo="Umidade",
  
  inclinacao="Inclinação",
  
  nivel_agua="Nível da água",
  
  altitude="Altitude",
  
  argila="Argila"
  
)

coeficientes$Variavel <-
  
  nomes_pt[coeficientes$Variavel]

grafico_forest <- function(dados,titulo,cor){
  
  dados <-
    
    dados %>%
    
    arrange(Estimativa)
  
  dados$Variavel <-
    
    factor(
      
      dados$Variavel,
      
      levels=dados$Variavel
      
    )
  
  ggplot(
    
    dados,
    
    aes(
      
      Estimativa,
      
      Variavel
      
    )
    
  )+
    
    geom_vline(
      
      xintercept=0,
      
      colour="grey80",
      
      linewidth=.5,
      
      linetype=2
      
    )+
    
    geom_errorbar(
      
      aes(
        
        xmin=IC_inf,
        
        xmax=IC_sup
        
      ),
      
      orientation="y",
      
      width=.18,
      
      linewidth=.8,
      
      colour="grey45"
      
    )+
    
    geom_point(
      
      size=3.8,
      
      shape=21,
      
      fill=cor,
      
      colour="black",
      
      stroke=.4
      
    )+
    
    labs(
      
      title=titulo,
      
      x="Standardized coefficient",
      
      y=NULL
      
    )+
    
    tema_dissertacao()+
    
    theme(
      
      plot.title=
        
        element_text(
          
          hjust=.5,
          
          size=12
          
        ),
      
      panel.grid.major.y=
        
        element_blank(),
      
      panel.grid.minor=
        
        element_blank(),
      
      axis.text.y=
        
        element_text(size=10),
      
      legend.position="none"
      
    )
  
}
g_S <-
  
  grafico_forest(
    
    filter(
      
      coeficientes,
      
      Modelo=="Richness"
      
    ),
    
    "Richness",
    
    "#1b9e77"
    
  )

g_N <-
  
  grafico_forest(
    
    filter(
      
      coeficientes,
      
      Modelo=="Abundance"
      
    ),
    
    "Abundance",
    
    "#d95f02"
    
  )

g_H <-
  
  grafico_forest(
    
    filter(
      
      coeficientes,
      
      Modelo=="Shannon"
      
    ),
    
    "Shannon",
    
    "#3182bd"
    
  )
forest_glm <-
  
  g_S +
  
  g_N +
  
  g_H +
  
  plot_layout(
    
    ncol=3
  )
print(forest_glm)

salvar_figura(
  
  forest_glm,
  
  "GLM_Forest_PT",
  
  tipo="painel",
  
  idioma="PT"
  
)

#=========================================================
# TABELA RESUMO DOS GLMs
#=========================================================

tabela_glm <- coeficientes %>%
  
  mutate(
    
    Significancia = case_when(
      
      p < 0.001 ~ "***",
      
      p < 0.01 ~ "**",
      
      p < 0.05 ~ "*",
      
      p < 0.10 ~ ".",
      
      TRUE ~ ""
      
    ),
    
    IC95 = paste0(
      
      round(IC_inf,2),
      
      " ; ",
      
      round(IC_sup,2)
      
    )
    
  ) %>%
  
  transmute(
    
    Modelo,
    
    Variável = Variavel,
    
    Estimativa = round(Estimativa,3),
    
    `Erro padrão` = round(Erro,3),
    
    `IC 95%` = IC95,
    
    `Valor de p` = signif(p,3),
    
    Significância = Significancia
    
  )
print(tabela_glm)

View(tabela_glm)
salvar_tabela(
  
  tabela_glm,
  
  "GLM_Resultados"
  
)

#=========================================================
# PAINEL DE RESPOSTA DA ABUNDÂNCIA (TODAS AS VARIÁVEIS)
#=========================================================

library(ggeffects)
library(ggplot2)
library(patchwork)

#---------------------------------------------------------
# NOMES DAS VARIÁVEIS
#---------------------------------------------------------

nomes_pt <- c(
  canoppy = "Cobertura do dossel",
  pH_solo = "pH",
  carbono_solo = "Carbono",
  umidade_solo = "Umidade do solo",
  inclinacao = "Inclinação",
  nivel_agua = "Nível da água",
  altitude = "Altitude",
  argila = "Argila"
)

#---------------------------------------------------------
# FUNÇÃO
#---------------------------------------------------------

grafico_glm <- function(variavel, mostrar_y = FALSE){
  
  pred <- ggpredict(
    modelo_N,
    terms = variavel
  )
  
  p <- ggplot() +
    
    geom_point(
      data = df_glm_scaled,
      aes_string(
        x = variavel,
        y = "N"
      ),
      colour = "grey60",
      alpha = 0.50,
      size = 2
    ) +
    
    geom_ribbon(
      data = pred,
      aes(
        x = x,
        ymin = conf.low,
        ymax = conf.high
      ),
      fill = "#9ecae1",
      alpha = .30
    ) +
    
    geom_line(
      data = pred,
      aes(
        x = x,
        y = predicted
      ),
      colour = "#2171b5",
      linewidth = 1.2
    ) +
    
    labs(
      x = nomes_pt[variavel]
    ) +
    
    tema_dissertacao() +
    
    theme(
      
      panel.border = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      panel.grid.major.x = element_blank(),
      
      panel.grid.major.y = element_line(
        colour = "grey90",
        linewidth = .3
      ),
      
      axis.title.x = element_text(size = 10),
      
      axis.text = element_text(size = 9),
      
      plot.margin = margin(4,4,4,4)
      
    )
  
  if(mostrar_y){
    
    p <- p +
      labs(y = "Abundância")
    
  } else{
    
    p <- p +
      labs(y = NULL) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
  }
  
  return(p)
  
}

#=========================================================
# GRÁFICOS
#=========================================================

g1 <- grafico_glm("canoppy", TRUE)

g2 <- grafico_glm("pH_solo")

g3 <- grafico_glm("carbono_solo")

g4 <- grafico_glm("umidade_solo")

g5 <- grafico_glm("inclinacao", TRUE)

g6 <- grafico_glm("nivel_agua")

g7 <- grafico_glm("altitude")

g8 <- grafico_glm("argila")

#=========================================================
# PAINEL
#=========================================================

painel_abundancia <-
  
  ((g1 + g2 + g3 + g4) /
     (g5 + g6 + g7 + g8)) +
  
  plot_layout(guides = "collect")

print(painel_abundancia)

salvar_figura(
  painel_abundancia,
  "GLM_Abundancia_Painel_PT",
  tipo = "painel"
)

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

#=========================================================
# DADOS PARA OS PAINÉIS
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

nomes_pt <- c(
  
  canoppy="Cobertura do dossel",
  
  pH_solo="pH",
  
  carbono_solo="Carbono",
  
  umidade_solo="Umidade do solo",
  
  inclinacao="Inclinação",
  
  nivel_agua="Nível da água",
  
  altitude="Altitude",
  
  argila="Argila"
  
)
#=========================================================
# PAINEL PADRÃO
#=========================================================

painel_glm <- function(resposta,
                       titulo=NULL){
  
  dados <-
    
    df_glm |>
    
    select(
      
      all_of(preditores),
      
      all_of(resposta)
      
    ) |>
    
    pivot_longer(
      
      cols=all_of(preditores),
      
      names_to="Variavel",
      
      values_to="Valor"
      
    )
  
  ggplot(
    
    dados,
    
    aes(
      
      Valor,
      
      .data[[resposta]]
      
    )
    
  )+
    
    geom_point(
      
      colour="#74a892",
      
      alpha=.75,
      
      size=2.2
      
    )+
    
    geom_smooth(
      
      method="glm",
      
      method.args=list(
        
        family="poisson"
        
      ),
      
      colour="#145a32",
      
      fill="#c8e6c9",
      
      linewidth=1,
      
      alpha=.35
      
    )+
    
    facet_wrap(
      
      ~Variavel,
      
      scales="free_x",
      
      ncol=4,
      
      labeller=labeller(
        
        Variavel=nomes_pt
        
      )
      
    )+
    
    labs(
      
      x="",
      
      y=titulo
      
    )+
    
    tema_dissertacao()+
    
    theme(
      
      strip.text=
        
        element_text(
          
          face="plain",
          
          size=10
          
        ),
      
      panel.grid.minor=
        
        element_blank(),
      
      panel.grid.major.x=
        
        element_blank(),
      
      panel.grid.major.y=
        
        element_line(
          
          colour="grey90",
          
          linewidth=.3
          
        ),
      
      axis.title=
        
        element_text(
          
          size=11
          
        ),
      
      axis.text=
        
        element_text(
          
          size=9
          
        )
      
    )
  
}
grafico_riqueza <-
  
  painel_glm(
    
    "S",
    
    "Riqueza"
    
  )

print(grafico_riqueza)

salvar_figura(
  
  grafico_riqueza,
  
  "Painel_Riqueza_PT",
  
  tipo="painel"
  
)
grafico_abundancia <-
  
  painel_glm(
    
    "N",
    
    "Abundância"
    
  )

print(grafico_abundancia)

salvar_figura(
  
  grafico_abundancia,
  
  "Painel_Abundancia_PT",
  
  tipo="painel"
  
)
grafico_shannon <-
  
  painel_glm(
    
    "Shannon",
    
    "Diversidade de Shannon"
    
  )

print(grafico_shannon)

salvar_figura(
  
  grafico_shannon,
  
  "Painel_Shannon_PT",
  
  tipo="painel"
  
)


