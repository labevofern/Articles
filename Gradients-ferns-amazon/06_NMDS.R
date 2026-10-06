#===============================================================================
# 4. NMDS
#===============================================================================

library(vegan)
library(dplyr)
library(ggplot2)
library(ggrepel)

#===============================================================================
# DEFINIÇÃO DOS GRUPOS
#===============================================================================

parcelas_riparia <- c(
  1,2,5,6,13,18,22
)

parcelas_terra_firme <- c(
  3,4,7,10,11,12,14,15,16,17,
  19,20,21,23,26,27
)

#===============================================================================
# MATRIZES
#===============================================================================

matriz_ab <- community_data

matriz_riq <- ifelse(
  community_data > 0,
  1,
  0
)

#===============================================================================
# NMDS - ABUNDÂNCIA
#===============================================================================

set.seed(123)

nmds_ab <-
  
  metaMDS(
    
    matriz_ab,
    
    distance="bray",
    
    autotransform=TRUE,
    
    trymax=1000
    
  )

df_ab <-
  
  as.data.frame(
    
    scores(
      
      nmds_ab,
      
      display="sites"
      
    )
    
  )

df_ab$plot <- rownames(df_ab)

df_ab <-
  
  df_ab |>
  
  mutate(
    
    Habitat=
      
      case_when(
        
        as.numeric(plot) %in% parcelas_riparia ~ "Ripária",
        
        as.numeric(plot) %in% parcelas_terra_firme ~ "Terra firme"
        
      )
    
  )

grafico_ab <-
  
  ggplot(
    
    df_ab,
    
    aes(
      
      NMDS1,
      
      NMDS2,
      
      colour=Habitat
      
    )
    
  )+
  
  geom_point(
    
    size=3.5,
    
    alpha=.85
    
  )+
  
  geom_text_repel(
    
    aes(label=plot),
    
    size=3,
    
    colour="grey20",
    
    show.legend=FALSE,
    
    max.overlaps=50
    
  )+
  
  scale_colour_manual(
    
    values=c(
      
      "Ripária"="#1b9e77",
      
      "Terra firme"="#d95f02"
      
    )
    
  )+
  
  labs(
    
    x="NMDS1",
    
    y="NMDS2",
    
    colour=NULL,
    
    caption=paste0(
      
      "Stress = ",
      
      round(nmds_ab$stress,3)
      
    )
    
  )+
  
  tema_dissertacao()+
  
  theme(
    
    legend.position="top",
    
    panel.grid.major=element_line(
      
      colour="grey92",
      
      linewidth=.3
      
    ),
    
    panel.grid.minor=element_blank(),
    
    plot.title=element_blank(),
    
    axis.title=element_text(
      
      face="plain"
      
    )
    
  )

print(grafico_ab)

salvar_figura(
  
  grafico_ab,
  
  "NMDS_Abundancia",
  
  tipo="multivariada"
  
)

#===============================================================================
# NMDS - RIQUEZA
#===============================================================================

set.seed(123)

nmds_riq <-
  
  metaMDS(
    
    matriz_riq,
    
    distance="bray",
    
    autotransform=TRUE,
    
    trymax=1000
    
  )

df_riq <-
  
  as.data.frame(
    
    scores(
      
      nmds_riq,
      
      display="sites"
      
    )
    
  )

df_riq$plot <- rownames(df_riq)

df_riq <-
  
  df_riq |>
  
  mutate(
    
    Habitat=
      
      case_when(
        
        as.numeric(plot) %in% parcelas_riparia ~ "Ripária",
        
        as.numeric(plot) %in% parcelas_terra_firme ~ "Terra firme"
        
      )
    
  )

grafico_riq <-
  
  ggplot(
    
    df_riq,
    
    aes(
      
      NMDS1,
      
      NMDS2,
      
      colour=Habitat
      
    )
    
  )+
  
  geom_point(
    
    size=3.5,
    
    alpha=.85
    
  )+
  
  geom_text_repel(
    
    aes(label=plot),
    
    size=3,
    
    colour="grey20",
    
    show.legend=FALSE,
    
    max.overlaps=50
    
  )+
  
  scale_colour_manual(
    
    values=c(
      
      "Ripária"="#1b9e77",
      
      "Terra firme"="#d95f02"
      
    )
    
  )+
  
  labs(
    
    x="NMDS1",
    
    y="NMDS2",
    
    colour=NULL,
    
    caption=paste0(
      
      "Stress = ",
      
      round(nmds_riq$stress,3)
      
    )
    
  )+
  
  tema_dissertacao()+
  
  theme(
    
    legend.position="top",
    
    panel.grid.major=element_line(
      
      colour="grey92",
      
      linewidth=.3
      
    ),
    
    panel.grid.minor=element_blank(),
    
    plot.title=element_blank(),
    
    axis.title=element_text(
      
      face="plain"
      
    )
    
  )

print(grafico_riq)

salvar_figura(
  
  grafico_riq,
  
  "NMDS_Riqueza",
  
  tipo="multivariada"
  
)

#===============================================================================
# 5. ENVFIT
#===============================================================================

library(vegan)
library(ggplot2)
library(ggrepel)

#===============================================================================
# MATRIZ FLORÍSTICA
#===============================================================================

matriz_ab <- community_data

#===============================================================================
# VARIÁVEIS AMBIENTAIS
#===============================================================================

vars_env <-
  
  env_data[,c(
    
    "canoppy",
    
    "pH_solo",
    
    "carbono_solo",
    
    "argila",
    
    "umidade_solo",
    
    "inclinacao",
    
    "nivel_agua",
    
    "altitude"
    
  )]

vars_env <-
  
  as.data.frame(
    
    lapply(
      
      vars_env,
      
      as.numeric
      
    )
    
  )

#===============================================================================
# REMOVER NAs
#===============================================================================

keep <- complete.cases(vars_env)

matriz_limpa <- matriz_ab[keep,]

env_limpo <- vars_env[keep,]

plots_limpos <- as.numeric(rownames(matriz_limpa))

#===============================================================================
# NMDS
#===============================================================================

set.seed(123)

nmds_res <-
  
  metaMDS(
    
    matriz_limpa,
    
    distance="bray",
    
    k=2,
    
    trymax=1000,
    
    autotransform=TRUE
    
  )

#===============================================================================
# ENVFIT
#===============================================================================

set.seed(123)

fit <-
  
  envfit(
    
    nmds_res,
    
    env_limpo,
    
    permutations=999
    
  )

print(fit)

#===============================================================================
# SCORES
#===============================================================================

df_sites <-
  
  as.data.frame(
    
    scores(
      
      nmds_res,
      
      display="sites"
      
    )
    
  )

df_sites$plot <- plots_limpos

parcelas_riparia <- c(
  
  1,2,5,6,13,18,22
  
)

parcelas_terra_firme <- c(
  
  3,4,7,10,11,12,14,15,16,17,
  
  19,20,21,23,26,27
  
)

df_sites$Ambiente <-
  
  ifelse(
    
    df_sites$plot %in% parcelas_riparia,
    
    "Ripária",
    
    "Terra firme"
    
  )

df_sites$Ambiente <-
  
  factor(
    
    df_sites$Ambiente,
    
    levels=c(
      
      "Ripária",
      
      "Terra firme"
      
    )
    
  )

#===============================================================================
# VARIÁVEIS DO ENVFIT
#===============================================================================

env_coord <-
  
  as.data.frame(
    
    scores(
      
      fit,
      
      display="vectors"
      
    )
    
  )

env_coord$Variavel <- rownames(env_coord)

env_coord$p <- fit$vectors$pvals

nomes_pt <- c(
  
  canoppy="Cobertura do dossel",
  
  pH_solo="pH",
  
  carbono_solo="Carbono",
  
  argila="Argila",
  
  umidade_solo="Umidade",
  
  inclinacao="Inclinação",
  
  nivel_agua="Lençol freático",
  
  altitude="Altitude"
  
)

env_coord$Variavel <-
  
  nomes_pt[env_coord$Variavel]

env_coord_sig <-
  
  subset(
    
    env_coord,
    
    p < 0.05
    
  )

#===============================================================================
# FATOR DAS SETAS
#===============================================================================

mult <- 1.4

#===============================================================================
# GRÁFICO
#===============================================================================

grafico_envfit <-
  
  ggplot() +
  
  geom_hline(
    
    yintercept=0,
    
    colour="grey80",
    
    linewidth=.4
    
  )+
  
  geom_vline(
    
    xintercept=0,
    
    colour="grey80",
    
    linewidth=.4
    
  )+
  
  geom_point(
    
    data=df_sites,
    
    aes(
      
      NMDS1,
      
      NMDS2,
      
      colour=Ambiente
      
    ),
    
    size=3.5,
    
    alpha=.85
    
  )+
  
  geom_text_repel(
    
    data=df_sites,
    
    aes(
      
      NMDS1,
      
      NMDS2,
      
      label=plot
      
    ),
    
    size=3,
    
    colour="grey25",
    
    show.legend=FALSE,
    
    max.overlaps=50
    
  )+
  
  geom_segment(
    
    data=env_coord_sig,
    
    aes(
      
      x=0,
      
      y=0,
      
      xend=NMDS1*mult,
      
      yend=NMDS2*mult
      
    ),
    
    arrow=arrow(
      
      length=unit(
        
        0.18,
        
        "cm"
        
      )
      
    ),
    
    linewidth=.8,
    
    colour="#2E6E3E"
    
  )+
  
  geom_text_repel(
    
    data=env_coord_sig,
    
    aes(
      
      NMDS1*mult,
      
      NMDS2*mult,
      
      label=Variavel
      
    ),
    
    size=3.3,
    
    colour="black",
    
    fontface="plain"
    
  )+
  
  scale_colour_manual(
    
    values=c(
      
      "Ripária"="#1b9e77",
      
      "Terra firme"="#d95f02"
      
    )
    
  )+
  
  labs(
    
    x="NMDS1",
    
    y="NMDS2",
    
    colour=NULL,
    
    caption=paste(
      
      "Stress =",
      
      round(
        
        nmds_res$stress,
        
        3
        
      )
      
    )
    
  )+
  
  coord_equal()+
  
  tema_dissertacao()+
  
  theme(
    
    legend.position="top",
    
    panel.grid.major=element_line(
      
      colour="grey92",
      
      linewidth=.3
      
    ),
    
    panel.grid.minor=element_blank(),
    
    axis.title=element_text(
      
      size=11,
      
      face="plain"
      
    ),
    
    axis.text=element_text(
      
      size=10
      
    ),
    
    plot.title=element_blank()
    
  )

#===============================================================================
# VISUALIZAR
#===============================================================================

print(grafico_envfit)

#===============================================================================
# SALVAR
#===============================================================================

salvar_figura(
  
  grafico_envfit,
  
  "NMDS_ENVFIT",
  
  tipo="multivariada"
  
)