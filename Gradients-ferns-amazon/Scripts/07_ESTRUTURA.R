#=========================================================
# 07 - ESTRUTURA DA COMUNIDADE
# PERMANOVA + BETADISPER + dbRDA
#=========================================================

library(vegan)
library(dplyr)
library(ggplot2)
library(ggrepel)

#=========================================================
# 1. MATRIZ DE ABUNDÂNCIA
#=========================================================

abund <- community_data

#=========================================================
# 2. VARIÁVEIS AMBIENTAIS
#=========================================================

env <- env_data %>%
  
  dplyr::select(
    
    canoppy,
    
    pH_solo,
    
    carbono_solo,
    
    umidade_solo,
    
    inclinacao,
    
    nivel_agua,
    
    altitude,
    
    argila
    
  )

#=========================================================
# 3. DEFINIR OS GRUPOS
#=========================================================

grupo <- factor(
  
  ifelse(
    
    as.numeric(rownames(abund)) %in%
      
      c(1,2,5,6,13,18,22),
    
    "Ripária",
    
    "Terra firme"
    
  )
  
)

env$Grupo <- grupo

#=========================================================
# 4. PERMANOVA
#=========================================================

set.seed(123)

permanova <- adonis2(
  
  abund ~
    
    canoppy +
    
    pH_solo +
    
    carbono_solo +
    
    umidade_solo +
    
    inclinacao +
    
    nivel_agua +
    
    altitude +
    
    argila +
    
    Grupo,
  
  data = env,
  
  method = "bray",
  
  permutations = 999
  
)

permanova

permanova_df <-
  
  as.data.frame(permanova)

permanova_df$Termo <-
  
  rownames(permanova_df)

salvar_tabela(
  
  permanova_df,
  
  "PERMANOVA"
  
)
permanova_df <-
  
  as.data.frame(permanova)

permanova_df$Termo <-
  
  rownames(permanova_df)

salvar_tabela(
  
  permanova_df,
  
  "PERMANOVA"
  
)
#=========================================================
# 5. BETADISPER
#=========================================================

dist_bray <- vegdist(
  
  abund,
  
  method="bray"
  
)

beta <- betadisper(
  
  dist_bray,
  
  grupo
  
)

anova(beta)

permutest(
  
  beta,
  
  permutations=999
  
)

#=========================================================
# 6. dbRDA
#=========================================================

modelo_dbrda <-
  
  capscale(
    
    abund ~
      
      canoppy +
      
      pH_solo +
      
      carbono_solo +
      
      umidade_solo +
      
      inclinacao +
      
      nivel_agua +
      
      altitude +
      
      argila,
    
    data=env,
    
    distance="bray"
    
  )

summary(modelo_dbrda)

anova(
  
  modelo_dbrda,
  
  permutations=999
  
)
anova(
  
  modelo_dbrda,
  
  by="axis",
  
  permutations=999
  
)
anova(
  
  modelo_dbrda,
  
  by="terms",
  
  permutations=999
  
)
sites <-
  
  scores(
    
    modelo_dbrda,
    
    display="sites"
    
  )|>
  
  as.data.frame()

sites$Grupo <- grupo

vars <-
  
  scores(
    
    modelo_dbrda,
    
    display="bp"
    
  )|>
  
  as.data.frame()

vars$Variavel <-
  
  rownames(vars)

vars$Categoria <- c(
  
  "Estrutura",
  
  "Solo",
  
  "Solo",
  
  "Solo",
  
  "Topografia",
  
  "Hidrologia",
  
  "Topografia",
  
  "Solo"
  
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
mult <-
  
  min(
    
    diff(range(sites$CAP1)),
    
    diff(range(sites$CAP2))
    
  )*0.35

vars$CAP1 <- vars$CAP1*mult

vars$CAP2 <- vars$CAP2*mult
grafico_dbrda <-
  
  ggplot()+
  
  geom_hline(
    
    yintercept=0,
    
    colour="grey90"
    
  )+
  
  geom_vline(
    
    xintercept=0,
    
    colour="grey90"
    
  )+
  
  geom_point(
    
    data=sites,
    
    aes(
      
      CAP1,
      
      CAP2,
      
      fill=Grupo
      
    ),
    
    shape=21,
    
    size=3.3,
    
    colour="black"
    
  )+
  
  geom_segment(
    
    data=vars,
    
    aes(
      
      x=0,
      
      y=0,
      
      xend=CAP1,
      
      yend=CAP2,
      
      colour=Categoria
      
    ),
    
    arrow=arrow(
      
      length=unit(.25,"cm"),
      
      type="closed"
      
    ),
    
    linewidth=.9
    
  )+
  
  geom_text_repel(
    
    data=vars,
    
    aes(
      
      CAP1,
      
      CAP2,
      
      label=nomes_pt[Variavel],
      
      colour=Categoria
      
    ),
    
    fontface="plain",
    
    size=3.8,
    
    show.legend=FALSE
    
  )+
  
  coord_equal()+
  
  scale_fill_manual(
    
    values=c(
      
      "Ripária"="#1b9e77",
      
      "Terra firme"="#d95f02"
      
    )
    
  )+
  
  scale_colour_manual(
    
    values=c(
      
      "Estrutura"="#238443",
      
      "Solo"="#8c510a",
      
      "Topografia"="#636363",
      
      "Hidrologia"="#3182bd"
      
    )
    
  )+
  
  labs(
    
    x="dbRDA1",
    
    y="dbRDA2"
    
  )+
  
  tema_dissertacao()

salvar_figura(
  
  grafico_dbrda,
  
  "dbRDA_PT",
  
  tipo="multivariada"
  
)
