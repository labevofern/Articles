#=========================================================
# 09 - DIVERSIDADE TAXONÔMICA
#=========================================================

library(vegan)
library(tidyverse)
library(ggdendro)

#=========================================================
# LEITURA DA TAXONOMIA
#=========================================================

taxonomia <- read.csv(
  
  "Dados/diversidade_taxonomica.csv",
  
  sep = ";",
  
  check.names = FALSE,
  
  stringsAsFactors = FALSE
  
)

#=========================================================
# SINCRONIZAÇÃO DAS ESPÉCIES
#=========================================================

taxonomia_filtrada <-
  
  taxonomia |>
  
  filter(
    
    especie %in% colnames(community_data)
    
  )

especies_comuns <-
  
  intersect(
    
    colnames(community_data),
    
    taxonomia_filtrada$especie
    
  )

abundancia_limpa <-
  
  community_data[ , especies_comuns]

rownames(taxonomia_filtrada) <-
  
  taxonomia_filtrada$especie

taxon_hierarquia <-
  
  taxonomia_filtrada[,-1]
#=========================================================
# DISTÂNCIA TAXONÔMICA
#=========================================================

taxon_dist <-
  
  taxa2dist(
    
    taxon_hierarquia,
    
    varstep=TRUE
    
  )

res_tax <-
  
  taxondive(
    
    abundancia_limpa,
    
    taxon_dist
    
  )
#=========================================================
# DATA FRAME
#=========================================================

df_taxonomico <-
  
  data.frame(
    
    Parcela=
      
      rownames(abundancia_limpa),
    
    Delta_Plus=
      
      res_tax$Dplus
    
  )

rownames(df_taxonomico) <-
  
  df_taxonomico$Parcela

df_taxonomico <-
  
  cbind(
    
    df_taxonomico,
    
    env_data
  )

#=========================================================
# GLM
#=========================================================

modelo_taxonomico <-
  
  glm(
    
    Delta_Plus ~
      
      canoppy +
      
      pH_solo +
      
      carbono_solo +
      
      umidade_solo +
      
      inclinacao +
      
      nivel_agua +
      
      altitude +
      
      argila,
    
    family=gaussian,
    
    data=df_taxonomico,
    
    na.action = na.omit # <--- CORREÇÃO ADICIONADA AQUI
    
  )

summary(modelo_taxonomico)

coef_tax <-
  
  as.data.frame(
    
    summary(
      
      modelo_taxonomico
      
    )$coefficients
    
  )

coef_tax$Variavel <-
  
  rownames(coef_tax)

salvar_tabela(
  
  coef_tax,
  
  "GLM_Taxonomico"
  
)

plot(
  
  res_tax,
  
  var="Dplus"
  
)
#=========================================================
# CLADOGRAMA
#=========================================================

clado_hierarquia <-
  
  hclust(
    
    taxon_dist
    
  )

plot_clado <-
  
  ggdendrogram(
    
    clado_hierarquia,
    
    rotate=TRUE,
    
    theme_dendro=FALSE
    
  )+
  
  labs(
    
    x="",
    
    y=""
    
  )+
  
  tema_dissertacao()+
  
  theme(
    
    axis.text.y=
      
      element_text(
        
        size=8,
        
        face="italic",
        
        colour="black"
        
      ),
    
    axis.text.x=
      
      element_text(
        
        size=5
        
      ),
    
    panel.grid.minor=
      
      element_blank(),
    
    panel.grid.major.x=
      
      element_line(
        
        colour="grey90"
        
      )
    
  )

print(
  
  plot_clado
  
)

salvar_figura(
  
  plot_clado,
  
  "Cladograma_PT",
  
  tipo="painel"
  
)

delta_df <-
  
  data.frame(
    
    Parcela=
      
      rownames(abundancia_limpa),
    
    Delta_Plus=
      
      round(
        
        res_tax$Dplus,
        
        3
        
      )
    
  )

salvar_tabela(
  
  delta_df,
  
  "Delta_Plus"
  
)
