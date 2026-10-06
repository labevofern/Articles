#===============================================================================
# 3. PCA - VARIÁVEIS AMBIENTAIS
#===============================================================================

library(vegan)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(factoextra)

#===============================================================================
# PREPARAÇÃO DOS DADOS
#===============================================================================

env_total <- env_data

parcelas_riparia <- c(1,2,5,6,13,18,22)

parcelas_terra_firme <- c(
  3,4,7,10,11,12,14,15,16,17,
  19,20,21,23,26,27
)

env_total$plot <- as.numeric(rownames(env_total))

env_total$Ambiente <- ifelse(
  env_total$plot %in% parcelas_riparia,
  "Ripária",
  "Terra firme"
)

env_total$Ambiente <- factor(
  env_total$Ambiente,
  levels = c("Ripária","Terra firme")
)

#===============================================================================
# NOMES DAS VARIÁVEIS
#===============================================================================

nomes_pt <- c(
  
  canoppy = "Cobertura do dossel",
  
  pH_solo = "pH",
  
  umidade_solo = "Umidade",
  
  carbono_solo = "Carbono",
  
  argila = "Argila",
  
  nivel_agua = "Lençol freático",
  
  altitude = "Altitude",
  
  inclinacao = "Inclinação"
  
)

#===============================================================================
# SELEÇÃO DAS VARIÁVEIS
#===============================================================================

vars_pca <- c(
  
  "canoppy",
  
  "pH_solo",
  
  "carbono_solo",
  
  "umidade_solo",
  
  "argila",
  
  "nivel_agua",
  
  "altitude",
  
  "inclinacao"
  
)

dados_pca <-
  
  env_total |>
  
  dplyr::select(
    
    plot,
    
    Ambiente,
    
    all_of(vars_pca)
    
  ) |>
  
  na.omit()

#===============================================================================
# EXECUÇÃO DA PCA
#===============================================================================

pca_res <-
  
  rda(
    
    dados_pca[,vars_pca],
    
    scale = TRUE
    
  )

#===============================================================================
# VARIÂNCIA EXPLICADA
#===============================================================================

eig <- eigenvals(pca_res)

var_exp <- eig/sum(eig)*100

tabela_pca <-
  
  data.frame(
    
    Eixo = paste0("PC",1:length(eig)),
    
    Autovalor = eig,
    
    Variancia = round(var_exp,2),
    
    Variancia_Acumulada = round(cumsum(var_exp),2)
    
  )

print(tabela_pca)

salvar_tabela(
  
  tabela_pca,
  
  "Tabela_PCA"
  
)

#===============================================================================
# SCORES
#===============================================================================

scores_sites <-
  
  as.data.frame(
    
    scores(
      
      pca_res,
      
      display="sites"
      
    )
    
  )

scores_sites$plot <- dados_pca$plot

scores_sites$Ambiente <- dados_pca$Ambiente

scores_var <-
  
  as.data.frame(
    
    scores(
      
      pca_res,
      
      display="species"
      
    )
    
  )

scores_var$Variavel <- rownames(scores_var)

scores_var$Variavel <-
  
  nomes_pt[scores_var$Variavel]

#===============================================================================
# MULTIPLICADOR DAS SETAS
#===============================================================================

mult <- 2

#===============================================================================
# PCA FINAL
#===============================================================================

grafico_pca <-
  
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
    
    data=scores_sites,
    
    aes(
      
      PC1,
      
      PC2,
      
      colour=Ambiente
      
    ),
    
    size=3
    
  )+
  
  geom_text_repel(
    
    data=scores_sites,
    
    aes(
      
      PC1,
      
      PC2,
      
      label=plot
      
    ),
    
    size=3,
    
    colour="grey30",
    
    fontface="plain",
    
    max.overlaps=50
    
  )+
  
  geom_segment(
    
    data=scores_var,
    
    aes(
      
      x=0,
      
      y=0,
      
      xend=PC1*mult,
      
      yend=PC2*mult
      
    ),
    
    colour="#2E6E3E",
    
    linewidth=.7,
    
    arrow=arrow(
      
      length=unit(.18,"cm")
      
    )
    
  )+
  
  geom_text_repel(
    
    data=scores_var,
    
    aes(
      
      PC1*mult,
      
      PC2*mult,
      
      label=Variavel
      
    ),
    
    colour="black",
    
    size=3.4,
    
    fontface="plain"
    
  )+
  
  scale_colour_manual(
    
    values=c(
      
      "Ripária"="#1b9e77",
      
      "Terra firme"="#d95f02"
      
    )
    
  )+
  
  labs(
    
    x=paste0(
      
      "PC1 (",
      
      round(var_exp[1],1),
      
      "%)"
      
    ),
    
    y=paste0(
      
      "PC2 (",
      
      round(var_exp[2],1),
      
      "%)"
      
    ),
    
    colour=NULL
    
  )+
  
  coord_equal()+
  
  tema_dissertacao()+
  
  theme(
    
    legend.position="top",
    
    panel.grid.minor=element_blank(),
    
    panel.grid.major=element_line(
      
      colour="grey92",
      
      linewidth=.3
      
    ),
    
    axis.title=element_text(
      
      size=11,
      
      face="plain"
      
    ),
    
    axis.text=element_text(
      
      size=10
      
    ),
    
    legend.text=element_text(
      
      size=10
      
    ),
    
    plot.title=element_blank()
    
  )

#===============================================================================
# VISUALIZAÇÃO
#===============================================================================

print(grafico_pca)

#===============================================================================
# SALVAR
#===============================================================================

salvar_figura(
  
  grafico_pca,
  
  "PCA_Ambiental",
  
  tipo="multivariada"
  
)

