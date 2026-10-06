#====================================================
# 04 - COLINEARIDADE
#====================================================

# 1. Pacotes

# 2. Selecionar variáveis ambientais

# 3. Estatísticas descritivas

# 4. Matriz de correlação

# 5. Heatmap

# 6. Dendrograma

# 7. VIF

# 8. Seleção das variáveis

# 9. Exportar resultados

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
library(psych)

descr <- psych::describe(env_data[, preditores])

descr
salvar_tabela(descr,"Resumo_Ambiental")
cor_mat <- cor(
  
  env_data[, preditores],
  
  method="pearson"
  
)
salvar_tabela(
  
  as.data.frame(cor_mat),
  
  "Matriz_Correlacao"
  
)
library(ggcorrplot)

grafico_cor <- ggcorrplot(
  
  cor_mat,
  
  type="lower",
  
  lab=TRUE,
  
  hc.order=TRUE,
  
  outline.color="white"
  
)+
  
  tema_dissertacao()
salvar_figura(
  
  grafico_cor,
  
  "Heatmap_Correlacao",
  
  tipo="painel"
  
)
library(ggdendro)

distancia <- as.dist(
  
  1-abs(cor_mat)
  
)

hc <- hclust(
  
  distancia,
  
  method="average"
  
)

grafico_dendro <-
  
  ggdendrogram(
    
    hc,
    
    rotate=TRUE
    
  )+
  
  tema_dissertacao()
salvar_figura(
  
  grafico_dendro,
  
  "Dendrograma",
  
  tipo="painel"
  
)
library(usdm)

vif_resultado <- vif(
  
  env_data[, preditores]
  
)

vif_resultado
salvar_tabela(
  
  vif_resultado,
  
  "Resumo_VIF"
  
)
vif_cor <- vifcor(
  
  env_data[, preditores],
  
  th=0.7
  
)

vif_cor
env_final <-
  
  exclude(
    
    env_data[, preditores],
    
    vif_cor
    
  )
print(colnames(env_final))