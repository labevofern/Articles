#===========================================================
# TEMA PADRÃO DA DISSERTAÇÃO
# Vanessa Fernandes Ferreira
#===========================================================

library(ggplot2)

tema_dissertacao <- function(){
  
  theme_bw(base_size = 12) +
    
    theme(
      
      # Fundo
      panel.background = element_rect(fill = "white", colour = NA),
      
      panel.border = element_rect(
        colour = "black",
        linewidth = 0.8
      ),
      
      panel.grid.major = element_blank(),
      
      panel.grid.minor = element_blank(),
      
      # Títulos dos eixos
      axis.title = element_text(
        size = 13,
        face = "bold"
      ),
      
      # Texto dos eixos
      axis.text = element_text(
        size = 11,
        colour = "black"
      ),
      
      # Título do gráfico
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = 0.5
      ),
      
      # Subtítulo
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5
      ),
      
      # Legenda
      legend.position = "right",
      
      legend.title = element_text(
        size = 11,
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 10
      ),
      
      # Facetas
      strip.background = element_rect(
        fill = "grey92",
        colour = "black"
      ),
      
      strip.text = element_text(
        size = 11,
        face = "bold"
      )
      
    )
  
}#===========================================================
# FUNÇÃO PARA SALVAR FIGURAS
#===========================================================

salvar_figura <- function(grafico,
                          nome,
                          largura = 8,
                          altura = 6){
  
  ggsave(
    paste0("Figuras/PT/", nome, ".png"),
    grafico,
    width = largura,
    height = altura,
    dpi = 600
  )
  
  ggsave(
    paste0("Figuras/PT/", nome, ".pdf"),
    grafico,
    width = largura,
    height = altura
  )
  
}
#===========================================================
# PALETA OFICIAL
#===========================================================

cores <- list(
  
  vegetacao = "#3C7D3E",
  
  solo = "#8C6D31",
  
  agua = "#3B7EA1",
  
  topografia = "#5F6368",
  
  destaque = "#C0392B",
  
  neutro = "#8E8E8E"
  
)
#===========================================================
# NOMES DAS VARIÁVEIS
# PORTUGUÊS
#===========================================================

nomes_pt <- c(
  
  canoppy = "Cobertura do dossel (%)",
  
  pH_solo = "pH do solo",
  
  carbono_solo = "Carbono do solo",
  
  umidade_solo = "Umidade do solo",
  
  inclinacao = "Inclinação (°)",
  
  nivel_agua = "Nível do lençol freático",
  
  altitude = "Altitude (m)",
  
  areia = "Areia (%)",
  
  argila = "Argila (%)",
  
  silte = "Silte (%)"
  
)
#===========================================================
# ENGLISH
#===========================================================

nomes_en <- c(
  
  canoppy = "Canopy cover (%)",
  
  pH_solo = "Soil pH",
  
  carbono_solo = "Soil carbon",
  
  umidade_solo = "Soil moisture",
  
  inclinacao = "Slope (°)",
  
  nivel_agua = "Water table",
  
  altitude = "Elevation (m)",
  
  areia = "Sand (%)",
  
  argila = "Clay (%)",
  
  silte = "Silt (%)"
  
)
#===========================================================
# TAMANHOS PADRÃO
#===========================================================

figuras <- list(
  
  simples = c(8,6),
  
  painel = c(10,8),
  
  multivariada = c(11,9),
  
  artigo = c(7,5)
  
)
salvar_figura <- function(
    
  grafico,
  
  nome,
  
  tipo="simples",
  
  idioma="PT"
  
){
  
  tam <- figuras[[tipo]]
  
  ggsave(
    
    paste0("Figuras/",idioma,"/",nome,".png"),
    
    grafico,
    
    width=tam[1],
    
    height=tam[2],
    
    dpi=600
    
  )
  
  ggsave(
    
    paste0("Figuras/",idioma,"/",nome,".pdf"),
    
    grafico,
    
    width=tam[1],
    
    height=tam[2]
    
  )
  
}
salvar_tabela <- function(tabela,nome){
  
  openxlsx::write.xlsx(
    
    tabela,
    
    paste0(
      
      "Tabelas/",
      
      nome,
      
      ".xlsx"
      
    ),
    
    overwrite=TRUE
    
  )
  
}