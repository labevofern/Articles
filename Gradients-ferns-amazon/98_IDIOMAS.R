#=========================================================
# DICIONÁRIO DE IDIOMAS
#=========================================================

nomes_variaveis <- list(
  
  PT = c(
    canoppy = "Cobertura do dossel",
    pH_solo = "pH",
    carbono_solo = "Carbono",
    umidade_solo = "Umidade",
    inclinacao = "Inclinação",
    nivel_agua = "Nível da água",
    altitude = "Altitude",
    argila = "Argila"
  ),
  
  EN = c(
    canoppy = "Canopy cover",
    pH_solo = "Soil pH",
    carbono_solo = "Soil carbon",
    umidade_solo = "Soil moisture",
    inclinacao = "Slope",
    nivel_agua = "Water table",
    altitude = "Elevation",
    argila = "Clay"
  
  )
  
)
  

grupos <- list(
  
  PT = c(
    
    "Ripária"="Ripária",
    
    "Terra firme"="Terra firme"
    
  ),
  
  EN = c(
    
    "Ripária"="Riparian forest",
    
    "Terra firme"="Terra-firme forest"
    
  )
  
)

categorias <- list(
  
  PT = c(
    
    "Estrutura"="Estrutura",
    
    "Solo"="Solo",
    
    "Topografia"="Topografia",
    
    "Hidrologia"="Hidrologia"
    
  ),
  
  EN = c(
    
    "Estrutura"="Forest structure",
    
    "Solo"="Soil",
    
    "Topografia"="Topography",
    
    "Hidrologia"="Hydrology"
    
  )
  
)

rotulos <- list(
  
  PT=list(
    
    ambiente="Ambiente",
    
    categoria="Categoria da variável"
    
  ),
  
  EN=list(
    
    ambiente="Environment",
    
    categoria="Variable category"
    
  )
  
)
#=========================================================
# Função para traduzir automaticamente
#=========================================================

traduzir <- function(idioma = "PT"){
  
  if(idioma == "PT"){
    
    nomes <- c(
      
      canoppy = "Cobertura do dossel",
      pH_solo = "pH",
      carbono_solo = "Carbono",
      umidade_solo = "Umidade",
      inclinacao = "Inclinação",
      nivel_agua = "Nível da água",
      altitude = "Altitude",
      argila = "Argila",
      
      
    )
    
    grupo_riparia <- "Ripária"
    grupo_tf <- "Terra firme"
    
    leg_grupo <- "Ambiente"
    leg_categoria <- "Categoria"
    
  }else{
    
    nomes <- c(
      
      canoppy = "Canopy cover",
      pH_solo = "Soil pH",
      carbono_solo = "Soil carbon",
      umidade_solo = "Soil moisture",
      inclinacao = "Slope",
      nivel_agua = "Water table",
      altitude = "Elevation",
      argila = "Clay",
      
    )
    
    grupo_riparia <- "Riparian forest"
    grupo_tf <- "Terra-firme forest"
    
    leg_grupo <- "Environment"
    leg_categoria <- "Variable"
    
  }
  
  list(
    
    nomes = nomes,
    
    grupo_riparia = grupo_riparia,
    
    grupo_tf = grupo_tf,
    
    leg_grupo = leg_grupo,
    
    leg_categoria = leg_categoria
    
  )
  
}