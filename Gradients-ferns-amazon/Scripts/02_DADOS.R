#===========================================================
# LEITURA DOS DADOS
#===========================================================
setwd("C:/Users/vanes/OneDrive/Área de Trabalho/_VANESSA FERNANDES/bases de dados/DISSERTACAO_FERNS")

community_data <- read.csv(
  "Dados/ferns_analise_abundancia.csv",
  sep = ";",
  row.names = 1,
  check.names = FALSE
)

env_geral <- read.csv(
  "Dados/variaveis_analise_ferns.csv",
  sep = ";",
  dec = ",",
  row.names = 1,
  check.names = FALSE
)

env_solo <- read.csv(
  "Dados/variaveis_solo_ferns.csv",
  sep = ";",
  dec = ",",
  row.names = 1,
  check.names = FALSE
)

#===========================================================
# JUNÇÃO DAS VARIÁVEIS AMBIENTAIS
#===========================================================

env_data <- merge(
  env_geral,
  env_solo,
  by = "row.names"
)

rownames(env_data) <- env_data$Row.names
env_data$Row.names <- NULL

#===========================================================
# SUBSTITUIR NAs PELA MÉDIA
#===========================================================

for(i in seq_len(ncol(env_data))){
  
  env_data[is.na(env_data[, i]), i] <-
    mean(env_data[, i], na.rm = TRUE)
  
}

#===========================================================
# SINCRONIZAR PARCELAS
#===========================================================

comum <- intersect(
  rownames(community_data),
  rownames(env_data)
)

community_data <- community_data[comum, ]

env_data <- env_data[comum, ]

cat("Número de parcelas:", nrow(community_data), "\n")
cat("Número de espécies:", ncol(community_data), "\n")
cat("Número de variáveis ambientais:", ncol(env_data), "\n")

