# =============================================================================
# CRIAR TABELAS DE MODELOS COM MÉDIAS (Aba, Ada e AbaAda)
# =============================================================================
# Article: ........................... 
# Authors: Marise H.V. de Oliveira, Niksoney A. Mendonça and Thaís E. Almeida
# Script author: Niksoney Azevedo Mendonça
# E-mail: niksoneyazevedo2017@gmail.com
# =============================================================================
# PACOTES PARA NECESSÁRIOS
# =============================================================================

# Limpa o ambiente e fecha todas as visualizações
rm(list = ls()) 
# =============================================================================

install.packages("dplyr")

library(dplyr) # Manipulação de dados (filtros, seleção, agrupamento)
# =============================================================================
# FILTRANDO DADOS PARA CRIAR TABELAS
# =============================================================================

# Diretorio das tabelas
setwd("C:/Users/Jhonathan/Desktop/RESULTADOS_ANALISES/NIR/FILOGENETICA")

# Importar a tabela dados NIR
Matriz_NIR <- read.table(file='dado_filogenetico_2026_snv.csv',header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8", colClasses = "character")

# Exluir numeros dedepois de ABA ou ADA
Matriz_NIR <- Matriz_NIR %>%
  filter(substr(leitura, 1, 3) %in% c("aba", "ada")) %>%
  mutate(leitura = substr(leitura, 1, 3))  # Remove números

# Definir coluna e classes para filtrar
quecoluna <- "leitura"  # Nome da coluna
queclasse1 <- "aba"                # Filtro 1 (3 primeiras letras)
queclasse2 <- "ada"                # Filtro 2 (3 primeiras letras)

# cria um vetor logico, ou seja TRUE/FALSE para cada linha que corresponde ou não ao filtro
vl <- Matriz_NIR[,quecoluna]==queclasse1
v2 <- Matriz_NIR[,quecoluna]==queclasse2

# Filtrar os dados
dados.abaxialbrutos <- Matriz_NIR[vl, ]
dados.adaxialbrutos <- Matriz_NIR[v2, ]

# Combinar os dados filtrados em uma única tabela
dados.combinados <- rbind(dados.abaxialbrutos, dados.adaxialbrutos)

# salva o dado filtrado
write.table(dados.abaxialbrutos, file='DADOS BRUTOS_abaxial_filogenetico_snv.csv',row.names = FALSE, sep = ";", quote = FALSE, dec = ".", fileEncoding = "UTF-8")              
write.table(dados.adaxialbrutos, file='DADOS BRUTOS_adaxial_filogenetico_snv.csv',row.names = FALSE, sep = ";", quote = FALSE, dec = ".", fileEncoding = "UTF-8") 
write.table(dados.combinados, file='DADOS BRUTOS_abaada_filogenetico_snv.csv',row.names = FALSE, sep = ";", quote = FALSE, dec = ".", fileEncoding = "UTF-8") 

# =============================================================================
# FILTRANDO DADOS PARA CRIAR TABELAS COM DADOS MÉDIOS
# =============================================================================

rm(list=ls()) #Limpar a lista de arquivos

# Importar a tabela com dados brutos 3001
dados_Aba <- read.table(file='DADOS BRUTOS_abaxial_filogenetico_snv.csv',header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8", colClasses = "character")
dados_Ada <- read.table(file='DADOS BRUTOS_adaxial_filogenetico_snv.csv',header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8", colClasses = "character")
dados_AbaAda <- read.table(file='DADOS BRUTOS_abaada_filogenetico_snv.csv',header = TRUE, sep = ";", dec = ".", fileEncoding = "UTF-8", colClasses = "character")

colmorfo <- "especie"
colcollector <- "individuo"
colABAD <- "leitura"

# 3. Lista com os dataframes e seus nomes de saída
lista_dados <- list(
  "aba" = dados_Aba,
  "ada" = dados_Ada,
  "abaada" = dados_AbaAda)

# 4. Processamento automatizado para cada dataframe
for (nome in names(lista_dados)) {
  dados <- lista_dados[[nome]]
  
  # Selecionar colunas NIR (X...)
  colunas_nir <- grep("^X", colnames(dados), value = TRUE)
  
  # Converter para numérico e calcular média por coletor
  dados_nir <- as.data.frame(lapply(dados[, colunas_nir], as.numeric))
  medias_nir <- aggregate(dados_nir, by = list(Coletor = dados[[colcollector]]), FUN = mean, na.rm = TRUE)
  colnames(medias_nir)[1] <- colcollector
  
  # Extrair metadados únicos
  metadados <- dados[, c(colmorfo, colcollector, colABAD)]
  metadados <- metadados[!duplicated(metadados[[colcollector]]), ]
  
  # Juntar tudo
  dados_processados <- merge(metadados, medias_nir, by = colcollector)
  
  # Salvar em arquivo
  nome_arquivo <- paste0("DADOS MEDIOS_", nome, "_filogenetico_snv.csv")
  write.csv(dados_processados, nome_arquivo, row.names = FALSE)
  
  cat("Arquivo salvo:", nome_arquivo, "\n")
}

# Após o loop, carregue os arquivos salvos 3001:
View(read.csv("DADOS MEDIOS_aba_filogenetico.csv"))
View(read.csv("DADOS MEDIOS_ada_filogenetico.csv"))
View(read.csv("DADOS MEDIOS_abaada_filogenetico.csv"))
