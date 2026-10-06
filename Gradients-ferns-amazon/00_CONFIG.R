#===========================================================
# CONFIGURAÇÕES GERAIS DO PROJETO
#===========================================================

options(stringsAsFactors = FALSE)

set.seed(123)

# Tema padrão dos gráficos
library(ggplot2)
theme_set(theme_bw())

# Criar pastas automaticamente
dir.create("Figuras/PT", recursive = TRUE, showWarnings = FALSE)
dir.create("Figuras/EN", recursive = TRUE, showWarnings = FALSE)
dir.create("Tabelas", recursive = TRUE, showWarnings = FALSE)
dir.create("Relatorios", recursive = TRUE, showWarnings = FALSE)