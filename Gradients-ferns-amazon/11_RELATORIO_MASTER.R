#===============================================================================
# 11. RELATÓRIO ESTATÍSTICO COMPLETO (CONSULTA)
#===============================================================================

suppressPackageStartupMessages(library(vegan))
suppressPackageStartupMessages(library(psych))

dir.create("Resultados", showWarnings = FALSE)
sink("Resultados/Relatorio_Estatistico.txt")

cat("================================================================================\n")
cat(" RELATÓRIO ESTATÍSTICO COMPLETO - DISSERTAÇÃO\n")
cat("================================================================================\n\n")
cat("Gerado em:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n\n")

#-------------------------------------------------------------------------------
# FUNÇÃO AUXILIAR PARA EXTRAÇÃO COMPATÍVEL COM GLMMTMB, GLM E LM
#-------------------------------------------------------------------------------
obter_df_coeficientes <- function(mod) {
  resumo <- summary(mod)
  # Extração especial caso seja um modelo glmmTMB (estrutura em lista com $cond)
  if (inherits(mod, "glmmTMB") || (is.list(resumo$coefficients) && !is.data.frame(resumo$coefficients))) {
    if (!is.null(resumo$coefficients$cond)) {
      coef_mat <- resumo$coefficients$cond
    } else {
      coef_mat <- resumo$coefficients[[1]]
    }
  } else {
    coef_mat <- coef(resumo)
    if (is.null(coef_mat)) coef_mat <- resumo$coefficients
  }
  return(as.data.frame(coef_mat))
}

#-------------------------------------------------------------------------------
# FUNÇÕES DE GLM BLINDADAS
#-------------------------------------------------------------------------------
imprimir_glm_relatorio <- function(nome_obj, titulo) {
  if(exists(nome_obj, envir = .GlobalEnv)) {
    mod <- get(nome_obj, envir = .GlobalEnv)
    cat("------------------------------------------------------------\n")
    cat(titulo, "\n")
    
    tryCatch({ cat("Fórmula:", deparse(formula(mod)), "\n\n") }, error = function(e) { cat("Fórmula: Não identificada\n\n") })
    
    resumo <- summary(mod)
    cat("Coeficientes:\n")
    
    tryCatch({
      coef_df <- obter_df_coeficientes(mod)
      if(!is.null(coef_df) && nrow(coef_df) > 0) {
        num_cols <- sapply(coef_df, is.numeric)
        coef_df[num_cols] <- lapply(coef_df[num_cols], function(x) round(x, 4))
        print(coef_df)
      } else { print(resumo) }
    }, error = function(e) { print(resumo) })
    
    cat("\nMétricas de Ajuste:\n")
    tryCatch({
      aic_val <- if(!is.null(AIC(mod))) round(AIC(mod), 2) else "N/A"
      dev_val <- if(!is.null(resumo$deviance)) round(resumo$deviance, 2) else "N/A"
      cat("AIC:", aic_val, "| Deviance Residual:", dev_val, "\n\n")
    }, error = function(e) { cat("AIC/Deviance: Não aplicável\n\n") })
    
    cat("Tabela ANOVA / Teste de Significância:\n")
    tryCatch({
      print(anova(mod, test = "Chisq"))
    }, error = function(e) {
      tryCatch({ print(anova(mod)) }, error = function(e2) { cat("ANOVA não disponível para este modelo.\n") })
    })
    cat("\n")
  }
}

gerar_texto_glm <- function(nome_obj, var_resposta) {
  if(exists(nome_obj, envir = .GlobalEnv)) {
    mod <- get(nome_obj, envir = .GlobalEnv)
    tryCatch({
      coef_df <- obter_df_coeficientes(mod)
      
      if(!is.null(coef_df) && nrow(coef_df) > 0) {
        # Remove a linha do Intercepto (se existir)
        if(nrow(coef_df) > 1 && grepl("Intercept|intercept", rownames(coef_df)[1], ignore.case = TRUE)) {
          coef_validos <- coef_df[-1, , drop = FALSE]
        } else { 
          coef_validos <- coef_df 
        }
        
        # Localiza dinamicamente as colunas de P-valor e Beta (Estimativa)
        p_col <- grep("Pr|p", colnames(coef_validos), ignore.case = TRUE)
        p_col <- if(length(p_col) > 0) p_col[1] else ncol(coef_validos)
        
        est_col <- grep("Estimate|coef|β", colnames(coef_validos), ignore.case = TRUE)
        est_col <- if(length(est_col) > 0) est_col[1] else 1
        
        p_vals <- as.numeric(coef_validos[[p_col]])
        beta_vals <- as.numeric(coef_validos[[est_col]])
        sig_idx <- which(!is.na(p_vals) & p_vals < 0.05)
        
        if(length(sig_idx) > 0) {
          cat(sprintf("Para a %s, os Modelos Lineares Generalizados (GLMs) indicaram efeitos significativos. ", var_resposta))
          for(i in sig_idx) {
            var_name <- rownames(coef_validos)[i]
            beta_val <- beta_vals[i]
            p_val <- p_vals[i]
            efeito <- ifelse(beta_val > 0, "positivo", "negativo")
            cat(sprintf("A variável %s exerceu um efeito %s e significativo (β = %.3f; p = %.4f). ", var_name, efeito, beta_val, p_val))
          }
          cat("\n\n")
        } else {
          cat(sprintf("A modelagem preditiva (GLM) não evidenciou preditores ambientais significativos para a %s (p > 0.05 para todas as variáveis analisadas).\n\n", var_resposta))
        }
      }
    }, error = function(e) { cat(sprintf("Não foi possível extrair a interpretação textual do modelo de %s.\n\n", var_resposta)) })
  }
}

#-------------------------------------------------------------------------------
# 1. DESCRIÇÃO DA COMUNIDADE
#-------------------------------------------------------------------------------
cat("================================================================================\n")
cat(" 1. DESCRIÇÃO DA COMUNIDADE\n")
cat("================================================================================\n\n")

if(exists("community_data", envir = .GlobalEnv)) {
  comm <- get("community_data", envir = .GlobalEnv)
  riqueza <- specnumber(comm)
  abundancia <- rowSums(comm)
  shannon <- diversity(comm, index = "shannon")
  ocorrencia <- colSums(comm > 0)
  abund_especie <- colSums(comm)
  
  cat("RESUMO GERAL:\n")
  cat("Número de parcelas (N):", nrow(comm), "\n")
  cat("Número de espécies (Riqueza total):", ncol(comm), "\n")
  cat("Número total de indivíduos (Abundância total):", sum(comm), "\n\n")
  
  estat_desc <- function(x) c(Min = min(x), Max = max(x), Media = mean(x), Mediana = median(x), Desvio = sd(x))
  
  cat("RIQUEZA POR PARCELA:\n"); print(round(estat_desc(riqueza), 2))
  cat("\nABUNDÂNCIA POR PARCELA:\n"); print(round(estat_desc(abundancia), 2))
  cat("\nÍNDICE DE SHANNON POR PARCELA:\n"); print(round(estat_desc(shannon), 2))
  
  cat("\nSINGLETONS E DISTRIBUIÇÃO:\n")
  cat("Espécies registradas em apenas uma parcela (Singletons):", sum(ocorrencia == 1), "\n")
  cat("Espécies presentes em todas as parcelas:", sum(ocorrencia == nrow(comm)), "\n\n")
  
  cat("------------------------------------------------------------\n")
  cat("ESPÉCIES MAIS FREQUENTES (Top 10)\n")
  freq <- data.frame(Especie = names(ocorrencia), Parcelas = ocorrencia, Freq_Perc = round(ocorrencia/nrow(comm)*100, 1))
  print(head(freq[order(-freq$Parcelas), ], 10), row.names = FALSE)
  
  cat("\n------------------------------------------------------------\n")
  cat("ESPÉCIES MAIS ABUNDANTES (Top 10)\n")
  abund_df <- data.frame(Especie = names(abund_especie), Abundancia = abund_especie, Relativa_Perc = round(abund_especie/sum(abund_especie)*100, 1))
  print(head(abund_df[order(-abund_df$Abundancia), ], 10), row.names = FALSE)
  cat("\n")
} else { cat("[Aviso] Objeto 'community_data' não encontrado na memória.\n\n") }

#-------------------------------------------------------------------------------
# 2 e 3. VARIÁVEIS AMBIENTAIS E MULTICOLINEARIDADE
#-------------------------------------------------------------------------------
cat("================================================================================\n")
cat(" 2 & 3. DESCRITIVAS AMBIENTAIS E CORRELAÇÃO\n")
cat("================================================================================\n\n")

if(exists("env_data", envir = .GlobalEnv)) {
  env <- get("env_data", envir = .GlobalEnv)
  desc_env <- psych::describe(env)
  desc_env$cv <- (desc_env$sd / desc_env$mean) * 100
  desc_env <- desc_env[, c("mean", "median", "sd", "se", "cv", "min", "max", "skew", "kurtosis")]
  colnames(desc_env) <- c("Media", "Mediana", "Desvio_Padrao", "Erro_Padrao", "CV(%)", "Minimo", "Maximo", "Assimetria", "Curtose")
  
  cat("ESTATÍSTICAS DESCRITIVAS:\n")
  print(round(desc_env, 3))
  
  cat("\nMATRIZ DE CORRELAÇÃO (Pearson):\n")
  mat_cor <- cor(env, use = "pairwise.complete.obs")
  print(round(mat_cor, 3))
  
  cat("\nINTERPRETAÇÃO DE CORRELAÇÕES ELEVADAS (|r| > 0.7):\n")
  alta_cor <- which(abs(mat_cor) > 0.7 & abs(mat_cor) < 1, arr.ind = TRUE)
  if(nrow(alta_cor) > 0) {
    for(i in 1:nrow(alta_cor)) {
      if(alta_cor[i,1] < alta_cor[i,2]) {
        cat(sprintf("-> Forte correlação entre %s e %s (r = %.3f)\n", rownames(mat_cor)[alta_cor[i,1]], colnames(mat_cor)[alta_cor[i,2]], mat_cor[alta_cor[i,1], alta_cor[i,2]]))
      }
    }
  } else { cat("-> Nenhuma correlação severa (|r| > 0.7) encontrada.\n") }
  cat("\n")
} else { cat("[Aviso] Objeto 'env_data' não encontrado na memória.\n\n") }

#-------------------------------------------------------------------------------
# 4. PCA
#-------------------------------------------------------------------------------
cat("================================================================================\n")
cat(" 4. PCA - ANÁLISE DE COMPONENTES PRINCIPAIS\n")
cat("================================================================================\n\n")

if(exists("pca_res", envir = .GlobalEnv)) {
  pca_res <- get("pca_res", envir = .GlobalEnv)
  eig <- eigenvals(pca_res)
  perc <- (eig/sum(eig))*100
  
  cat("AUTOVALORES E VARIÂNCIA EXPLICADA:\n")
  print(round(data.frame(Autovalor = eig, Explicada_Perc = perc, Acumulada_Perc = cumsum(perc))[1:4,], 2))
  
  cat("\nLOADINGS (Eixos 1 e 2):\n")
  print(round(scores(pca_res, display = "species", choices = 1:2), 3))
  cat("\n")
} else { cat("[Aviso] Objeto 'pca_res' não encontrado.\n\n") }

#-------------------------------------------------------------------------------
# 5 e 6. GLMs
#-------------------------------------------------------------------------------
cat("================================================================================\n")
cat(" 5 & 6. MODELOS LINEARES GENERALIZADOS (GLMs)\n")
cat("================================================================================\n\n")

imprimir_glm_relatorio("best_S", "GLM - RIQUEZA")
imprimir_glm_relatorio("best_N", "GLM - ABUNDÂNCIA")
imprimir_glm_relatorio("best_H", "GLM - SHANNON")
imprimir_glm_relatorio("modelo_taxonomico", "GLM - DIVERSIDADE TAXONÔMICA")

#-------------------------------------------------------------------------------
# 7 a 11. ANÁLISES MULTIVARIADAS
#-------------------------------------------------------------------------------
cat("================================================================================\n")
cat(" 7 a 11. ESTRUTURA DA COMUNIDADE MULTIVARIADA\n")
cat("================================================================================\n\n")

if(exists("nmds_ab", envir = .GlobalEnv)) {
  cat("NMDS - ABUNDÂNCIA\nStress:", round(get("nmds_ab", envir = .GlobalEnv)$stress, 4), "\n\n")
}
if(exists("nmds_riq", envir = .GlobalEnv)) {
  cat("NMDS - RIQUEZA\nStress:", round(get("nmds_riq", envir = .GlobalEnv)$stress, 4), "\n\n")
}
if(exists("fit", envir = .GlobalEnv)) {
  cat("ENVFIT - VETORES AMBIENTAIS\n"); print(get("fit", envir = .GlobalEnv)); cat("\n")
}
if(exists("permanova", envir = .GlobalEnv)) {
  cat("PERMANOVA\n"); print(get("permanova", envir = .GlobalEnv)); cat("\n")
}
if(exists("beta", envir = .GlobalEnv) && inherits(get("beta", envir = .GlobalEnv), "betadisper")) {
  cat("BETADISPER (Homogeneidade de Dispersão)\n"); print(anova(get("beta", envir = .GlobalEnv))); cat("\n")
}
if(exists("modelo_dbrda", envir = .GlobalEnv)) {
  mod_db <- get("modelo_dbrda", envir = .GlobalEnv)
  cat("dbRDA (Redundancy Analysis)\nTeste Global:\n")
  print(anova(mod_db, permutations = 999))
  cat("\n")
}

sink()
cat("✔ Relatório Estatístico (Completo) salvo em: Resultados/Relatorio_Estatistico.txt\n")

#===============================================================================
# 12. TEXTOS GERADOS AUTOMATICAMENTE PARA A DISSERTAÇÃO
#===============================================================================

sink("Resultados/Resultados_Para_Dissertacao.txt")
cat("================================================================================\n")
cat(" TEXTOS PARA A SEÇÃO DE RESULTADOS DA DISSERTAÇÃO\n")
cat("================================================================================\n\n")

if(exists("community_data", envir = .GlobalEnv)) {
  comm <- get("community_data", envir = .GlobalEnv)
  cat("1. DESCRIÇÃO DA COMUNIDADE\n")
  cat(sprintf(
    "A comunidade estudada apresentou um total de %d indivíduos, distribuídos em %d espécies e registradas em %d parcelas amostrais. A riqueza média por parcela foi de %.1f espécies (±%.1f DP), enquanto a abundância média localizou-se em %.1f indivíduos (±%.1f DP). O índice de diversidade de Shannon (H') médio para a área de estudo foi de %.2f (±%.2f DP). Notavelmente, %d espécies foram classificadas como singletons (ocorrendo em apenas uma parcela).\n\n",
    sum(comm), ncol(comm), nrow(comm), mean(specnumber(comm)), sd(specnumber(comm)), mean(rowSums(comm)), sd(rowSums(comm)), mean(diversity(comm, index="shannon")), sd(diversity(comm, index="shannon")), sum(colSums(comm > 0) == 1)
  ))
}

if(exists("pca_res", envir = .GlobalEnv)) {
  perc <- (eigenvals(get("pca_res", envir = .GlobalEnv))/sum(eigenvals(get("pca_res", envir = .GlobalEnv))))*100
  cat("2. ORDENAÇÃO AMBIENTAL (PCA)\n")
  cat(sprintf("A Análise de Componentes Principais (PCA) demonstrou que os dois primeiros eixos explicaram cumulativamente %.1f%% da variação ambiental observada na área de estudo. O primeiro eixo (PC1) foi responsável por %.1f%% da variância, enquanto o segundo eixo (PC2) reteve %.1f%% da variação.\n\n", perc[1] + perc[2], perc[1], perc[2]))
}

cat("3. EFEITO DO AMBIENTE NAS MÉTRICAS UNIVARIADAS (GLMs)\n")
gerar_texto_glm("best_S", "riqueza de espécies")
gerar_texto_glm("best_N", "abundância")
gerar_texto_glm("best_H", "diversidade de Shannon")
gerar_texto_glm("modelo_taxonomico", "diversidade taxonômica")

cat("4. ESTRUTURAÇÃO MULTIVARIADA DA COMUNIDADE (NMDS e Envfit)\n")
if(exists("nmds_ab", envir = .GlobalEnv)) cat(sprintf("A ordenação baseada em abundância (NMDS) representou de forma adequada a dissimilaridade entre as parcelas, apresentando um valor de stress de %.3f. ", get("nmds_ab", envir = .GlobalEnv)$stress))
if(exists("fit", envir = .GlobalEnv)) {
  p_vals <- get("fit", envir = .GlobalEnv)$vectors$pvals
  sig_env <- names(p_vals[p_vals < 0.05])
  if(length(sig_env) > 0) { cat("O ajuste a posteriori dos gradientes ambientais (Envfit) revelou que a composição da comunidade é estruturada significativamente pelas seguintes variáveis: ", paste(sig_env, collapse = ", "), " (p < 0.05).\n\n") } else { cat("O ajuste de vetores ambientais (Envfit) demonstrou que nenhuma variável ambiental testada estruturou significativamente a composição da comunidade.\n\n") }
}

cat("5. TESTES DE HIPÓTESE MULTIVARIADOS (PERMANOVA, Betadisper e dbRDA)\n")
if(exists("permanova", envir = .GlobalEnv)) {
  permanova <- get("permanova", envir = .GlobalEnv)
  sig_termos <- which(!is.na(permanova$`Pr(>F)`) & permanova$`Pr(>F)` < 0.05)
  if(length(sig_termos) > 0) {
    cat("A PERMANOVA confirmou que a composição da comunidade variou significativamente em resposta ao ambiente. ")
    for(i in sig_termos) {
      nome_termo <- rownames(permanova)[i]
      if(grepl("^Model$", nome_termo, ignore.case = TRUE)) {
        cat(sprintf("O modelo ambiental global explicou significativamente a variação biológica (R² = %.3f; p = %.3f). ", permanova$R2[i], permanova$`Pr(>F)`[i]))
      } else {
        cat(sprintf("A variável %s explicou parte significativa da variação biológica (R² = %.3f; p = %.3f). ", nome_termo, permanova$R2[i], permanova$`Pr(>F)`[i]))
      }
    }
    cat("\n")
  }
}
if(exists("beta", envir = .GlobalEnv) && inherits(get("beta", envir = .GlobalEnv), "betadisper")) {
  p_beta <- anova(get("beta", envir = .GlobalEnv))$`Pr(>F)`[1]
  if(!is.na(p_beta) & p_beta < 0.05) { cat(sprintf("Entretanto, o teste de Betadisper foi significativo (p = %.3f), indicando heterogeneidade nas dispersões biológicas entre os grupos.\n", p_beta)) } else { cat(sprintf("O teste de Betadisper indicou homogeneidade na dispersão multivariada (p = %.3f), garantindo a validade dos padrões observados na PERMANOVA.\n", p_beta)) }
}
if(exists("modelo_dbrda", envir = .GlobalEnv)) {
  p_db <- anova(get("modelo_dbrda", envir = .GlobalEnv))$`Pr(>F)`[1]
  if(!is.na(p_db) & p_db < 0.05) { cat(sprintf("A Análise de Redundância baseada em distância (dbRDA) reforçou a estruturação ambiental (Modelo Global: p = %.4f).\n\n", p_db)) } else { cat(sprintf("O modelo global da dbRDA não apresentou significância estatística (p = %.4f).\n\n", p_db)) }
}

sink()
cat("✔ Textos automáticos salvos em: Resultados/Resultados_Para_Dissertacao.txt\n")