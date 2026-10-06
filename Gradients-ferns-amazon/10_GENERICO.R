#===============================================================================
# NOVA FUNÇÃO FABRICERA - OTIMIZADA PARA QUALIDADE E LAYOUT (PNG)
#===============================================================================
fabricera_otimizada <- function(
    table, gradient=NULL, sort.guild=NULL,
    col=1, col.places=2, col.species=3, col.gradient="gray",
    highlight.species=NA, highlight.places=NA,
    xyratio=1.0,  
    cex.species=0.8, lwd=0.5, cex.lab=1.5, cex.title=1.2, cex.gradient=1.0,
    file=NULL, title=NULL, xlab="Ordered places", ylab="Abundance",
    show.grad=TRUE, show.places=TRUE, gradlab="Gradient",
    lty=1, lty.lines=1, border=1) {
  
  if (!is.null(file)) {
    # AJUSTE: Usando png() nativo, que costuma ter compatibilidade universal no R
    png(filename=file, width=300, height=200, units="mm", res=300)
    on.exit(dev.off()) # Garante o fechamento do arquivo se houver erro
  }
  
  # Margens ajustadas para a imagem horizontal
  par(mar=c(7, 7, 3, 14), mgp=c(3, 1, 0)) 
  
  ### Cálculo e Ordenação
  if(is.null(gradient)){gradient<-prcomp(table)$x[,1]}
  if(is.null(sort.guild)){sort.guild<-order(colSums(table*gradient)/colSums(table))}else{
    sort.guild<-order((colSums(table*gradient)/colSums(table))*sort.guild)}
  
  tabela<-table[order(gradient),sort.guild]
  gradient_sorted <- gradient[order(gradient)]
  
  if(length(highlight.species)==length(col.species)){
    col.species<-col.species[order(colSums(table*gradient)/colSums(table))]
  }
  
  ### Inicialização de Plotagem
  plot.new()
  if (show.grad) {
    showg_gap <- 25 
  } else {
    showg_gap <- 0
  }
  plot.window(xlim=c(0, 100), ylim=c(0, 100 + showg_gap))
  
  dimx <- 100 / nrow(tabela)
  dimy <- 100 / ncol(tabela) 
  
  col_vec <-rep(col,nrow(tabela))
  if(!all(is.na(highlight.places))) {
    col_vec[match(highlight.places,rownames(tabela))]<-col.places
  }
  
  ### Loop de Abundância 
  for(i in 1:ncol(tabela)){
    
    col.sp<-match(highlight.species,colnames(tabela)[i])
    
    if(sum(col.sp,na.rm=T)!=0){col.2=col.species}else{col.2=col_vec}
    if(length(highlight.species)==length(col.species)){col.2=col.species[i]}
    
    rect((0:(nrow(tabela)-1))*dimx, (i-1)*dimy, 
         (1:nrow(tabela))*dimx - dimx*.1, 
         (i-1)*dimy + (tabela[,i]/max(tabela))*dimy*.8, 
         col=col.2, lty=lty, lwd=0.01, border="black")
  }
  
  ### Nomes das Espécies
  par(xpd=TRUE) 
  text(rep(100 + 2,ncol(tabela)), 
       cumsum(rep(100/ncol(tabela),ncol(tabela)))-100/ncol(tabela)/2,
       gsub("_"," ",colnames(tabela)),
       cex=cex.species, adj=0, font=3)
  par(xpd=FALSE) 
  
  ### Títulos e Eixos
  title(main=title, cex.main=cex.title)
  title(ylab=ylab, cex.lab=cex.lab, line=3.5)
  title(xlab=xlab, cex.lab=cex.lab, line=4)
  
  ### Barra de Gradiente
  if(show.grad){
    # Proteção extra para evitar erros se os valores do gradiente forem todos zero
    gradient_range <- abs(max(c(gradient_sorted,0))-min(c(gradient_sorted,0)))
    if(gradient_range == 0) gradient_range <- 1 
    
    gradient2 <- gradient_sorted / gradient_range
    val = 100 + 5 
    
    rect((0:(nrow(tabela)-1))*dimx, val, 
         (1:nrow(tabela))*dimx-dimx*.1, 
         val+gradient2*15, 
         lwd=lwd,col=col.gradient)
    
    par(xpd=TRUE)
    label_y = val + 15 + 5
    text(100/2, label_y, gradlab, cex=cex.gradient, font=2)
    text(0, val - 3, round(min(c(0,gradient_sorted)),2), adj=1, cex=.7)
    text(100, val - 3, round(max(c(0,gradient_sorted)),2), adj=0, cex=.7)
    par(xpd=FALSE)
  }
  
  ### Eixo Inferior (Locais)
  if(show.places){
    axis(1,at=((0:(nrow(tabela)-1))*dimx+(1:nrow(tabela))*dimx-dimx*.1)/2,
         labels=rownames(tabela), las=2, cex.axis=cex.species, tick=F, 
         mgp=c(3, 1, 0), line=0.5) 
  }
  
}
#-----------------------------------------------------------------------
# 1. PREPARAÇÃO DOS DADOS
#-----------------------------------------------------------------------
library(dplyr)

# Transforma os dados de abundância usando log(x + 1)
# (Certifique-se de que 'community_data' já esteja carregado no seu R)
sp_transf <- log1p(community_data)

# Define as variáveis que serão lidas do seu dataframe 'env_data'
variaveis <- c(
  "canoppy",
  "pH_solo",
  "carbono_solo",
  "umidade_solo",
  "argila",
  "inclinacao",
  "nivel_agua",
  "altitude"
)

# Define os nomes bonitos que aparecerão nos gráficos
nomes <- c(
  canoppy = "Cobertura_Dossel",
  pH_solo = "pH",
  carbono_solo = "Carbono",
  umidade_solo = "Umidade",
  argila = "Argila",
  inclinacao = "Inclinacao",
  nivel_agua = "Nivel_Agua",
  altitude = "Altitude"
)

#-----------------------------------------------------------------------
# 2. LOOP - GERAÇÃO DOS GRÁFICOS EM PNG
#-----------------------------------------------------------------------

# Cria o diretório 'Figuras' se não existir
if (!dir.exists("Figuras")) {
  dir.create("Figuras")
  cat("Diretório 'Figuras' criado no local de trabalho.\n")
}

for(i in variaveis){
  
  cat("Gerando gráfico otimizado para o gradiente:", nomes[i], "...\n")
  
  # Chamada da nova função
  # (Certifique-se de ter rodado a função 'fabricera_otimizada' antes)
  fabricera_otimizada(
    table=sp_transf,
    gradient=env_data[[i]],
    
    col=1, 
    col.places=1, 
    col.species=3, 
    col.gradient="black", 
    
    xyratio=1.0, 
    cex.species=0.7, 
    lwd=0.5, 
    cex.lab=1.0,     
    cex.title=1.0,   
    cex.gradient=0.8,
    
    gradlab=nomes[i],
    title="",
    ylab="Abundância (log(x+1))", 
    xlab="Ordered places",       
    
    lty=0, 
    show.grad=TRUE,
    show.places=TRUE,
    
    # Salvando em PNG
    file=paste0("Figuras/Fabricera_", nomes[i], ".png")
  )
}

cat("✅ Concluído! Todos os gráficos em PNG foram salvos na pasta 'Figuras'.\n")
  