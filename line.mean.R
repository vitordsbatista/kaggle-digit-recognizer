line.mean <- function(data){
  #Remove o label
	#data <- data[,-1]
  #Cria o dataframe com 28 linhas
  #c <- data.frame(1:28)
  l <-matrix(ncol=28)
  #Verifica cada linha
	for (i in c(1:nrow(data))){
    #Transforma cada linha em uma matrz 28x28
		q <- matrix(as.numeric(data[i,]), 28, 28)
    #Calcula as medias das colunas de cada linha e concatena
    #c <- cbind(c, colMeans(q))
    #l <- cbind(l, rowMeans(q))
    
    a <- rowMeans(q)
		a <- t(a)
		#c[i,] <- a
		a <- rowMeans(q)
		l <- rbind(l, a)
	}
	return(l)
}