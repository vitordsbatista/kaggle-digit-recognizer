col.mean <- function(data){
  #Remove o label
	#data <- data[,-1]
  #Cria o dataframe com 28 linhas
  c <- matrix(ncol=28)
  #l <- data.frame(1:28)
  #Verifica cada linha
  #for (i in c(1:(length(row(data))/784))){
  for (i in c(1:nrow(data))){
    #Transforma cada linha em uma matrz 28x28
		q <- matrix(as.numeric(data[i,]), 28, 28)
    #Calcula as medias das colunas de cada linha e concatena
    a <- colMeans(q)
    a <- t(a)
		#c[i,] <- a
    a <- colMeans(q)
    c <- rbind(c, a)
		#c[i,1:28] <- a[1:28]
    #l <- cbind(l, rowMeans(q))
	}
  #return(matrix(a))
  return (c)
}