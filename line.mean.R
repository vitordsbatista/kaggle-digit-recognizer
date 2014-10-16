line.mean <- function(data){
  #Remove o label
	#data <- data[,-1]
  #Cria o dataframe com 28 linhas
  #c <- data.frame(1:28)
  l <- data.frame(1:28)
  #Verifica cada linha
	for (i in c(1:(length(row(data))/785))){
    #Transforma cada linha em uma matrz 28x28
		q <- matrix(as.numeric(data[i,]), 28, 28)
    #Calcula as medias das colunas de cada linha e concatena
    #c <- cbind(c, colMeans(q))
    l <- cbind(l, rowMeans(q))
	}
	return(matrix(l))
}