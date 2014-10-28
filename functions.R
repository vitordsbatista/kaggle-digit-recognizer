cr.mean <- function(data){
  c <- matrix(ncol=28)
  r <-matrix(ncol=28)
  label <- data[,1]
  data <- data[,-1]
  for (i in c(1:nrow(data))){
    #Transforma cada linha em uma matrz 28x28
    q <- matrix(as.numeric(data[i,]), 28, 28)
    #Calcula as medias das colunas de cada linha e concatena
    a <- colMeans(q)
    b <- rowMeans(q)
    
    a <- t(a)
    b <- t(b)
    
    c <- rbind(c, a)
    r <- rbind(r, b)
    #c[i,1:28] <- a[1:28]
    #l <- cbind(l, rowMeans(q))
  }
  d <- data.frame(label, c[-1,], r[-1,])
  return (d)
}