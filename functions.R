data.crmean <- function(data){
  n <- sqrt(length(data[1,]))
  c <- matrix(ncol=n)
  r <- matrix(ncol=n)
  label <- data[,1]
  data <- data[,-1]
  for (i in c(1:nrow(data))){
    #Transforma cada linha em uma matrz 28x28
    q <- matrix(as.numeric(data[i,]), n, n)
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

data.sobel <- function(data){
  #Aplicação do filtro de sobel para um dataset com cada linha uma matrix
  n <- sqrt(length(data[1,]))
  #remove o label do dataset
  label <- data[,1]
  data <- data[,-1]
  s1 <- matrix(c(1,2,1, 0,0,0, -1,-2,-1), 3, 3)
  s2 <- matrix(c(1,0,-1, 2,0,-2, 1,0,-1), 3, 3)
  for (z in c(1:nrow(data))){
    #data[z,] <- sobel(data[z,])
    q <- matrix(as.numeric(data[z,]), n, n)
    a1 <- filter2(q, s1)
    a2 <- filter2(q, s2)
    q <- round(sqrt(a1^2 + a2^2))
    q[which(q>255)] <- 255
    data[z,] <- as.numeric(q)
  }
  return(data.frame(label, data))
}

data.quarters.mean <- function(data){
  #Divide a imagem em 4 partes e calcula as medias de cada parte
  #remove o label do dataset
  label <- data[,1]
  data <- data[,-1]
  means <- matrix(ncol = 4)
  n <- sqrt(length(data[1,]))
  #varre os dados
  for (z in c(1:nrow(data))){
    #transforma a imagem numa matrix 28x28
    q <- matrix(as.numeric(data[z,]), n, n)
    #calcula a media de cada parte
    q1 <- mean(as.numeric(q[1:n/2, 1:n/2]))
    q2 <- mean(as.numeric(q[1:n/2, (n/2+1):n]))
    q3 <- mean(as.numeric(q[(n/2+1):n, 1:n/2]))
    q4 <- mean(as.numeric(q[(n/2+1):n, (n/2+1):n]))
    
    means <- rbind(means, c(q1, q2, q3, q4))
  }
  return(data.frame(label, means[-1,]))
}

data.ratio <- function(data){
  #Remove as linhas e colunas nulas e retira o ratio
  #remove o label do dataset
  label <- data[,1]
  data <- data[,-1]
  n <- sqrt(length(data[1,]))
  ratio <- c()
  for (z in c(1:nrow(data))){
    q <- matrix(as.numeric(data[z,]), n, n)
    q <- q[-which(rowSums(q) == 0), -which(colSums(q) == 0)]
    print ( nrow(q)/ncol(q))
    ratio[z] <- as.numeric(nrow(q)/ncol(q))
  }
  return (ratio)
}

data.rmblank <- function(data){
  #Remove linhas e colunas em branco e dá um resize na imagem (de 28x28 a imagem passou para 18x18)
  label <- data[,1]
  data <- data[,-1]
  n <- sqrt(length(data[1,]))
  data2 <- matrix(ncol=324)
  for (z in c(1:nrow(data))){
    q <- matrix(as.numeric(data[z,]), n, n)
    q <- q[-which(rowSums(q) == 0), -which(colSums(q) == 0)]
    q <- resize(q, 18, 18)
    q <- as.numeric(imageData(q))
    q <- round(q)
    data2 <- rbind(data2, q)
    #data[z,] <- as.numeric(q)
  }
  return(data.frame(label, data2[-1,]))
}

data.4mean <- function(data){
  #Remove o label
  label <- data[,1]
  data <- data[,-1]
  n <- sqrt(length(data[1,]))
  data2 <- matrix(ncol=(n/2)^2)
  data3 <- matrix(0, ncol=n/2, nrow=n/2)
  for (z in c(1:nrow(data))){
    q <- matrix(as.numeric(data[z,]), n, n)
    for (i in seq(1, n, by=2)){
      for (j in seq(1, n, by=2)){
        data3[i/2 + 0.5,j/2 + 0.5] <- mean(q[i,j], q[i+1, j], q[i, j=1], q[i+1, j+1])
      }
    }
  data2 <- rbind(data2, as.numeric(data3))
  }
  return (data.frame(label, data2[-1,]))
}

data.gauss <- function(data){
  n <- sqrt(length(data[1,]))
  label <- data[,1]
  data <- data[,-1]
  for (z in c(1:nrow(data))){
    #data[z,] <- sobel(data[z,])-
    q <- matrix(as.numeric(data[z,]), n, n)
    q <- gblur(q, sigma=1)
    data[z,] <- as.numeric(q)
  }
  
  return(data.frame(label, data))
}

data.threshold <- function(data){
  n <- sqrt(length(data[1,]))
  label <- data[,1]
  data <- data[,-1]
  data <- apply(data, 2, data.threshold2)
  return(data.frame(label, data))
}

data.threshold2 <- function(data){
  data[which(data<=127)] <- 0
  data[which(data>127)] <- 255
  return (data)
}