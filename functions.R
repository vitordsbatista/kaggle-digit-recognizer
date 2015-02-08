data.crmean2 <- function(data){
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
data.crmean <- function(data){
  label <- data[,1]
  data <- data[,-1]
  data2 <- data.frame
  data2 <- apply(data, 1, img.crmean)
  return (data.frame(label, t(data2)))
}
img.crmean <- function(img){
  #Transforma cada linha em uma matrz 18x18
  q <- matrix(as.numeric(img), 18, 18)
  #Calcula as medias das colunas de cada linha
  c <- colMeans(q)
  r <- rowMeans(q)
  t <- data.frame(c, r)
  return (t(t))
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

data.4mean <- function(data){
  #Divide a imagem em 4 partes e calcula as medias de cada parte
  #remove o label do dataset
  label <- data[,1]
  data <- data[,-1]
  data2 <- data.frame
  data2 <- apply(data, 1, img.4mean)
  return(data.frame(label, t(data2)))
}
img.4mean <- function(img){
  n <- 18
  q <- matrix(as.numeric(img), n, n)
  #calcula a media de cada parte
  q1 <- mean(as.numeric(q[1:n/2, 1:n/2]))
  q2 <- mean(as.numeric(q[1:n/2, (n/2+1):n]))
  q3 <- mean(as.numeric(q[(n/2+1):n, 1:n/2]))
  q4 <- mean(as.numeric(q[(n/2+1):n, (n/2+1):n]))
  
  s1 <- sd(as.numeric(q[1:n/2, 1:n/2]))
  s2 <- sd(as.numeric(q[1:n/2, (n/2+1):n]))
  s3 <- sd(as.numeric(q[(n/2+1):n, 1:n/2]))
  s4 <- sd(as.numeric(q[(n/2+1):n, (n/2+1):n])) 
  
  return (c(q1, q2, q3, q4, s1, s2, s3, s4))
  
}

data.ratio2 <- function(data){
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

data.ratio <- function(data){
  #Remove as linhas e colunas nulas e retira o ratio
  #remove o label do dataset
  label <- data[,1]
  data <- data[,-1]
  data2 <- data.frame
  data2 <- apply(data, 1, img.ratio)
  return (data.frame(label, data2))
}
img.ratio <- function(img){
  n <- 28
  q <- matrix(as.numeric(img), n, n)
  q <- q[-which(rowSums(q) == 0), -which(colSums(q) == 0)]
  ratio <- as.numeric(nrow(q)/ncol(q))
  return (ratio)
}

data.rmblank2 <- function(data){
  #Remove linhas e colunas em branco e dá um resize na imagem (de 28x28 a imagem passou para 18x18)
  label <- data[,1]
  data <- data[,-1]
  n <- sqrt(length(data[1,]))
  data2 <- matrix(ncol=324)
  #data2 <- matrix(ncol=784)
  for (z in c(1:nrow(data))){
    q <- matrix(as.numeric(data[z,]), n, n)
    q <- q[-which(rowSums(q) < 50), -which(colSums(q) < 50)]
    q <- resize(q, 18, 18)
    #q <- resize(q, 28, 28)
    q <- as.numeric(imageData(q))
    q <- round(q)
    data2 <- rbind(data2, q)
    #data[z,] <- as.numeric(q)
  }
  return(data.frame(label, data2[-1,]))
}
data.rmblank <- function(data){
  #Remove linhas e colunas em branco e dá um resize na imagem (de 28x28 a imagem passou para 18x18)
  
  label <- data[,1]
  data <- data[,-1]
  #data2 <- matrix(ncol=324)
  data2 <- data.frame
  data2 <- apply(data, 1, img.rmblank)
  data2 <- t(data2)
  return(data.frame(label, data2))
}
img.rmblank <- function(img){
  #n <- length(data[1,])
  #print (data)
  q <- matrix(as.numeric(img), 28, 28)
  q <- q[-which(rowSums(q) < 50), -which(colSums(q) < 50)]
  q <- resize(q, 18, 18)
  q <- as.numeric(imageData(q))
  q <- round(q)
  return(q)
}

data.196mean <- function(data){
  #Remove o label
  label <- data[,1]
  data <- data[,-1]
  data2 <- data.frame
  data2 <- apply(data, 1, img.196mean)
  data2 <- t(data2)
 
  return (data.frame(label, data2))
}
img.196mean <- function(img){
  n <- 28
  q <- matrix(as.numeric(img), n, n)
  r <- matrix(0, ncol=n/2, nrow=n/2)
  for (i in seq(1, n, by=2)){
    for (j in seq(1, n, by=2)){
      r[i/2 + 0.5,j/2 + 0.5] <- mean(q[i,j], q[i+1, j], q[i, j=1], q[i+1, j+1])
    }
  }
  return (r)
  
  
}
data.196mean2 <- function(data){
  #Não remove o label pq não tem
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
  return (data.frame(data2[-1,]))
}

data.median2 <- function(data){
  n <- sqrt(length(data[1,]))
  label <- data[,1]
  data <- data[,-1]
  for (z in c(1:nrow(data))){
    #data[z,] <- sobel(data[z,])-
    q <- matrix(data[z,]/255, n, n)
    q <- medianFilter(q, 2)
    data[z,] <- as.numeric(q*255)
  }
  
  return(data.frame(label, data))
}
data.gauss <- function(data){
  n <- sqrt(length(data[1,]))
  label <- data[,1]
  data <- data[,-1]
  data <- apply(data, 1, img.gauss)
  data <- t(data)
  colnames(data) <- c(paste("pixel", 0:783, sep=''))
  return(data.frame(label, data))
}
img.gauss <- function(img){
  q <- matrix(as.numeric(img)/255, 28, 28)
  q <- as.numeric(gblur(q, sigma=0.5) * 255)
  q <- round(q)
  return(q)
}

data.threshold <- function(data){
  n <- sqrt(length(data[1,]))
  label <- data[,1]
  data <- data[,-1]
  data <- apply(data, 2, img.threshold)
  return(data.frame(label, data))
}
img.threshold <- function(img){
  #data[which(data<=100)] <- 0
  #data[which(data>100)] <- 1
  
  return (img>otsu(img))
}

divisors <- function(x){
  #  Vector of numberes to test against
  y <- seq_len(x)
  #  Modulo division. If remainder is 0 that number is a divisor of x so return it
  y[ x%%y == 0 ]
}

data.area <- function(data){
  label <- data[,1]
  data <- data[,-1]
  data2 <- data.frame
  data2 <- apply(data, 1, img.area)
  return (data.frame(label, data2))  
}
img.area <- function(img){
  area <- length(which(img == 1))
  return (area)
}

data.thin <- function(data){
  label <- data[,1]
  data <- data[,-1]
  data2 <- data.frame
  data2 <- apply(data, 1, img.thin)
  return (data.frame(label, t(data2)))  
}
img.thin <- function(img){
  q <- matrix(as.numeric(img), 18, 18)
  q <- thinImage(q)
  return (q)
}

data.peri <- function(data){
  label <- data[,1]
  data <- data[,-1]
  data2 <- data.frame
  data2 <- apply(data, 1, img.peri)
  return (data.frame(label, data2))  
}

img.peri <- function(img){
  q <- matrix(as.numeric(img), 18, 18)
  k <- matrix(c(0,1,0,1,1,1,0,1,0),3,3)
  q <- q-erode(q,k)
  peri <- length(which(q == 1))
  return (peri)
}
#=======================-------------------------===========================

absDiff <- function(matrix1,matrix2){
  r <- nrow(matrix1)
  c <- ncol(matrix1)
  destMatrix <- matrix1
  for(r in 0:r-1)
  {
    for(c in 0:c-1)
    {
      destMatrix[r,c] <- abs(matrix1[r,c]-matrix1[r,c])
    }
  }
  return(destMatrix)
}

countNonZero <- function(inputMatrix){
  return(length(inputMatrix[inputMatrix > 0]))
}

thinningIteration <- function(imageMatrix, iter){
  imageInput <- imageMatrix
  r <- nrow(imageInput) - 1
  c <- ncol(imageInput) - 1
  for(i in 2:r)
  {
    for(j in 2:c)
    {
      p2 <- imageInput[i-1, j]
      p3 <- imageInput[i-1, j+1]
      p4 <- imageInput[i, j+1]
      p5 <- imageInput[i+1, j+1]
      p6 <- imageInput[i+1, j]
      p7 <- imageInput[i+1, j-1]
      p8 <- imageInput[i, j-1]
      p9 <- imageInput[i-1, j-1]
      A  <- (p2 == 0 && p3 == 1) + (p3 == 0 && p4 == 1) + 
        (p4 == 0 && p5 == 1) + (p5 == 0 && p6 == 1) + 
        (p6 == 0 && p7 == 1) + (p7 == 0 && p8 == 1) +
        (p8 == 0 && p9 == 1) + (p9 == 0 && p2 == 1)
      B  <- p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9
      if(iter == 0){
        m1 <- (p2 * p4 * p6)
        m2 <- (p4 * p6 * p8)
      }
      else {
        m1 <- (p2 * p4 * p8)
        m2 <- (p2 * p6 * p8)
      }
      if (A == 1 && (B >= 2 && B <= 6) && m1 == 0 && m2 == 0)
      {
        imageInput[i,j] <- 0
      }
    }
  }
  return(imageInput)
}

thinImage <- function(imageMatrix){
  library(seriation)
  im <- imageMatrix
  prev <- im
  repeat {
    im <- thinningIteration(im, 0)
    im <- thinningIteration(im, 1)
    diff <- absDiff(im, prev)
    prev <- im
    if(countNonZero(diff) <= 0)
    {
      break
    }
  } 
  
  return(im)
}


#======================++++++++++++++======================+++++++++++++===========

img.rolate <- function(img){
  q <- matrix(as.numeric(img), 28, 28)
  dim2 <- 0
  angle <- 0
  #_-----____________----______---___---____---___---__
  #rmblank
  q <- q[-which(rowSums(q) < 50), -which(colSums(q) < 50)]
  #dimensão da img original
  dim1 <- sum(dim(q))
  while (TRUE){
    
    
    
  }
  
  
  
  
  
}