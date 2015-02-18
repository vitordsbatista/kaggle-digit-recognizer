# load csv
d <- read.csv("data/train.csv", header=T)
library(caTools)
# split the dataset, 65% train 35% test
split <- sample.split(d$label, SplitRatio=.65)
# transform the target variable to factor
d$label <- as.factor(d$label)
# split the dataset
train <- subset(d, split==T)
test <- subset(d, split==F)

# check label's distribution in the new datasets
table(train$label)
#0    1    2    3    4    5    6    7    8    9 
#2686 3045 2715 2828 2647 2467 2689 2861 2641 2722 
table(test$label)
#0    1    2    3    4    5    6    7    8    9 
#1446 1639 1462 1523 1425 1328 1448 1540 1422 1466 

# saving the files
save(train, file="data/train.RData")
save(test, file="data/test.RData")


# train a randomForest
library(randomForest)
rf <- randomForest(label ~ ., data=train)
# saving the model...
save(rf, file="data/rf.RData")
pred <- predict(rf, test)

p.acertos <- sum(pred == test$label)/length(pred)



#renomeia as colunas (label, c(1:18), r(1:18), q(1:18), ratio)
#colnames(train.r) <- c("label", paste("c", 1:18, sep=''), paste("r", 1:18, sep=''), paste("q", 1:4, sep=''), "ratio")
#colnames(test.r) <- c("label", paste("c", 1:18, sep=''), paste("r", 1:18, sep=''), paste("q", 1:4, sep=''), "ratio")




#Rede Neural
#abertura e organização do arquivo
data <- read.csv("train.csv")
#o train possui 500 digitos para testes
train <- data[1:500,]
label <- train[,1]
train <- train[,-1]
rm <- line.mean(train)
cm <- col.mean(train)
data2 <- data.frame(label, cm, rm)
#o test possui 15001 digitos
test <- data[,501:2001]
test <- data[501:2001,]
#o real contem os valores corretos do test
real <- test[,1]
test <- test[,-1]
#treinamento da rede
n1 <- nnet(label~.,data = tr, size = 20, rang = 0.5, decay = 5e-4, maxit = 10000, MaxNWts = 2000)
pred <- predict(n1, test, type="class")
n1 <- nnet(label~.,data = data2, size = 20, rang = 0.5, decay = 5e-4, maxit = 10000, MaxNWts = 2000)
#teste
pred <- predict(n1, test, type="class")
#n.acertos - numero de acertos da rede
#p.acertos - porcentagem de acertos da rede
n.acertos <- length(which(pred == real))
p.acertos <- 100*n.acertos/length(real)

n1 <- nnet(label~.,data = tr, size = 100, maxit = 10, MaxNWts = 54000)
pred <- predict(n1, te[,-1], type="class")
n.acertos <- length(which(pred == te[,1]))
p.acertos <- 100*n.acertos/length(te[,1])
p.acertos

#2 teste com rede neural (resultados entre 0 e 1) - 88.42533% de acertos
n1 <- nnet(d2[,1]~.,data = d2, size = 10, rang = 0.5, decay = 0.02, maxit = 10000, MaxNWts = 2000)
#3 teste com randomForest e as médias das linhas e colunas - 90.95177% de acertos
  #mesmo esquema realizado acima, contudo a randomForest foi treinada com as médias das linhas e colunas

#4 teste com randomForest com o filtro sobel e como entrada a media das linhas e colunas, medias dos quadrosb e ratio (altura/largura) conseguiu 89.50949%
  #Provavelmente ocorreu este resultado por causa do filtro sobel, que dilatou as linhas
  #talvez o filtro de sobel não é muito util para imagens pequenas pq ele aumenta a image
  #o proximo teste é realizar todos os passos sem o filtro sobel para comprovar a teoria
#5 teste com randomForest sem o filtro sobel e como entrada a media das linhas e colunas, medias dos quadros e ratio (altura/largura) conseguiu 92.7641%,
  #confirmando a teoria acima
#6 teste com randomForest e dados com linhas e colunas removidas e redimensionado para 18x18 e média dos quadrantes conseguiu 96.80931%


#Pre-processamento
#--Filtro gaussian
test.gauss <- data.gauss(test)
train.gauss <- data.gauss(train)
#--Remoção de linhas e colunas em branco (0)
test.rmblank <- data.rmblank(test.gauss)
train.rmblank <- data.rmblank(train.gauss)
#--Transformação em imagem binária
test.threshold <- data.threshold(test.rmblank)
train.threshold <- data.threshold(train.rmblank)

#Caracteristicas
#--Média das linhas e colunas
test.crmean <- data.crmean(test.threshold)
train.crmean <- data.crmean(train.threshold)
#--Divide a imagem em 4 partes e tira a média e o desvio padrão de cada parte
test.4mean <- data.4mean(test.threshold)
train.4mean <- data.4mean(train.threshold)
#--Agrupa de 4 em 4 e tira a media e o desvio padrão
test.196mean <- data.196mean(test.threshold)
train.196mean <- data.196mean(train.threshold)
#--Area da imagem
test.area <- data.area(test.threshold)
train.area <- data.area(train.threshold)
#--Perimetro
test.peri <- data.peri(test.threshold)
train.peri <- data.peri(train.threshold)
#--Ratio
test.ratio <- data.ratio(test)
train.ratio <- data.ratio(train)
#--Hough
test.hough <- data.hough(test.threshold)
train.hough <- data.hough(train.threshold)

test1 <- data.frame(test.196mean, test.4mean[,-1], test.crmean[,-1],  test.area[,-1],test.peri[,-1])
train1 <- data.frame(train.196mean, train.4mean[,-1], train.crmean[,-1], train.area[,-1], train.peri[,-1])

colnames(train1)[128] <- "peri"
colnames(test1)[128] <- "peri"

colnames(train1)[127] <- "area"
colnames(test1)[127] <- "area"

rf <- randomForest(label~., data=train1)
pred <- predict(rf, test1)
p.acertos <- sum(pred == test1$label)/length(pred)
p.acertos * 100

labels <- as.factor(train1[,1])
train1.var <- train1[,-1]
rf <- randomForest(train1.var, labels, xtest = test1[,-1], ntree = 1000)



#94.93843

#TESTES:96.40792

test1 <- data.frame(test.196mean)
train1 <- data.frame(train.196mean)

test1 <- data.frame(test.4mean)
train1 <- data.frame(train.4mean)

test1 <- data.frame(test.crmean, test.196mean[,-1])
train1 <- data.frame(train.crmean, train.196mean[,-1])

test1 <- data.frame(test.crmean, test.196mean[,-1], test.peri[,-1])
train1 <- data.frame(train.crmean, train.196mean[,-1], train.peri[,-1])

colnames(train1)[119] <- "peri"
colnames(test1)[119] <- "peri"

test1 <- data.frame(test.crmean, test.196mean[,-1], test.area[,-1])
train1 <- data.frame(train.crmean, train.196mean[,-1], train.area[,-1])

colnames(train1)[522] <- "area"
colnames(test1)[522] <- "area"

#95.27179

test1 <- data.frame(test.threshold, test.196mean[,-1])
train1 <- data.frame(train.threshold, train.196mean[,-1])



test1 <- data.frame(test.rmblank, test.196mean2[,-1], test.hough[,-1])
train1 <- data.frame(train.rmblank, train.196mean2[,-1], train.hough[,-1])

test1 <- data.frame(test.rmblank, test.196mean2[,-1], test.area[,-1])
train1 <- data.frame(train.rmblank, train.196mean2[,-1], train.area[,-1])





rf <- randomForest(label~., data=tr3)
pred <- predict(rf, te3)
p.acertos <- sum(pred == te3$label)/length(pred)
p.acertos * 100



s1 <- svm(label~., data=tr3, cost = 2, gamma = 0.01)
pred <- predict(s1, te3, type = "class")
p.acertos <- sum(pred == te3$label)/length(pred)
p.acertos * 100

colnames(tr) <- c("label", paste("c", 1:520, sep=''))
colnames(te) <- c("label", paste("c", 1:520, sep=''))


detach("package:EBImage", unload=TRUE)
library("randomForest", lib.loc="~/R/i686-pc-linux-gnu-library/3.1")

detach("package:randomForest", unload=TRUE)
library("EBImage", lib.loc="~/R/i686-pc-linux-gnu-library/3.1")

tr <- data.frame(tr3, tr4[,-1]/255)
te <- data.frame(te3, te4[,-1]/255)

tr1 <- data.gauss(tr)
te1 <- data.gauss(te)

tr2 <- data.threshold(tr1)
te2 <- data.threshold(te1)

tr3 <- data.thin(tr2)
te3 <- data.thin(te2)

tr4 <- data.hough(tr3)
te4 <- data.hough(tr3)
#Separar os numeros
n1 <- tr4[,which(tr4[1,] == 1)]
n2 <- tr4[,which(tr4[1,] == 2)]
n3 <- tr4[,which(tr4[1,] == 3)]
n4 <- tr4[,which(tr4[1,] == 4)]
n5 <- tr4[,which(tr4[1,] == 5)]
n6 <- tr4[,which(tr4[1,] == 6)]
n7 <- tr4[,which(tr4[1,] == 7)]
n8 <- tr4[,which(tr4[1,] == 8)]
n9 <- tr4[,which(tr4[1,] == 9)]
n0 <- tr4[,which(tr4[1,] == 0)]
#maximo, minimo e media das retas de cada numero na transformata de hough
#--Acha as retas com o valor maximo
nmax1 <- apply(n1, 2, max)
nmax2 <- apply(n2, 2, max)
nmax3 <- apply(n3, 2, max)
nmax4 <- apply(n4, 2, max)
nmax5 <- apply(n5, 2, max)
nmax0 <- apply(n0, 2, max)

#--Max das retas
max1 <- max(nmax1)
max2 <- max(nmax2)
max3 <- max(nmax3)
max4 <- max(nmax4)
max5 <- max(nmax5)
max0 <- max(nmax0)
#--Min das retas
min1 <- min(nmax1)
min2 <- min(nmax2)
min3 <- min(nmax3)
min4 <- min(nmax4)
min5 <- min(nmax5)
min0 <- min(nmax0)
#--Mean das retas
mean1 <- mean(as.numeric(nmax1))
mean2 <- mean(nmax2)
mean3 <- mean(nmax3)
mean4 <- mean(nmax4)
mean5 <- mean(nmax5)
mean0 <- mean(nmax0)

#tentar separar atrabés disso
#calcular os circulos e elipses na transformata
