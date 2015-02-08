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
n1 <- nnet(label~.,data = data2, size = 20, rang = 0.5, decay = 5e-4, maxit = 10000, MaxNWts = 2000)
#teste
pred <- predict(n1, test, type="class")
#n.acertos - numero de acertos da rede
#p.acertos - porcentagem de acertos da rede
n.acertos <- length(which(pred == real))
p.acertos <- 100*n.acertos/length(real)

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

colnames(train1)[119] <- "area"
colnames(test1)[119] <- "area"

#95.27179

test1 <- data.frame(test.threshold, test.196mean[,-1])
train1 <- data.frame(train.threshold, train.196mean[,-1])



test1 <- data.frame(test.rmblank, test.196mean2[,-1])
train1 <- data.frame(train.rmblank, train.196mean2[,-1])
