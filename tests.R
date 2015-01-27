rf <- randomForest(label~., data=tr1)
pred <- predict(rf, te1)
p.acertos[1] <- sum(pred == te1$label)/length(pred)
p.acertos[1] * 100

rf <- randomForest(label~., data=tr2)
pred <- predict(rf, te2)
p.acertos[2] <- sum(pred == te2$label)/length(pred)
p.acertos[2] * 100

rf <- randomForest(label~., data=tr3)
pred <- predict(rf, te3)
p.acertos[3] <- sum(pred == te3$label)/length(pred)
p.acertos[3] * 100

rf <- randomForest(label~., data=tr4)
pred <- predict(rf, te4)
p.acertos[4] <- sum(pred == te4$label)/length(pred)
p.acertos[4] * 100

#original
tr5 <- data.quarters.mean(tr1)
te5 <- data.quarters.mean(te1)
rf <- randomForest(label~., data=tr5)
pred <- predict(rf, te5)
p.acertos[11] <- sum(pred == te5$label)/length(pred)
p.acertos[11] * 100
#t6
tr5.1 <- data.quarters.mean(tr6)
te5.1 <- data.quarters.mean(te6)
rf <- randomForest(label~., data=tr5.1)
pred <- predict(rf, te5.1)
p.acertos[12] <- sum(pred == te5.1$label)/length(pred)
p.acertos[12] * 100
#t9
tr5.2 <- data.quarters.mean(tr9)
te5.2 <- data.quarters.mean(te9)
rf <- randomForest(label~., data=tr5.2)
pred <- predict(rf, te5.2)
p.acertos[13] <- sum(pred == te5.2$label)/length(pred)
p.acertos[13] * 100
#t10 - area
tr10 <- data.area(tr1)
te10 <- data.area(te1)



rf <- randomForest(label~., data=tr6)
pred <- predict(rf, te6)
p.acertos[6] <- sum(pred == te6$label)/length(pred)
p.acertos[6] * 100

rf <- randomForest(label~., data=tr7)
pred <- predict(rf, te7)
p.acertos[7] <- sum(pred == te7$label)/length(pred)
p.acertos[7] * 100

rf <- randomForest(label~., data=tr9)
pred <- predict(rf, te9)
p.acertos[8] <- sum(pred == te9$label)/length(pred)
p.acertos[8] * 100

rf <- randomForest(label~., data=tr689)
pred <- predict(rf, te689)
p.acertos[9] <- sum(pred == te689$label)/length(pred)
p.acertos[9] * 100


train1.1 <- data.quarters.mean(train)
test1.1 <- data.quarters.mean(test)

train1.2 <- data.rmblank(train)
test1.2 <- data.rmblank(test)

train1.r <- data.frame(train1.1, train1.2[,-1], tr10, tr8)
test1.r <- data.frame(test1.1, test1.2[,-1], tr10, tr8)

colnames(train1.r) <- c("label", paste("c", 1:4, sep=''), paste("r", 1:324, sep=''))
colnames(test1.r) <- c("label", paste("c", 1:4, sep=''), paste("r", 1:324, sep=''))

rf <- randomForest(label~., train1.r)
pred <- predict(rf, test1.r)
p.acertos[10] <- sum(pred == test1.r$label)/length(pred)
p.acertos[10] * 100

train.tmp <- data.frame(tr6, tr9[,-1], tr10)
test.tmp <- data.frame(te6, te9[,-1], te10)

colnames(train.tmp) <- c("label", paste("c", 1:196, sep=''), paste("r", 1:324, sep=''), "area")
colnames(test.tmp) <- c("label", paste("c", 1:196, sep=''), paste("r", 1:324, sep=''), "area")

rf <- randomForest(label~., train.tmp)
pred <- predict(rf, test.tmp)
p.acertos[101] <- sum(pred == test.tmp$label)/length(pred)
p.acertos[101] * 100


#__________________--------------------____________________-----------------___________________--
#Teste para publicação
train.p2 <- data.rmblank(d)
test.p2 <- data.rmblank2(t)

train.p1 <- data.4mean(d)
test.p1 <- data.4mean2(t)

train.p <- data.frame(train.p1, train.p2[,-1])
test.p <- data.frame(test.p1, test.p2)

colnames(train.p) <- c("label", paste("c", 1:196, sep=''), paste("r", 1:324, sep=''))
colnames(test.p) <- c(paste("c", 1:196, sep=''), paste("r", 1:324, sep=''))

labels <- train.p[,1]
train.p <- train.p[,-1]

rf <- randomForest(train.p, labels, xtest=test.p, ntree=1000)
Label <- levels(labels)[rf$test$predicted]
ImageId <- c(1:28000)
out <- data.frame(cbind(ImageId, Label))
write.csv(out,file='rf_vitor.csv', quote=FALSE, row.names=FALSE)


