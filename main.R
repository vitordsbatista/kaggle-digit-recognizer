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
