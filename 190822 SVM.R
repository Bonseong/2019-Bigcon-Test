library(e1071)

train_dt<-as.data.frame(train)


train_new<-train_dt[1:1000,]

train_new

train_index <- sample(1:nrow(train_new), round(nrow(train_new)*0.7))
delayTrain <- train_new[train_index,]
delayTest <- train_new[-train_index,]

tobj <- tune.svm(DLY_onehot ~ ., data = delayTrain, 
                 cost = 10^(-3:3), gamma = 10^(-3:3))

tobj$best.parameters

svm.model <- svm(DLY_onehot ~ ., data = delayTrain, 
                 cost = tobj$best.parameters$cost, 
                 gamma = tobj$best.parameters$gamma)

dim(delayTest)[2]


pred <- predict(svm.model, delayTest[,-(dim(delayTest)[2])])
a<-table(pred, delayTest$DLY_onehot)

diag(a)
