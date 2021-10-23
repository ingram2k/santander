library(ggplot2)

library(xgboost)

library(dplyr)

library(data.table)

library(caret)

library(tidyr)

library(corrplot)
raw_data<-fread("C:/Users/bruce/Desktop/data/santander/train.csv")

set.seed(123)

pre.train <- raw_data %>% dplyr::sample_frac(.75)

pre.test  <- dplyr::anti_join(raw_data, pre.train, by = 'ID_code')

mean_train<-mean(pre.train$target)

train<- pre.train %>% select(-ID_code,-target)
test<- pre.test %>% select(-ID_code,-target)

train.mat<-xgb.DMatrix(as.matrix(train), label = pre.train$target)
test.mat<-xgb.DMatrix(as.matrix(test), label = pre.test$target)

start<-Sys.time()
model<-xgb.train(data=train.mat
                 ,objective = "binary:logistic"
                 ,base_score = mean_train
                 ,eval_metric = "auc"
                 ,verbose = 1
                 ,eta = 0.1
                 ,nround= 800
                 ,min_child_weight = 50
                 ,max_depth = 6
                 ,colsample_bytree = 0.8
                 ,subsample = 0.8
                 ,early_stopping_rounds = 15
                 ,watchlist = list(train=train.mat, test=test.mat)
                 )
end<-Sys.time()
var.imp.1<-xgb.importance(model = model)
write.csv(var.imp.1,"C:/Users/phan3/OneDrive/Desktop/data/santander/var.imp.1.csv")
end-start

######################################
set.seed(1231)

pre.train <- raw_data %>% dplyr::sample_frac(.75)

pre.test  <- dplyr::anti_join(raw_data, pre.train, by = 'ID_code')

mean_train<-mean(pre.train$target)

train<- pre.train %>% select(-ID_code,-target)
test<- pre.test %>% select(-ID_code,-target)

train.mat<-xgb.DMatrix(as.matrix(train), label = pre.train$target)
test.mat<-xgb.DMatrix(as.matrix(test), label = pre.test$target)

start<-Sys.time()
model<-xgb.train(data=train.mat
                 ,objective = "binary:logistic"
                 ,base_score = mean_train
                 ,eval_metric = "auc"
                 ,verbose = 1
                 ,eta = 0.1
                 ,nround= 800
                 ,min_child_weight = 50
                 ,max_depth = 6
                 ,colsample_bytree = 0.8
                 ,subsample = 0.8
                 ,early_stopping_rounds = 15
                 ,watchlist = list(train=train.mat, test=test.mat)
)
end<-Sys.time()
var.imp.2<-xgb.importance(model = model)
write.csv(var.imp.2,"C:/Users/phan3/OneDrive/Desktop/data/santander/var.imp.2.csv")
end-start


######################################
set.seed(2231)

pre.train <- raw_data %>% dplyr::sample_frac(.75)

pre.test  <- dplyr::anti_join(raw_data, pre.train, by = 'ID_code')

mean_train<-mean(pre.train$target)

train<- pre.train %>% select(-ID_code,-target)
test<- pre.test %>% select(-ID_code,-target)

train.mat<-xgb.DMatrix(as.matrix(train), label = pre.train$target)
test.mat<-xgb.DMatrix(as.matrix(test), label = pre.test$target)

start<-Sys.time()
model<-xgb.train(data=train.mat
                 ,objective = "binary:logistic"
                 ,base_score = mean_train
                 ,eval_metric = "auc"
                 ,verbose = 1
                 ,eta = 0.1
                 ,nround= 800
                 ,min_child_weight = 50
                 ,max_depth = 6
                 ,colsample_bytree = 0.8
                 ,subsample = 0.8
                 ,early_stopping_rounds = 15
                 ,watchlist = list(train=train.mat, test=test.mat)
)
end<-Sys.time()
var.imp.3<-xgb.importance(model = model)
write.csv(var.imp.3,"C:/Users/phan3/OneDrive/Desktop/data/santander/var.imp.3.csv")
end-start


