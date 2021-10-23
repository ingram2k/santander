
fit_data <- raw_data 

pre.train <- fit_data %>% dplyr::sample_frac(.85)

pre.test  <- dplyr::anti_join(fit_data, pre.train, by = 'ID_code')

mean_train<-mean(pre.train$target)

train<- pre.train %>% select(-ID_code,-target)
test<- pre.test %>% select(-ID_code,-target)

train.mat<-xgb.DMatrix(as.matrix(train), label = pre.train$target)
test.mat<-xgb.DMatrix(as.matrix(test), label = pre.test$target)

model_fit<-xgb.train(data=train.mat
                 ,objective = "binary:logistic"
                 ,base_score = mean_train
                 ,eval_metric = "auc"
                 ,verbose = 1
                 ,eta = 0.01
                 ,nround= 100000
                 ,min_child_weight = 30
                 ,max_depth = 7
                 ,colsample_bytree = 0.5
                 ,subsample = 0.5
                 ,early_stopping_rounds = 3000
                 ,watchlist = list(train=train.mat, test=test.mat))

saveRDS(model_fit,paste0("C:/Users/phan3/OneDrive/Desktop/data/santander/","model_fit_full",".RDS"))

# model_fit_1<-xgb.train(data=train.mat
#                      ,objective = "binary:logistic"
#                      ,base_score = mean_train
#                      ,eval_metric = "logloss"
#                      ,verbose = 1
#                      ,eta = 0.05
#                      ,nround= 100000
#                      ,min_child_weight = 110
#                      ,max_depth = 5
#                      ,colsample_bytree = 0.5
#                      ,subsample = 0.5
#                      ,early_stopping_rounds = 100
#                      ,watchlist = list(train=train.mat, test=test.mat)
#                      ,model=model_fit
#                      )
# 
# saveRDS(model_fit_1,paste0("C:/Users/phan3/OneDrive/Desktop/data/santander/","model_fit_1",".RDS"))
# 
