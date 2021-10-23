
variable<-names(raw_data_freq)

variable<-variable[1:200]


set.seed(0.123)

for(this.round in 1:length(variable)) {



fit_data <- raw_data_freq %>% select(ID_code,target,variable[this.round],paste0(variable[this.round],"_freq"))


pre.train <- fit_data %>% dplyr::sample_frac(.9)

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
                 ,nround= 100
                 ,min_child_weight = 10
                 ,max_depth = 1
                 ,colsample_bytree = 0.5
                 ,subsample = 0.5
                 ,early_stopping_rounds = 100
                 ,watchlist = list(train=train.mat, test=test.mat))

saveRDS(model_fit,paste0("C:/Users/phan3/OneDrive/Desktop/data/santander/200_models/","model_",variable[this.round],".RDS"))

}



