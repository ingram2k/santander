
test_data<-fread("C:/Users/phan3/OneDrive/Desktop/data/santander/test.csv")

store<-rep(1,nrow(raw_data))

variable<-names(test_data)

variable<-variable[2:201]

variable

for(this.round in 1:length(variable)) {

modelvar<-readRDS(paste0("C:/Users/phan3/OneDrive/Desktop/data/santander/200_models/","model_",variable[this.round],".RDS"))

fit_data<-test_data_freq %>% select (variable[this.round])

train_pred_var<-data.frame(pred=predict(modelvar,xgb.DMatrix(as.matrix(fit_data))))

store<-store*train_pred_var*10
}

pred200<-store 

submit<-data.frame(ID_code = test_data_freq$ID_code,target = pred200$pred)


write.csv(submit,"C:/Users/phan3/OneDrive/Desktop/data/santander/test_pred_200_0.05eta_20mcw.csv")


single_lift(target,pred200$pred,200)


modelvar<-readRDS(paste0("C:/Users/phan3/OneDrive/Desktop/data/santander/200_models/","model_var_1",".RDS"))