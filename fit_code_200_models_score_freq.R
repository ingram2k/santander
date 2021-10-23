


store<-rep(1,nrow(raw_data))

variable<-names(test_data_freq)

variable<-variable[1:200]

for(this.round in 1:length(variable)) {

modelvar<-readRDS(paste0("C:/Users/phan3/OneDrive/Desktop/data/santander/200_models/","model_",variable[this.round],".RDS"))

fit_data<-test_data_freq %>% select (variable[this.round],paste0(variable[this.round],"_freq"))

train_pred_var<-data.frame(pred=predict(modelvar,xgb.DMatrix(as.matrix(fit_data))))

store<-store*train_pred_var*10
}

pred200<-store 

submit<-data.frame(ID_code = test_data_freq$ID_code,target = pred200$pred,sort=as.numeric(substr(submit$ID_code,6,700)))



sort=as.numeric(substr(submit$ID_code,6,700))

write.csv(submit,"C:/Users/phan3/OneDrive/Desktop/data/santander/test_pred_200_freq.csv")


single_lift(target,pred200$pred,200)