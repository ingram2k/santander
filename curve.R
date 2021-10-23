fit_data <- raw_data %>% select(var_81,	var_139,	var_12,	var_53,	var_110,	var_146,	var_26,	var_6,
                                var_174,	var_76,	var_80,	var_22,	var_21,	var_99,	var_165,	var_166,	var_13,	var_109,
                                var_133,	var_2,	var_148,	var_198,	var_190,	var_78,	var_34,	var_0,	var_1,	var_40,
                                var_179,	var_44,	var_170,	var_94,	var_164,	var_108,	var_115,	var_33,	var_92,	
                                var_154,	var_191,	var_184,	var_149,	var_169,	var_9,	var_122,	var_123,	var_177,
                                var_18,	var_121,	var_192,	var_173,	var_67,	var_86,	var_75,	var_91,	var_147,
                                var_118,	var_127,	var_107,	var_89,	var_56,	var_95,	var_188,	var_155,	var_172,
                                var_36,	var_93,	var_197,	var_5,	var_180,	var_106,	var_87,	var_157,	var_35,	
                                var_119,	var_48,	var_71,	var_163,	var_186,	var_162,	var_141,	var_32,	var_130,
                                var_131,	var_145,	var_49,	var_167,	var_151,	var_90,	var_135,	var_125,	var_150,
                                var_195,	var_24,	var_128,	var_137,	var_104,	var_70,	var_43,	var_52,	var_51,
                                var_112,	var_199,	var_82,	var_58,	var_111,	var_31,	var_28,	var_132,	var_23,	
                                var_114,	var_196,	var_11,	var_83,	var_105,	var_85,	var_116,	var_175,	var_88,	
                                var_194,	var_20,	var_66,	var_156,	var_45,	var_55,	var_178,	var_102,	var_74,
                                var_144,	var_77,	var_97,	var_138,	var_193,	var_168,	var_134,	var_54,	var_142,
                                var_15,	var_62,	var_72,	var_171,	var_8,	var_187,	var_50,	var_140,
                                var_63,	var_181,	var_57,	var_159,	var_4,	var_64
)

library(pROC)

model<-readRDS("C:/Users/phan3/OneDrive/Desktop/data/santander/model_fit_7.RDS")

model_fit_0.001<-readRDS("C:/Users/phan3/OneDrive/Desktop/data/santander/model_fit_0.001.RDS")

model_fit_0.001v2<-readRDS("C:/Users/phan3/OneDrive/Desktop/data/santander/model_fit_0.001v2.RDS")

model$feature_names
model$params


model1<-readRDS("C:/Users/phan3/OneDrive/Desktop/data/santander/model_fit_cv_1.RDS")
model2<-readRDS("C:/Users/phan3/OneDrive/Desktop/data/santander/model_fit_cv_2.RDS")
model3<-readRDS("C:/Users/phan3/OneDrive/Desktop/data/santander/model_fit_cv_3.RDS")
model4<-readRDS("C:/Users/phan3/OneDrive/Desktop/data/santander/model_fit_cv_4.RDS")
model5<-readRDS("C:/Users/phan3/OneDrive/Desktop/data/santander/model_fit_cv_5.RDS")


train_pred1<-data.frame(pred=predict(model1,xgb.DMatrix(as.matrix(fit_data))))
train_pred2<-data.frame(pred=predict(model2,xgb.DMatrix(as.matrix(fit_data))))
train_pred3<-data.frame(pred=predict(model3,xgb.DMatrix(as.matrix(fit_data))))
train_pred4<-data.frame(pred=predict(model4,xgb.DMatrix(as.matrix(fit_data))))
train_pred5<-data.frame(pred=predict(model5,xgb.DMatrix(as.matrix(fit_data))))

train_pred<-(train_pred1+train_pred2+train_pred3+train_pred4+train_pred5)/5

rm(train_pred1)

rm(train_pred2)

rm(train_pred3)

rm(train_pred4)

rm(train_pred5)

train_pred_normal<-data.frame(pred=predict(model,xgb.DMatrix(as.matrix(fit_data))))

train_pred_0.001<-data.frame(pred=predict(model_fit_0.001,xgb.DMatrix(as.matrix(fit_data))))

train_pred_0.001v2<-data.frame(pred=predict(model_fit_0.001v2,xgb.DMatrix(as.matrix(fit_data))))

target<-raw_data$target


single_lift<-function(actual,predicted,nbin)
{
  
plot_data<-data.table(actual=actual,pred=predicted) 
  
plot_data1<-plot_data[order(pred),]

bins<- cut(1:nrow(plot_data),nbin)

 plot_data2<-data.frame(actual=plot_data1$actual,predicted=plot_data1$pred,bins)

 rm(plot_data1)

rm(plot_data)

actual_mean<-setDT(plot_data2)[, .(actual = mean(actual)), by = .(bins)]

predicted_mean<-setDT(plot_data2)[, .(predicted = mean(predicted)), by = .(bins)]

mean_frame<-data.frame(bins=1:nbin,actual=actual_mean$actual,predicted=predicted_mean$predicted)

ggplot() +
geom_point(data = mean_frame, aes(x = bins, y = actual,color = "blue")) +
geom_point(data = mean_frame, aes(x = bins, y = predicted,color = "red"))+
  scale_color_discrete(name = "Mean Prob", labels = c("actual", "predicted"))

}


double_lift<-function(actual,predicted_new,predicted_old,nbin)
{
  
  plot_data<-data.table(actual=actual,pred_old=predicted_old
                        ,pred_new=predicted_new,sort_ratio=predicted_old/predicted_new) 
  
  plot_data1<-plot_data[order(sort_ratio),]
  
  bins<- cut(1:nrow(plot_data),nbin)
  
  plot_data2<-data.frame(actual=plot_data1$actual,predicted_old=plot_data1$pred_old
                         ,predicted_new=plot_data1$pred_new,bins)
  
  rm(plot_data1)
  
  rm(plot_data)
  
  actual_mean<-setDT(plot_data2)[, .(actual = mean(actual)), by = .(bins)]
  
  predicted_mean_old<-setDT(plot_data2)[, .(predicted_old = mean(predicted_old)), by = .(bins)]
  
  predicted_mean_new<-setDT(plot_data2)[, .(predicted_new = mean(predicted_new)), by = .(bins)]
  
  mean_frame<-data.frame(bins=1:nbin,actual=actual_mean$actual,predicted_mean_old=predicted_mean_old$predicted_old
                         ,predicted_mean_new=predicted_mean_new$predicted_new
                         )
  
  plot_melt<-melt(mean_frame,c("bins"))
  ggplot(plot_melt, aes(x=bins, y=value, color=variable)) +geom_point()+
    scale_color_discrete("Mean_Prob", labels=c("actual", "old","new"))
  }
 


single_lift(target,train_pred$pred,200)+ggtitle("Version 2: 5 models combine")

single_lift(target,train_pred_normal$pred,200)+ggtitle("Version 1: 1 model")

single_lift(target,train_pred_0.001$pred,200)+ggtitle("Version 0.001 eta: 1 model")

single_lift(target,train_pred_0.001v2$pred,200)+ggtitle("Version 0.001 eta: 1 model")

double_lift(target,train_pred$pred,train_pred_normal$pred,50)+ggtitle("Version 1 vs Version 2")



double_lift(target,train_pred_0.001$pred,train_pred_normal$pred,50)+ggtitle("Version 1 vs Version 0.001")

double_lift(target,train_pred_0.001v2$pred,train_pred_normal$pred,50)+ggtitle("Version 1 vs Version 0.001v2")

double_lift(target,train_pred_0.001v2$pred,train_pred_0.001$pred,50)+ggtitle("Version 0.001 vs Version 0.001v2")

faked_pred<-rep(0.1,nrow(raw_data))

double_lift(target,train_pred$pred,faked_pred,50)+ggtitle("Version 2 vs 0.1 Mean Prob")


auc(target,train_pred$pred)

auc(target,train_pred_normal$pred)


tapply(plot_data2$actual,plot_data2$bins,mean)

tapply(plot_data2$predicted,plot_data2$bins,mean)

tapply(plot_data_linear2$predicted,plot_data_linear2$bins,mean)

actual_mean<-setDT(plot_data2)[, .(actual = mean(actual)), by = .(bins)]


predicted_mean<-setDT(plot_data2)[, .(predicted = mean(predicted)), by = .(bins)]


