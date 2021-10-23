plot_data<-data.table(actual=target,pred_old=train_pred_normal$pred
                      ,pred_new=train_pred$pred,sort_ratio=train_pred_normal$pred/train_pred$pred) 

plot_data1<-plot_data[order(sort_ratio),]

bins<- cut(1:nrow(plot_data),50)

plot_data2<-data.frame(actual=plot_data1$actual,predicted_old=plot_data1$pred_old
                       ,predicted_new=plot_data1$pred_new,bins,sort_ratio=plot_data1$sort_ratio)

rm(plot_data1)

rm(plot_data)

actual_mean<-setDT(plot_data2)[, .(actual = mean(actual)), by = .(bins)]

predicted_mean_old<-setDT(plot_data2)[, .(predicted_old = mean(predicted_old)), by = .(bins)]

predicted_mean_new<-setDT(plot_data2)[, .(predicted_new = mean(predicted_new)), by = .(bins)]

mean_frame<-data.frame(bins=1:50,actual=actual_mean$actual,predicted_mean_old=predicted_mean_old$predicted_old
                       ,predicted_mean_new=predicted_mean_new$predicted_new
)


plot_melt<-melt(mean_frame,c("bins"))
ggplot(plot_melt, aes(x=bins, y=value, color=variable)) +geom_point()+
  scale_color_discrete("Mean_Prob", labels=c("actual", "old","new"))