

ggplot(raw_data, aes(x=target)) +geom_bar()


ggplot(raw_data, aes(x=as.factor(target),y=var_0)) +geom_boxplot()

ggplot(raw_data, aes(x=as.factor(target),y=var_1)) +geom_boxplot()


ggplot(raw_data, aes(x=as.factor(target),y=var_1)) +geom_bar()

corr_data<-raw_data%>%select(target,var_81,	var_139,	var_12,	var_53,	var_110,	var_146,	var_26,	var_6,
                             var_174,	var_76,	var_80,	var_22,	var_21,	var_99,	var_165,	var_166,	var_13,	var_109)

M<-cor(corr_data)

corrplot(M, method="color")


var<-raw_data%>%select(target,var_81)



ggplot(raw_data, aes(x=var_81 ,fill=factor(target))) + geom_density(alpha = 0.2) + 
  labs(title="Density Plot", x="var_81")



ggplot(raw_data, aes(x=var_139 ,fill=factor(target))) + geom_density(alpha = 0.2) + 
  labs(title="Density Plot", x="var_139")




target_df <- data.frame(table(raw_data$target))
colnames(target_df) <- c("target", "freq")
ggplot(data=target_df, aes(x=target, y=freq, fill=target)) +
  geom_bar(position = 'dodge', stat='identity', alpha=0.5) +
  scale_fill_manual("legend", values = c("1" = "dodgerblue", "0"="firebrick1")) +
  theme_classic()

train<-raw_data

feature_groups <- 3:22
col_names <- colnames(train)[c(2,feature_groups)]
temp <- gather(train[,col_names, with=F], key="features", value="value", -target)
temp$target <- factor(temp$target)
temp$features <- factor(temp$features, levels=col_names[-1], labels=col_names[-1])
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=target, color=target), alpha=0.3) +
  scale_color_manual(values = c("1" = "dodgerblue", "0"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")

col_names <- colnames(train)[c(2,feature_groups+20)]
temp <- gather(train[,col_names, with=F], key="features", value="value", -target)
temp$target <- factor(temp$target)
temp$features <- factor(temp$features, levels=col_names[-1], labels=col_names[-1])
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=target, color=target), alpha=0.3) +
  scale_color_manual(values = c("1" = "dodgerblue", "0"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")

col_names <- colnames(train)[c(2,feature_groups+80)]
temp <- gather(train[,col_names, with=F], key="features", value="value", -target)
temp$target <- factor(temp$target)
temp$features <- factor(temp$features, levels=col_names[-1], labels=col_names[-1])
ggplot(data=temp, aes(x=value)) +
  geom_density(aes(fill=target, color=target), alpha=0.3) +
  scale_color_manual(values = c("1" = "dodgerblue", "0"="firebrick1")) +
  theme_classic() +
  facet_wrap(~ features, ncol = 4, scales = "free")