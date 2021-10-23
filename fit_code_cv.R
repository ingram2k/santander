

fit_data <- raw_data %>% select(ID_code,target, var_81,	var_139,	var_12,	var_53,	var_110,	var_146,	var_26,	var_6,
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



set.seed(0.123)

target <-fit_data$target 
nrounds = 5
folds = createFolds(factor(target), k = 5, list = FALSE)

mean_train<-mean(fit_data$target)

fit_data<- fit_data %>% select (-ID_code,-target)



for(this.round in 1:nrounds) {
test.index <- c(1:length(target)) [folds == this.round]
train.index <- c(1:length(target)) [folds != this.round]

dtrain<- xgb.DMatrix(data= as.matrix(fit_data[train.index,]), 
                     label= target[train.index])

dtest <- xgb.DMatrix(data= as.matrix(fit_data[test.index,]) , 
                      label= target[test.index])

model_fit<-xgb.train(data=dtrain
                 ,objective = "binary:logistic"
                 ,base_score = mean_train
                 ,eval_metric = "auc"
                 ,verbose = 1
                 ,eta = 0.01
                 ,nround= 20000
                 ,min_child_weight = 90
                 ,max_depth = 3
                 ,gamma = 5
                 ,colsample_bytree = 0.5
                 ,subsample = 0.5
                 ,early_stopping_rounds = 100
                 ,watchlist = list(train=dtrain, test=dtest))

saveRDS(model_fit,paste0("C:/Users/phan3/OneDrive/Desktop/data/santander/","model_fit_cv_",this.round,".RDS"))

}



