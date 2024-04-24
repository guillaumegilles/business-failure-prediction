
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Baseline model

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


if (!require(pROC)) install.packages("pROC")
if (!require(tidyverse)) install.packages("tidyverse")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###comparison RF and boosting

resTest_boosting=readRDS("E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/df_resTest_DatasetCrossLags_tuning_boosting.RDS") 
prop.table(table(as.numeric(resTest_boosting$Y)))#0.03394708%
dim(resTest_boosting)

resTest_RF=readRDS("E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/df_resTest_DatasetCrossLags_RF.RDS") 
prop.table(table(as.numeric(resTest_RF$Y)))# 
dim(resTest_RF)

#I can't join the tables because Y are not in the same order apparently...
#resTest_RF <- resTest_RF%>% 
#                rename(Y_RF = Y)
#resTest <- cbind(resTest_boosting , resTest_RF)
#all(resTest$Y == resTest$Y_RF)
#identical(resTest[['Y']],resTest[['Y_RF']])


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

###analyse resultats classification:
#https://towardsdatascience.com/the-5-classification-evaluation-metrics-you-must-know-aa97784ff226

#A. Accuracy: is the proportion of true results among the total number of cases examined.
#valid choice of evaluation for classification problems which are well balanced and not skewed
#e.g. What if we are predicting if an asteroid will hit the earth? Just say No all the time. And you will be 99% accurate. My model can be reasonably accurate, but not at all valuable.

#B. Precision:  answers the question what proportion of predicted Positives is truly Positive?
#valid choice of evaluation metric when we want to be very sure of our prediction
#e.g. if you ask for high precision in predicting default, you are taking the risk of having some defaults that you didn't predict

#C. Sensitivity or Recall: answers the question what proportion of actual Positives is correctly classified?
#valid choice of evaluation metric when we want to capture as many positives as possible
#e.g. If we are building a system to predict if a person has cancer or not, we want to capture the disease even if we are not very sure.

#D. F1 Score: is a number between 0 and 1 and is the harmonic mean of precision and recall.  
#The main problem with the F1 score is that it gives equal weight to precision and recall.

#E. Log Loss or Binary Crossentropy: Log Loss takes into account the uncertainty of your prediction based on how much it varies from the actual label. This gives us a more nuanced view of the performance of our model.
#You want to use it (and have it small) when the output of a classifier is prediction probabilities, but it is susceptible in case of imbalanced datasets

#F. AUC: is the area under the ROC curve.
#AUC ROC indicates how well the probabilities from the positive classes are separated from the negative classes
#If we have got the probabilities from our classifier, we can use various threshold values to plot the sensitivity(True positive rate) and (1-specificity)(False positive rate) on the curve and we will have a ROC curve.
#We can use the ROC curves to decide on a Threshold value. The choice of threshold value will also depend on how the classifier is intended to be used (e.g. If it is a cancer classification application you don’t want your threshold to be as big as 0.5. Even if a patient has a 0.3 probability of having cancer you would classify him to be 1)
#AUC is scale-invariant. It measures how well predictions are ranked, rather than their absolute values.
#AUC is also classification-threshold-invariant like log loss. It measures the quality of the model’s predictions irrespective of what classification threshold is chosen, unlike F1 score or accuracy which depend on the choice of threshold.


#AUC;
auc_resTest_DatasetPanel_tuning_boosting <- as.numeric(apply(resTest_boosting[,-1],2,auc,response=resTest_boosting$Y))
auc_resTest_DatasetPanel_RF <- as.numeric(apply(resTest_RF[,-1],2,auc,response=resTest_RF$Y))
#auc(resTest_RF[,-1],response=resTest_RF$Y)
#I want it large (it should be above 0.5)
auc <- c(auc_resTest_DatasetPanel_tuning_boosting,auc_resTest_DatasetPanel_RF)


#one by one to get the performance of different algos:
rocada <- roc(resTest_boosting$Y,resTest_boosting$adaboost)#response, then predictor
# auc(rocada)#gives value of AUC=area under ROC curve
# plot(rocada)#graph of ROC
# 
roclogit<- roc(resTest_boosting$Y,resTest_boosting$logitboost)
rocada_150iter <- roc(resTest_boosting$Y,resTest_boosting$adaboost_150iter)
roclogit_150iter<- roc(resTest_boosting$Y,resTest_boosting$logitboost_150iter)
rocada_03shr <- roc(resTest_boosting$Y,resTest_boosting$adaboost_03shr)
roclogit_03shr<- roc(resTest_boosting$Y,resTest_boosting$logitboost_03shr)
rocada_05shr <- roc(resTest_boosting$Y,resTest_boosting$adaboost_05shr)
roclogit_05shr<- roc(resTest_boosting$Y,resTest_boosting$logitboost_05shr)
rocada_1000iter <- roc(resTest_boosting$Y,resTest_boosting$adaboost_1000iter)
roclogit_1000iter<- roc(resTest_boosting$Y,resTest_boosting$logitboost_1000iter)
rocada_1000iter_03shr <- roc(resTest_boosting$Y,resTest_boosting$adaboost_1000iter_03shr)
roclogit_1000iter_03shr<- roc(resTest_boosting$Y,resTest_boosting$logitboost_1000iter_03shr)
rocada_1000iter_05shr <- roc(resTest_boosting$Y,resTest_boosting$adaboost_1000iter_05shr)
roclogit_1000iter_05shr<- roc(resTest_boosting$Y,resTest_boosting$logitboost_1000iter_05shr)
rocada_2000iter_03shr <- roc(resTest_boosting$Y,resTest_boosting$adaboost_2000iter_03shr)

rocxgb<- roc(resTest_boosting$Y,resTest_boosting$xgboost)
rocxgb_150iter<- roc(resTest_boosting$Y,resTest_boosting$xgboost_150iter)
rocxgb_03shr<- roc(resTest_boosting$Y,resTest_boosting$xgboost_03shr)
rocxgb_05shr<- roc(resTest_boosting$Y,resTest_boosting$xgboost_05shr)
rocxgb_1000iter<- roc(resTest_boosting$Y,resTest_boosting$xgboost_1000iter)
rocxgb_1000iter_03shr<- roc(resTest_boosting$Y,resTest_boosting$xgboost_1000iter_03shr)
rocxgb_1000iter_05shr<- roc(resTest_boosting$Y,resTest_boosting$xgboost_1000iter_05shr)
rocxgb_2000iter_03shr<- roc(resTest_boosting$Y,resTest_boosting$xgboost_2000iter_03shr)

roclogit_2000iter_03shr<- roc(resTest_boosting$Y,resTest_boosting$logitboost_2000iter_03shr)
roclogit_3000iter_03shr<- roc(resTest_boosting$Y,resTest_boosting$logitboost_3000iter_03shr)
rocxgb_2000iter_05shr<- roc(resTest_boosting$Y,resTest_boosting$xgboost_2000iter_05shr)
rocxgb_3000iter_05shr<- roc(resTest_boosting$Y,resTest_boosting$xgboost_3000iter_05shr)
rocxgb_3000iter_03shr<- roc(resTest_boosting$Y,resTest_boosting$xgboost_3000iter_03shr)

# auc(rocxgb)
# plot(rocxgb, add=T,col=2)
# 
rocRF100<- roc(resTest_RF$Y,resTest_RF$RF100)
# auc(rocRF)
# plot(rocRF, add=T,col=3)
rocRF100_100l<- roc(resTest_RF$Y,resTest_RF$RF100_100l)
rocRF50_50l<- roc(resTest_RF$Y,resTest_RF$RF50_50l)
rocRF50<- roc(resTest_RF$Y,resTest_RF$RF50)
rocRF50_100l<- roc(resTest_RF$Y,resTest_RF$RF50_100l)


##To get several performance measures for an algo with the best threshold (based on sum of sensitivity+specificity), for each model:
perfmeas_boostingADA <- coords(rocada,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingLOGIT <- coords(roclogit,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingADA_150iter <- coords(rocada_150iter,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingLOGIT_150iter <- coords(roclogit_150iter,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingADA_03shr <- coords(rocada_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingLOGIT_03shr <- coords(roclogit_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingADA_05shr <- coords(rocada_05shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingLOGIT_05shr <- coords(roclogit_05shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingADA_1000iter <- coords(rocada_1000iter,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingLOGIT_1000iter <- coords(roclogit_1000iter,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingADA_1000iter_03shr <- coords(rocada_1000iter_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingLOGIT_1000iter_03shr <- coords(roclogit_1000iter_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingADA_1000iter_05shr <- coords(rocada_1000iter_05shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingLOGIT_1000iter_05shr <- coords(roclogit_1000iter_05shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingADA_2000iter_03shr <- coords(rocada_2000iter_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity

perfmeas_boostingXGB <- coords(rocxgb,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_150iter <- coords(rocxgb_150iter,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_03shr <- coords(rocxgb_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_05shr <- coords(rocxgb_05shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_1000iter <- coords(rocxgb_1000iter,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_1000iter_03shr <- coords(rocxgb_1000iter_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_1000iter_05shr <- coords(rocxgb_1000iter_05shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_2000iter_03shr <- coords(rocxgb_2000iter_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity

perfmeas_boostingLOGIT_2000iter_03shr <- coords(roclogit_2000iter_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingLOGIT_3000iter_03shr <- coords(roclogit_3000iter_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_2000iter_05shr <- coords(rocxgb_2000iter_05shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_3000iter_05shr <- coords(rocxgb_3000iter_05shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB_3000iter_03shr <- coords(rocxgb_3000iter_03shr,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity

perfmeas_RF100 <- coords(rocRF100,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_RF100_100l <- coords(rocRF100_100l,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_RF50_50l <- coords(rocRF50_50l,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_RF50 <- coords(rocRF50,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_RF50_100l <- coords(rocRF50_100l,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity


rownames(perfmeas_boostingADA) = "boostingADA"
rownames(perfmeas_boostingLOGIT) = "boostingLOGIT"
rownames(perfmeas_boostingADA_150iter) = "boostingADA_150iter"
rownames(perfmeas_boostingLOGIT_150iter) = "boostingLOGIT_150iter"
rownames(perfmeas_boostingADA_03shr) = "boostingADA_03shr"
rownames(perfmeas_boostingLOGIT_03shr) = "boostingLOGIT_03shr"
rownames(perfmeas_boostingADA_05shr) = "boostingADA_05shr"
rownames(perfmeas_boostingLOGIT_05shr) = "boostingLOGIT_05shr"
rownames(perfmeas_boostingADA_1000iter) = "boostingADA_1000iter"
rownames(perfmeas_boostingLOGIT_1000iter) = "boostingLOGIT_1000iter"
rownames(perfmeas_boostingADA_1000iter_03shr) = "boostingADA_1000iter_03shr"
rownames(perfmeas_boostingLOGIT_1000iter_03shr) = "boostingLOGIT_1000iter_03shr"
rownames(perfmeas_boostingADA_1000iter_05shr) = "boostingADA_1000iter_05shr"
rownames(perfmeas_boostingLOGIT_1000iter_05shr) = "boostingLOGIT_1000iter_05shr"
rownames(perfmeas_boostingADA_2000iter_03shr) = "boostingADA_2000iter_03shr"

rownames(perfmeas_boostingXGB) = "boostingXGB"
rownames(perfmeas_boostingXGB_150iter) = "boostingXGB_150iter"
rownames(perfmeas_boostingXGB_03shr) = "boostingXGB_03shr"
rownames(perfmeas_boostingXGB_05shr) = "boostingXGB_05shr"
rownames(perfmeas_boostingXGB_1000iter) = "boostingXGB_1000iter"
rownames(perfmeas_boostingXGB_1000iter_03shr) = "boostingXGB_1000iter_03shr"
rownames(perfmeas_boostingXGB_1000iter_05shr) = "boostingXGB_1000iter_05shr"
rownames(perfmeas_boostingXGB_2000iter_03shr) = "boostingXGB_2000iter_03shr"

rownames(perfmeas_boostingLOGIT_2000iter_03shr) = "boostingLOGIT_2000iter_03shr"
rownames(perfmeas_boostingLOGIT_3000iter_03shr) = "boostingLOGIT_3000iter_03shr"
rownames(perfmeas_boostingXGB_2000iter_05shr) = "boostingXGB_2000iter_05shr"
rownames(perfmeas_boostingXGB_3000iter_05shr) = "boostingXGB_3000iter_05shr"
rownames(perfmeas_boostingXGB_3000iter_03shr) = "boostingXGB_3000iter_03shr"

rownames(perfmeas_RF100) = "RF100"
rownames(perfmeas_RF100_100l) = "RF100_100l"
rownames(perfmeas_RF50_50l) = "RF50_50l"
rownames(perfmeas_RF50) = "RF50"
rownames(perfmeas_RF50_100l) = "RF50_100l"

perfmeas_df <- rbind(perfmeas_boostingADA, perfmeas_boostingLOGIT,
                     perfmeas_boostingADA_150iter, perfmeas_boostingLOGIT_150iter,
                     perfmeas_boostingADA_03shr, perfmeas_boostingLOGIT_03shr,
                     perfmeas_boostingADA_05shr, perfmeas_boostingLOGIT_05shr,
                     perfmeas_boostingADA_1000iter, perfmeas_boostingLOGIT_1000iter,
                     perfmeas_boostingADA_1000iter_03shr, perfmeas_boostingLOGIT_1000iter_03shr,
                     perfmeas_boostingADA_1000iter_05shr, perfmeas_boostingLOGIT_1000iter_05shr,
                     perfmeas_boostingADA_2000iter_03shr,
                     perfmeas_boostingXGB , perfmeas_boostingXGB_150iter, 
                     perfmeas_boostingXGB_03shr, perfmeas_boostingXGB_05shr,
                     perfmeas_boostingXGB_1000iter, perfmeas_boostingXGB_1000iter_03shr, perfmeas_boostingXGB_1000iter_05shr,
                     perfmeas_boostingXGB_2000iter_03shr,
                     perfmeas_boostingLOGIT_2000iter_03shr, perfmeas_boostingLOGIT_3000iter_03shr,
                     perfmeas_boostingXGB_2000iter_05shr, perfmeas_boostingXGB_3000iter_05shr, perfmeas_boostingXGB_3000iter_03shr,
                     perfmeas_RF100, perfmeas_RF100_100l, perfmeas_RF50_50l, perfmeas_RF50, perfmeas_RF50_100l) %>% 
  mutate(F1=2*(precision*sensitivity)/(precision+sensitivity))
View(perfmeas_df)

#add auc
perfmeas_df = perfmeas_df %>% cbind(auc)

#change columns order
perfmeas_df <- perfmeas_df %>% dplyr::relocate(auc,F1)

  
saveRDS(perfmeas_df,"E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/perfmeas_df_DatasetCrossLags.RDS")


# Represent best one:
p <- resTest_boosting %>%
  ggplot( aes(x=logitboost_2000iter_03shr, fill=as.factor(Y))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

p2 <- ggplot(data=resTest_boosting, aes(x=logitboost_2000iter_03shr, group=as.factor(Y), fill=as.factor(Y))) +
  geom_density(adjust=1.5, alpha=.4)
p2
