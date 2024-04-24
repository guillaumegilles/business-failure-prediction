
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Baseline model

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


if (!require(pROC)) install.packages("pROC")
if (!require(tidyverse)) install.packages("tidyverse")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###comparison RF and boosting

resTest_boosting=readRDS("E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/df_resTest_DatasetLags_boosting.RDS") 
prop.table(table(as.numeric(resTest_boosting$Y)))#0.01139515%
dim(resTest_boosting)

resTest_RF=readRDS("E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/df_resTest_DatasetLags_RF.RDS") 
prop.table(table(as.numeric(resTest_RF$Y)))#0.01135232 
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

#AUC:
auc=c()

rocada <- roc(resTest_boosting$Y,resTest_boosting$adaboost)
aucada <- auc(rocada)#gives value of AUC=area under ROC curve: 0.861
plot(rocada)#graph of ROC

rocxgb<- roc(resTest_boosting$Y,resTest_boosting$xgboost)
aucxgb <- auc(rocxgb)#0.8683
plot(rocxgb, add=T,col=2)

rocRF100_100l<- roc(resTest_RF$Y,resTest_RF$RF100_100l)
aucRF100_100l <- auc(rocRF100_100l)#0.7035
plot(rocRF100_100l, add=T,col=3)

rocRF50_100l<- roc(resTest_RF$Y,resTest_RF$RF50_100l)
aucRF50_100l <- auc(rocRF50_100l)#0.6514
plot(rocRF50_100l, add=T,col=4)

auc = c(as.numeric(aucada),as.numeric(aucxgb),as.numeric(aucRF100_100l),as.numeric(aucRF50_100l))
                                    
                                    
##To get several performance measures for an algo with the best threshold (based on sum of sensitivity+specificity), for each model:
perfmeas_boostingADA <- coords(rocada,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_boostingXGB <- coords(rocxgb,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_RF100_100l <- coords(rocRF100_100l,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
perfmeas_RF50_100l <- coords(rocRF50_100l,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity

rownames(perfmeas_boostingADA) = "boostingADA"
rownames(perfmeas_boostingXGB) = "boostingXGB"
rownames(perfmeas_RF100_100l) = "RF100_100l"
rownames(perfmeas_RF50_100l) = "RF50_100l"

perfmeas_df <- rbind(perfmeas_boostingADA, perfmeas_boostingXGB , perfmeas_RF100_100l, perfmeas_RF50_100l) %>% 
  mutate(F1=2*(precision*sensitivity)/(precision+sensitivity))
View(perfmeas_df)

#add auc
perfmeas_df = perfmeas_df %>% cbind(auc)

#change columns order
perfmeas_df <- perfmeas_df %>% dplyr::relocate(auc,F1)


saveRDS(perfmeas_df,"E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/perfmeas_df_DatasetLags.RDS")

