getwd()
####################################
#
# Estimating with backtesting 
#
####################################

#if (!require(glmnet)) install.packages("glmnet")
#if (!require(rpart)) install.packages("rpart")
#if (!require(randomForest)) install.packages("randomForest")
if (!require(pROC)) install.packages("pROC")
#if (!require(plm)) install.packages("plm")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(gbm)) install.packages("gbm")
if (!require(xgboost)) install.packages("xgboost")
#if (!require(e1071)) install.packages("e1071")


####################################

#load cleaned data
list_don_def =list()
list_don_def[["2022"]]=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df2022_lags_clean.RDS") 
list_don_def[["2021"]]=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df2021_lags_clean.RDS") 
list_don_def[["2020"]]=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df2020_lags_clean.RDS") 
list_don_def[["2019"]]=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df2019_lags_clean.RDS") 
list_don_def[["2018"]]=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df2018_lags_clean.RDS") 


####################################

list_don_train = list()
list_don_test = list()
list_results <- list()

for(year_pred in 2018:2022){
  
  #fix covariates
  
  #don_def2022$year <- as.factor(don_def2022$year)
  
  #!for the moment I have issues with too many categories so I just drop them
  list_don_def[[as.character(year_pred)]] <- select(list_don_def[[as.character(year_pred)]],-"dpt")
  #don_def2022 <- select(don_def2022,-"region")
  #don_def2022 <- select(don_def2022,-"sect_NACE1")
  list_don_def[[as.character(year_pred)]] <- select(list_don_def[[as.character(year_pred)]],-"sect_NACE2")
  list_don_def[[as.character(year_pred)]] <- select(list_don_def[[as.character(year_pred)]],-"sect_NACE3")
  list_don_def[[as.character(year_pred)]] <- select(list_don_def[[as.character(year_pred)]],-"sect_NACE4")
  list_don_def[[as.character(year_pred)]] <- select(list_don_def[[as.character(year_pred)]],-"sect_NACE5")
  #don_def2022 <- select(don_def2022,-"year")
  
  
  #prepare samples for training and testing

  n <- nrow(list_don_def[[as.character(year_pred)]]) 
  set.seed(123)
  
  #test sample will be 10% of total obs
  part_test <- 0.1
  
  #learning sample is the inferior level of 80% (inferior level in case 20% is not integer) of total obs
  n_train <- floor(n*(1-part_test))
  n_train 
  
  #randomly select the position of the observations that will be training set in the whole dataset 
  obs_train <- sample(x=1:n,size=n_train)#between 1 and the length of the whole dataset, randomly pick the position of the obs that will be n_train 
  length(obs_train) #length of a vector 
  
  #build the TRAINING set taking from the whole dataset the rows corresponding to the numbers randomly selected in obs_train:
  list_don_train[[as.character(year_pred)]] <- list_don_def[[as.character(year_pred)]][obs_train,]
  nrow(list_don_train[[as.character(year_pred)]]) # [N.B. length for a dataset gives the number of variables]
  
  #build the TEST set for validation with the remaining obs from the whole dataset
  list_don_test[[as.character(year_pred)]] <- list_don_def[[as.character(year_pred)]][-obs_train,]
  nrow(list_don_test[[as.character(year_pred)]]) 
  
  list_results[[as.character(year_pred)]] <- data.frame(Y = list_don_test[[as.character(year_pred)]]$Y)  
  
  #%%%%%%%%%%%
  
  ###Estimation 
  
  #reconvert year into factor
  #list_don_train[[as.character(year_pred)]]$year <- as.factor(list_don_train[[as.character(year_pred)]]$year) 
  #list_don_test[[as.character(year_pred)]]$year <- as.factor(list_don_test[[as.character(year_pred)]]$year)
  
  
  #drop year
  list_don_train[[as.character(year_pred)]] <- select(list_don_train[[as.character(year_pred)]],-"year")
  list_don_test[[as.character(year_pred)]] <- select(list_don_test[[as.character(year_pred)]],-"year")
  
  
  #svm
  #svm_lin <- svm(Y~., data=list_don_train[[as.character(year_pred)]], kernel="linear", probability=TRUE) #if I want the probabilities
  #svm_lin_pred <- predict(svm_lin, newdata=list_don_test[[as.character(year_pred)]], probability=TRUE)
  #list_results[[as.character(year_pred)]]$svm_lin <- attr(x=svm_lin_pred,which="probabilities")[,"1"]  #put proba into a variable in the test set
  
  
  #boosting
  list_don_train[[as.character(year_pred)]]$Y <- as.numeric(list_don_train[[as.character(year_pred)]]$Y)-1 #-1 because once transformed in factor it stocks in 1,2
  list_don_test[[as.character(year_pred)]]$Y <- as.numeric(list_don_test[[as.character(year_pred)]]$Y)-1 #-1 because once transformed in factor it stocks in 1,2
  
  
  #logitboost
  gbm_logit_2000iter_03shr <- gbm(Y~., data=list_don_train[[as.character(year_pred)]], distribution="bernoulli",n.trees = 2000,shrinkage = 0.3)
  list_results[[as.character(year_pred)]]$gbm_logit_2000iter_03shr  <- predict(gbm_logit_2000iter_03shr, newdata=list_don_test[[as.character(year_pred)]], type="response")
  
  summary(list_results[[as.character(year_pred)]]$gbm_logit_2000iter_03shr)
  
  
}


saveRDS(list_results,"E:\\DataScientist\\project\\forecastingDEF\\results\\4_BackTesting/df_resTest_DatasetCrossLags_Backtesting.RDS")


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


#list_results=readRDS("E:\\DataScientist\\project\\forecastingDEF\\results\\5_BackTesting/df_resTest_boosting.RDS") 
#resRF$prev_logit <- (resRF$prev_proba_logit>0.01)*1


#compute AUC in different years:
auc=c()

for(iii in 2018:2022){
  auc[as.character(iii)] = as.numeric(auc(list_results[[as.character(iii)]]$Y,list_results[[as.character(iii)]]$gbm_logit_2000iter_03shr))
}
# auc2018 <- auc(list_results[["2018"]]$Y,list_results[["2018"]]$gbm_logit_2000iter_03shr) #0.8797
# auc2019 <- auc(list_results[["2019"]]$Y,list_results[["2019"]]$gbm_logit_2000iter_03shr) #0.8801
# auc2020 <- auc(list_results[["2020"]]$Y,list_results[["2020"]]$gbm_logit_2000iter_03shr) #0.8876
# auc2021 <- auc(list_results[["2021"]]$Y,list_results[["2021"]]$gbm_logit_2000iter_03shr) #0.8975
# auc2022 <- auc(list_results[["2022"]]$Y,list_results[["2022"]]$gbm_logit_2000iter_03shr) #0.8915


#performance in different years:
#roc <- list()
#roc[["2018"]] <- roc(list_results[["2018"]]$Y,list_results[["2018"]]$gbm_logit_2000iter_03shr)

roc2018 <- roc(list_results[["2018"]]$Y,list_results[["2018"]]$gbm_logit_2000iter_03shr)
roc2019 <- roc(list_results[["2019"]]$Y,list_results[["2019"]]$gbm_logit_2000iter_03shr)
roc2020 <- roc(list_results[["2020"]]$Y,list_results[["2020"]]$gbm_logit_2000iter_03shr)
roc2021 <- roc(list_results[["2021"]]$Y,list_results[["2021"]]$gbm_logit_2000iter_03shr)
roc2022 <- roc(list_results[["2022"]]$Y,list_results[["2022"]]$gbm_logit_2000iter_03shr)

##To get several performance measures for an algo with the best threshold (based on sum of sensitivity+specificity), for each model:
perfmeas_boosting2018 <- coords(roc2018,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
rownames(perfmeas_boosting2018) = "boosting2018"

perfmeas_boosting2019 <- coords(roc2019,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
rownames(perfmeas_boosting2019) = "boosting2019"

perfmeas_boosting2020 <- coords(roc2020,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
rownames(perfmeas_boosting2020) = "boosting2020"

perfmeas_boosting2021 <- coords(roc2021,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
rownames(perfmeas_boosting2021) = "boosting2021"

perfmeas_boosting2022 <- coords(roc2022,x="best",ret=c("precision","sensitivity","fpr","fnr","threshold"))#threshold is optimized based on sum of sensitivity+specificity
rownames(perfmeas_boosting2022) = "boosting2022"


perfmeas_df <- rbind(perfmeas_boosting2018, 
                     perfmeas_boosting2019,
                     perfmeas_boosting2020, 
                     perfmeas_boosting2021, 
                     perfmeas_boosting2022) %>% 
  mutate(F1=2*(precision*sensitivity)/(precision+sensitivity))
View(perfmeas_df)


#add auc
perfmeas_df = perfmeas_df %>% cbind(auc)

#change columns order
perfmeas_df <- perfmeas_df %>% dplyr::relocate(auc,F1)

saveRDS(perfmeas_df,"E:\\DataScientist\\project\\forecastingDEF\\results\\4_BackTesting/perfmeas_df_DatasetCrossLags_BackTesting.RDS")
write.csv(perfmeas_df,"E:\\DataScientist\\project\\forecastingDEF\\results\\4_BackTesting/perfmeas_df_DatasetCrossLags_BackTesting.csv")

