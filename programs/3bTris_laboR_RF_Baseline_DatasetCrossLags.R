getwd()

####################################
#
# Estimating Random Forest (no pb with categoricals)
#
####################################

#if (!require(glmnet)) install.packages("glmnet")
if (!require(rpart)) install.packages("rpart")
if (!require(randomForest)) install.packages("randomForest")
if (!require(pROC)) install.packages("pROC")
#if (!require(plm)) install.packages("plm")
#if (!require(tidyverse)) install.packages("tidyverse")
if (!require(dplyr)) install.packages("dplyr")
#library(gbm)
#library(xgboost)
#library(e1071)


####################################

#load cleaned data
#don_all=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df_clean.RDS") 
don_all=readRDS("/home/labdl/appli/bdf/users/n825781/df2022_lags_clean.RDS") 


####################################

#fix covariates

colnames(don_all)

don_all$year <- as.factor(don_all$year)


str(don_all)

#randomForest cannot handle categorical predictors with more than 53 categories.
#don_all <- select(don_all,-"region")
don_all <- select(don_all,-"dpt")
#don_all <- select(don_all,-"sect_NACE1")
don_all <- select(don_all,-"sect_NACE2")
don_all <- select(don_all,-"sect_NACE3")
don_all <- select(don_all,-"sect_NACE4")
don_all <- select(don_all,-"sect_NACE5")
#don_all <- select(don_all,-"size")
don_all <- select(don_all,-"year")

dim(don_all)
#410049    164

colnames(don_all)
str(don_all)
table(don_all$Y)
prop.table(table(don_all$Y)) #0.03388619%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#cut out test set
#N.B. I don't need validation set because there is no overfitting with RF

n <- nrow(don_all) 
set.seed(123)

#test sample will be 10% of total obs
part_test <- 0.1

#learning sample is the inferior level of 90% (inferior level in case 10% is not integer) of total obs
n_train <- floor(n*(1-part_test))
n_train 

#randomly select the position of the observations that will be training set in the whole dataset 
obs_train <- sample(x=1:n,size=n_train)#between 1 and the length of the whole dataset, randomly pick the position of the obs that will be n_train 
length(obs_train) #length of a vector 

#build the training set taking from the whole dataset the rows corresponding to the numbers randomly selected in obs_train:
don_train <- don_all[obs_train,]
nrow(don_train) # [N.B. length for a dataset gives the number of variables]

#build the test set for validation with the remaining obs from the whole dataset
don_test <- don_all[-obs_train,]
nrow(don_test) 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###Estimation of Random Forest models

rf100_df <- randomForest(Y~.,
                     data=don_train,#training set (within the 90% obs) to learn
                     ntree=100) #with big dataset it is better to start with 100 trees and increasing if necessary
plot(rf100_df)
#green errors shown are computed on observations that have been not use to train (error out of bag)

#computes Gini index and plots in decreasing order:
importance(rf100_df)
varImpPlot(rf100_df)

#%%%%%%%%%%%%%%

rf100_100l_df <- randomForest(Y~.,
                         data=don_train,#training set (within the 90% obs) to learn
                         ntree=100,
                         nodesize=100) #with big dataset it is better to start with 100 trees and increasing if necessary
plot(rf100_100l_df)
#green errors shown are computed on observations that have been not use to train (error out of bag)

#computes Gini index and plots in decreasing order:
importance(rf100_100l_df)
varImpPlot(rf100_100l_df)

#%%%%%%%%%%%%%%

rf50_50l_df <- randomForest(Y~.,
                      data=don_train,#training set (within the 90% obs) to learn
                      ntree=50,
                      family="binomial",
                      nodesize=50) 

#saveRDS(rf50_50l_df,"/home/labdl/appli/bdf/users/n825781/rf50_50l_df.RDS")

plot(rf50_50l_df)
#green erors shown are computed on observations that have been not use to train (error out of bag)

#computes Gini index and plots in decreasing order:
importance(rf50_50l_df)
varImpPlot(rf50_50l_df)

#%%%%%%%%%%%%%%

rf50_100l_df <- randomForest(Y~.,
                            data=don_train,#training set (within the 90% obs) to learn
                            ntree=50,
                            family="binomial",
                            nodesize=100) 

#saveRDS(rf50_50l_df,"/home/labdl/appli/bdf/users/n825781/rf50_50l_df.RDS")

plot(rf50_100l_df)
#green erors shown are computed on observations that have been not use to train (error out of bag)

#computes Gini index and plots in decreasing order:
importance(rf50_100l_df)
varImpPlot(rf50_100l_df)

#%%%%%%%%%%%%%%

rf50_df <- randomForest(Y~.,
                            data=don_train,#training set (within the 90% obs) to learn
                            ntree=50,
                            family="binomial") 

#saveRDS(rf50_50l_df,"/home/labdl/appli/bdf/users/n825781/rf50_50l_df.RDS")

plot(rf50_df)
#green erors shown are computed on observations that have been not use to train (error out of bag)

#computes Gini index and plots in decreasing order:
importance(rf50_df)
varImpPlot(rf50_df)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Prediction on test set

resTest_RF=data.frame(Y=don_test$Y)
resTest_RF$Y <- as.factor(resTest_RF$Y)

resTest_RF$RF100 <- predict(rf100_df,don_test, type="prob")[,2]
summary(resTest_RF$RF100)

resTest_RF$RF100_100l <- predict(rf100_100l_df,don_test, type="prob")[,2]
summary(resTest_RF$RF100_100l)
        
resTest_RF$RF50_50l <- predict(rf50_50l_df,don_test, type="prob")[,2]
summary(resTest_RF$RF50_50l)

resTest_RF$RF50 <- predict(rf50_df,don_test, type="prob")[,2]
summary(resTest_RF$RF50)

resTest_RF$RF50_100l <- predict(rf50_100l_df,don_test, type="prob")[,2]
summary(resTest_RF$RF100_50l)

#saveRDS(resTest_RF,"E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/df_resTest_RF.RDS")
saveRDS(resTest_RF,"/home/labdl/appli/bdf/users/n825781/df_DatasetCrossLags_resTest_RF.RDS")

  
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


prop.table(table(as.numeric(resTest_RF$Y))) # 0.03289843%


rocRF100 <- roc(resTest_RF$Y,resTest_RF$RF100)
auc(rocRF100)#gives value of AUC=area under ROC curve: 0.8597
plot(rocRF100)#graph of ROC

rocRF100_100l <- roc(resTest_RF$Y,resTest_RF$RF100_100l)
auc(rocRF100_100l)#gives value of AUC=area under ROC curve: 0.8007
plot(rocRF100_100l,col=2,add=T)

rocRF50_50l<- roc(resTest_RF$Y,resTest_RF$RF50_50l)
auc(rocRF50_50l)#0.8102
plot(rocRF50_50l,col=3,add=T)

rocRF50<- roc(resTest_RF$Y,resTest_RF$RF50)
auc(rocRF50)#0.8406
plot(rocRF50,col=4,add=T)

rocRF50_100l <- roc(resTest_RF$Y,resTest_RF$RF50_100l)
auc(rocRF50_100l)#gives value of AUC=area under ROC curve: 0.7538
plot(rocRF50_100l,col=5,add=T)


##To get several performance measures for an algo with an arbitrary threshold:  
coords(rocRF100,x=0.0114,ret=c("npv", "precision","specificity","sensitivity","fpr","fnr","fdr", "accuracy", "threshold"))#arbitrary threshold
coords(rocRF50,x=0.0114,ret=c("npv", "precision","specificity","sensitivity","fpr","fnr","fdr", "accuracy", "threshold"))#arbitrary threshold

##I could choose threshold that predicts same percentage of 1 as in sample:
plot(1:length(resTest_RF$Y),sort(resTest_RF$rocRF100))
#abline(v=0.0114)


##To get specificity and sensitivity with the best threshold (based on sum of sensitivity+specificity), for each algo:
besthreshold_rocada <- coords(rocRF100,x="best")#threshold is optimized based on sum of sensitivity+specificity
besthreshold_rocada
#besthreshold_rocada$threshold (here 0.010)

besthreshold_rocxgb <- coords(rocRF50,x="best")#threshold is optimized based on sum of sensitivity+specificity
besthreshold_rocxgb
#besthreshold_rocxgb$threshold (here 0.018)


##To get several performance measures for an algo with the best threshold (based on sum of sensitivity+specificity), for each model:
coords(rocRF100,x="best",ret=c("npv", "precision","specificity","sensitivity","fpr","fnr","fdr", "accuracy", "threshold"))#threshold is optimized based on sum of sensitivity+specificity
coords(rocRF50,x="best",ret=c("npv", "precision","specificity","sensitivity","fpr","fnr","fdr", "accuracy", "threshold"))#threshold is optimized based on sum of sensitivity+specificity


