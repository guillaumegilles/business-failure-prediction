####################################
#
# Estimating Boosting on dataset with lags
#
####################################

##to load a package and install it in case it's not already:
if (!require(glmnet)) install.packages("glmnet")
if (!require(rpart)) install.packages("rpart")
if (!require(randomForest)) install.packages("randomForest")
if (!require(pROC)) install.packages("pROC")
#if (!require(plm)) install.packages("plm")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(gbm)) install.packages("gbm")
if (!require(xgboost)) install.packages("xgboost")
if (!require(e1071)) install.packages("e1071")
if (!require(esquisse)) install.packages("esquisse")


####################################

#load cleaned data

don_all=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df_lags_clean.RDS") 

####################################

#fix covariates

colnames(don_all)
str(don_all)

don_all$year <- as.factor(don_all$year)
#don_all$region <- as.factor(don_all$region)
#don_all$sect_NACE1 <- as.factor(don_all$sect_NACE1)


#!for the moment I have issues with too many categories so I just drop them
don_all <- select(don_all,-"dpt")
#don_all <- select(don_all,-"region")
#don_all <- select(don_all,-"sect_NACE1")
don_all <- select(don_all,-"sect_NACE2")
don_all <- select(don_all,-"sect_NACE3")
don_all <- select(don_all,-"sect_NACE4")
don_all <- select(don_all,-"sect_NACE5")
don_all <- select(don_all,-"year")


dim(don_all)
#3315925     164

colnames(don_all)
str(don_all)
table(don_all$Y)


#%%%%%%%%%%%%%%%%%%%%%%%%%

#previous subsamples:


#keeps many failures
#don1 <- don_all %>%
 # filter(Y==1)  %>%
  #sample_n(5000)

#keeps some non failures  
#don0 <- don_all %>%
 # filter(Y==0) %>%
  #sample_n(10000)

#don <- don1 %>%
 # bind_rows(don0)


#or don=sample_n(don_all,500)

#or: don <- don %>% sample_frac(0.001)

#or I could by restricting data to one year (2019) but 300k
#don <- filter(don_all, year==2019)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

don <- don_all
dim(don)
str(don)


table(don$Y)
prop.table(table(as.numeric(as.character(don$Y))))#0.009371442%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#cut out test set

n <- nrow(don) 
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
don_train <- don[obs_train,]
nrow(don_train) # [N.B. length for a dataset gives the number of variables]

#build the TEST set for validation with the remaining obs from the whole dataset
don_test <- don[-obs_train,]
nrow(don_test) 


don_train$Y <- as.numeric(don_train$Y)-1 #-1 because once transformed in factor it stocks in 1,2
don_test$Y <- as.numeric(don_test$Y)-1 #-1 because once transformed in factor it stocks in 1,2


resTest_boosting <- data.frame(Y=don_test$Y)
#resRF$Y <- as.factor(resRF$Y)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###Estimation of Boosting models

#adaboost

gbm_ada <- gbm(Y~., data=don_train, distribution="adaboost")
resTest_boosting$adaboost  <- predict(gbm_ada, newdata=don_test, type="response")

summary(resTest_boosting$adaboost )


#xgboost  
train_x = data.matrix(select(don_train,-Y))
train_y = don_train$Y
test_x = data.matrix(select(don_test,-Y))
test_y = don_test$Y
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)
watchlist = list(train=xgb_train,test=xgb_test)
algoxgt <- xgb.train(data = xgb_train, max.depth = 1, watchlist=watchlist, nrounds = 2000,eta=0.3, verbose=0)
iter <- which.min(algoxgt$evaluation_log$test_rmse)
xgboost <- xgboost(data = xgb_train,eta=0.3,max.depth = 1, nrounds = iter, verbose = 1)
resTest_boosting$xgboost <- predict(xgboost,xgb_test, type="prob")

summary(resTest_boosting$xgboost)
resTest_boosting$xgboost[resTest_boosting$xgboost <0] <- 0
resTest_boosting$xgboost[resTest_boosting$xgboost >1] <- 1


saveRDS(resTest_boosting,"E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/df_resTest_DatasetLags_boosting.RDS")
  

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


#resTest_boosting=readRDS("E:\\DataScientist\\project\\forecastingDEF\\results\\3_ModelSelection/df_resTest_DatasetLags_boosting.RDS") 
#resRF$prev_logit <- (resRF$prev_proba_logit>0.01)*1

prop.table(table(as.numeric(resTest_boosting$Y)))#0.009104535%


##ROC: relation between specificity (prob of true negatives) and sensitivity (probability of true positives)
apply(resTest_boosting[,-1],2,auc,response=resTest_boosting$Y)
#I want it large (it should be above 0.5)


#or one by one to get the performance of different algos in a plot:
rocada <- roc(resTest_boosting$Y,resTest_boosting$adaboost)#response, then predictor
auc(rocada)#gives value of AUC=area under ROC curve: 0.861
plot(rocada)#graph of ROC

rocxgb<- roc(resTest_boosting$Y,resTest_boosting$xgboost)
auc(rocxgb) #.8683
plot(rocxgb, add=T,col=2)


##To get several performance measures for an algo with an arbitrary threshold:  
coords(rocada,x=0.0114,ret=c("npv", "precision","specificity","sensitivity","fpr","fnr","fdr", "accuracy", "threshold"))#arbitrary threshold
coords(rocxgb,x=0.0114,ret=c("npv", "precision","specificity","sensitivity","fpr","fnr","fdr", "accuracy", "threshold"))#arbitrary threshold

##I could choose threshold that predicts same percentage of 1 as in sample:
plot(1:length(resTest_boosting$Y),sort(resTest_boosting$rocada))
#abline(v=0.0114)


##To get specificity and sensitivity with the best threshold (based on sum of sensitivity+specificity), for each algo:
besthreshold_rocada <- coords(rocada,x="best")#threshold is optimized based on sum of sensitivity+specificity
besthreshold_rocada
#besthreshold_rocada$threshold (here 0.010)

besthreshold_rocxgb <- coords(rocxgb,x="best")#threshold is optimized based on sum of sensitivity+specificity
besthreshold_rocxgb
#besthreshold_rocxgb$threshold (here 0.018)


##To get several performance measures for an algo with the best threshold (based on sum of sensitivity+specificity), for each model:
coords(rocada,x="best",ret=c("npv", "precision","specificity","sensitivity","fpr","fnr","fdr", "accuracy", "threshold"))#threshold is optimized based on sum of sensitivity+specificity
coords(rocxgb,x="best",ret=c("npv", "precision","specificity","sensitivity","fpr","fnr","fdr", "accuracy", "threshold"))#threshold is optimized based on sum of sensitivity+specificity

