####################################
#
# Estimating Boosting on dataset with lags
#
####################################

##to load a package and install it in case it's not already:
#if (!require(glmnet)) install.packages("glmnet")
#if (!require(rpart)) install.packages("rpart")
#if (!require(randomForest)) install.packages("randomForest")
if (!require(pROC)) install.packages("pROC")
#if (!require(plm)) install.packages("plm")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(gbm)) install.packages("gbm")
if (!require(xgboost)) install.packages("xgboost")
#if (!require(e1071)) install.packages("e1071")
#if (!require(esquisse)) install.packages("esquisse")


####################################

#load cleaned data

don_all=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df2022_lags_clean.RDS") 

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
#410049    164

colnames(don_all)
str(don_all)
table(don_all$Y)
prop.table(table(as.numeric(as.character(don_all$Y))))#0.03388619%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

don_all$Y <- as.numeric(don_all$Y)-1 #-1 because once transformed in factor it stocks in 1,2


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###Estimation of best Boosting models

gbm_logit_2000iter_03shr <- gbm(Y~., data=don_all, distribution="bernoulli",n.trees = 2000,shrinkage = 0.3)

saveRDS(gbm_logit_2000iter_03shr,"E:\\DataScientist\\project\\forecastingDEF\\results\\5_FinalModel/df_DatasetCrossLags_final_boosting.RDS")
  
