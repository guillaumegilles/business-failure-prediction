####################################
#
# Exploring usefulness of categorical variables with many categories
#
####################################


if (!require(visNetwork)) install.packages("visNetwork")
if (!require(sparkline)) install.packages("sparkline")
if (!require(remotes)) install.packages("remotes")
if (!require(rpart)) install.packages("rpart")
if (!require(tidyverse)) install.packages("tidyverse")



don_all=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df_clean.RDS") 


don=sample_n(don_all,15000)

dim(don)

#model_dpt = rpart(Y~dpt,data=don)
#visNetwork::visTree(model_dpt,fallenLeaves =T)

#model_size = rpart(Y~size,data=don)
#visNetwork::visTree(model_size,fallenLeaves =T)

model = rpart(Y~.,data=don,cp=0.001 )
#model = rpart(Y~.,data=don_all,cp=0.001 ) #this runs but below it does no show a tree

visNetwork::visTree(model,fallenLeaves =T)
#N.B. to know which variables are important: it is not important what variable is on the top, but how many times it appears.
#In particular, I look whether departement and sectors with many categories are used by the tree or not

model$variable.importance
#N.B here I have the ranking of importance of variables. However, trees are not very stable wrt to random forest.
