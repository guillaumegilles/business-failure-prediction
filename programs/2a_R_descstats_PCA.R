####################################
#
# Descriptive Stats
#
####################################

getwd()
setwd("E:/DataScientist/R/Rwork")


#######################################

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(outliers)) install.packages("outliers")
if (!require(DescTools)) install.packages("DescTools")
if (!require(DataExplorer)) install.packages("DataExplorer")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#load cleaned data
df=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/df_clean.RDS") 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###Check dataset

df$year <- as.factor(df$year)
#def_des <- select(def,-"year")

plot_intro(df)
#nice to visualise characteristics of dataset

plot_missing(df)
#see where missing are

str(df)
#create_report(def_des)
create_report(df)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##descriptive stats

#plot_bar(def)
plot_bar(df$size)

#plot_qq(def)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##correlations

library(FactoMineR)
library(Factoshiny)
library(FactoInvestigate)



#for PCA analysis, drop Y
df_cov <- select(df,-"Y")


#drop categorical covariates
str(df_cov)
#in theory it would have been nice to leave the identifiers but too many categories
df_covq <- select(df_cov,-"dpt")
df_covq <- select(df_covq,-"region")
df_covq <- select(df_covq,-"sect_NACE1")
df_covq <- select(df_covq,-"sect_NACE2")
df_covq <- select(df_covq,-"sect_NACE3")
df_covq <- select(df_covq,-"sect_NACE4")
df_covq <- select(df_covq,-"sect_NACE5")
df_covq <- select(df_covq,-"size")
df_covq <- select(df_covq,-"year")

str(df_covq)

plot_correlation(na.omit(df_covq), maxcat = 5L)#correlation matrix among variables to decide redundant ones


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#visualize the distribution of all continuous variables based on a categorical one with a boxplot
plot_boxplot(df_cov, by = "size")



#Pearson's chi-squared test is a statistical test applied to sets of categorical data to evaluate how likely it is that any observed difference between the sets arose by chance. 
#If the test statistic exceeds the critical value of O2, the null hypothesis (H_{0} = there is no difference between the distributions) can be rejected, 
#and the alternative hypothesis (H_{1} = there is a difference between the distributions) can be accepted
#If the estimated data in any given cell is below 5, then there is not enough data to perform a Chi-square test.
#p-value of 0.0455 means a 4.6% likelihood that the null hypothesis is correct. To put it best, if the distribution of this data is due entirely to chance, then you have a 4.6% chance of finding a discrepancy between the observed and expected distributions that is at least this extreme.
#The reason we want a low one in this case is because we are trying to disprove the hypothesis H0 that the variables are independent.

#res <- chisq.test(df)
#res
#names(res)


#############################################

##PCA

#for PCA analysis, drop missings (but I already have none)
#pca_df <- na.omit(def_covq)

#for PCA analysis, standardize variables (but PCAshiny does it)
PCAshiny(def_covq)

#PCAshiny allows for qualitative variables
PCAshiny(def_cov)


#as it doesn't run even only with quantitative, I try subsample. Does not work with 100k. 
#It does work with 10k
def_covq_s=sample_n(df_covq,10000)
PCAshiny(df_covq_s)

plot_prcomp(df_covq_s, variance_cap = 0.9, nrow = 2L, ncol = 2L)#relative importance of variables in principal component analysis

#It does work, even with categorical (except dept) with 10k
def_cov_s10k=sample_n(df_cov,10000)
def_cov_s10k <- select(df_cov_s10k,-"dpt")
PCAshiny(df_cov_s10k)

def_cov_s15k=sample_n(df_cov,15000)
def_cov_s15k <- select(df_cov_s15k,-"dpt")
PCAshiny(df_cov_s15k)

def_cov_s50k=sample_n(df_cov,50000)
def_cov_s50k <- select(df_cov_s50k,-"dpt")
PCAshiny(df_cov_s50k)









