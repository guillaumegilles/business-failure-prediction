####################################
#
# Clustering
#
####################################

if (!require(tidyverse)) install.packages("tidyverse")
library(Factoshiny)


#%%%%%%%%%%%%%%%%%%%%%%%

#load cleaned data
df=readRDS("E:\\DataScientist\\project\\forecastingDEF\\data/def_clean.RDS") 

#for clustering analysis, drop Y
df_cov <- select(df,-"Y")


#%%%%%%%%%%%%%%%%%%%%%%%

#tutor suggested we only use some variables for clustering:
#1.	Year
#2.	Age
#3.	Equity_ratio (pour mémoire= capitaux propres / total bilan)
#4.	EBIT_ratio (pour mémoire= résultat d'exploitation ou production / total bilan)
#5.	OCF_ratio (pour mémoire= capacité d'autofinancement / total de bilan)
#6.	Levnet_ratio (pour mémoire= dette financière / total bilan)
#7.	Sales_ratio (pour mémoire= chiffre d'affaires / total bilan)
#8.	var_pop_dep (pour mémoire= ln du pop du dept_year t - ln du pop du dept_year t-1 )
#9.	depinv_dep_avg (pour mémoire=investissement du dept sur population)






#%%%%%%%%%%%%%%%%%%%%%%%

#previous attempts on subsamples

df_cov_s10k=sample_n(df_cov,10000)
df_cov_s10k <- select(df_cov_s10k,-"dpt")

Factoshiny(df_cov_s10k)

clus_kmeans_s10k <- kmeans(df_cov,centers = 4)
#Error in do_one(nmeth) : NA/NaN/Inf in foreign function call (arg 1)
#In addition: Warning message:
  #In storage.mode(x) <- "double" : NAs introduced by coercion

df_covq <- select(df_cov,-"dpt")
df_covq <- select(df_covq,-"sect")
df_covq <- select(df_covq,-"size")
df_covq <- select(df_covq,-"year")

clus_kmeans_s10k <- kmeans(df_covq,centers = 4)


#loop to look for a good number of centers
nb=20#start with 20 groups
intra=1:nb
inter=1:nb
for(ii in 1:nb) {
  tmp <- kmeans(df_covq,centers = ii, nstart=10)
  intra[ii] <- tmp$tot.withinss 
  inter[ii] <- tmp$betweenss
}

intra=intra/tmp$totss*100 #adds to table intra the denominator. Now I see the intra-inertia that exists with 1 group (by definition all of it), with 2 groups, etc
inter=inter/tmp$totss*100

plot(intra,type="b") 
lines(inter,col=2,type="b")
#intra in black and inter in red
#index means number of groups

plot(inter,type="h")
#maybe about 7 groups



