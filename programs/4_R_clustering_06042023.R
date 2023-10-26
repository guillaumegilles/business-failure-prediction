###########################
# Load required libraries
library(tidyverse)

# For correlation analysis
library(corrr)
library(ggcorrplot)
library(factoextra)

# For faster plotting
library(scattermore)

###########################
# Set working directory
setwd("set your working directory here")

# Load previous data from RDS file
res_kmeans <- readRDS("res_kmeans.RDS")

# Load data from RDS file
financial_data <- readRDS("df_clean.RDS")

## Extract some fields from financial data
df <- financial_data
df_clust <- select(df,c("year","age","equity_ratio","EBIT_ratio","OCF_ratio","levnet_ratio","sales_ratio","var_pop_dep","depinv_dep_avg"))

## Perform a correlation matrix on data
corr_matrix <- cor(df_clust)
ggcorrplot(corr_matrix)

## Perform kmeans with 5 centers and 1000xrandom init start
# Load RDS file for res_kmeans
#res_kmeans <- kmeans(df_clust,5,nstart=1000)
#saveRDS(res_kmeans,"res_kmeans.RDS")
res_kmeans_df <- data.frame(res_kmeans$cluster,df_clust)

###########################
## Function plots_gen
##

IND <- 0
REF <- 0

plots_gen <- function(REF) {
  IND <- 8*REF
  for(idx in 1:8){
    plot_i <- get(paste("plot",idx-1,sep=""))
    plots[[idx+IND]] <- plot_i
  }
  return(plots)
}

###########################
## Function df_extraction
##

df_extraction <- function(df_extract_fct,feature){
  df_extract_fct_q5 <- quantile(df_extract_fct[,feature],0.05)
  df_extract_fct_q95 <- quantile(df_extract_fct[,feature],0.95)
  df_extract_fct <- df_extract_fct[df_extract_fct[,feature]>=df_extract_fct_q5 & df_extract_fct[,feature]<=df_extract_fct_q95,]
  
  return(df_extract_fct)
}

##########################
# Start of plots
##########################
plots <- NULL
##########################
# Plot by year
##

df_extract <- res_kmeans_df[,c("year","age")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"year")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("year by age")
##
df_extract <- res_kmeans_df[,c("year","equity_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"year")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("year by equity_ratio")
##
df_extract <- res_kmeans_df[,c("year","EBIT_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"year")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("year by EBIT_ratio")
##
df_extract <- res_kmeans_df[,c("year","OCF_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"year")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("year by OCF_ratio")
##
df_extract <- res_kmeans_df[,c("year","levnet_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"year")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("year by levnet_ratio")
## !
df_extract <- res_kmeans_df[,c("year","sales_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"year")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("year by sales_ratio")
##
df_extract <- res_kmeans_df[,c("year","var_pop_dep")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"year")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("year by var_pop_dep")
##
df_extract <- res_kmeans_df[,c("year","depinv_dep_avg")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"year")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("year by depinv_dep_avg")

plots <- plots_gen(0)

##########################
# Plot by age
##

df_extract <- res_kmeans_df[,c("age","year")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"age")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("age by year")
##
df_extract <- res_kmeans_df[,c("age","equity_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"age")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("age by equity_ratio")
##
df_extract <- res_kmeans_df[,c("age","EBIT_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"age")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("age by EBIT_ratio")
##
df_extract <- res_kmeans_df[,c("age","OCF_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"age")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("age by OCF_ratio")
##
df_extract <- res_kmeans_df[,c("age","levnet_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"age")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("age by levnet_ratio")
## !
df_extract <- res_kmeans_df[,c("age","sales_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"age")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("age by sales_ratio")
##
df_extract <- res_kmeans_df[,c("age","var_pop_dep")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"age")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("age by var_pop_dep")
##
df_extract <- res_kmeans_df[,c("age","depinv_dep_avg")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"age")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("age by depinv_dep_avg")

plots <- plots_gen(1)

##########################
# Plot by equity_ratio
##

df_extract <- res_kmeans_df[,c("equity_ratio","year")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"equity_ratio")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("equity_ratio by year")
##
df_extract <- res_kmeans_df[,c("equity_ratio","age")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"equity_ratio")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("equity_ratio by age")
##
df_extract <- res_kmeans_df[,c("equity_ratio","EBIT_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"equity_ratio")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("equity_ratio by EBIT_ratio")
##
df_extract <- res_kmeans_df[,c("equity_ratio","OCF_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"equity_ratio")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("equity_ratio by OCF_ratio")
##
df_extract <- res_kmeans_df[,c("equity_ratio","levnet_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"equity_ratio")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("equity_ratio by levnet_ratio")
## !
df_extract <- res_kmeans_df[,c("equity_ratio","sales_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"equity_ratio")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("equity_ratio by sales_ratio")
##
df_extract <- res_kmeans_df[,c("equity_ratio","var_pop_dep")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"equity_ratio")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("equity_ratio by var_pop_dep")
##
df_extract <- res_kmeans_df[,c("equity_ratio","depinv_dep_avg")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"equity_ratio")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("equity_ratio by depinv_dep_avg")

plots <- plots_gen(2)

##########################
# Plot by EBIT_ratio
##

df_extract <- res_kmeans_df[,c("EBIT_ratio","year")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"EBIT_ratio")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("EBIT_ratio by year")
##
df_extract <- res_kmeans_df[,c("EBIT_ratio","age")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"EBIT_ratio")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("EBIT_ratio by age")
##
df_extract <- res_kmeans_df[,c("EBIT_ratio","equity_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"EBIT_ratio")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("EBIT_ratio by equity_ratio")
##
df_extract <- res_kmeans_df[,c("EBIT_ratio","OCF_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"EBIT_ratio")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("EBIT_ratio by OCF_ratio")
##
df_extract <- res_kmeans_df[,c("EBIT_ratio","levnet_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"EBIT_ratio")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("EBIT_ratio by levnet_ratio")
## !
df_extract <- res_kmeans_df[,c("EBIT_ratio","sales_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"EBIT_ratio")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("EBIT_ratio by sales_ratio")
##
df_extract <- res_kmeans_df[,c("EBIT_ratio","var_pop_dep")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"EBIT_ratio")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("EBIT_ratio by var_pop_dep")
##
df_extract <- res_kmeans_df[,c("EBIT_ratio","depinv_dep_avg")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"EBIT_ratio")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("EBIT_ratio by depinv_dep_avg")

plots <- plots_gen(3)

##########################
# Plot by OCF_ratio
##

df_extract <- res_kmeans_df[,c("OCF_ratio","year")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"OCF_ratio")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("OCF_ratio by year")
##
df_extract <- res_kmeans_df[,c("OCF_ratio","age")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"OCF_ratio")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("OCF_ratio by age")
##
df_extract <- res_kmeans_df[,c("OCF_ratio","equity_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"OCF_ratio")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("OCF_ratio by equity_ratio")
##
df_extract <- res_kmeans_df[,c("OCF_ratio","EBIT_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"OCF_ratio")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("OCF_ratio by EBIT_ratio")
##
df_extract <- res_kmeans_df[,c("OCF_ratio","levnet_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"OCF_ratio")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("OCF_ratio by levnet_ratio")
## !
df_extract <- res_kmeans_df[,c("OCF_ratio","sales_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"OCF_ratio")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("OCF_ratio by sales_ratio")
##
df_extract <- res_kmeans_df[,c("OCF_ratio","var_pop_dep")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"OCF_ratio")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("OCF_ratio by var_pop_dep")
##
df_extract <- res_kmeans_df[,c("OCF_ratio","depinv_dep_avg")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"OCF_ratio")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("OCF_ratio by depinv_dep_avg")

plots <- plots_gen(4)

##########################
# Plot by levnet_ratio
##

df_extract <- res_kmeans_df[,c("levnet_ratio","year")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"levnet_ratio")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("levnet_ratio by year")
##
df_extract <- res_kmeans_df[,c("levnet_ratio","age")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"levnet_ratio")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("levnet_ratio by age")
##
df_extract <- res_kmeans_df[,c("levnet_ratio","equity_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"levnet_ratio")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("levnet_ratio by equity_ratio")
##
df_extract <- res_kmeans_df[,c("levnet_ratio","EBIT_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"levnet_ratio")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("levnet_ratio by EBIT_ratio")
##
df_extract <- res_kmeans_df[,c("levnet_ratio","OCF_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"levnet_ratio")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("levnet_ratio by OCF_ratio")
## !
df_extract <- res_kmeans_df[,c("levnet_ratio","sales_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"levnet_ratio")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("levnet_ratio by sales_ratio")
##
df_extract <- res_kmeans_df[,c("levnet_ratio","var_pop_dep")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"levnet_ratio")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("levnet_ratio by var_pop_dep")
##
df_extract <- res_kmeans_df[,c("levnet_ratio","depinv_dep_avg")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"levnet_ratio")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("levnet_ratio by depinv_dep_avg")

plots <- plots_gen(5)

##########################
# Plot by sales_ratio
##

df_extract <- res_kmeans_df[,c("sales_ratio","year")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"sales_ratio")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("sales_ratio by year")
##
df_extract <- res_kmeans_df[,c("sales_ratio","age")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"sales_ratio")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("sales_ratio by age")
##
df_extract <- res_kmeans_df[,c("sales_ratio","equity_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"sales_ratio")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("sales_ratio by equity_ratio")
##
df_extract <- res_kmeans_df[,c("sales_ratio","EBIT_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"sales_ratio")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("sales_ratio by EBIT_ratio")
##
df_extract <- res_kmeans_df[,c("sales_ratio","OCF_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"sales_ratio")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("sales_ratio by OCF_ratio")
## !
df_extract <- res_kmeans_df[,c("sales_ratio","levnet_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"sales_ratio")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("sales_ratio by levnet_ratio")
##
df_extract <- res_kmeans_df[,c("sales_ratio","var_pop_dep")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"sales_ratio")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("sales_ratio by var_pop_dep")
##
df_extract <- res_kmeans_df[,c("sales_ratio","depinv_dep_avg")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"sales_ratio")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("sales_ratio by depinv_dep_avg")

plots <- plots_gen(6)

##########################
# Plot by var_pop_dep
##

df_extract <- res_kmeans_df[,c("var_pop_dep","year")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"var_pop_dep")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("var_pop_dep by year")
##
df_extract <- res_kmeans_df[,c("var_pop_dep","age")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"var_pop_dep")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("var_pop_dep by age")
##
df_extract <- res_kmeans_df[,c("var_pop_dep","equity_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"var_pop_dep")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("var_pop_dep by equity_ratio")
##
df_extract <- res_kmeans_df[,c("var_pop_dep","EBIT_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"var_pop_dep")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("var_pop_dep by EBIT_ratio")
##
df_extract <- res_kmeans_df[,c("var_pop_dep","OCF_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"var_pop_dep")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("var_pop_dep by OCF_ratio")
## !
df_extract <- res_kmeans_df[,c("var_pop_dep","sales_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"var_pop_dep")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("var_pop_dep by sales_ratio")
##
df_extract <- res_kmeans_df[,c("var_pop_dep","levnet_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"var_pop_dep")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("var_pop_dep by levnet_ratio")
##
df_extract <- res_kmeans_df[,c("var_pop_dep","depinv_dep_avg")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"var_pop_dep")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("var_pop_dep by depinv_dep_avg")

plots <- plots_gen(7)

##########################
# Plot by depinv_dep_avg
##

df_extract <- res_kmeans_df[,c("depinv_dep_avg","year")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"depinv_dep_avg")
plot0 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("depinv_dep_avg by year")
##
df_extract <- res_kmeans_df[,c("depinv_dep_avg","age")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"depinv_dep_avg")
plot1 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("depinv_dep_avg by age")
##
df_extract <- res_kmeans_df[,c("depinv_dep_avg","equity_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"depinv_dep_avg")
plot2 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("depinv_dep_avg by equity_ratio")
##
df_extract <- res_kmeans_df[,c("depinv_dep_avg","EBIT_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"depinv_dep_avg")
plot3 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("depinv_dep_avg by EBIT_ratio")
##
df_extract <- res_kmeans_df[,c("depinv_dep_avg","OCF_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"depinv_dep_avg")
plot4 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("depinv_dep_avg by OCF_ratio")
## !
df_extract <- res_kmeans_df[,c("depinv_dep_avg","sales_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"depinv_dep_avg")
plot5 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("depinv_dep_avg by sales_ratio")
##
df_extract <- res_kmeans_df[,c("depinv_dep_avg","levnet_ratio")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"depinv_dep_avg")
plot6 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("depinv_dep_avg by levnet_ratio")
##
df_extract <- res_kmeans_df[,c("depinv_dep_avg","var_pop_dep")]
df_extract_cluster_bind <- cbind(df_extract,res_kmeans$cluster)
df_extract <- df_extraction(df_extract_cluster_bind,"depinv_dep_avg")
plot7 <- ggplot() + geom_scattermost(df_extract,col=df_extract[,"res_kmeans$cluster"],pointsize=1) + ggtitle("depinv_dep_avg by var_pop_dep")

plots <- plots_gen(8)

##########################
# End of plots
##########################

##########################
### Plot saving

i <- 0
filename <- "plot_"
path <- paste(getwd(),"/scaled-p5-p95",sep="")
for(plot in plots){
  filename_i <- as.character(paste(filename,i,sep=""))
  filename_i <- as.character(paste(filename_i,".png",sep=""))
  i <- i+1
  print(filename_i)
  ggsave(filename=filename_i,plot=plot,path=path,device="png",width=1200,height=800,units="px",dpi="screen")
}

##########################
##########################
