# Load required libraries
library(tidyverse)

###########################

# Set working directory
setwd("< set your /working/directory/here >")

# Load data from RDS file
financial_data <- readRDS("df_clean.RDS")

## Extract some fields from financial data
df <- financial_data
df_clust <- select(df,c("YP","age","equity_ratio","EBIT_ratio","OCF_ratio","levnet_ratio","sales_ratio","var_pop_dep","depinv_dep_avg"))

## Scale variables of df_clust
df_clust_scaled <- scale(df_clust)

## Perform kmeans with 20 centers and 10xrandom init start
res_kmeans_scaled <- kmeans(df_clust_scaled,20,iter.max = 100,nstart=10,algorithm="MacQueen")
res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df_clust_scaled)
res_kmeans_df <- rename(res_kmeans_df, "cluster"="res_kmeans_scaled.cluster")

## Perform kmeans with 12 centers and 10xrandom init start
res_kmeans_scaled <- kmeans(df_clust_scaled,12,iter.max = 100,nstart=10,algorithm="MacQueen")
res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df_clust_scaled)
res_kmeans_df <- rename(res_kmeans_df, "cluster"="res_kmeans_scaled.cluster")

## Perform kmeans with 7 centers and 10xrandom init start
res_kmeans_scaled <- kmeans(df_clust_scaled,7,iter.max = 100,nstart=10,algorithm="MacQueen")
res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df_clust_scaled)
res_kmeans_df <- rename(res_kmeans_df, "cluster"="res_kmeans_scaled.cluster")

## Perform kmeans with 6 centers and 10xrandom init start
res_kmeans_scaled <- kmeans(df_clust_scaled,6,iter.max = 100,nstart=10,algorithm="MacQueen")
res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df_clust_scaled)
res_kmeans_df <- rename(res_kmeans_df, "cluster"="res_kmeans_scaled.cluster")

## Perform kmeans with 5 centers and 10xrandom init start
res_kmeans_scaled <- kmeans(df_clust_scaled,5,iter.max = 100,nstart=10,algorithm="MacQueen")
res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df_clust_scaled)
res_kmeans_df <- rename(res_kmeans_df, "cluster"="res_kmeans_scaled.cluster")

## Perform kmeans with 4 centers and 10xrandom init start
res_kmeans_scaled <- kmeans(df_clust_scaled,4,iter.max = 100,nstart=10,algorithm="MacQueen")
res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df_clust_scaled)
res_kmeans_df <- rename(res_kmeans_df, "cluster"="res_kmeans_scaled.cluster")

## Perform kmeans with 3 centers and 10xrandom init start
res_kmeans_scaled <- kmeans(df_clust_scaled,3,iter.max = 100,nstart=10,algorithm="MacQueen")
res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df_clust_scaled)
res_kmeans_df <- rename(res_kmeans_df, "cluster"="res_kmeans_scaled.cluster")

## Perform kmeans with 2 centers and 10xrandom init start
res_kmeans_scaled <- kmeans(df_clust_scaled,2,iter.max = 100,nstart=10,algorithm="MacQueen")
res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df_clust_scaled)
res_kmeans_df <- rename(res_kmeans_df, "cluster"="res_kmeans_scaled.cluster")

#final_res_kmeans_df <- data.frame(res_kmeans_scaled$cluster,df)

##########################

IND <- 0
REF <- 0

plots_gen <- function(REF) {
  IND <- 9*REF
  for(idx in 1:9){
    plot_i <- get(paste("plot",idx-1,sep=""))
    plots[[idx+IND]] <- plot_i
  }
  return(plots)
}

##########################
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
# Boxplots
##

## List of variables to plot:
## "YP","age","equity_ratio","EBIT_ratio","OCF_ratio","levnet_ratio","sales_ratio","var_pop_dep","depinv_dep_avg"

df_extract <- res_kmeans_df
df_extract[,"cluster"] <- as.factor(df_extract[,"cluster"])
#summary(df_extract)

#ggplot(df_extract,aes(x=df_extract$cluster,y=df_extract$age, fill=df_extract$cluster)) + geom_boxplot() + ggtitle("YP by cluster")
#plot0 <- 

## YP
plot0 <- ggplot(df_extract,aes(x=cluster,y=YP, fill=cluster)) + geom_boxplot() + ggtitle("YP by cluster")

# plot0 <- ggplot(df_extract,aes(x=cluster,y=YP, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("YP by cluster")

## age
plot1 <- ggplot(df_extract,aes(x=cluster,y=age, fill=cluster)) + geom_boxplot() + ggtitle("age by cluster")

# plot1 <- ggplot(df_extract,aes(x=cluster,y=age, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("age by cluster")

## equity_ratio
plot2 <- ggplot(df_extract,aes(x=cluster,y=equity_ratio, fill=cluster)) + geom_boxplot() + ggtitle("equity_ratio by cluster")

# plot2 <- ggplot(df_extract,aes(x=cluster,y=equity_ratio, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("equity_ratio by cluster")

## EBIT_ratio
plot3 <- ggplot(df_extract,aes(x=cluster,y=EBIT_ratio, fill=cluster)) + geom_boxplot() + ggtitle("EBIT_ratio by cluster")

# plot3 <- ggplot(df_extract,aes(x=cluster,y=EBIT_ratio, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("EBIT_ratio by cluster")

## OCF_ratio
plot4 <- ggplot(df_extract,aes(x=cluster,y=OCF_ratio, fill=cluster)) + geom_boxplot() + ggtitle("OCF_ratio by cluster")

# plot4 <- ggplot(df_extract,aes(x=cluster,y=OCF_ratio, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("OCF_ratio by cluster")

## levnet_ratio
plot5 <- ggplot(df_extract,aes(x=cluster,y=levnet_ratio, fill=cluster)) + geom_boxplot() + ggtitle("levnet_ratio by cluster")

# plot5 <- ggplot(df_extract,aes(x=cluster,y=levnet_ratio, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("levnet_ratio by cluster")

## sales_ratio
plot6 <- ggplot(df_extract,aes(x=cluster,y=sales_ratio, fill=cluster)) + geom_boxplot() + ggtitle("sales_ratio by cluster")

# plot6 <- ggplot(df_extract,aes(x=cluster,y=sales_ratio, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("sales_ratio by cluster")

## var_pop_dep
plot7 <- ggplot(df_extract,aes(x=cluster,y=var_pop_dep, fill=cluster)) + geom_boxplot() + ggtitle("var_pop_dep by cluster")

# plot7 <- ggplot(df_extract,aes(x=cluster,y=var_pop_dep, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("var_pop_dep by cluster")

## depinv_dep_avg
plot8 <- ggplot(df_extract,aes(x=cluster,y=depinv_dep_avg, fill=cluster)) + geom_boxplot() + ggtitle("depinv_dep_avg by cluster")

# plot8 <- ggplot(df_extract,aes(x=cluster,y=depinv_dep_avg, fill=cluster)) + geom_boxplot() +   stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
#   stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
#                vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1)))) + ggtitle("depinv_dep_avg by cluster")

plots <- plots_gen(0)

##########################
# End of boxplots
##########################

##########################
### Plot saving

i <- 0
filename <- "plot_"
path <- paste(getwd(),"/clustering",sep="")
for(plot in plots){
  filename_i <- as.character(paste(filename,i,sep=""))
  filename_i <- as.character(paste(filename_i,".png",sep=""))
  i <- i+1
  print(filename_i)
  ggsave(filename=filename_i,plot=plot,path=path,device="png",width=1200,height=800,units="px",dpi="screen")
}
