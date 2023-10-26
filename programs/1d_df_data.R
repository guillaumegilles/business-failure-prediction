####################################
#
# Create final dataset
#
####################################

# Load required libraries
library(reshape2)
library(tidyverse)

# Set working directory
#setwd("//set/your/working/directory/here")

# Load data from RDS file
def <- readRDS("E:\\DataScientist\\project\\forecastingDEF\\data\\def_clean.RDS")
dim(def)
comptes_communes_final <- readRDS("E:\\DataScientist\\project\\forecastingDEF\\data\\comptes_communes_final.RDS")

# Get some information from financial data
summary(financial_data)

# Get some information from comptes_communes data
summary(comptes_communes_final)

# Perform a left join on financial dataset using the comptes_communes_final dataset
df <- def %>% 
  left_join(comptes_communes_final, by = c("year","dpt"))

df$dpt <- as.factor(df$dpt)

# Get some information from df data
str(df)
dim(df)
summary(df)
head(df)

View(df[1:100,])

saveRDS(df,"E:\\DataScientist\\project\\forecastingDEF\\data/df_clean.RDS")
