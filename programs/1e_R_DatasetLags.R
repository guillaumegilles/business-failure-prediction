####################################
#
# Create final dataset with lags
#
####################################

getwd()
setwd("E:/DataScientist/R/Rwork")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#if (!require(foreign)) install.packages("foreign")
#if (!require(haven)) install.packages("haven")
if (!require(tidyverse)) install.packages("tidyverse")
#if (!require(dplyr)) install.packages("dplyr")
#if (!require(lubridate)) install.packages("lubridate")
#if (!require(outliers)) install.packages("outliers")
#if (!require(DescTools)) install.packages("DescTools")
#if (!require(DataExplorer)) install.packages("DataExplorer")
if (!require(timetk)) install.packages("timetk")
#if (!require(dtplyr)) install.packages("dtplyr")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#def <- select(def,-"id_hashed")

# Load data from RDS file
def <- readRDS("E:\\DataScientist\\project\\forecastingDEF\\data\\def_id_clean.RDS")
dim(def)
comptes_communes_final <- readRDS("E:\\DataScientist\\project\\forecastingDEF\\data\\comptes_communes_final.RDS")

# Perform a left join on financial dataset using the comptes_communes_final dataset
df <- def %>% 
  left_join(comptes_communes_final, by = c("year","dpt"))

df$dpt <- as.factor(df$dpt)

str(df)
#View(df[1:100,])


#df$year <- as.factor(df$year)



########################################################à

##prepare alternative dataset with some variable lagged a few years 

#df <-df[1:10000,]


#%%%

#this one works but is too long


#df <- df %>%
 # group_by(id_hashed) %>%
 # slice_tail(n = 5) %>%
 # ungroup()

#dim(df) #2287328 obs      

#df <- df %>%
 # select(where(is.factor), everything())
#str(df)

#df_factor <-df[,1:9]
#df <-df[,10:length(names(df))]


#for(varnamez in names(df)[3:length(names(df))]){
#  print(varnamez)
#  df <- df %>% 
#    group_by(id_hashed) %>%
#   tk_augment_lags(varnamez, .lags = 1:4)%>% 
#    ungroup()  
#}


#df_lagged <- cbind(df,df_factor)
#glimpse(df_lagged)
#dim(df)

#saveRDS(df,"E:\\DataScientist\\project\\forecastingDEF\\data/df_lags_cleaned0.RDS")


#%%%

multilag <- function(data, lag_level) {
  data %>%
    group_by(id_hashed) %>% 
    # on applique lag sur les colonnes numeric, sauf year
    mutate(across(c(where(is.numeric),-year), ~lag(., lag_level))) %>%
    # on renomme les colonnes répondant aux mêmes conditions avec un suffixe _lag_n
    rename_with(~ paste0(., "_lag_", lag_level), c(where(is.numeric),-year)) 
}


# on init une liste de df
df_list = list()
a = Sys.time()
## ii = nb de lags à appliquer, ici 1 à 4
for(ii in 1:4){
  print(ii)
  df_list[[ii]] <- multilag(df, lag_level = ii) %>% 
    # dans les df temp de la liste, on garde uniquement les variables lagged
    keep(is.numeric) %>%
    # et on vire aussi year pour ne pas l'avoir ii fois ensuite
    select(-year)
}

# on attache les colonnes ensemble, avec df au début
df_lagged <- bind_cols(df, df_list)
b = Sys.time()

print("Time = ", b-a)

#saveRDS(object = df_lagged, file = "C:/temp/projet_nico/df_clean_lagged.RDS")
#df_lagged <- readRDS("E:\\DataScientist\\project\\forecastingDEF\\data\\df_clean_lagged.RDS")

df_lagged %>% dim() #5603253     171
df_lagged %>% str()

df_noNA <- df_lagged %>%
  na.omit() # remove rows with missing values due to lagged variables

dim(df_noNA) #3315925     171

df200420 <- df_noNA %>% filter(year>2003) 

dim(df200420)#3315537     171

prop.table(table(as.numeric(df200420$Y))) # 0.009370729%

df_noId <- select(df_noNA,-"id_hashed")
saveRDS(df_noId,"E:\\DataScientist\\project\\forecastingDEF\\data/df_lags_clean.RDS")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#datasets with one obs per firm

df2022 <- df %>%
  group_by(id_hashed) %>%
  slice_tail(n = 1) %>%
  ungroup()

dim(df2022)

df2022 <- select(df2022,-"id_hashed")
                     
#df$year <- as.factor(df$year)   

                     
saveRDS(df2022,"E:\\DataScientist\\project\\forecastingDEF\\data/df2022_lags_clean.RDS")


#N.B. the proportion of defaillances is higher in this dataset with lags than in panel one
prop.table(table(as.numeric(df2022$Y))) # 0.03388619%

#indeed in the last obs of firms before last year I have a lot of defaillances (and some firms just disappearing)
df_nodef2022 = df2022 %>% filter(year<2020)
prop.table(table(as.numeric(df_nodef2022$Y))) # 0.07300476%

#and more than compensate the very low defaillance rate of the last year
df_def2022 = df2022 %>% filter(year==2020)
prop.table(table(as.numeric(df_def2022$Y))) # 0.005640132%


#%%%

#dataset for backtesting:

df2021 <- df %>%
  filter(year<2020) %>%
  group_by(id_hashed) %>%
  slice_tail(n = 1) %>%
  ungroup()
df2021 <- select(df2021,-"id_hashed")
#df$year <- as.factor(df$year)   
prop.table(table(as.numeric(df2021$Y))) # %
saveRDS(df2021,"E:\\DataScientist\\project\\forecastingDEF\\data/df2021_lags_clean.RDS")

df2020 <- df %>%
  filter(year<2019) %>%
  group_by(id_hashed) %>%
  slice_tail(n = 1) %>%
  ungroup()
df2020 <- select(df2020,-"id_hashed")
#df$year <- as.factor(df$year)   
prop.table(table(as.numeric(df2020$Y))) # %
saveRDS(df2020,"E:\\DataScientist\\project\\forecastingDEF\\data/df2020_lags_clean.RDS")

df2019 <- df %>%
  filter(year<2018) %>%
  group_by(id_hashed) %>%
  slice_tail(n = 1) %>%
  ungroup()
df2019 <- select(df2019,-"id_hashed")
#df$year <- as.factor(df$year)   
prop.table(table(as.numeric(df2019$Y))) # 0.03514474%
saveRDS(df2019,"E:\\DataScientist\\project\\forecastingDEF\\data/df2019_lags_clean.RDS")

df2018 <- df %>%
  filter(year<2017) %>%
  group_by(id_hashed) %>%
  slice_tail(n = 1) %>%
  ungroup()
df2018<- select(df2018,-"id_hashed")
#df$year <- as.factor(df$year) 
prop.table(table(as.numeric(df2018$Y))) # 0.03453526%
saveRDS(df2018,"E:\\DataScientist\\project\\forecastingDEF\\data/df2018_lags_clean.RDS")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
