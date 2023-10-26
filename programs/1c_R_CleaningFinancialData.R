####################################
#
# Financial data
#
####################################

getwd()
setwd("E:/DataScientist/R/Rwork")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (!require(foreign)) install.packages("foreign")
if (!require(haven)) install.packages("haven")
if (!require(tidyverse)) install.packages("tidyverse")
#if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(outliers)) install.packages("outliers")
if (!require(DescTools)) install.packages("DescTools")
if (!require(DataExplorer)) install.packages("DataExplorer")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##upload financial data

def <- read_sas("E:\\DataScientist\\project\\forecastingDEF\\data/fiben_2000_2020_def_2002_2022.sas7bdat")
def <- as.data.frame(def)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dim(def) 
#6 millions obs: 5738381            

#check no duplicates:
def <- distinct(def) 
dim(def)
#5738381 obs  


head(def)

View(def[1:100,])

colnames(def)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Target variable

sum(!is.na(def$def))
#65096 defaillances, so about 1%

def$Y <- replace(def$def, is.na(def$def),0)
table(def$Y)
def$Y <- as.factor(def$Y)
summary(def$Y)
#plot_bar(def$Y)

def <- select(def,-"def")
def <- select(def,-"year_def")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###Explore dataset

plot_intro(def)#nice to visualise characteristics of dataset

plot_missing(def)#see where missing are


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##quantitative variables
summary(def)


##all the things we don't do with machine learning:

#%winsoring
#def$equity <- Winsorize(def$equity,minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE, type = 7)

#%logs of skewed covariates

#%compute ratios

#%fixing outliers with log transformations (e.g. totalasset, equity) 


##deal with missing values:

summary(def$grossProfitMargin_delta)
summary(def$RCAI_ratio_delta)
summary(def$grossProfitMargin)
summary(def$costInput_ratio)
summary(def$RCAI_ratio)
summary(def$KIntensity_ratio)
summary(def$debtfin_deltaln)
summary(def$cashHoldings_deltaln)
summary(def$sales_deltaln)
summary(def$tauxMarge_delta)
summary(def$tauxMarge)
summary(def$DPO)
summary(def$OCF_ratio_delta)
summary(def$OCF_ratio)
summary(def$equity_ratio)
summary(def$levnet_ratio)
summary(def$finexp_ratio)
summary(def$WK_ratio)
summary(def$EBIT_ratio)
summary(def$retainedearnings_ratio)
summary(def$sales_ratio)
summary(def$equity_totliab_ratio)


#def_nomissing <- set_missing(def, list(0L, "unknown"))#missing are put to 0 or unknown


#missing to 0: YP, grossProfitMargin_delta, RCAI_ratio_delta, grossProfitMargin, RCAI_ratio, costInput_ratio, KIntensity_ratio, tauxMarge_delta, OCF_ratio_delta
def <- def %>%   
  mutate(YP = replace_na(YP,0),
        grossProfitMargin_delta=replace_na(grossProfitMargin_delta,0),
        RCAI_ratio_delta=replace_na(RCAI_ratio_delta,0),
        grossProfitMargin=replace_na(grossProfitMargin,0),
        RCAI_ratio=replace_na(RCAI_ratio,0),
        costInput_ratio=replace_na(costInput_ratio,0),
        KIntensity_ratio=replace_na(KIntensity_ratio,0),
        tauxMarge_delta=replace_na(tauxMarge_delta,0),
        OCF_ratio_delta=replace_na(OCF_ratio_delta,0),
        debtfin_deltaln=replace_na(debtfin_deltaln,0),
        cashHoldings_deltaln=replace_na(cashHoldings_deltaln,0),
        sales_deltaln=replace_na(sales_deltaln,0)
  )

#missing to mean: age, debtfin_deltaln, cashHoldings_deltaln, sales_deltaln, tauxMarge, DPO, OCF_ratio, equity_ratio, finexp_ratio
def <- def %>%
  mutate(age=replace_na(age,mean(age, na.rm = TRUE)), 
         tauxMarge=replace_na(tauxMarge,mean(tauxMarge, na.rm = TRUE)),
         DPO=replace_na(DPO,mean(DPO, na.rm = TRUE)),
         OCF_ratio=replace_na(OCF_ratio,mean(OCF_ratio, na.rm = TRUE)),
         equity_ratio=replace_na(equity_ratio,mean(equity_ratio, na.rm = TRUE)),
         levnet_ratio=replace_na(levnet_ratio,mean(levnet_ratio, na.rm = TRUE)),
         finexp_ratio=replace_na(finexp_ratio,mean(finexp_ratio, na.rm = TRUE)),
         WK_ratio=replace_na(WK_ratio,mean(WK_ratio, na.rm = TRUE)),
         EBIT_ratio=replace_na(EBIT_ratio,mean(EBIT_ratio, na.rm = TRUE)),
         retainedearnings_ratio=replace_na(retainedearnings_ratio,mean(retainedearnings_ratio, na.rm = TRUE)),
         sales_ratio=replace_na(sales_ratio,mean(sales_ratio, na.rm = TRUE)),
         equity_totliab_ratio=replace_na(equity_totliab_ratio,mean(equity_totliab_ratio, na.rm = TRUE))
  )

plot_missing(def)#see where missing are


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##qualitative var

#declare factors

#create different variables for sectors from NACE1 to NACE5
def$sect_NACE2 <- substr(def$sector, 1, 2)
def$sn <- as.numeric(def$sect_NACE2)
def$sect_NACE2 <- as.factor(def$sect_NACE2)
barplot(summary(def$sect_NACE2),horiz=TRUE)
table(def$sect_NACE2)


def$sect_NACE1 <- ifelse(def$sn <= 3, "A", 
                   ifelse((def$sn >= 5) & (def$sn <= 9), "B", 
                    ifelse((def$sn >= 10) & (def$sn <= 33), "C", 
                      ifelse(def$sn == 35, "D", 
                       ifelse((def$sn >= 36) & (def$sn <= 39), "E", 
                        ifelse((def$sn >= 41) & (def$sn <= 43), "F", 
                          ifelse((def$sn >= 45) & (def$sn <= 47), "G", 
                            ifelse((def$sn >= 49) & (def$sn <= 53), "H", 
                             ifelse((def$sn >= 55) & (def$sn <= 56), "I", 
                              ifelse((def$sn >= 58) & (def$sn <= 63), "J", 
                                ifelse((def$sn >= 64) & (def$sn <= 66), "K", 
                                  ifelse(def$sn == 68, "L", 
                                    ifelse((def$sn >= 69) & (def$sn <= 75), "M", 
                                      ifelse((def$sn >= 77) & (def$sn <= 82), "N", 
                                        ifelse(def$sn == 84, "O", 
                                          ifelse(def$sn == 85, "P", 
                                            ifelse((def$sn >= 86) & (def$sn <= 88), "Q", 
                                              ifelse((def$sn >= 90) & (def$sn <= 93), "R", 
                                                ifelse((def$sn >= 94) & (def$sn <= 96), "S", 
                                                  ifelse((def$sn >= 97) & (def$sn <= 98), "T", 
                                                    #ifelse(def$sn == 99, "U",                                                      
                                                                                             "U")) ))))))))))))))))))
table(def$sect_NACE1)
def$sect_NACE1 <- as.factor(def$sect_NACE1)
def <- select(def,-"sn")


def$sect_NACE3 <- substr(def$sector, 1, 3) 
def$sect_NACE3 <- as.factor(def$sect_NACE3)

def$sect_NACE4 <- substr(def$sector, 1, 4) 
def$sect_NACE4 <- as.factor(def$sect_NACE4)

def$sect_NACE5 <- as.factor(def$sector)
#barplot(summary(def$sect_NACE5),horiz=TRUE)
def <- select(def,-"sector")


#filter out dept outre mer
def <- def %>%
  filter(as.numeric(depart)<=95)

def$dpt <- as.factor(def$depart)
barplot(summary(def$dpt),horiz=TRUE)

#adding region
dpt_region <- read.csv("E:\\DataScientist\\project\\forecastingDEF\\data/departements-region.csv",sep=",",header=T)
def <- def %>% 
  left_join(dpt_region, by = c("depart" = "num_dep"))%>% 
  rename(region=region_name) 
def$region <- as.factor(def$region)
def <- select(def,-"depart",-"dep_name")


#improve categorical variable for zie of firm
table(def$TAILLE_UL,useNA="ifany")
def <- def %>%  
  mutate(TAILLE_UL = replace_na(TAILLE_UL,0))%>%#NA are transformed into what will become category 0
  mutate(TAILLE_UL = replace(TAILLE_UL, TAILLE_UL==6, 5)) #aggregate size 6 to category 5
def$size <- as.factor(def$TAILLE_UL)
#def$size <-addNA(def$size)#say that if NA is a category  
levels(def$size)
#def$size <- as_label(def$TAILLE_UL)
table(def$size)
plot_bar(def$size)
def <- select(def,-"TAILLE_UL")


#def$year <- as.factor(def$year)

str(def)

plot_missing(def)#see where missing are


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

colnames(def)

glimpse(def)

summary(def)


def <- def %>% 
  arrange(id_hashed, year)

saveRDS(def,"E:\\DataScientist\\project\\forecastingDEF\\data/def_id_clean.RDS")
def <- select(def,-"id_hashed")

plot_intro(def)#nice to visualise characteristics of dataset


saveRDS(def,"E:\\DataScientist\\project\\forecastingDEF\\data/def_clean.RDS")



