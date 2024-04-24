####################################
#
# Extra financial data 
#
####################################

# Load required libraries
library(reshape2)
library(tidyverse)
if (!require(data.table)) install.packages("data.table")


# Set working directory
#setwd("//set/your/working/directory/here")

# Read data from csv original files
#comptes_communes <- read.csv("E:\\DataScientist\\project\\forecastingDEF\\data\\comptes-individuels-des-communes-fichier-global-a-compter-de-2000.csv",sep=";",header=T,dec=".")
comptes_communes <- fread("E:\\DataScientist\\project\\forecastingDEF\\data\\comptes-individuels-des-communes-fichier-global-a-compter-de-2000.csv")
pop_communes_1999 <- read.csv("E:\\DataScientist\\project\\forecastingDEF\\data\\pop_communes_1999.csv",sep=",",header=T)

# Paste 0 where necessary
comptes_communes$icom <- case_when(nchar(as.character(comptes_communes$icom))==1~paste("00",comptes_communes$icom,sep=""),nchar(as.character(comptes_communes$icom))==2~paste("0",comptes_communes$icom,sep=""),TRUE~as.character(comptes_communes$icom))
pop_communes_1999$COM <- paste("0",pop_communes_1999$COM,sep="")

# Rename fields
pop_communes_1999 <- rename(pop_communes_1999,pop1=PSDC99)
pop_communes_1999 <- rename(pop_communes_1999,code_dpt=COM)

# Add reference year 1999 and corresponding values
pop_communes_1999$an <- 1999

# Add dep field for reference and remove NCC field
pop_communes_1999$dep <- substr(pop_communes_1999$code_dpt,1,3)
pop_communes_1999 <- select(pop_communes_1999,-c(NCC,dep))

# Select fields of interest
comptes_communes_trie <- comptes_communes %>%
  select(an,dep,pop1,icom,inom,depinv,cafn,res2)

# Add a field for the year before
comptes_communes_trie <- comptes_communes_trie %>%
  mutate(an_prec=an-1)

# Paste a new field as new key for future left join
comptes_communes_trie <- comptes_communes_trie %>%
  mutate(code_dpt=paste(dep,icom))

# Drop the whitespaces for code_dpt
comptes_communes_trie$code_dpt <- sub(" ","",comptes_communes_trie$code_dpt)

# Create a small dataset as reference to perform left join
comptes_communes_trie_cut <- comptes_communes_trie %>%
  select(an,pop1,code_dpt)

# Paste the 1999 rows to comptes_communes_trie_cut dataset
comptes_communes_trie_cut <- rbind(comptes_communes_trie_cut,pop_communes_1999)

# Perform a left join on dataset using the newly created reference dataset
comptes_communes_trie <- comptes_communes_trie %>% 
  left_join(comptes_communes_trie_cut, by = c("an_prec" = "an","code_dpt"))

# Rename the modified fields
comptes_communes_trie <- comptes_communes_trie %>%
  rename(pop_prec=pop1.y,pop1=pop1.x)

# Replace NAs in pop_prec values by 0 values
sum(is.na(comptes_communes_trie$pop_prec))
comptes_communes_trie <- comptes_communes_trie %>%
  mutate(pop_prec=ifelse(is.na(pop_prec),pop1,pop_prec),depinv=ifelse(is.na(depinv),0,depinv),cafn=ifelse(is.na(cafn),0,cafn),res2=ifelse(is.na(res2),0,res2))

# Select prefinal data in new dataset
comptes_communes_prefinal <- comptes_communes_trie %>%
  select(an,an_prec,dep,pop1,pop_prec,depinv,cafn,res2,code_dpt)

# Select the final variables and drop non-metropolitan values (not same dep code in pop_communes_1999)
comptes_communes_final <- comptes_communes_prefinal %>%
  select(an,dep,pop1,pop_prec,depinv,cafn,res2,code_dpt) %>%
  filter(an>1999) %>%
  filter(substr(dep,1,1)==0)

# Get sums by year and department
comptes_communes_final <- comptes_communes_final %>%
  group_by(an,dep) %>%
  mutate(pop1_dep=sum(pop1),pop_prec_dep=sum(pop_prec),depinv_dep=sum(depinv),cafn_dep=sum(cafn),res2_dep=sum(res2))
summary(comptes_communes_final$pop1_dep)
summary(comptes_communes_final$pop_prec_dep)

# Get population variation for consecutive years
comptes_communes_final <- comptes_communes_final %>%
  mutate(var_pop_dep=log(pop1_dep)-log(pop_prec_dep))
sum(is.na(comptes_communes_trie$var_pop_dep))

# Calculate ratios on population variation
sum(is.na(comptes_communes_trie$pop1_dep))
comptes_communes_final <- comptes_communes_final %>%
  #mutate(var_pop_dep=ifelse(var_pop_dep==0,1,var_pop_dep)) %>%
  mutate(depinv_dep_avg=depinv_dep/pop1_dep,cafn_dep_avg=cafn_dep/pop1_dep,res2_dep_avg=res2_dep/pop1_dep) %>%
  select(an,dep,var_pop_dep,ends_with("_dep_avg"))
sum(is.na(comptes_communes_trie$depinv_dep_avg))

# Keep only 1 line per year and department
comptes_communes_final <- comptes_communes_final %>%
  distinct(an, dep, .keep_all = TRUE)

# Keep only 2 decimals 
comptes_communes_final$var_pop_dep <- round(comptes_communes_final$var_pop_dep,2)
comptes_communes_final$depinv_dep_avg <- round(comptes_communes_final$depinv_dep_avg,2)
comptes_communes_final$cafn_dep_avg <- round(comptes_communes_final$cafn_dep_avg,2)
comptes_communes_final$res2_dep_avg <- round(comptes_communes_final$res2_dep_avg,2)

# Remove excess 0 from dep field
comptes_communes_final <- comptes_communes_final %>%
  mutate(dep=substr(dep,2,length(comptes_communes_final$dep)))

# Rename an in year
comptes_communes_final <- rename(comptes_communes_final,year=an)

# Rename dep in dpt
comptes_communes_final <- rename(comptes_communes_final,dpt=dep)

# Get some information from final data
summary(comptes_communes_final)

# Save data in new RDS file
saveRDS(comptes_communes_final,"E:\\DataScientist\\project\\forecastingDEF\\data\\comptes_communes_final.RDS")
