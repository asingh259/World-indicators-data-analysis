library(moments)
library(tidyverse)
library(psych)


df_org<-read.csv("df_final_v2.csv")

dim(df_org)

#remove renewable energy
df_org <- df_org[,-c(26)]
# all decimal values till 2 decimal places
df_org <- data.frame(lapply(df_org, function(y) if(is.numeric(y)) round(y, 2) else y)) 

sum(is.na(df_org)) #4
country_code = df_org$Country.Code

library(missForest)
missforest <- missForest(df_org, ntree = 100, replace = TRUE, verbose = TRUE,mtry = 5, maxiter = 3)
missforest$OOBerror
#      NRMSE 
#2.735031e-12


df_imputation = missforest$ximp

df_new = df_imputation %>% mutate(country = country_code )
View(df_new)

sum(is.na(df_new)) #0

write.csv(df_new,'~/Langara/Sem 4/Dana/project/df_final_v2_clean.csv')
