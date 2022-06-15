library(moments)
library(tidyverse)
library(psych)


# LOADING AND MERGING DATA ---------------------------------------------------
df_org<-read.csv("C:/3_DANA 4840/Project/Data/Source/data.csv")
dim(df_org)
#214 33

sum(is.na(df_org))#1383 (19.58%)


#add the CO2 emission column

CO2 <- read.csv("C:/3_DANA 4840/Project/Data/Source/CO2_emission_2010.csv")

#Merge the df with 2010 CO2 data
require(dplyr)
df_merged <-
  left_join(df_org,
            CO2 %>% dplyr::select(Country.Code, X2010),
            by = "Country.Code") %>% rename(CO2emission = X2010) 


# CHECKING AND CLEANING NULL VALUES ---------------------------------------------------
# checking column-wise null percentage
Null_Cnt <- sapply(df_merged, function(x){ sum(is.na(x))})
Null_percnt <- sapply(df_merged, function(x){ round((sum(is.na(x))/length(x))*100,2) })
Null_Smry <- cbind(Null_Cnt,Null_percnt)
Null_Smry


# adding a column to check row-wise null percentage
df_merged$missingperc <- c(round((rowSums(is.na(df_merged))/dim(df_merged)[2])*100, 2))
View(df_merged)

# removing rows with more than 30% null values & removing columns with more than 50% null values
# every row is representing a country's feature if a country is missing more than 30% of its features 
# and we will impute the missing data and do analysis ; this will create bias
# same goes in column wise more  than 50% data is missing then that variable can be excluded from the analysis
df_cleaned<- df_merged[which(round(rowSums(is.na(df_merged))/dim(df_merged)[2]*100)<30),
                    which(round(colSums(is.na(df_merged))/dim(df_merged)[1]*100)<50)]

dim(df_cleaned)
#180 30
View(df_cleaned)

#Drop column - year, year code and missingperc
df_cleaned <- df_cleaned[,-c(1,2,30)]

dim(df_cleaned)#180  27
View(df_cleaned)

################################################################################################

#inaccuracy - none (precision of the numbers)
df_cleaned <- data.frame(lapply(df_cleaned, function(y) if(is.numeric(y)) round(y, 2) else y))
View(df_cleaned)

#before imputation summary (min Max std dev)
nums <- vapply(df_cleaned, is.numeric, FUN.VALUE = logical(1))
df_num <- df_cleaned[nums]


# OUTLIER CHECK BEFORE IMPUTATION  ---------------------------------------------------
md <- mahalanobis(df_num, colMeans(df_num), var(df_num), tol = 3.9262e-31)
cutoff <- qchisq(p = 0.95 , df = ncol(df_num))
length(which(md>cutoff)) #0

#summary(df_cleaned[nums])
write.csv(data.frame(describe(df_num)),"C:/3_DANA 4840/Project/Data/Outputs/summary_b4_imputed.csv", row.names = FALSE)
write.csv(df_cleaned, "C:/3_DANA 4840/Project/Data/Outputs/cleandf_b4_imputed.csv", row.names = FALSE)


write.csv(df_merged, "C:/3_DANA 4840/Project/Data/Outputs/df_merged.csv", row.names = FALSE)

#Missing values imputation
# The following input arguments are used in mice for multiple imputation:
# m: Number of imputed datasets (default is m=5)
# seed: Random seed for reproducible results
# method: method to use to impute missing values (default method for imputation of numeric variables is PMM)

df_cleaned_final = df_cleaned[,-1] # final data for data missing imputation removing country

View(df_cleaned_final)


# CORRELATION AND MULTICOLLINEARITY CHECK BEFORE IMPUTATION ---------------------------------------------------
#checking correlations BEFORE imputation
sum(is.na(df_cleaned_final)) #71 NA's

df_corr1<-cor(df_cleaned_final[complete.cases(df_cleaned_final),-1])#run corr on complete cases
head(round(df_corr1,2))


library(corrplot)
library(RColorBrewer)
library(ggcorrplot)
ggcorrplot(df_corr1, hc.order = TRUE, type = "upper",
           outline.col = "white", title = "Correlation Plot (Before Imputation)")


#checking multicollinearity BEFORE imputation
model_before <- lm(percap_gdp ~., data = df_cleaned_final[,-1])
car::vif(model_before)

#lifeexp,  mortrate_inf,  mortrate_un5
colnames(df_cleaned_final)

df_cleaned_final <- df_cleaned_final[, -c(17:19)]

# IMPUTATION USING MISSFOREST ---------------------------------------------------

#urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
#install.packages(urlPackage, repos=NULL, type="source") 
library(missForest)
set.seed(12345)
missforest <- missForest(df_cleaned_final[,-1], ntree = 100, replace = TRUE, verbose = TRUE,mtry = 5, maxiter = 3)
missforest$OOBerror
# NRMSE 
# 0.9767121
View(missforest)


df_imputation = missforest$ximp
country_code = df_cleaned$Country.Code
df_new = df_imputation %>% mutate(country = country_code)


View(df_new)
sum(is.na(df_new)) #0
 
write.csv(df_new, "C:/3_DANA 4840/Project/Data/Outputs/cleandf_imputed.csv", row.names = FALSE)

#after imputation summary

nums <- vapply(df_new, is.numeric, FUN.VALUE = logical(1))
df_num <- df_new[nums]
write.csv(data.frame(describe(df_num)),"C:/3_DANA 4840/Project/Data/Outputs/describecleaned.csv", row.names = FALSE)


rownames(df_new) <- df_new[,23]#change the rownames
df_final <- df_new[,-23]
str(df_final)#all numeric
sum(is.na(df_final))#re-check NA=0


# OUTLIER CHECK AFTER IMPUTATION : MAHALANOBIS DISTANCE -------------------------------------


md<-mahalanobis(df_final, colMeans(df_final), cov(df_final), tol = 3.9262e-31)

cutoff<-qchisq(0.95, ncol(df_final))

length(which(md > cutoff))#29 outliers

out <- df_final[md>cutoff,]

df_noout <- df_final[md<cutoff,]
write.csv(out, "C:/3_DANA 4840/Project/Data/Outputs/MDoutliers.csv")

write.csv(df_noout, "C:/3_DANA 4840/Project/Data/Outputs/df_noout.csv")



# SUMMARY/VISUALIZATION ---------------------------------------------------

#check min,mean, max and IQR
summary(df_final)


#Visualization
#Boxplot
library(RColorBrewer)
par(mfrow=c(3,3))
plot_list<-list()
for (i in 1:(ncol(df_final)))
{boxplot(df_final[i], main= colnames(df_final[i]), col=brewer.pal(n=6, name = "Set2"))}


#histogram
par(mfrow=c(3,3))
plot_list<-list()
for (i in 1:(ncol(df_final)))
{hist(df_final[, i], xlab = colnames(df_final[i]), main= colnames(df_final[i]), col=brewer.pal(n=6, name = "Blues"))}

#Scale the data
df_scale  = scale(df_final)

df_scale = as.data.frame(df_scale)

# comment: all the variables are numeric type
################################################################################
#Step1:  Chose the most relevant method
library(cluster)
# The most relevant method

eu = dist(df_scale)

# Kmedoid

sw <-function(label, d)  # label is a numeric vector (for KMEDOID method)
{
  s<-cluster::silhouette(label, d) 
  s<-mean(s[,3])
  return(s)
}

suggest_k_sw<-numeric()
suggest_k_ch<-numeric()

set.seed(123)

for (i in 1:5) # loop for 5 times
{
  sw_temp<-numeric()
  ch_temp<-numeric()
  
  for (j in 2:10) # k: from 2 to 10
  {
    pm<-pam(df_scale, j) # Kmedoid method
    sw_temp<-c(sw_temp, sw(pm$clustering, eu))                   # shihoutte : kmedoid clusers and euclean distance
    ch_temp<-c(ch_temp, fpc::calinhara(df_scale, pm$clustering)) # CH: kmedoid clusers and euclean distance
  }
  suggest_k_sw<-c(suggest_k_sw, which.max(sw_temp)+1)
  suggest_k_ch<-c(suggest_k_ch, which.max(ch_temp)+1)
}

sw_temp
# 0.3555782 0.2280046 0.1558372 0.1007705 0.1153654 0.1176308 0.1228319 0.1161643 0.1052671

ch_temp
#107.34374  79.26772  60.81243  49.74094  47.53399  42.35546  41.27374  38.53910  35.08164

table(suggest_k_sw)

#suggest_k_sw
#2 -> k =2 for KMEDOID
#5 

max(sw_temp) # 0.3555782(Shihoutte score of Kmedoid method), has 2 meanings: k =2, method = kmedoid
max(ch_temp) #107.3437(CH score of Kmedoid method)


##################################################
# find optimal k by KMEANS

sw<-function(km, d) 
{
  s<-silhouette(km$cluster, d)
  s<-mean(s[,3])
  return(s)
}

CH<-function(km)
{
  penalty<-(length(km$cluster)-length(km$size))/(length(km$size)-1) # penalty for increasing k
  ch<-penalty*km$betweenss/sum(km$withinss) # calculate CH Score
  return (ch)
}

sw_result1<-numeric()
CH_result1<-numeric()

set.seed(123) # set seed

for (k in 2:10) # try k-means with k = 2 to 10
{
  km<-kmeans(df_scale,k)
  sw_result1<-c(sw_result1, sw(km, eu))
  CH_result1<-c(CH_result1, CH(km))
}


which.max(sw_result1)+1 # k = 2 
which.max(CH_result1)+1 # k = 2 

max(sw_result1) #0.3511576(Shihoue score of kmeans method), has 2 meanings: k =2, method = kmeans
max(CH_result1) #110.2381 (CH score of Kmedoid method)

# Kmeans is better method
###################################
# HIERARICAL METHOD

hc_wardD2 = hclust(eu,method ="ward.D2")
set.seed(123)

ch_list1<-numeric()
sw_list1<-numeric()

for (i in 2:10)
{
  ch_list1<-c(ch_list1, fpc::calinhara(eu, cutree(hc_wardD2, i)))
  sw_list1<-c(sw_list1, mean(silhouette(cutree(hc_wardD2, i), eu)[,3]))
}

# if we use complete linkage, k will be = 2
which.max(ch_list1)+1 # 2
which.max(sw_list1)+1 #2

ch_list1 #164.36272 158.67000 131.65109 132.15571 118.24940 102.26244  91.73435  92.97035  90.97680
max(ch_list1) #164.3627(CH score for wardD2)

sw_list1 #  0.3694468 0.2421877 0.2148586 0.1850567 0.1846749 0.1763091 0.1831650 0.1528211 0.1499510
max(sw_list1) #164.3627 (Shihoutte score for wardD2)


max(sw_result1) #0.365258 (Shihoutte score of kmeans method), has 2 meanings: k =2, method = kmeans
max(CH_result1) # 106.8627 (CH score of kmeans method)

max(sw_temp) #0.3675986 (Shihoutte score of Kmedoid method), has 2 meanings: k =2, method = kmedoid
max(ch_temp) #105.2215 (CH score of Kmedoid method)


# From 3 methods, we can conclude that hierarichal, wardD2 is the best method since it has highest 
# CH score (168.3848) and Shihoutte score(0.3537714)
# with optimal k = 2 

##########################################################################

# chose the best method within hierarchical clustering

# perform diana()
library(cluster)
eu = dist(df_scale)
dc = diana(eu)


# check the divisive coef.
dc$dc # 0.8380965

# plot the dendrogram
windows()
plot(dc)

## Agglomerative Hierarchical Clustering
# single linkage
hc_single = hclust(eu, method = "single")

# complete linkage
hc_complete = hclust(eu, method = "complete")

# average linkage
hc_average = hclust(eu, method = "average")

# ward linkage
hc_wardD2 = hclust(eu, method = "ward.D2")


#Agglomerative coefficient: measures the amount of clustering structure of the dataset. 
#If observations quickly agglomerate into distinct clusters that later agglomerate into 
#a single cluster at much greater dissimilarities, the coefficient will approach 1.
#In contrast, no clustering for the dataset will have coefficient approaching zero

# agglomerative coef -> check the structure of the clusters
coef.hclust(hc_single) #0.4988596
coef.hclust(hc_complete) #0.8496924
coef.hclust(hc_average) #0.6895446
coef.hclust(hc_wardD2) #0.9526863 -> wardD2 is the best clustering method in term of structure

#Agglomerative ward clustering seems to give a better structure, 
#in comparison to the divisive clustering technique

# Comment:

# find the optimal k of hc1 - 4, using sw as metrics, consider k = 2 - 10
sw_list1<-numeric() #single
sw_list2<-numeric() #complete
sw_list3<-numeric() #hc_average
sw_list4<-numeric() # wardD2

for (i in 2:10)
{
  sw<-silhouette(cutree(hc_single, k = i), eu)
  sw_list1<-c(sw_list1, mean(sw[,3]))
}

for (i in 2:10)
{
  sw<-silhouette(cutree(hc_complete, k = i), eu)
  sw_list2<-c(sw_list2, mean(sw[,3]))
}

for (i in 2:10)
{
  sw<-silhouette(cutree(hc_average, k = i), eu)
  sw_list3<-c(sw_list3, mean(sw[,3]))
}

for (i in 2:10)
{
  sw<-silhouette(cutree(hc_wardD2, k = i), eu)
  sw_list4<-c(sw_list4, mean(sw[,3]))
}

# cbind all sw_list
sw_list<-cbind(sw_list1, sw_list2, sw_list3, sw_list4)
sw_list

#       sw_list1   sw_list2   sw_list3   sw_list4
#[1,]  0.24431939 0.3696676 0.3635434 0.3694468
#[2,]  0.16666165 0.2902638 0.3200097 0.2421877
#[3,]  0.13790036 0.2972885 0.2967349 0.2148586
#[4,]  0.07008383 0.2515810 0.2507863 0.1850567
#[5,]  0.05565885 0.1488498 0.2156399 0.1846749
#[6,] -0.01897703 0.1377229 0.2076657 0.1763091
#[7,] -0.03041767 0.1135680 0.1858980 0.1831650
#[8,] -0.15339242 0.1065394 0.1740821 0.1528211
#[9,] -0.16777476 0.1063986 0.1408716 0.1499510


# comment: k=2 for all linkage methods and highest for Ward D2
###############################################################################################

#. Quality: "goodness" of clusters
#. Assess the quality and reliability of clustering results

#Internal criterion: A good clustering will produce high quality clusters in which:
#the intra-class (that is, intra-cluster) similarity is high
#the inter-class similarity is low
#The measured quality of a clustering depends on both the document representation and the similarity measure used
#Internal criterion is used when we don't have a ground of truth or expert knowledge.
#Silhouette coefficient
#Dunn index

library(clValid)
clmethods = c("hierarchical","kmeans","pam")

internal = clValid(df_scale, nClust = 2:5, clMethods = clmethods, validation = "internal")

summary(internal)

#Clustering Methods:
#  hierarchical kmeans pam 

#Cluster sizes:
#  2 3 4 5 

#Validation Measures:
#  2        3        4        5

#hierarchical 

#Connectivity   13.4321  17.2901  27.1488  37.8016
#Dunn            0.2055   0.2055   0.2055   0.2743
#Silhouette      0.3635   0.3200   0.2967   0.2508

#kmeans       

#Connectivity   13.7425  40.6349  40.3405  68.6488
#Dunn            0.1846   0.1526   0.1664   0.1941
#Silhouette      0.3653   0.2666   0.2852   0.2121
#pam          

#Connectivity   17.9357  34.3770  63.8135 100.7433
#Dunn            0.1846   0.1684   0.1684   0.1474
#Silhouette      0.3676   0.2495   0.1672   0.1272

#Optimal Scores:

#  Score   Method       Clusters
#Connectivity 13.4321 hierarchical 2       
#Dunn          0.2743 hierarchical 5       
#Silhouette    0.3676 pam          2 

#The stability measures can be computed as follow
clmethods <- c("hierarchical","kmeans","pam")
stability <- clValid(df_scale, nClust = 2:5, clMethods = clmethods,
                     validation = "stability")
# Display only optimal Scores
optimalScores(stability)

#         Score Method Clusters
#APN 0.001186092 kmeans        2
#AD  4.083894989    pam        5
#ADM 0.009407507 kmeans        2
#FOM 0.654482091 kmeans        5

###############################################################################

##################################################################################

#With k = 2, use the decision tree to find out the key variables 
library(rpart)

tree1 = rpart(cutree(hc_wardD2, 2) ~ ., data = df_final)
tree1$variable.importance

# brate         fert_rate   agedep_work  agedep_young mortrate_un5f  mortrate_un5m    hiv_fe15up 
#26.9324460    25.8102608    24.1269829    24.1269829    22.1582545    22.1582545     1.9523810 

#lifeexp_f     lifeexp_m  ado_fert_rate 
#1.1156463     1.1156463     0.5578231

#  10 variables are the most important variables

nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.2, col = "blue")
plot(as.dendrogram(hc_wardD2), ylab = "Height", nodePar = nodePar, leaflab = "none")

#visualize the cluster characteristic. For each variable, which clusters has the higher mean?
# Could you try to label/name the cluster?

hc_wardD2 = hclust(eu, method = "ward.D2")

cluster = cutree(hc_wardD2 , k = 2)
df_final$cluster= cutree(hc_wardD2 , k = 2)

for (i in c(1,2))
{
  boxplot(df_final[df_final$cluster  ==i,1:23], main = paste0("Cluster ",i)) # 23 variables
}

for (i in 1:23)
{
  m1<-mean(df_final[df_final$cluster ==1,i])
  m2<-mean(df_final[df_final$cluster ==2,i])
  cat('Mean of ', colnames(df_final)[i],
      '\n Cluster 1: ',m1,
      '\n Cluster 2: ',m2,
      '\n=======\n')
}

#Mean of  adjsav_eduexp 
#Cluster 1:  3.457831 
#Cluster 2:  4.365974 
#=======
#  Mean of  ado_fert_rate 
#Cluster 1:  95.05467 
#Cluster 2:  35.13198 
#=======
#  Mean of  agedep_work 
#Cluster 1:  84.378 
#Cluster 2:  51.14066 
#=======
#  Mean of  agedep_young 
#Cluster 1:  78.60089 
#Cluster 2:  36.76575 
#=======
#  Mean of  agrland 
#Cluster 1:  45.73821 
#Cluster 2:  37.75085 
#=======
#  Mean of  brate 
#Cluster 1:  36.40689 
#Cluster 2:  16.90226 
#=======
#  Mean of  fert_rate 
#Cluster 1:  4.837778 
#Cluster 2:  2.132736 
#=======
#  Mean of  c_gdp 
#Cluster 1:  21480832920 
#Cluster 2:  235212552272 
#=======
#  Mean of  grow_gdp 
#Cluster 1:  5.878969 
#Cluster 2:  3.415767 
#=======
# Mean of  percap_gdp 
#Cluster 1:  2204.46 
#Cluster 2:  14098.43 
#=======
#  Mean of  percap_healthexp 
#Cluster 1:  83.58476 
#Cluster 2:  1145.922 
#=======
#  Mean of  healthexp_gdp 
#Cluster 1:  2.906556 
#Cluster 2:  4.392547 
#=======
#  Mean of  de_gdp 
#Cluster 1:  9.25408 
#Cluster 2:  4.959338 
#=======
#  Mean of  lifeexp_f 
#Cluster 1:  59.70244 
#Cluster 2:  77.38311 
#=======
#  Mean of  lifeexp_m 
#Cluster 1:  57.1 
#Cluster 2:  71.63368 
#=======
#  Mean of  mortrate_un5f 
#Cluster 1:  81.03556 
#Cluster 2:  15.98113 
#=======
#  Mean of  mortrate_un5m 
#Cluster 1:  92.15556 
#Cluster 2:  19.47453 
#=======
#  Mean of  grow_pop 
#Cluster 1:  2.581111 
#Cluster 2:  0.9412264 
# =======
#  Mean of  immu_dpt 
#Cluster 1:  77.88489 
#Cluster 2:  93.75472 
#=======
#  Mean of  immu_mea 
#Cluster 1:  74.89844 
#Cluster 2:  93.37736 
#=======
#  Mean of  hiv_fe15up 
#Cluster 1:  55.04224 
#Cluster 2:  29.88325 
#=======
#  Mean of  CO2emission 
#Cluster 1:  0.7151511 
#Cluster 2:  4.961169 
#=======
#  Mean of  cluster 
#Cluster 1:  1 
#Cluster 2:  2 
#=======

###############################################################################
# Let's examine the larger group, which is group 2

table(cluster)
#1   2 
#45 106 

cluster_k3 = cutree(hc_wardD2,k =3)
df_final$cluster_k3 = as.factor(cluster_k3)

table(cluster_k3)
# 1  2  3 
#45 61 45  = 153

table(cluster,cluster_k3 )

#          cluster_k3
# cluster  1  2  3
#       1 45  0  0
#       2  0 61 45

library(ggplot2)
df_subset = df_final[df_final$cluster == 2, ]  #including 106 countries

#View(df_subset)
dim(df_subset) # 106  24

plot_list<-list()
for (i in 1:(ncol(df_subset)-2))
{
  if (is.numeric(df_subset[,i]))
  {
    plot_list[[i]] <- ggplot(df_subset, aes_string(x = "cluster_k3",  # will show group 2 and 3 only
                                                   y = colnames(df_subset)[i], 
                                                   color = "cluster_k3")) + geom_boxplot()
  } 
  else
  {
    plot_list[[i]] <- ggplot(df_subset, aes_string(x = "cluster_k3", 
                                                   fill = colnames(df_subset)[i])) + geom_bar(stat='count')
  }
}

ggpubr::ggarrange(plotlist = plot_list, ncol = 5, nrow = 5)

length(cluster_k3)#153

# Comment:................

# find the most important variables
df_final$cluster_k3[df_final$cluster == 2]

length(df_final$cluster_k3[df_final$cluster == 2]) #106

#tree2 = rpart(df_final$cluster_k3[df_final$cluster == 1] ~ ., data = df_final[df_final$cluster == 1, 1:23])
#tree2$variable.importance 

tree3 = rpart(df_final$cluster_k3[df_final$cluster == 2] ~ ., data = df_subset)
tree3$variadf_subble.importance
tree3$variable.importance 
#percap_gdp  is the most important variable in group 1 of cluster with k=2

#percap_gdp percap_healthexp    mortrate_un5f    mortrate_un5m      CO2emission        lifeexp_f 
#37.647525        31.542521        31.427217        30.409717        26.455018        23.402516 
#agedep_young            brate        fert_rate         grow_pop 
#8.811594         7.342995         6.608696         5.140097 

tree3

#Interpretability and usability
# label the groups and inteprete

df_final

###############################################################################################
###############################################################################################
eu_sub = dist(df_subset)
dc_sub = diana(eu_sub)

# check the divisive coef.
dc_sub$dc # 0.9873123

# plot the dendrogram
plot(dc_sub)

pca1<-prcomp(df_scale)
summary(pca1)
loading<-pca1$rotation[,1:26] # reach 70% variance

# combine the PCs' loading with the scaled dataset
df_score<-as.matrix(df_scale)%*%as.matrix(loading)

df_score<-as.data.frame(df_score)
View(df_score)

###########
# bar plot of average co2:


plot = df_final %>% group_by(cluster) %>% summarise(co2 = sum(CO2emission), n = n())%>%
  group_by(cluster) %>% summarise(average = co2/n) %>% mutate(cluster=as.factor(cluster))

ggplot(plot,aes(x=cluster,y=average)) + geom_bar(stat='identity')







