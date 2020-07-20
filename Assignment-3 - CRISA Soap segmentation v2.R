library(tidyverse)
library(readxl)
library(factoextra)

bsData <- read_excel("C:/Users/ashok/Desktop/UIC/spring 2020/572/submissions/Assgt3_BathSoap_Data.xls", sheet = "DM_Sheet")

#the data read in may contain empty rows, columns, so remove these
bsData<-bsData[1:600, 1:46]

any(is.na(bsData))

#better to change the colNames which contain punctuation, space to _ (like affluence index,etc)
names(bsData) <- gsub("[[:punct:]]|\\s", "_", names(bsData))

#The data with '%' in values are read in as 'chr' type - change these to numeric
bsData[20:46]<-lapply(bsData[20:46],function(x)  as.numeric(sub("%", "e-2", x)))

#rename the data
bsd<- bsData

#for brLoyalty, calculate maxBr as max of purchase by different major brand (excl others)
bsd<-bsd %>% rowwise() %>%  mutate(maxBr=max(Br__Cd__57__144, Br__Cd__55,
Br__Cd__272, Br__Cd__286, Br__Cd__24, Br__Cd__481, Br__Cd__352, Br__Cd__5))

#Examine the data - can all attributes be considered as 'numeric'
summary(as.factor(bsd$FEH))

#convert this to dummies, since the values are not ordinal, and remove the '0' level dummy
bsd<-bsd %>% mutate(fehDummy=1) %>% pivot_wider(names_from = FEH,
values_from = fehDummy, names_prefix = "FEH_", 
values_fill = list(fehDummy=0))
bsd<- bsd %>% select(-FEH_0)  # can append this to the last line too

#explore MT
summary(as.factor(bsd$MT))
#keep levels 0, 4, 5, 10, 17 as dummies, with 0 in the dummies indicating 'other'
bsd<- bsd %>% mutate(MT=if_else(MT %in% c(0, 4, 5, 10, 17), MT, -1))
bsd<-bsd %>% mutate(mtDummy=1) %>% pivot_wider(names_from = MT, values_from = mtDummy, names_prefix = "MT_", values_fill = list(mtDummy=0)) 
bsd<- bsd %>% select(- `MT_-1`)

#similarly for CHILD, leave out the level '5' for unknown
bsd<-bsd %>% mutate(mtChild=1) %>% pivot_wider(names_from = CHILD, values_from = mtChild, names_prefix = "CHILD_", values_fill = list(mtChild=0)) %>% select(- CHILD_5) 


#clustering on  purchase behavior variables
PURCHASE_BEHAVIOR <- c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value',
                       #'Trans___Brand_Runs', 'Vol_Tran',
                       'Avg__Price', 'maxBr', 'Others_999')

#K=2

x<- bsd

kmClus_pb<- x %>% select(PURCHASE_BEHAVIOR) %>% scale() %>%kmeans(centers=2, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpb<-x %>% select(PURCHASE_BEHAVIOR) %>% scale() 

#kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pb$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()

#how many clusters is best
fviz_nbclust(xpb, kmeans, method = "wss")
fviz_nbclust(xpb, kmeans, method = "silhouette")


#K=3

x<- bsd

kmClus_pb<- x %>% select(PURCHASE_BEHAVIOR) %>% scale() %>%kmeans(centers=3, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpb<-x %>% select(PURCHASE_BEHAVIOR) %>% scale() 

#kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pb$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()



#K=4

kmClus_pb<- x %>% select(PURCHASE_BEHAVIOR) %>% scale() %>%kmeans(centers=4, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpb<-x %>% select(PURCHASE_BEHAVIOR) %>% scale() 

#kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pb$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()


#K=5

kmClus_pb<- x %>% select(PURCHASE_BEHAVIOR) %>% scale() %>%kmeans(centers=5, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpb<-x %>% select(PURCHASE_BEHAVIOR) %>% scale() 

#kmClus_pb

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pb$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()



#clustering on  Basis-for-purchase variables


#K=2

#We selected only few attributes for selling propositions, the ones with highest cummulative values
BASIS_FOR_PURCHASE <- c('PropCat_5', 'PropCat_6', 'PropCat_7', 'PropCat_8', 'PropCat_14',
                        'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__',
                        'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4'
                        )

x<- bsd


kmClus_bfp<- x %>% select(BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=2, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xbfp<-x %>% select(BASIS_FOR_PURCHASE) %>% scale() 

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_bfp, data=x %>% select(BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_bfp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()

#how many clusters is best
fviz_nbclust(xbfp, kmeans, method = "wss")
fviz_nbclust(xbfp, kmeans, method = "silhouette")



#K=3

#We selected only few attributes for selling propositions, the ones with highest cummulative values
BASIS_FOR_PURCHASE <- c('PropCat_5', 'PropCat_6', 'PropCat_7', 'PropCat_8', 'PropCat_14', 'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__', 'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4')

x<- bsd


kmClus_bfp<- x %>% select(BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=3, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xbfp<-x %>% select(BASIS_FOR_PURCHASE) %>% scale() 

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_bfp, data=x %>% select(BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_bfp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()


#K=4

kmClus_bfp<- x %>% select(BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=4, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xbfp<-x %>% select(BASIS_FOR_PURCHASE) %>% scale() 

#kmClus_bfp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_bfp, data=x %>% select(BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_bfp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()


#K=5

kmClus_bfp<- x %>% select(BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=5, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xbfp<-x %>% select(BASIS_FOR_PURCHASE) %>% scale() 

#kmClus_bfp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_bfp, data=x %>% select(BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_bfp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()


#clustering on purchase behaviour and Basis-for-purchase varables

#We selected only few attributes for selling propositions, the ones with highest cummulative values
PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE <- c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value', 
                                           #'Trans___Brand_Runs', 'Vol_Tran',
                                           'Avg__Price', 'maxBr', 'Others_999',
                                           'PropCat_5', 'PropCat_6', 'PropCat_7', 'PropCat_8', 'PropCat_14',
                                           'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__',
                                           'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4')

#K=2

x<- bsd

kmClus_pbbp<- x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=2, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() 

#kmClus_pbbp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pbbp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()

#how many clusters is best
fviz_nbclust(xpbbp, kmeans, method = "wss")
fviz_nbclust(xpbbp, kmeans, method = "silhouette")


#K=3

x<- bsd

kmClus_pbbp<- x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=3, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() 

#kmClus_pbbp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pbbp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()

#how many clusters is best
fviz_nbclust(xpbbp, kmeans, method = "wss")
fviz_nbclust(xpbbp, kmeans, method = "silhouette")

#K=4

x<- bsd

kmClus_pbbp<- x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=4, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() 

#kmClus_pbbp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pbbp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()


#K=5

x<- bsd

kmClus_pbbp<- x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=5, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() 

#kmClus_pbbp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pbbp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()

#K=6

x<- bsd

kmClus_pbbp<- x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=6, nstart=25)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE) %>% scale() 

#kmClus_pbbp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=x %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE))

#https://www.rdocumentation.org/packages/factoextra/versions/1.0.6/topics/fviz_cluster


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x <- x %>% mutate(clusKM=kmClus_pbbp$cluster)

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()


##PAM - Partitioning around mediods
library(cluster)
library(clusterSim)

#pam -- https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/pam

#Purchase Behavior

#k=2
pam_pb<-pam(xpb, k=2, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpb, pam_pb$clustering, centrotypes="centroids")$DB)

#k=2
pam_pb<-pam(xpb, k=2, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpb, pam_pb$clustering, centrotypes="centroids")$DB)

#k=3
pam_pb<-pam(xpb, k=3, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpb, pam_pb$clustering, centrotypes="centroids")$DB)

#k=3
pam_pb<-pam(xpb, k=3, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpb, pam_pb$clustering, centrotypes="centroids")$DB)

#k=4
pam_pb<-pam(xpb, k=4, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpb, pam_pb$clustering, centrotypes="centroids")$DB)


#k=4
pam_pb<-pam(xpb, k=4, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpb, pam_pb$clustering, centrotypes="centroids")$DB)


#PAM for Basis of Purchase

#k=2
pam_pb<-pam(xbfp, k=2, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xbfp, pam_pb$clustering, centrotypes="centroids")$DB)


#k=2
pam_pb<-pam(xbfp, k=2, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xbfp, pam_pb$clustering, centrotypes="centroids")$DB)


#k=3
pam_pb<-pam(xbfp, k=3, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xbfp, pam_pb$clustering, centrotypes="centroids")$DB)


#k=3
pam_pb<-pam(xbfp, k=3, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xbfp, pam_pb$clustering, centrotypes="centroids")$DB)


#k=4
pam_pb<-pam(xbfp, k=4, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xbfp, pam_pb$clustering, centrotypes="centroids")$DB)


#k=4
pam_pb<-pam(xbfp, k=4, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xbfp, pam_pb$clustering, centrotypes="centroids")$DB)

#PAM for Purchase Behavior and Basis of Purchase

#k=2
pam_pb<-pam(xpbbp, k=2, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpbbp, pam_pb$clustering, centrotypes="centroids")$DB)

#k=2
pam_pb<-pam(xpbbp, k=2, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpbbp, pam_pb$clustering, centrotypes="centroids")$DB)

#k=3
pam_pb<-pam(xpbbp, k=3, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpbbp, pam_pb$clustering, centrotypes="centroids")$DB)


#k=3
pam_pb<-pam(xpbbp, k=3, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpbbp, pam_pb$clustering, centrotypes="centroids")$DB)

#k=4
pam_pb<-pam(xpbbp, k=4, metric = "euclidean")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpbbp, pam_pb$clustering, centrotypes="centroids")$DB)

#k=4
pam_pb<-pam(xpbbp, k=4, metric = "manhattan")
#Partitioning Around Mediods

pam_pb
pam_pb$clusinfo

fviz_cluster(pam_pb)


#silhoutte plot - using the silhoutte function in the cluster package
#https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/silhouette
si <- silhouette(pam_pb)
summary(si)
plot(si, col=1:3, border=NA)
print(index.DB(xpbbp, pam_pb$clustering, centrotypes="centroids")$DB)

#Hierarchical Clustering
#clustering for Purchase Behaviour


xdist <- dist(xpb, method = "euclidean")
hierC_pb <- hclust(xdist, method = "average" )
plot(hierC_pb, cex=0.3, hang=-3, main="hclust-average")

hierC_pb_c <- hclust(xdist, method = "complete" )
hierC_pb_w <- hclust(xdist, method = "ward.D" )

#using agnes from the cluster package
hierC_pb_ag_c <- agnes(xdist, method = "complete" )

#check the agglomerative coeff given by agnes
hierC_pb_ag_c$ac


#use cuttree to assign different clusters to examples
cut3_hierC_pb_ac_c <- cutree(hierC_pb_ag_c, k = 3)
table(cut3_hierC_pb_ac_c)

cut3_hierC_pb_ac_w <- cutree(hierC_pb_w, k = 3)
table(cut3_hierC_pb_ac_w)



fviz_cluster(list(data=xpb,cluster=cut3_hierC_pb_ac_c ), main="agnes-complete")


#dendograms using fviz_dend
fviz_dend(hierC_pb_w)

fviz_dend(hierC_pb_w, k=3, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")

#circular dendogram
fviz_dend(hierC_pb_w, k=3, color_labels_by_k = TRUE, type="circular", rect=TRUE, main="agnes - Wards")




#clustering for Basis for purchase

xdist <- dist(xbfp, method = "euclidean")
hierC_bfp <- hclust(xdist, method = "average" )
plot(hierC_bfp, cex=0.3, hang=-3, main="hclust-average")

hierC_bfp_c <- hclust(xdist, method = "complete" )
hierC_bfp_w <- hclust(xdist, method = "ward.D" )

#using agnes from the cluster package
hierC_bfp_ag_c <- agnes(xdist, method = "complete" )

#check the agglomerative coeff given by agnes
hierC_bfp_ag_c$ac


#use cuttree to assign different clusters to examples
cut3_hierC_bfp_ac_c <- cutree(hierC_bfp_ag_c, k = 3)
table(cut3_hierC_bfp_ac_c)

cut3_hierC_bfp_ac_w <- cutree(hierC_bfp_w, k = 3)
table(cut3_hierC_bfp_ac_w)


fviz_cluster(list(data=xbfp,cluster=cut3_hierC_bfp_ac_c ), main="agnes-complete")


#dendograms using fviz_dend
fviz_dend(hierC_bfp_w)

fviz_dend(hierC_bfp_w, k=3, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")

#circular dendogram
fviz_dend(hierC_bfp_w, k=3, color_labels_by_k = TRUE, type="circular", rect=TRUE, main="agnes - Wards")




#clustering for Purchase Behaviour and Basis for purchase


xdist <- dist(xpbbp, method = "euclidean")
hierC_pbbp <- hclust(xdist, method = "average" )
plot(hierC_pbbp, cex=0.3, hang=-3, main="hclust-average")

hierC_pbbp_c <- hclust(xdist, method = "complete" )
hierC_pbbp_w <- hclust(xdist, method = "ward.D" )

#using agnes from the cluster package
hierC_pbbp_ag_c <- agnes(xdist, method = "complete" )

#check the agglomerative coeff given by agnes
hierC_pbbp_ag_c$ac


#use cuttree to assign different clusters to examples
cut3_hierC_pbbp_ac_c <- cutree(hierC_pbbp_ag_c, k = 3)
table(cut3_hierC_pbbp_ac_c)

cut3_hierC_pbbp_ac_w <- cutree(hierC_pbbp_w, k = 3)
table(cut3_hierC_pbbp_ac_w)


fviz_cluster(list(data=xpbbp,cluster=cut3_hierC_pbbp_ac_c ), main="agnes-complete")


#dendograms using fviz_dend
fviz_dend(hierC_pbbp_w)

fviz_dend(hierC_pbbp_w, k=3, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")

#circular dendogram
fviz_dend(hierC_pbbp_w, k=3, color_labels_by_k = TRUE, type="circular", rect=TRUE, main="agnes - Wards")

data("multishapes")

#Plot the points
multishapes %>% ggplot(aes(x=x,y=y, col=as.factor(shape)))+geom_point()

msKMeans <- kmeans(multishapes[,1:2], 5, nstart = 25)

fviz_cluster(msKMeans, data = multishapes[,1:2], main="kMeans on multishapes")

##Decision tree for question 4c

x<- bsd

#calculating number of rows
nr<-nrow(x)

#Splitting data into training/testing sets using random sampling
#Training: 70%, Testing: 30%
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE)
bsdTrn <- x[trnIndex, ] 
bsdTst <- x[-trnIndex, ]


PURCHASE_BEHAVIOR <- c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value',
                       #'Trans___Brand_Runs', 'Vol_Tran',
                       'Avg__Price', 'maxBr', 'Others_999')

#K=5

kmClus_pbbp<- bsdTrn %>% select(PURCHASE_BEHAVIOR) %>% scale() %>%kmeans(centers=5, nstart=25)
str(kmClus_pbbp)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-bsdTrn %>% select(PURCHASE_BEHAVIOR) %>% scale() 

bsdTrn<- bsdTrn %>% mutate(clustn = kmClus_pbbp$cluster)

#kmClus_pbbp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=bsdTrn %>% select(PURCHASE_BEHAVIOR))


#Information rpart model
bsd1 <- rpart(clustn ~., data=bsdTrn, method="class", parms = list(split = 
                                                                     "information"), control = rpart.control(minsplit = 30, cp=0.0001))

#View the tree
rpart.plot::prp(bsd1, type=2, extra = 1)
rpart.plot::prp(bsd1, box.palette = "GnYlRd", shadow.col = "gray", nn = True)

#Accuracy
predTrn=predict(bsd1, bsdTrn, type='class')
Metric <- c("Training Accuracy")
Result <- c(round(mean(predTrn==bsdTrn$clustn),2))
p <- as.data.frame(cbind(Metric, Result))
knitr::kable(p, align = c('c', 'c'))


##Decision tree for test data

#K=5

kmClus_pbbp<- bsdTst %>% select(PURCHASE_BEHAVIOR) %>% scale() %>%kmeans(centers=5, nstart=25)
str(kmClus_pbbp)
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-bsdTst %>% select(PURCHASE_BEHAVIOR) %>% scale() 

bsdTst<- bsdTst %>% mutate(clustn = kmClus_pbbp$cluster)

#kmClus_pbbp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=bsdTst %>% select(PURCHASE_BEHAVIOUR_BASIS_FOR_PURCHASE))

#Information rpart model
bsd2 <- rpart(clustn ~., data=bsdTst, method="class", parms = list(split = 
                                                                     "information"), control = rpart.control(minsplit = 30, cp=0.0001))

#View the tree
rpart.plot::prp(bsd2, type=2, extra = 1)
rpart.plot::prp(bsd2, box.palette = "GnYlRd", shadow.col = "gray", nn = True)

#Accuracy
predTrn=predict(bsd2, bsdTst, type='class')
Metric <- c("Test Accuracy")
Result <- c(round(mean(predTst==bsdTst$clustn),2))
p <- as.data.frame(cbind(Metric, Result))
knitr::kable(p, align = c('c', 'c'))

kkc_pbbp_rbf<-kkmeans( xpbbp,centers=5, kernel='rbfdot', kpar=list(sigma=0.2 ))


'
#Now use dbscan 
library(dbscan)

#dbscan - https://www.rdocumentation.org/packages/dbscan/versions/1.1-5/topics/dbscan

msDbscan <- dbscan(multishapes[,1:2], eps = 0.5, minPts = 5)

fviz_cluster(msDbscan, data=multishapes[,1:2], geom="point", ellipse  = FALSE, main="dbscan eps=0.5, minPts=5")

#optimal eps value
kNNdistplot(multishapes[,1:2], k=4)
#includes data points within ??-radius of a data point.
## eps is this distance that the algorithm uses to decide on whether to club the two points together. We will make use of the average distances of every point to its k nearest neighbors. These k distances are then plotted in ascending order. The point where you see an elbow like bend corresponds to the optimal *eps* value. At this point, a sharp change in the distance occurs, and thus this point serves as a threshold.
#https://www.rdocumentation.org/packages/dbscan/versions/1.1-5/topics/kNNdist

library(kernlab)

#kkmeans - https://www.rdocumentation.org/packages/kernlab/versions/0.9-29/topics/kkmeans



#kenel k for purchase behaviour



kkc_pb<-kkmeans( xpb,centers=3)
#uses default values - rbf kernal, and automatically sets the kernel
kkc_pb

#the cluster assignments for examples is in kkc_pb@.Data - use this for vizualizing using fviz_cluster

fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom="points", main="kkmeans")


#polynomial kernel with degree 2
kkc_pb_p2<-kkmeans( xpb,centers=3, kernel='polydot', kpar=list(degree=2))

#rbf kernel with specified sigma parameter
kkc_pb_rbf<-kkmeans( xpb,centers=3, kernel='rbfdot', kpar=list(sigma=0.2 ))



#kenel k for Basis for purchase



kkc_bfp<-kkmeans( xbfp,centers=3)
#uses default values - rbfp kernal, and automatically sets the kernel
kkc_bfp

#the cluster assignments for examples is in kkc_bfp@.Data - use this for vizualizing using fviz_cluster

fviz_cluster(list(data=xbfp, cluster=kkc_bfp@.Data), geom="points", main="kkmeans")


#polynomial kernel with degree 2
kkc_bfp_p2<-kkmeans( xbfp,centers=3, kernel='polydot', kpar=list(degree=2))

#rbf kernel with specified sigma parameter
kkc_bfp_rbf<-kkmeans( xbfp,centers=3, kernel='rbfdot', kpar=list(sigma=0.2 ))



#kenel k for purchase behaviour and basis for purchase



kkc_pbbp<-kkmeans( xpbbp,centers=3)
#uses default values - rbf kernal, and automatically sets the kernel
kkc_pbbp

#the cluster assignments for examples is in kkc_pbbp@.Data - use this for vizualizing using fviz_cluster

fviz_cluster(list(data=xpbbp, cluster=kkc_pbbp@.Data), geom="points", main="kkmeans")


#polynomial kernel with degree 2
kkc_pbbp_p2<-kkmeans( xpbbp,centers=3, kernel='polydot', kpar=list(degree=2))

#rbf kernel with specified sigma parameter
kkc_pbbp_rbf<-kkmeans( xpbbp,centers=3, kernel='rbfdot', kpar=list(sigma=0.2 ))
'