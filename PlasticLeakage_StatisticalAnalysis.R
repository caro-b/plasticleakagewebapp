#################### Statistical Analysis ####################
## 
## Author: Caroline Busse
## December 2021
## Email: caroline.busse@stud-mail.uni-wuerzburg.de
##


#### 0. SETUP ####

# install required packages (if not installed yet)
packagelist <- c("corrplot","clValid","cluster","DataExplorer","dbscan","dplyr","factoextra","fpc","ggplot2","NbClust","parameters","psych","readr","rgdal","sf","sp","tidyverse")
new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load required packages
lapply(packagelist, require, character.only = TRUE)

# set folder 'data' as directory from which to import data
dir <- 'C:/Users/carob/Documents/plasticleakagewebapp/data'


## import shapefile of vietnam
vietnam <- readOGR(paste(dir, "/vietnam/vietnam.shp", sep = ""))

## import landfill polygons
landfills <- readOGR(paste(dir, "/landfill_variables.gpkg", sep = ""))

landfills_factors <- landfills@data


#### I. Data Preparation

str(landfills)

# only keep plastic leakage factors
leakage_factors <- landfills@data[,c(5,7:14)]



#### II. Exploratory Data Analysis ####

describe(leakage_factors) # watermin has huge sd


#### Outlier Analysis

## Histogram
leakage_factors %>% plot_histogram()

## Boxplots
boxplot(leakage_factors)

par(mfrow=c(1,5))
#boxplot(leakage_factors$area_ha)
boxplot(leakage_factors$rain)
boxplot(leakage_factors$windspeed)
boxplot(leakage_factors$flood_risk)
boxplot(leakage_factors$slope)
boxplot(leakage_factors$watermin)

## extract outliers
out <- boxplot.stats(leakage_factors$area_ha)$out
# find corresponding row
out_ind <- which(leakage_factors %in% c(out))


## Density Plot
leakage_factors %>% plot_density()
## rain has different distribution

## Scatter Plots of all possible factor combinations
pairs(leakage_factors)

pairs.panels(leakage_factors[-1],
             gap = 0,
             pch=21)
# similar distributions of variables --> many landfills with low values, few with high values


#### Correlation Matrix
leakage_factors %>% plot_correlation() 
## positive correlation: high correlation between watermin & dist_water, waste & area
## negative correlation: rel. high between waste & rain, waste & distance water
# --> multicollinearity issues

## main variables
# drop single water distances
leakage_factors_main <- leakage_factors[-c(1,4,5,7)]
leakage_factors_main %>% plot_correlation() 



#### III. Factor Analysis ####
## alternative to manual assessment  of correlation matrix (sufficient as background knowledge)

describe(leakage_factors)
X <- leakage_factors
X2 <- leakage_factors[-c(4,5,7)]# only include watermin
X4 <- leakage_factors_main # pre-defined variable set (from correlation matrix)

# KMO test to measure of how suited your data is for Factor Analysis
KMO(r=cor(X)) # items with KMO < 0.5 should be dropped
KMO(r=cor(X2)) # 0.64 - factor analysis could make sense (must be > 0.6)
KMO(r=cor(X4)) # 0.54

cortest.bartlett(X3) # significance level < 0.05 indicates factor analysis might be useful
cortest.bartlett(X4) # doesn't make sense to do factor analysis


#### Principal Component Analysis

## Number of Components
parallel <- fa.parallel(X4) # 2 factors & 1 component

pc.1 <- principal(X4, rotate='none', nfactors=2) 
pc.2 <- principal(X4, rotate='none', nfactors=ncol(X4))
pc.3 <- principal(X4, rotate='none', nfactors=3) # PCA with pre-defined variable set (from correlation matrix)

print(loadings(pc.1), cutoff= .55) # only 0.515% of variance explained
print(loadings(pc.2), cutoff= .55) 
print(loadings(pc.3), cutoff= .55) # 0.687% of variance explained

# rotate factors
pc.3.r <- principal(X4, nfactors=3, rotate='varimax') # water & flood should have opposed sign
print(loadings(pc.3.r), cutoff= .55)

fa.diagram(pc.3, simple=TRUE, cut=.55, digits=2)


#### Factor Analysis
fa.none <- fa(r=X4, nfactors = 3, 
              fm="pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate="varimax")
print(fa.none, cut = .4)

## graph factor loadings
fa.diagram(fa.none)



#### IV. Cluster Analysis ####
## Unsupervised Machine Learning -- needs sufficient sample size --> more landfill data needed

#### 0) Normalize

leakage_factors_km <- leakage_factors_main

## Normalize Data to account for different scales 
# mean = 0 & standard deviation = 1
leakage_factors_norm <- scale(leakage_factors_km)

head(leakage_factors_norm)
hist(leakage_factors_norm)



#### a) Distance Measures

# similar objects are close to one another
distance <- get_dist(leakage_factors_km, "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



#### b) K-Means Clustering

# partitions observations into k clusters in which each observation belongs to the cluster with the closest average

## rather use similarity measure than distance measure as similar density plots ???


#### Optimal Number of Clusters

## 1. Elbow method

# minimize total intra-cluster variation
fviz_nbclust(leakage_factors_km, kmeans, method = "wss") # unstandardized: 3 clusters, standardized: 4 clusters


## 2. Silhouette Method
# how well each object lies within its cluster
fviz_nbclust(leakage_factors_norm, kmeans, method = "silhouette") # unstandardized: 8 clusters, standardized: 6 clusters


## 3. Gap statistic
gap_stat <- clusGap(leakage_factors_km,
                    FUN = kmeans,
                    K.max = ncol(leakage_factors_km),
                    nstart = 25,
                    B = 50)

# Plotten der Anzahl der Cluster vs. Lückenstatistik
fviz_gap_stat(gap_stat) # 1 cluster, unstandardized: 2 clusters


## 4. Consensus-based
n_clusters(leakage_factors_km, package = "all", standardize = F) # standardized: 4 clusters, unstandardized: 3 clusters



#### K-means Clustering

## clustering using standardized (normalized) data
set.seed(123) # set seed to make clustering reproducible
km <- kmeans(leakage_factors_norm, centers = 3, nstart = 25) # 3 clusters (low, medium & high risk), random starting condition
km 

## un-standardized data
set.seed(99)
km6 <- kmeans(leakage_factors_km, centers = 3, nstart = 25) # 3 clusters (low, medium & high risk), random starting condition
km6


## Quality of k-means clustering
# Between Sum of Squares divided by Total Sum of Squares
## 3 cluster
km$betweenss / km$totss * 100 # 44,76%
km6$betweenss / km6$totss * 100 # 70,35% 

# plot clustering results
fviz_cluster(km6, data = leakage_factors_km, stand = F)

# mean variable values per cluster
aggregate(leakage_factors_km, by=list(cluster=km6$cluster), mean)

# add clusters to original data
leakage_factors_main <- cbind(leakage_factors_main, km_cluster_unstand = km6$cluster)
leakage_factors_main <- cbind(leakage_factors_main, km_cluster_stand = km$cluster)


## downsides of K-means
# an object with an extremely large value may substantially distort the distribution of objects in clusters/groups
# e.g. watermin (range from 0 to 2014, standard deviation ~ 592)



#### k-mediod clustering - PAM 
## more robust to noise (e.g. outliers)
# kmeans uses centroid (average of all oints), kmediods uses mediod (most centrally located object or minimal average dissimilarity to all objects)
# PAM (Partitioning around medoids) most robust

pam <- pam(leakage_factors_km, 3, metric = "euclidean", stand = F, medoids = "random", nstart = 25)
pam

fviz_cluster(pam, data = leakage_factors_km, stand = F)

# cluster mediods
pam$id.med
pam$medoids

leakage_factors_main <- cbind(leakage_factors_main, pam_unstand = pam$cluster)



#### c) Hierarchical Clustering

# Compute the dissimilarity matrix
hc_dist <- dist(leakage_factors_norm, method = "euclidean")

## single linkage
# uses maximum distance between observations
hc_single <- eclust(leakage_factors_km, "hclust", k = 3, hc_method = "single", graph = F)
plot(hc_single)
cutree(hc_single, k = 3)

## complete linkage
# uses maximum distance between observations
hc_complete <- eclust(leakage_factors_km, "hclust", k = 3, hc_method = "complete", graph = F)
#hc_complete <- hclust(hc_dist, method = "complete")
plot(hc_complete) # 3 cluster
rect.hclust(hc_complete,
            k = 3, # k is used to specify the number of clusters
            border = "green")
cutree(hc_complete, k = 3)


## average linkage
# average distance
hc_average <- eclust(leakage_factors_main, "hclust", k = 3, hc_method = "average", graph = F)
#hc_average <- hclust(hc_dist, method = "average")
plot(hc_average)
rect.hclust(hc_average,
            k = 3, # number of clusters
            border = "green")
cutree(hc_average, k = 3)


## centroid
hc_centroid <- eclust(leakage_factors_km, "hclust", k = 3, hc_method = "centroid", graph = F)
#hc_centroid <- hclust(hc_dist, method = "centroid")
plot(hc_centroid) # 3 clusters
cutree(hc_centroid, k = 3)


## Ward D
hc_ward <- eclust(leakage_factors_km, "hclust", k = 3, hc_method = "ward.D", graph = F)
fviz_dend(hc_ward, k = 3, 
          k_colors = c("blue", "green", "orange"),
          rect = T # Add rectangle around groups
)

## Silhouette Method
# average distance between clusters - optimal value near 1
# find best hierarchical clustering approach
fviz_silhouette(hc_single) # 0.86 for 2 clusters
fviz_silhouette(hc_complete) # 0.81 for 2 clusters, 0.7 for 3 clusters
fviz_silhouette(hc_average) # 0.81 for 2 clusters
fviz_silhouette(hc_centroid) # 0.78 for 2 clusters
fviz_silhouette(hc_ward) # 0.78 for 2 clusters


aggregate(leakage_factors_km, by=list(cluster=hc_complete$cluster), mean)

leakage_factors_main <- cbind(leakage_factors_main, ec_cluster_unstand = hc_complete$cluster)



#### d) Fuzzy Clustering
fc <- fanny(leakage_factors_km, 3, metric = "euclidean")

# Dunn index - low value indicates fuzzy clustering, value close to 1 indicates a near-crisp clustering
fc$coeff # 0.67

fviz_cluster(fc, repel = T, palette = "jco", ggtheme = theme_minimal(), legend = "right")
fviz_silhouette(fc, palette = "jco", ggtheme = theme_minimal()) # 0.74 for 3 clusters

leakage_factors_main <- cbind(leakage_factors_main, fc_cluster_unstand = fc$clustering)



#### e) Density-Based Clustering 
## as kmeans & hierarchical clustering severely affected by noise and outliers in data

# find optimum eps value
kNNdistplot(leakage_factors_km, k = 3)
abline(h = 150, lty = 2)

# Compute DBSCAN using fpc package
set.seed(123)
db <- dbscan(leakage_factors_km, eps = 150, MinPts = 3)

# Plot DBSCAN results
fviz_cluster(db, col = db$cluster, data = leakage_factors_norm, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point", palette = "jco", ggtheme = theme_classic())

leakage_factors_main <- cbind(leakage_factors_main, db_cluster_unstand = db$cluster)



#### V. Manual Risk Assessment ####

describe(leakage_factors_main)

## attribute low, medium & high risk to landfills according to literature values & variable quantiles 
# low: 0, medium: 1, high: 2

## rain
# < 3 : low risk 
# < 15: medium risk
quantile(leakage_factors_main$rain)
quantile(leakage_factors_main$rain, probs = c(1/3, 2/3))

## wind
# 0: low risk
# < 11: medium risk
quantile(leakage_factors_main$windspeed)
quantile(leakage_factors_main$windspeed, probs = c(1/3, 2/3))

## water distance
# > 1km: low risk
# < 1km & > 500m: medium risk
# < 500m: high risk
quantile(leakage_factors_main$watermin)
quantile(leakage_factors_main$watermin, probs = c(1/3, 2/3))

## flooding (%)
# < 10: low risk
# < 20: medium risk
quantile(leakage_factors_main$flood_risk)
quantile(leakage_factors_main$flood_risk, probs = c(1/3, 2/3))

## slope 
# first convert slope from degrees into percentage
leakage_factors_main$slope_perc <- leakage_factors_main$slope * 100 / 360

# < 0.15: low risk
# < 0.3: medium risk
quantile(leakage_factors_main$slope)
quantile(leakage_factors_main$slope, probs = c(1/3, 2/3))

## (landfill area)
# < 1: low risk
# < 6: medium risk
quantile(leakage_factors_main$area_ha)
quantile(leakage_factors_main$area_ha, probs = c(1/3, 2/3))

# ## storms
# # = 0: low risk
# # < 1: medium risk
# quantile(leakage_factors_main$no_storms)
# quantile(leakage_factors_main$no_storms, probs = c(1/3, 2/3))

#leakage_factors_main$risk1 <- leakage_factors_main$risk
leakage_factors_main$risk <- 0
## compute risk value per landfill
# 0: low, 1: medium, 2: high

i <- 1
while (i <= nrow(leakage_factors_main)) {
  ## rain risk
  ifelse(leakage_factors_main$rain[i] < 3, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$rain[i] < 15, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))

  ## windspeed
  ifelse(leakage_factors_main$windspeed[i] == 0, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$windspeed[i] < 11, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))
  
  ## water
  ifelse(leakage_factors_main$watermin[i] > 1000, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$watermin[i] > 500, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))

  ## flooding
  ifelse(leakage_factors_main$flood_risk[i] < 10, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$flood_risk[i] < 20, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))
  
  ## slope
  ifelse(leakage_factors_main$slope[i] < 0.15, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
         ifelse(leakage_factors_main$slope[i] < 0.45, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
                leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))
  
  # ## area
  # ifelse(leakage_factors_main$area_ha[i] < 1, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
  #        ifelse(leakage_factors_main$area_ha[i] < 6, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
  #               leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))
  # 
  # ## storms
  # ifelse(leakage_factors_main$no_storms[i] == 0, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 0, 
  #        ifelse(leakage_factors_main$no_storms[i] <= 1, leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 1, 
  #               leakage_factors_main$risk[i] <- leakage_factors_main$risk[i] + 2))
  
  ## increment i
  i <- i+1
}


## add low, medium or high risk to risk value
leakage_factors_main$risk_label <- "medium"
leakage_factors_main$risk_label[leakage_factors_main$risk <= 3] <- "low"
leakage_factors_main$risk_label[leakage_factors_main$risk >= 7] <- "high"


## add risk data to landfills
landfills$km_cluster_unstand <- leakage_factors_main$km_cluster_unstand
landfills$risk <- leakage_factors_main$risk
landfills$risk_label <- leakage_factors_main$risk_label



#### VI. Cluster Validation ####

## 1. Internal Validation

## assess cluster Connectivity, Dunn Index & Silhouette Width
# Connectivity: degree of connectedness of the cluster - should be minimzed (0)
# Silhouette: how similar an object is to other objects in its own cluster versus those in the neighbor cluster - interval [-1,1], well-clustered near 1 and poorly clustered near -1
# Dunn: ratio of smallest distance between observations not in the same cluster to the largest intra-cluster distance - should be maximized (infinity)
intern <- clValid(leakage_factors_km, 2:4, clMethods=c("hierarchical","kmeans","pam"),
                  validation="internal")
summary(intern)
optimalScores(intern) # kmeans returns optimal clusters, PAM has similar results
plot(intern)


## 2. External Validation

plot_qq(leakage_factors_main[,1:8], by = "km_cluster_unstand")
plot_qq(leakage_factors_main[,c(1:5,14)], by = "risk_label")

#leakage_factors_main[,1:8] %>% plot_density(geom_density_args = list("fill" = "km_cluster_unstand"))

plot_boxplot(leakage_factors_main[,c(1:6)], by = "km_cluster_unstand")
plot_boxplot(leakage_factors_main[,c(1:5,7)], by = "km_cluster_stand") # not good results, maxima spread across clusters
plot_boxplot(leakage_factors_main[,c(1:5,9)], by = "ec_cluster_unstand") # similar to kmeans
plot_boxplot(leakage_factors_main[,c(1:5,10)], by = "fc_cluster_unstand")
plot_boxplot(leakage_factors_main[,c(1:5,14)], by = "risk_label")

plot_scatterplot(split_columns(leakage_factors_main[,c(1:6)])$continuous, by = "km_cluster_unstand")
plot_scatterplot(split_columns(leakage_factors_main[,c(1:5,9)])$continuous, by = "ec_cluster_unstand")
plot_scatterplot(split_columns(leakage_factors_main[,c(1:5,13)])$continuous, by = "risk")



## class shares of best cluster results
table(leakage_factors_main$km_cluster_unstand)


## convert to sf object for easier plotting
landfills_sf <- st_as_sf(landfills)

## save results as shapefile
st_write(landfills_sf[,-c(16:17)], paste(dir,"/landfill_clusters.gpkg", sep= ""), overwrite=T, append=F)

