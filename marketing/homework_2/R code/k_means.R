# Marketing k-means homework
 
# A company is trying to identify need-based segments in the current beer market 
# for their new  beer and would like to target the selected segments
# using specific marketing campaigns. The company collected data from over
# 300 individuals about their preferences when purchasing/choosing beer.

# Use the non-hierarchical/  k--means segmentation method to get distinct
# segments using the sample data. Describe the characteristics
# of each of the beer segments in terms of the people's preferences
# and demographics.

library(NbClust)
library(ggplot2)
library(dplyr)
library(rgl)

setwd('C:/Users/Will/Documents/MSA/fall3/marketing/homework2/data')
df <- read.csv('beer.csv', stringsAsFactors = F)
names(df) <- tolower(names(df))
str(df)
summary(df)

#main issue
for(i in 1:length(df)){
  hist(df[,i], main = names(df)[i])
  readline() 
}

# This looked questionable but Dr. D said it was not a problem
table(df$good_taste)
ggplot(df, aes(good_taste)) + geom_bar() + ggtitle('Responses for Good Taste') + xlab('Preference Level')


##################create necessary functions################
determine_k_means <- function(norm_df, max_k = 100, seed = 15){
  sse_vect <- NULL
  pct_improve_vect <- NULL
  set.seed(seed)
  par(mfrow = c(1,1))
  for(i in 1:max_k){
    k_means <- kmeans(norm_df,i, iter.max = 40)
    ss <- k_means$tot.withinss
    sse_vect <- c(sse_vect, sse)
    pct_improve <- ifelse(i > 1, (sse_vect[i-1] - sse_vect[i])/(sse_vect[i-1]),0)
    pct_improve_vect <- c(pct_improve_vect, pct_improve)
  }
  return(list('sse_vect' = sse_vect, 'pct_improve_vect'= pct_improve_vect))
}

normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

standardize <- function(x){
  return((x - mean(x))/sd(x))
}
#############################################################

#start out by normalizing the dataset using range standardization
norm_df <- sapply(df[,-c(1,14,15)], normalize) 
# we remove gender because we want to know the kind of beer drinker they are
# not their gender. We will look to see if the clusters are predominately male or female later

#we also try out standardizing the dataset
std_df <- sapply(df[,-c(1,14,15)], standardize)
# we remove gender because we want to know the kind of beer drinker they are
# not their gender. We will look to see if the clusters are predominately male or female later

###################################################################
#look at SSE reduction by adding additional clusters
norm_plot_df <- determine_k_means(norm_df)
plot(y = norm_plot_df$sse_vect, x = 1:length(norm_plot_df$sse_vect), 
     ylab = 'SSE', xlab = 'K means', xlim = c(1,20))
abline(v=2)
abline(v = 3)
abline(v = 4)

#look at percent SSE reduction by adding additional clusters
plot(y = norm_plot_df$pct_improve_vect[-1], x = 1:(length(norm_plot_df$pct_improve_vect)-1), 
     ylab = 'Percent SSE Reduction ', xlab = 'K means',
     type = 'b', xlim = c(0,20), col = 'blue', main = 'Percent Reduction in SSE at K means')
abline(v = 3, col = 'orange', lwd= 3)
text(y = 0.1, x = 3.6, 'k = 3')
abline(v = 8) # it look like the percent reduction in SSE flatens out after 8

#look at SSE reduction by adding additional clusters
std_plot_df <- determine_k_means(std_df)
plot(y = std_plot_df$sse_vect, x = 1:length(std_plot_df$sse_vect), ylab = 'SSE', xlab = 'K means')
abline(v=8)
abline(v = 10)
abline(v = 15)

#look at percent SSE reduction by adding additional clusters
plot(y = std_plot_df$pct_improve_vect[-1], x = 1:(length(std_plot_df$pct_improve_vect)-1), 
     ylab = 'Percent SSE Reduction ', xlab = 'K means', type = 'b')
abline(v = 2)
abline(v = 5)
abline(v = 10) # it look like the percent reduction in SSE flatens out around 15
####################################################################


# It appears the optimal number of clusters is 2
##############Find Optimal Number Clusters within normalized df######################################
#lets try to determine how many clusters using nbclust package
euclid_complete <- NbClust(norm_df, distance="euclidean", min.nc=2, max.nc=15, method = "complete")
# * 12 proposed 2 as the best number of clusters 
# * 1 proposed 3 as the best number of clusters 
# * 6 proposed 5 as the best number of clusters 
# * 1 proposed 6 as the best number of clusters 
# * 1 proposed 7 as the best number of clusters 
# * 2 proposed 15 as the best number of clusters

manhattan_complete <- NbClust(norm_df, distance="manhattan", min.nc=2, max.nc=15, method = "complete")
# * 11 proposed 2 as the best number of clusters 
# * 3 proposed 3 as the best number of clusters 
# * 1 proposed 5 as the best number of clusters 
# * 4 proposed 6 as the best number of clusters 
# * 1 proposed 9 as the best number of clusters 
# * 2 proposed 14 as the best number of clusters 
# * 1 proposed 15 as the best number of clusters 

euclid_wards <- NbClust(norm_df, distance="euclidean", min.nc=2, max.nc=15, method = "ward.D")
# * 11 proposed 2 as the best number of clusters 
# * 4 proposed 3 as the best number of clusters 
# * 5 proposed 4 as the best number of clusters 
# * 1 proposed 12 as the best number of clusters 
# * 3 proposed 15 as the best number of clusters

manhattan_wards <- NbClust(norm_df, distance="manhattan", min.nc=2, max.nc=15, method = "ward.D")
# * 9 proposed 2 as the best number of clusters 
# * 5 proposed 3 as the best number of clusters 
# * 3 proposed 4 as the best number of clusters 
# * 2 proposed 5 as the best number of clusters 
# * 2 proposed 6 as the best number of clusters 
# * 2 proposed 15 as the best number of clusters 


euclid_centroid <- NbClust(norm_df, distance="euclidean", min.nc=2, max.nc=15, method = "centroid")
# * 10 proposed 2 as the best number of clusters 
# * 1 proposed 3 as the best number of clusters 
# * 2 proposed 7 as the best number of clusters 
# * 1 proposed 11 as the best number of clusters 
# * 3 proposed 12 as the best number of clusters 
# * 3 proposed 13 as the best number of clusters 
# * 3 proposed 15 as the best number of clusters


manhattan_centroid <- NbClust(norm_df, distance="manhattan", min.nc=2, max.nc=15, method = "centroid")
# * 8 proposed 2 as the best number of clusters 
# * 2 proposed 3 as the best number of clusters 
# * 4 proposed 4 as the best number of clusters 
# * 1 proposed 7 as the best number of clusters 
# * 6 proposed 8 as the best number of clusters 
# * 1 proposed 13 as the best number of clusters 
# * 2 proposed 15 as the best number of clusters 
####################### End ################################################


#Again it appears like there are 2 major clusters
##############Find Optimal number clusters within standardized df###############
#lets try to determine how many clusters using nbclust package
euclid_complete <- NbClust(std_df, distance="euclidean", min.nc=2, max.nc=15, method = "complete")
# * 4 proposed 2 as the best number of clusters 
# * 14 proposed 3 as the best number of clusters 
# * 2 proposed 4 as the best number of clusters 
# * 2 proposed 12 as the best number of clusters 
# * 2 proposed 15 as the best number of clusters 

manhattan_complete <- NbClust(std_df, distance="manhattan", min.nc=2, max.nc=15, method = "complete")
# * 10 proposed 2 as the best number of clusters 
# * 1 proposed 3 as the best number of clusters 
# * 1 proposed 4 as the best number of clusters 
# * 8 proposed 9 as the best number of clusters 
# * 1 proposed 14 as the best number of clusters 
# * 2 proposed 15 as the best number of clusters

euclid_wards <- NbClust(std_df, distance="euclidean", min.nc=2, max.nc=15, method = "ward.D")
# * 6 proposed 2 as the best number of clusters 
# * 9 proposed 3 as the best number of clusters 
# * 1 proposed 4 as the best number of clusters 
# * 1 proposed 9 as the best number of clusters 
# * 1 proposed 13 as the best number of clusters 
# * 5 proposed 15 as the best number of clusters 

manhattan_wards <- NbClust(std_df, distance="manhattan", min.nc=2, max.nc=15, method = "ward.D")
# * 7 proposed 2 as the best number of clusters 
# * 7 proposed 3 as the best number of clusters 
# * 3 proposed 4 as the best number of clusters 
# * 1 proposed 12 as the best number of clusters 
# * 2 proposed 13 as the best number of clusters 
# * 3 proposed 15 as the best number of clusters 

euclid_centroid <- NbClust(std_df, distance="euclidean", min.nc=2, max.nc=15, method = "centroid")
# * 10 proposed 2 as the best number of clusters 
# * 7 proposed 3 as the best number of clusters 
# * 1 proposed 4 as the best number of clusters 
# * 1 proposed 6 as the best number of clusters 
# * 1 proposed 7 as the best number of clusters 
# * 3 proposed 15 as the best number of clusters 


manhattan_centroid <- NbClust(std_df, distance="manhattan", min.nc=2, max.nc=15, method = "centroid")
# * 9 proposed 2 as the best number of clusters 
# * 2 proposed 3 as the best number of clusters 
# * 5 proposed 4 as the best number of clusters 
# * 1 proposed 6 as the best number of clusters 
# * 2 proposed 8 as the best number of clusters 
# * 3 proposed 9 as the best number of clusters 
# * 1 proposed 14 as the best number of clusters 
# * 1 proposed 15 as the best number of clusters
############################ End ###########################################




######################### Hierarchical ################
#hierarchical clustering
norm_dist <- dist(norm_df, method = 'euclidean')
plot(hclust(norm_dist, method = 'ward.D'))  # 2 main clusters  up to 15
plot(hclust(norm_dist, method = 'centroid')) # hard to read
plot(hclust(norm_dist, method = 'average')) # hard to read


std_dist <- dist(std_df, method = 'euclidean')# 2 or 4, up to 15
plot(hclust(std_dist, method = 'ward.D')) # 
plot(hclust(std_dist, method = 'centroid'))
plot(hclust(std_dist, method = 'average'))
####################### End ##############################


k_means_norm_2 <- kmeans(norm_df,2, iter.max = 40) #  this was the winner
k_means_norm_3 <- kmeans(norm_df,3, iter.max = 40) #  this was another common recommendation
k_means_norm_4 <- kmeans(norm_df,4, iter.max = 40) #  this was another common recommendation
k_means_norm_5 <- kmeans(norm_df,5, iter.max = 40) #  this was another common recommendation

k_means_std_2 <- kmeans(std_df,2, iter.max = 40) #  this was the winner
k_means_std_3 <- kmeans(std_df,3, iter.max = 40) #  this was another common recommendation
k_means_std_4 <- kmeans(std_df,4, iter.max = 40) #  this was another common recommendation


# lets see what the three principal compononents look like
norm_pca <- princomp(norm_df)
# the first 3 principal components account for ~50% of the variation
# lets see if there are visible clusters
norm_pca_df<- data.frame(norm_pca$scores[,1:3])
norm_pca_df$clust <- k_means_norm_5$cluster
#it doesnt appear as though we have clearly defined clusters
plot3d(norm_pca_df$Comp.1, norm_pca_df$Comp.2, norm_pca_df$Comp.3, col = norm_pca_df$clust)


std_pca <- princomp(std_df)
# the first 3 principal components account for ~50% of the variation
# lets see if there are visible clusters
std_pca_df<- data.frame(std_pca$scores[,1:3])
std_pca_df$clust <- k_means_std_4$cluster
#it doesnt appear as though we have clearly defined clusters
plot3d(std_pca_df$Comp.1, std_pca_df$Comp.2, std_pca_df$Comp.3, col = std_pca_df$clust)


###################################################
#Now lets take a look at the attributes of these clusters

#The based off these three components the clusters dont look well defined

# lets look at a summary of the clusters
#we'll choose the cluster that has the lowest wss
k_means_std_3$betweenss/k_means_std_3$totss 
k_means_norm_3$betweenss/k_means_norm_3$totss 
# out of both the std and norm at 3 clusters the between ss is higher for
# norm so use those clusters

df$cluster <- k_means_norm_3$cluster
df %>% 
  group_by(cluster) %>%
  mutate(n = n(), n_female = sum(sex ==2)) %>%
  mutate(pct_female = n_female/n) %>% 
  summarise_each(funs(mean), -id) %>% 
  select(-sex)-> summary_df


data.frame(summary_df)



################################ Validate the clusters #############

# Simulate to see if there is any actual clusters
sim_sse <- NULL
for(i in 1:10000){
  simulated_df <- apply(norm_df,2, function(x){
    x <- x[sample(1:length(x),length(x))]
    return(x)
  })
  
  k_means_sim <- kmeans(simulated_df,3, iter.max = 40) #  this was another common recommendation
  sim_sse <- c(sim_sse,k_means_sim$tot.withinss)
}


hist(sim_sse, xlim = c(250,400), main = 'Simulated Sum of Squared Errors of the Segments')
abline(v= k_means_norm_3$tot.withinss, lwd = 3, col = 'blue')
text(x = k_means_norm_3$tot.withinss - 20, y = 2000, 'Observed Segment \nWithin Sum of \nSquares')
(k_means_norm_3$tot.withinss - mean(sim_sse))/mean(sim_sse)
#####################################################################





# For the preference variables, the respondents were asked to indicate the
# importance of the variables in  their preference for beer 
#(0=Not Important to 9 =Extremely Important). 

# 1.Rich_flbd  (Rich and full bodied)  
# 2.No_aftertst  (No aftertaste) 
# 4.Refreshing  (Refreshing) 
# 5.Goes_easily  (Goes easy on me) 
# 6.Good_taste  (Good taste) 
# 7.Lo_price  (Low price) 
# 8.Attr_bottle  (Attractive bottle) 
# 9.Prestigious_brand   (Prestigious brand) 
# 10.Hi_quality  (High quality) 
# 11.Masculine  (Masculine) 
# 
# Categorical  variables (including demographics) 
# 12.Wk_consmp  (Weekly consumption by number of bottles) 
# 13.Age  (Age categories, 1= 21-25 years, 2=26-30 years,  
# 3=31-35 years, 4=36-40 years, 5=41-50 years, 6=51-60 years,
# 7=>60 years) 
# 14.Sex  (1=male, 2=female) 




#16.5% tighter clusters than would have appeared if there was no pattern
# Shows there are some separation between clusters,
# more than would occor at random





