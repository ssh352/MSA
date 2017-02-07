# clustering example
library(ggplot2)
class <- c('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c','c')
x1 <- c(1,5,2,30,35,40,15,16,18)
x2 <- c(1,1,1,3,10,10,20,17,18)

clust_df <- data.frame(class, x1, x2)
ggplot(clust_df) + geom_point(aes(x = x1, y = x2, color = class))

#Step 1, Normalize the variables
# (This can be a normalization, or standardize)
normalize <- function(x){
  (x - min(x))/(max(x) - min(x))
}

clust_df_norm <- data.frame(sapply(clust_df[,2:3], normalize), class)
clust_matrix_norm <- sapply(clust_df[,2:3], normalize)
ggplot(clust_df_norm) + geom_point(aes(x = x1, y = x2, color = class))


#Step 2: Calculate distance matrix
dist_matrix <- dist(clust_matrix_norm, method = 'euclidean')
hier_clust_centroid <- hclust(dist_matrix, method = 'centroid')
hier_clust_ward <- hclust(dist_matrix, method = 'ward.D')

#Step 3. Perform Hierarchical clustering
plot(hier_clust_ward)
plot(hier_clust_centroid)


determine_k_means <- function(norm_df, max_k = 100, seed = 15){
  sse_vect <- NULL
  pct_improve_vect <- NULL
  set.seed(seed)
  par(mfrow = c(1,1))
  for(i in 1:max_k){
    k_means <- kmeans(norm_df,i, iter.max = 40)
    sse <- k_means$tot.withinss
    sse_vect <- c(sse_vect, sse)
    pct_improve <- ifelse(i > 1, (sse_vect[i-1] - sse_vect[i])/(sse_vect[i-1]),0)
    pct_improve_vect <- c(pct_improve_vect, pct_improve)
  }
  return(list('sse_vect' = sse_vect, 'pct_improve_vect'= pct_improve_vect))
}


norm_plot_df <- determine_k_means(clust_df_norm[,1:2], max_k = 8)
plot(y = norm_plot_df$sse_vect, x = 1:length(norm_plot_df$sse_vect), ylab = 'SSE', xlab = 'K means')



################ Example 2 ######################
################   Iris    ######################

iris <- datasets::iris

iris_norm <- sapply(iris[-5], normalize)
iris_dist <- dist(normalize(iris_norm), method = 'euclidean')

iris_hier <- hclust(iris_dist)
plot(iris_hier)

iris_pca <- princomp(iris)
new_iris <- data.frame(iris_pca$scores[,1:3])
new_iris$species = iris$Species
rgl::plot3d(x=new_iris$Comp.1, y=new_iris$Comp.2 ,z= new_iris$Comp.3, col = as.integer(new_iris$species))
#looks like there are three clusters


####K means
determine_k_means <- function(norm_df, max_k = 100, seed = 15){
  sse_vect <- NULL
  pct_improve_vect <- NULL
  set.seed(seed)
  par(mfrow = c(1,1))
  for(i in 1:max_k){
    k_means <- kmeans(norm_df,i, iter.max = 40)
    sse <- k_means$tot.withinss
    sse_vect <- c(sse_vect, sse)
    pct_improve <- ifelse(i > 1, (sse_vect[i-1] - sse_vect[i])/(sse_vect[i-1]),0)
    pct_improve_vect <- c(pct_improve_vect, pct_improve)
  }
  return(list('sse_vect' = sse_vect, 'pct_improve_vect'= pct_improve_vect))
}


k_means <- determine_k_means(iris_norm,max_k = 20)
plot(k_means$sse_vect, type = 'l')
abline(v = 3)
plot(k_means$pct_improve_vect[-1], type = 'l')
abline(v = 3)
k_means_3 <- kmeans(iris_norm, 3)

#simulation with a shuffled dataset
sim_wss <- NULL
for(i in 1:10000){
  simulated_df <- apply(iris_norm,2, function(x){
    x <- x[sample(1:length(x),length(x))]
    return(x)
  })
  
  k_means_sim <- kmeans(simulated_df,3, iter.max = 40) #  this was another common recommendation
  sim_wss <- c(sim_wss,k_means_sim$tot.withinss)
}

sim_norm_dist <- dist(simulated_df, method = 'euclidean')
plot(hclust(sim_norm_dist, method = 'ward.D'))  # 2 main clusters  up to 15

hist(sim_wss, xlim = c(0,40))
abline(v= k_means_3$tot.withinss, col = 'red')

(k_means_3$tot.withinss - mean(sim_wss))/mean(sim_wss)
#67% tighter clusters than if there was no pattern in the data

# Shows there are some separation between clusters,
# 













