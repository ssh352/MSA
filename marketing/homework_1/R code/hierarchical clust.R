
# Marketing Homework 1
# Marketing director wants to find different segments
#' so the company can invest in productions that may be
#' more attractive by the different segments
library(randomForest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rpart)
library(NbClust)

setwd('C:/Users/Will/Documents/MSA/fall3/marketing/homework1/data')
df <- read.csv('theater.csv')
str(df)

#notice there are 23 NA values for age
#and 26 NA values for education
summary(df)

############################### impute missing values #################
#######################################################################
#we will build a model to impute the missing values
set.seed(1343)
full_df <- df[complete.cases(df),]
na_df <- df[!complete.cases(df),]
sample <- sample(1:nrow(full_df),nrow(full_df)*0.75)
train <- full_df[sample,]
test <- full_df[-sample,]
 
####### Start by imputing education
#build linear and random forest models
linear_mod_educ <- lm(educ ~ attitude + planning + goodval + 
                   income + cnty, data = train[,-6])
set.seed(5)
forest_educ <- randomForest(educ ~ ., data = train[,-6])

#calculate predictions and plot residuals
educ_pred_forest <- round(predict(forest_educ, test[-6]))
educ_pred_lm <- round(predict(linear_mod_educ, test[,-6]))
edu_pred_df <- data.frame(actual = test$educ, forest = educ_pred_forest, 
                          forest_resid = educ_pred_forest - test$educ,
                          forest_sq_er = (educ_pred_forest - test$educ)^2,
                          lm = educ_pred_lm,
                          lm_resid = educ_pred_lm - test$educ,
                          lm_sq_er = (educ_pred_lm - test$educ)^2,
                          mean_val = (round(mean(train$educ)) - test$educ)^2,
                          ensemble_sq_er = ((round((educ_pred_forest + educ_pred_lm)/2) - test$educ)^2))

mse_ensemble = sum(edu_pred_df$ensemble_sq_er)/nrow(edu_pred_df)
mse_forest = sum(edu_pred_df$forest_sq_er)/nrow(edu_pred_df)
mse_lm = sum(edu_pred_df$lm_sq_er)/nrow(edu_pred_df)
mse_mean = sum(edu_pred_df$mean)/nrow(edu_pred_df)



####### Now we impute for age
#build linear and random forest models
linear_mod_age <- lm(age ~ attitude + parents + goodval + income, data = train[,-7])
set.seed(5)
forest_age <- randomForest(age ~ ., data = train[,-7])

#calculate predictions and plot residuals
age_pred_forest <- round(predict(forest_age, test[-7]))
age_pred_lm <- round(predict(linear_mod_age, test[,-7]))
age_pred_df <- data.frame(actual = test$age, forest = age_pred_forest, 
                          forest_resid = age_pred_forest - test$age,
                          forest_sq_er = (age_pred_forest - test$age)^2,
                          lm = age_pred_lm,
                          lm_resid = age_pred_lm - test$age,
                          lm_sq_er = (age_pred_lm - test$age)^2,
                          mean_val = (round(mean(train$age)) - test$age)^2,
                          ensemble_sq_er = (round((age_pred_forest + age_pred_lm)/2) - test$age)^2)

mse_ensemble = sum(age_pred_df$ensemble_sq_er)/nrow(age_pred_df)
mse_forest = sum(age_pred_df$forest_sq_er)/nrow(age_pred_df)
mse_lm = sum(age_pred_df$lm_sq_er)/nrow(age_pred_df)
mse_mean = sum(age_pred_df$mean)/nrow(age_pred_df)




#' ensemble had lower MSE on the test set for age, but the 
#' basic linear model had a lower MSE on the test set for education
#'  so we will impute using these two methods
#'  

######   Imputation
impute_educ_df <- na_df[is.na(na_df$educ),]
impute_age_df <- na_df[is.na(na_df$age),]

new_educ <- round(predict(linear_mod_educ, impute_educ_df)) 
new_age <- round((predict(forest_age, impute_age_df) + 
                   predict(linear_mod_age, impute_age_df))/2)

df$age[as.numeric(names(new_age))] <- new_age
df$educ[as.numeric(names(new_educ))] <- new_educ

summary(df)  ##NA values are all gone
###########################################################################
############################### Done imputing###############################


#Before clustering the data we must transform the data so the scales are comparable
#Here we normalize the data to be between 0 and 1

#Create normalize function
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}

#apply the normalize function to each column
norm_df <- sapply(df[1:(length(df)-1)],normalize)


# first lets look at the clusters without taking location into account
dist_matrix_euclidean <- dist(norm_df, method = 'euclidean')
dist_matrix_manhattan <- dist(norm_df, method = 'manhattan')

# It appears 3 clusters does appear to be a correct number of clusters
hier_clust_euclid_ward <- hclust(dist_matrix_euclidean, method = 'ward.D')
plot(hier_clust_euclid_ward, xlab = 'test')

# It is hard to tell what is going on
hier_clust_euclid_average <- hclust(dist_matrix_euclidean, method = 'average')
plot(hier_clust_euclid_average)

#This looks like we could use either 2 or 5 cluster
hier_clust_manhattan_ward <- hclust(dist_matrix_manhattan, method = 'ward.D')
plot(hier_clust_manhattan_ward)# using manhatttan distance instead of ward, we still come up with the

# Hard to tell what is going on
hier_clust_manhattan_average <- hclust(dist_matrix_manhattan, method = 'average')
plot(hier_clust_manhattan_average)



#####Look at attributes for each cluster
clusters <- cutree(hier_clust_euclid_ward, k = 5)
df$clusters <- clusters

#3 does not make it easy to make recommendations, so we make 5

df %>% 
  group_by(clusters) %>% 
  mutate(n = n()) %>% 
  summarise_each(funs(mean), -cnty) ->  df_summary


# clust 1 - yo-pro
# clust 2 - Blue collar - low income, low education, late middle age, avg to get to, avg value, avg parent interest, avg 
# clust 3 - Typical middle class young adult
# clust 4 - elderly that really enjoys theatre
# clust 5 - Wealthy and sophisticated



#Data Dictionary

# 1.	Attitude: Respondents	were	asked	to	rate	the	experience	of	attending	live	theater	using	a	seven-point	
# scale	on	10	questions.	The	10	questions	were	very	highly	correlated	and	thus,	were	averaged	to	form	
# the	first	variable	called	"attitude".


# The	10	questions	are	1=not	fun	and	7=fun,	1=not	exciting	and	7=exciting,	1=boring	and	7=stimulating,	
# bad	and	7=good,	1=uncomfortable	and	7=comfortable,	1=cannot	appreciate	and	7=can	appreciate,	
# 1=irritating	and	7=relaxing,	1=disliked	and	7=lived	and	1=not	educational	and	7=educational.


# 2.	Planning:	1=spur	of	the	moment	and	7=requires	planning
# 3.	Parents:	1	=	my	parents	disliked	plays	and	7=my	parents	liked	plays
# 4.	Goodval:	1=too	expensive	and	7=good	value	for	money
# 5.	Getto:	1=	hard	to	get	to	and	7=easy	to	get	to	
# 6.  Age=Binned	into	categories	where	higher	number	indicates	older	people
# 7.	Educ=Binned	into	categories	where	higher	number	indicates	more	educated	
# 8.	Income:	Binned	into	categories	where	higher	number	indicates	higher	income
# 9.	Cnty:	Represents location	of	the	respondents	(county)



















#Next for clustering, we need to calculate a distance matrix that 
# the clustering algorithms can use.  In the NbClust package that 
# distance matrix is calculated for you. This package looks at 19 different
# indexes to consider the optimal number of clusters

# use NbClust package to determine the number of clusters to use
clust_euclidean_ward <- NbClust(norm_df, distance = 'euclidean', method = 'ward.D')
# * Among all indices:                                                
# * 4 proposed 2 as the best number of clusters 
# * 9 proposed 3 as the best number of clusters 
# * 1 proposed 4 as the best number of clusters 
# * 2 proposed 5 as the best number of clusters 
# * 2 proposed 6 as the best number of clusters 
# * 3 proposed 7 as the best number of clusters 
# * 2 proposed 15 as the best number of clusters 
#***** Conclusion *****                            

#  * According to the majority rule, the best number of clusters is  3 
# The indices that vote 3 are CH, Hartigan, Scott, TrCovW, TraceW, Rubin, Silhoutte,
# Ratkowsky, Ball

clust_manhattan_ward <- NbClust(norm_df, distance = 'manhattan', method = 'ward.D')
#   * Among all indices:                                                
# * 7 proposed 2 as the best number of clusters 
# * 2 proposed 3 as the best number of clusters 
# * 5 proposed 4 as the best number of clusters 
# * 2 proposed 5 as the best number of clusters 
# * 1 proposed 6 as the best number of clusters 
# * 2 proposed 11 as the best number of clusters 
# * 1 proposed 13 as the best number of clusters 
# * 3 proposed 15 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  2 
# 
# The indices that vote 2 are CH, CCC, Silhouette, Duda, PsuedoT2, Beale,
# mcClain



##############Extra code .. Not used for assignment #########################################################
#######################################################################
#lets take random samples and see how many clusters form
par(mfrow = c(2,4))
set.seed(15)
for(i in 1:8){
  sample <- sample(1:nrow(df), 30)
  samp_df <- norm_df[sample,]
  dist_matrix <- dist(samp_df, method = 'euclidean')
  hier_clust <- hclust(dist_matrix, method = 'Ward.s')
  print(plot(hier_clust))
}

#it is still difficult to tell which clusters are correct
#### Go to k-means from here and look into other distance metrics
sse_vect <- NULL
pct_improve_vect <- NULL
set.seed(15)
par(mfrow = c(1,1))
for(i in 1:100){
  k_means <- kmeans(norm_df,i, iter.max = 40)
  sse <- k_means$tot.withinss
  sse_vect <- c(sse_vect, sse)
  pct_improve <- ifelse(i > 1, (sse_vect[i-1] - sse_vect[i])/(sse_vect[i-1]),0)
  pct_improve_vect <- c(pct_improve_vect, pct_improve)
}
plot(y = sse_vect, x = 1:length(sse_vect), ylab = 'MSE', xlab = 'K means')
plot(y = pct_improve_vect[-1], x = 2:length(pct_improve_vect), 
     ylab = 'Percent SSE Reduction ', xlab = 'K means')
plot(y = sse_vect[1:20], x = 1:20, ylab = 'MSE', xlab = 'K means')
abline(v=5)
abline(v = 10)
abline(v = 15)
#look at percent improvement

#Choose a preliminary 5 and 10 clusters to look at
k_means_5 <- kmeans(norm_df,5)
k_means_10 <- kmeans(norm_df,10)
k_means_15 <- kmeans(norm_df,15)
df$clust_5 <- k_means_5$cluster
df$clust_10 <- k_means_10$cluster
df$clust_15 <- k_means_15$cluster
#lets learn about our clusters using decision trees
tree_5 <- rpart(as.factor(clust_5) ~ attitude + planning + parents + 
                  goodval + getto + age + educ + income, data = df)
tree_10 <- rpart(as.factor(clust_10) ~ attitude + planning + parents + 
                   goodval + getto + age + educ + income, data = df)
tree_15 <- rpart(as.factor(clust_15) ~ attitude + planning + parents + 
                   goodval + getto + age + educ + income, data = df)

plot(tree_5)
text(tree_5)
# clust_5: 
# cluster 1: high income, think they are easy to get to, elderly, doesn't require much planning
# cluster 2: High income, old, good attitude about theatre, requires planning
# cluster 3: Lower income
# cluster 4: High income, not old, don't have a great attitude about theatre
# cluster 5: High income, Think the plays are hard to get to 


plot(tree_10)  # these trees are actually pretty misleading of the actual 
text(tree_10)  # values

df  %>% 
  group_by(clust_10) %>% 
  summarise_each(funs(mean), -cnty) -> summ_10

# clust_10:  6, 10, 9, 1, 5 are the highest 

#          Yo-pros
# Market | cluster 1: Enjoys Theater, bad value, young, middle income, requires planning,very easy to get too

#          Will not Purchase
# Do not Market | cluster 2: Does not enjoy theater, no planning, parents disliked, bad value, hard to get to

#          Unlikely customers
# Market | cluster 3: somewhat enjoys theatre, lots of planning, parents disliked, bad value, hard to get to, older
# Market | cluster 4: somewhat enjoys theatre, lots of planning, parents enjoyed, bad value, hard to get to, young, high education, high income

#         Will not purchase
# Do not Market | cluster 5: Does not enjoy theatre, very young, low education, low income

#         Sophisticated upper class
# market | cluster 6: somewhat enjoys theatre, wealthy, middle aged, highly educated, average in other categories

#         Long night out
# Market | cluster 7: enjoys theater, high planning, parents liked, good value, easy to get to, middle aged, low education, low age

#         Something to do on free night
# Market | cluster 8: enjoys theater, no planning, parents liked, good value, easy to get to, middle aged

#         Will not purchase
# Market | cluster 9: does not enjoy theater, high planning, parents dislike, very old, very low education and income (retirees)

#         Infrequent customer
#Market | cluster 10:  enjoys theater, high planning, parents really liked, poor value, hard to get to, middle aged, middle edu, middle income


library(rgl)
df %>% 
  filter(clust_10 %in% c(6, 5)) -> plot_df 
plot3d(x = plot_df$income, y = plot_df$attitude, z = plot_df$getto, col = plot_df$clust_10)



