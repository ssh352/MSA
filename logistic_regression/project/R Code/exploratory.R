
#exploratory analysis

library(sas7bdat)
library(dplyr)
library(ggplot2)
df <- read.sas7bdat('C:/Users/Will/Documents/MSA/fall/logistic_regression/MSA_logistic_proj/construction.sas7bdat')
df <- data.frame(df)
#look at data structure
str(df)

#look at first few rows
head(df)

#idetify number of unique variables
sapply(df, function(x) length(unique(x)))

#make binary variables factors
df %>%
  mutate_each(funs(as.factor),c(4,grep('competitor', names(df))) ) -> df

names(df) <- tolower(names(df))

#expore sector variable
table(df$sector,df$win_bid)[,2]/table(df$sector)

df$sector <-ifelse(df$sector %in% c(1,3,8,9,10),1,
                   ifelse(df$sector %in% c(4,5,6),2,3))

#potentially combine sectors 1,3,8,9,10 into one sector (around .2) 
#                            4,5,6      into one sector
#                            1,7        into one sector

#expore region variable
table(df$region_of_country,df$win_bid)[,2]/table(df$region_of_country)

df$region_of_country <- ifelse(df$region_of_country %in% c('Southeast', 'West'),'SE_W',
                               ifelse(df$region_of_country %in% c('Mid-west', 'Southwest'), 'M_S_W',
                               'Northeast'))
                               
#potentially combine Southeast and West
#potentially combine mid-west, southwest


table(df$region_of_country, df$sector, df$win_bid)


#explore continuous variables
cor(df[,sapply(df, is.numeric)])
pairs(df[,sapply(df, is.numeric)])



ggplot(df, aes(x = winning_bid_price__millions_ , fill = factor(win_bid))) + geom_density(alpha = .3) 
ggplot(df, aes(x = bid_price__millions_ , fill = factor(win_bid))) + geom_density(alpha = .3) 
ggplot(df, aes(x = estimated_cost__millions_ , fill = factor(win_bid))) + geom_density(alpha = .3) 


#These 3 variables are almost perfectly correlated
df  %>% 
  select(estimated_cost__millions_,
         bid_price__millions_,
         winning_bid_price__millions_) -> pca_df

#the 3 variables are converted into a single variable using PCA
pca = prcomp(pca_df, scale=F) 
summary(pca)

new_var <- pca$x[,1]
pca_df$pc <- new_var

#new variable plottes in density plot
ggplot(df, aes(x = new_var , fill = factor(win_bid))) + geom_density(alpha = .3) 



#The remaining variables are both very interesting
ggplot(df, aes(x = estimated_years_to_complete, fill = factor(win_bid))) + geom_density(alpha = .3) 
ggplot(df, aes(x = as.numeric(number_of_competitor_bids), fill = factor(win_bid))) + geom_density(alpha = .3) 



#should competitor bids be plotted as continuous or some other type

      comp_pct <- table(factor(df$number_of_competitor_bids), df$win_bid)[,2]/table(factor(df$number_of_competitor_bids))
      
      comp_pct <- data.frame(num_competitors = as.numeric(rownames(comp_pct)), prob_win = as.numeric(comp_pct),
                             n = as.numeric(table(factor(df$number_of_competitor_bids,))),
                             odds = as.numeric(comp_pct)/(1-as.numeric(comp_pct)),
                             log_odds = log(as.numeric(comp_pct)/(1-as.numeric(comp_pct))))
      
      #appear to have non-linearity in the log-odds
      ggplot(comp_pct) + geom_point(aes(x = num_competitors, y = prob_win, size = n))
      ggplot(comp_pct) + geom_point(aes(x = num_competitors, y = log_odds, size = n))
      
      log_mod <- glm(win_bid ~number_of_competitor_bids, data = df, family = "binomial")
      df_comp <- df[!duplicated(df$number_of_competitor_bids),]
      df_comp <- df_comp[order(df_comp$number_of_competitor_bids),]
      
      probs <- predict(log_mod,df_comp , type = 'response')
      comp_pct$pred_prob <- probs
      
      
      #It appears like the number of competitors can be modeled fine as a continuous variable
      ggplot(comp_pct, aes(x=num_competitors, y=prob_win))+ geom_point(aes(size = n)) +  
        geom_line(aes(x = num_competitors, y = pred_prob))
       

      

