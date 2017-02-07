

# The restaurant owner would like to add wine suggestions to the menu next to each meat and analyze her
# customer ordering habits in general. She is also interested in showing a picture of a popular meal of hers in
# an ad in Gourmet Diner magazine. Your job is to address her interests and also to report anything else of
# interest in terms of customer ordering habits, all in a nice summary report that is complete but to the point.

#Association Homework
library(arules)
library(arulesViz)
library(dplyr)
library(tidyr)
setwd('C:/Users/Will/Documents/MSA/fall/data_mining/homework_1/data')
data <- read.csv('restaurantData.csv', stringsAsFactors = T)


#interest 1.   Find associations between wine and meat

data %>%
  dplyr::filter(type == 'Meat' | type == 'Wine') -> meat_wine 
  
meat_wine %>% 
  mutate(orderNumber = sort(rep(seq(1,nrow(meat_wine)/2),2))) %>%
  mutate(orderNumber = factor(orderNumber)) %>% 
  tidyr::spread(type , order)-> meat_wine

meat_wine <- as(meat_wine, 'transactions')

#target is association rules,
#minimum length of association is two factors
#min confidence = .01
#min support = .01
#min lift = 1
apriori_rules <- apriori(meat_wine, list(target = 'rules', supp=0.01, minlen = 2, conf = .01))  

#Return support for pairs:
head(inspect(sort(apriori_rules, by = 'lift')),10)

plot(apriori_rules, cex = 1)

#interest 2.   Most popular meal 
 
data %>% 
  mutate(orderNumber = sort(rep(seq(1,nrow(data)/3),3))) %>% 
  tidyr::spread(type,order) %>% 
  mutate(meal_combos  = paste(Meat, Side, Wine, sep = ', ')) %>% 
  group_by(meal_combos) %>% 
  summarise(Number_of_orders = n()) %>% 
  arrange(desc(Number_of_orders))-> meat_wine_side
  
head(meat_wine_side)

#The most popular meal combo is:
# Pork Tenderloin, Roasted Root Veg, Cantina Pinot Bianco with 1154 orders,
# 82 orders higher than the next highest order



