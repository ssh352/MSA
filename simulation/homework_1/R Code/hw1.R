# Simulation Homework 1


#Simulate possible future values of 2017 drilling costs.
# o Currently, only previous information is available for 1960 - 2007.
# o Since the industry has changed tremendously over those decades, only
#   the information from 1990 - 2006 will be useful for this analysis. 2007
#   was an outlier and should be ignored.
# o Instead of looking at the distribution of actual costs, the Company's 
#   analysts recommend simulating possible annual changes in costs to get to 2017.
#   They have calculated geometric changes in the data set already, but they are open to 
#   other options if you explain why you chose them.
# o Instead of focusing on costs for oil, gas, and dry wells individually, the
#   Company's analysts recommend to treat them all equally and assume an average
#   cost applies to them all. (HINT: You should have 48 observations. 
#   Geometric changes from 1991 - 2006.)

# Previously the Price Analysis group has worked under the assumption that these 
# geometric changes from one year to the next follow a Normal distribution.
# Use QQ-plots or formal tests to see if you agree.


# Build a kernel density estimate of the distribution of geometric changes,
# using the 48 observations described above.


# Simulate possible future values of 2017 drilling costs under 
# both the assumption of Normality as well as under the kernel density 
# estimate you created (HINT: Run two simulations). Make a recommendation 
# for which one you feel the company should use.


library(tidyr)
library(dplyr)
library(ggplot2)
library(ks)
setwd('C:/Users/Will/Documents/MSA/fall3/simulation/data')
cost <- read.csv('cost.csv')


#####################################################
################ Exploration ########################
#####################################################

#2007 is an outlier and should be ignored
cost <- cost[cost$Date != 2007,]
cost$oil_return <- as.numeric(as.character(cost$oil_return))
cost$gas_return <- as.numeric(as.character(cost$gas_return))
cost$dry_return <- as.numeric(as.character(cost$dry_return))

#look at different costs
cost %>% 
  select(Date, oil_well_cost, gas_well_cost, dry_well_cost) %>% 
  gather(key = 'type', value = 'cost', oil_well_cost,
         gas_well_cost, dry_well_cost)-> drill_types_cost

ggplot(drill_types_cost, aes(factor(type), cost)) + geom_violin()
ggplot(drill_types_cost, aes(factor(type), cost)) + geom_boxplot()
ggplot(drill_types_cost, aes(x = Date, y = cost, color = type)) + geom_line() + 
  geom_point() + ggtitle('Cost of Drilling Over Time by Type')

#performing an anova on the costs across the different types,
# there is no difference among groups and the associated cost

fit <- aov(cost ~ type, data = drill_types_cost)
summary(fit)


#look at geometric changes in cost over time
cost %>% 
  select(Date, oil_return, gas_return, dry_return) %>% 
  gather(key = 'return', value = 'cost', oil_return,
         gas_return, dry_return) %>% 
  mutate(cost = as.numeric(cost))-> drill_types_pct

ggplot(drill_types_pct, aes(factor(return), cost)) + geom_violin()


#anova assumptions:
#1. equal variance across groups
#2. errors are normally distributed
#3. observations are independent  

#1. 
#check for equal variance
bartlett.test(drill_types_pct$cost, factor(drill_types_pct$return))
car::leveneTest(drill_types_pct$cost, factor(drill_types_pct$return))
ggplot(drill_types_pct, aes(factor(return), cost)) + geom_boxplot() +
  geom_point(position = position_jitter(width = 0.2))
#Both the levene's test and bartletts test failed to reject the null

#2. 
# Check for normality
qqnorm(drill_types_pct$cost[drill_types_pct$return == 'oil_return'])
qqline(drill_types_pct$cost[drill_types_pct$return == 'oil_return'])
nortest::ad.test(drill_types_pct$cost[drill_types_pct$return == 'oil_return'])

qqnorm(drill_types_pct$cost[drill_types_pct$return == 'gas_return'])
qqline(drill_types_pct$cost[drill_types_pct$return == 'gas_return'])
nortest::ad.test(drill_types_pct$cost[drill_types_pct$return == 'gas_return'])

qqnorm(drill_types_pct$cost[drill_types_pct$return == 'dry_return'])
qqline(drill_types_pct$cost[drill_types_pct$return == 'dry_return'])
nortest::ad.test(drill_types_pct$cost[drill_types_pct$return == 'dry_return'])
# This is the only one that appears to be significantly different form normal
# I'd say for the most part this assumption checks out


#3.
#Check independence 
  independence_check <- lm(oil_return ~ lag(oil_return), data = cost)
  summary(independence_check)
  ggplot(drill_types_pct, aes(x = Date, y = cost, color = return)) + geom_line() + 
  geom_point() + ggtitle('Cost of Drilling Over Time by Type')
#seems to check out, the lag is not significant and there is no visual pattern

  
# after checking the assumptions now we can perform an anova
# performing an anova across the different types to see if there
# are any differences
fit <- aov(cost ~ return, data = drill_types_pct)
summary(fit) 

#The global F-test fails to reject the null that there is no difference between
# the means of each group. 


#####################################################
################ End of Exploration #################
#####################################################


# since there is no difference between groups, lets add them all 
# into the same distribution

#since one wasn't normal, lets try the anderson darling on all
# and also the qqplot
return_pct <- drill_types_pct$cost
return_pct <- return_pct[!is.na(return_pct)]

qqnorm(return_pct)
qqline(return_pct)
nortest::ad.test(return_pct)

# based on the qqplot and the anderson darling test, I would disagree that 
# the changes come from a normal population

#The anderson darling test rejects the null that the distribution is normal
hist(return_pct)  # appears slightly left skewed

#calculate optimal bandwidth
bw <- density(return_pct,na.rm = T, bw="SJ-ste")$bw


#estimate kernal density 
density_function <- kde(x = return_pct, h = bw)

#calculate average cost
drill_types_cost %>% 
  filter(Date == 2006) %>% 
  select(cost) -> average_cost 
average_cost <- mean(average_cost[,1])


#simulation based on estimated density function
simulate_return <- matrix(rkde(n = 110000, fhat= density_function), ncol = 11)
simulate_return <- data.frame(simulate_return)
names(simulate_return) <- paste0('y_', 2007:2017)
simulate_return$final_return = apply(simulate_return,1,sum)
simulate_return$final <- average_cost + (exp(simulate_return$final_return) * average_cost)

par(mfrow = c(1,1))
hist(simulate_return$final)



#simulation based on a normal distribution
return_std_dev <- sd(return_pct)
return_mean <- mean(return_pct)
simulate_norm_return <- matrix(rnorm(110000,
                                      mean = return_mean, 
                                      sd = return_std_dev),ncol = 11)
simulate_norm_return <- data.frame(simulate_norm_return)
names(simulate_norm_return) <- paste0('y_', 2007:2017)
simulate_norm_return$final_return = apply(simulate_norm_return,1,sum)
simulate_norm_return$final <- exp(simulate_norm_return$final) * average_cost


#compare both distributions
par(mfrow = c(1,2))
hist(simulate_norm_return$final, xlab ='2017 cost', 
     main = 'Simulated using normal distribution',
     xlim = c(0,10000), breaks = 10)
hist(simulate_return$final, xlab = '2017 cost',
     main = 'Simulated using calcluated distribution', 
     xlim = c(0,10000), breaks = 12)

both_df <- data.frame('normal_final' = simulate_norm_return$final, 
                      'calculated_final' = simulate_return$final)

both_df <- gather(both_df)

#They are very similar
ggplot(both_df, aes(value)) + 
  geom_histogram(aes(fill = key), position ='identity', alpha =0.5)

# I think the company should use the calculated denstiy function instead 
# of the normal distribution. It appeared the data was left skewed,
# and fow that reason, choosing the normal may throw off results

 