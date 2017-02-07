


#survival analysis
#assumptions: 1) non-informative censoring
#             2)  that the survival curves for two strata (determined by the particular choices of values for
#                  the $x$-variables) must have hazard functions that are proportional over time 
#                  (i.e. constant relative hazard)

# #origin  = Date of first transaction
#end = Date of first claim 
#We are modeling the life of a policy

#one person can have multiple origins if they have two separate policies

#predicting without the use of time varying covariates: predict before they enter
# the system so we can accurately assign their rate




#install.packages('survival')
lib <- c('dplyr', 'survival', 'tidyr', 'ggplot2','parallel', 'pec')
sapply(lib, require, character.only = TRUE)


#read in data for survival analysis
fam_medical <- read.csv('fam_medical.csv')
cust_info <- read.csv('cust_info.csv')
cust_info$Birthday <- as.Date(cust_info$Birthday)
cust_medical <- read.csv('cust_medical.csv')
cust_medical$Date <- as.Date(cust_medical$Date)
cust_trans <- read.csv('cust_trans.csv')
cust_trans$Date <- as.Date(cust_trans$Date)

#select potential customer info variables
cust_info %>% 
  select(Gender,Race, Birthday, Cust_ID, Pounds, FeetInches, BloodType, Marriage) -> surv_cust_info


#select the first time a customer reported their health info
#for a certain policy
cust_medical %>% 
  group_by(Cov_ID) %>% 
  filter(Date == min(Date))-> surv_cust_medical



#create customer transaction database needed for survival analysis
#contains one observation per cov_id (policy) 
# end with variables: Cust_ID, Date, Cov_ID, Transaction, 
#                     Type, Cov_Limit, Income, policy_life, censored, age.
cust_trans %>%  
  group_by(Cust_ID, Cov_ID) %>% 
  filter(Transaction %in% c("IN", "CL")) %>% 
  mutate(policy_life = ifelse(as.numeric(max(Date) - min(Date)) != 0,
                              as.numeric(max(Date) - min(Date))/365.25,
                              NA)) %>%  
  mutate(censored = ifelse(is.na(policy_life), 1,0)) %>% 
  mutate(policy_life = ifelse(is.na(policy_life), 
                              as.numeric(as.Date('2013-07-01') - min(Date))/365.25,
                              policy_life))%>% 
  filter(Transaction == 'IN') %>% 
  select(-c(Reward_R, Reward_A)) -> surv_cust_trans
  
surv_cust_info %>% 
  select(Birthday, Cust_ID) -> birthday
  
surv_cust_trans <- merge(surv_cust_trans, birthday, by = 'Cust_ID')
surv_cust_trans%>% 
  mutate(age = as.numeric(Date - Birthday)/365.25) %>% 
  select(-c(Birthday, Date)) -> surv_cust_trans


#Finally, remove cust_id and Date from datasets since we are only worried
# about policy life

#combine surv_cust_trans, surv_cust_medical, surv_cust_info


surv_data <- merge(surv_cust_trans, surv_cust_info, by = 'Cust_ID')
surv_data <- merge(surv_data, fam_medical, by = 'Cust_ID')
surv_data <- surv_data %>% select(-Cust_ID)
surv_data <- merge(surv_data, surv_cust_medical, by = 'Cov_ID')
surv_data <- surv_data %>% select(-c(Cust_ID, Date, Birthday))

#To Determine if the cust_health info effects a persons survival curve
cust_trans %>%  
  group_by(Cust_ID) %>% 
  filter(Transaction != 'RE') %>% 
  summarise(max_date = max(Date)[1],
         censored = ifelse('CL' %in% Transaction,0,1)) %>% 
  mutate(Cov_ID = paste0(Cust_ID,'A'))-> surv_cust_trans

surv_cust_info %>% 
  select(Birthday, Cust_ID) -> birthday

surv_cust_trans <- merge(surv_cust_trans, birthday, by = 'Cust_ID')
surv_cust_trans%>% 
  mutate(age = as.numeric(max_date - Birthday)/365.25) %>% 
  select(-c(Birthday, max_date)) -> age_cust_trans
#Finally, remove cust_id and Date from datasets since we are only worried
# about policy life

#combine surv_cust_trans, surv_cust_medical, surv_cust_info


surv_data <- merge(surv_cust_trans, surv_cust_info, by = 'Cust_ID')
surv_data <- merge(surv_data, fam_medical, by = 'Cust_ID')
surv_data <- surv_data %>% select(-Cust_ID)
surv_data <- merge(surv_data, surv_cust_medical, by = 'Cov_ID')
surv_data <- surv_data %>% select(-c(Cust_ID, Date, Birthday))










surv_data %>%
  select(age,censored,ends_with("Num"), Tobacco, Alcohol, Caffeine, starts_with("Med")) -> health_info
refactor <- health_info[,6:25]
health_info <- health_info[,1:5]
refactor <- data.frame(lapply(refactor, factor))
health_info <- cbind(health_info,refactor)


surv <- Surv(health_info$age, health_info$censored)
health_info %>% 
  select(-c(age,censored)) -> health_info
cox <- coxph(surv~. , data = health_info)

#approximately 0 percent of the variation in life of a person can be explained
#by the health factors.. which we know to be untrue






# fit <- survfit(surv~1, conf.int = 0.95)
# plot(fit, xlab = 'Time', ylab = 'Survival Function', main = 'Survival Function:  P(T >= t)')

dat <- surv_data %>% select(-c(Cov_ID,censored, policy_life, Transaction))
refactor <- dat[,14:67]
dat <- dat[,1:13]
refactor <- data.frame(lapply(refactor, factor))
dat <- cbind(dat,refactor)
plot(cox.zph(cox))
x <- dat[surv_data$censored == 0,]
y <- surv_data[surv_data$censored == 0,]
ggplot(x,aes(x = FeetInches, y = y$policy_life)) + stat_binhex() + stat_smooth(method = "lm", formula =  y ~ poly(x,5))
ggplot(x,aes(x = Pounds, y = y$policy_life)) + stat_binhex() + stat_smooth(method = "lm", formula =  y ~ poly(x,5))
ggplot(x,aes(x = age, y = y$policy_life)) + geom_point(alpha = .02) + stat_smooth(method = "lm", formula =  y ~ poly(x,5))
ggplot(x,aes(x = y$policy_life, fill = Gender)) + geom_density(alpha = .3)


#ten fold cross validation
#needed to sample of the test data because it takes too long to run 
#on all test observations
set.seed(15)
data_rows <- 1:nrow(dat)
output_tbl <- NULL
surv_data %>% 
  select(age, Pounds, FeetInches, Gender, policy_life, censored) -> mod_dat

for(i in 5:10){
  #take sample to split on training and test sets
  sample <- sample(data_rows, round(nrow(mod_dat) * 0.1))
  
  #take sample of the test sample bc it is too computationally 
  #expensive to estimate a survival function for every observation
  test_sample <- sample(sample,500)
  
  #create the training dataset
  train <- mod_dat[-sample,]
  
  #create the test dataset
  test <- mod_dat[test_sample,]
  
  #create survival object
  survival <-  Surv(test$policy_life, test$censored)
  
  #train a cox_proportional model
  cox <- coxph(survival~ I(Pounds * FeetInches) + I(Pounds * age) +  
                 poly(age,5) + Gender + poly(Pounds,5) + poly(FeetInches,5), data = train)
  
  #estimate the survival function for each person in the test set
  list <- split(test, seq(nrow(test)))
  preds <- apply(list, function(x){
    as.numeric(read.table(textConnection(capture.output(survfit(cox, x))),skip=2,header=TRUE)[3])
  })
  
  #combine predictions along with the cv fold and the data
  #used to make the prediction
  preds <- unlist(preds)
  k_fold <- rep(i,length(preds))
  obs <- test_sample
  add <- data.frame(preds,k_fold, obs, test)
  
  
  #output predictions into dataset ad save as csv
  output_tbl <- rbind(output_tbl,add)
  cat(paste('fold', i , sep = ' '))
  write.csv(output_tbl, 'output_tbl2.csv', row.names = F)
  cat(paste('csv for fold', i ,' outputted', sep = ' '))
  data_rows <- data_rows[-sample]
}

output_tbl1 <- read.csv('output_tbl.csv')
output_tbl2 <- read.csv('output_tbl2.csv')

output_tbl <- rbind(output_tbl1, output_tbl2)
output_tbl$censored <- mod_dat$censored[output_tbl$obs]

#testing output table against actual values
output_tbl <- na.omit(output_tbl)
output_tbl <- output_tbl[output_tbl$censored == 0,]
output_tbl$actual <- mod_dat$policy_life[output_tbl$obs]
output_tbl$mean <- mean(mod_dat$policy_life)
output_tbl$median <- median(mod_dat$policy_life)
output_tbl$residual_pred <- output_tbl$preds - output_tbl$actual
output_tbl$residual_mean <- output_tbl$mean - output_tbl$actual
output_tbl$residual_median <- output_tbl$median - output_tbl$actual

ggplot(output_tbl, aes(x = actual, y = residual_pred)) + geom_point() +geom_abline(aes(slope = 0, intercept =0 ))



#look at error rate across k-folds;
output_tbl %>% 
  group_by(k_fold) %>% 
  summarise(pred_mean_err = mean(abs(residual_pred)),
            mean_err = mean(abs(residual_mean)),
            mean_err = mean(abs(residual_median)),
            sd_err = sd(residual_pred),
            var_err= var(residual_pred),
            n = n()) -> tbl



#calcualte mean squared error
mean_sq_error_preds <- sqrt(mean(output_tbl$residual_pred^2))
mean_sq_error_mean <- sqrt(mean(output_tbl$residual_mean^2))
mean_sq_error_median <- sqrt(mean(output_tbl$residual_median^2))





# 
# plot mean height over time
# plot mean age over time
# plot mean weight over time
#look at residuals over time. See if there is any predictive power in the first 10 years



cust_trans %>% 
  group_by(Cust_ID)%>% 
  summarise(Date = min(Date)) -> temp

time <- merge(temp, cust_info, by = 'Cust_ID')

time %>% 
  select(Cust_ID, FeetInches, Pounds, Birthday, Date) %>% 
  mutate(age = as.numeric(Date - Birthday)/365.25) %>% 
  arrange(Date) %>% 
  mutate(roll_age = rollmean(age, 1000, na.pad= T, align = 'center')) %>% 
  mutate(roll_weight = rollmean(Pounds, 1000, na.pad= T, align = 'center')) %>% 
  mutate(roll_height = rollmean(FeetInches, 1000, na.pad= T, align = 'center'))-> t



ggplot(t) + geom_line(aes(x = Date, y = roll_age))
ggplot(t) + geom_line(aes(x = Date, y = roll_weight))
ggplot(t) + geom_line(aes(x = Date, y = roll_height))


time %>% 
  mutate(age = as.numeric(Date - Birthday)/365.25) %>%
  select(Cust_ID, FeetInches, Pounds, Birthday, Date) %>% 
  mutate(year = year(Date)) -> all
  
all$year <- as.integer(all$year)
first <- 




ggplot(all) + 