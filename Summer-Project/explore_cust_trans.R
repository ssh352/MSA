
#load necessary libraries
libs <- c('ggplot2', 'plotly', 'dplyr', 'tidyr', 'timeseries', 'zoo', 'psych','hexbin')
sapply(libs, require, character.only = TRUE)

#read in data
emp <- read.csv('emp.csv')
fam_medical <- read.csv('fam_medical.csv')
cust_info <- read.csv('cust_info.csv')
cust_medical <- read.csv('cust_medical.csv')
cust_trans <- read.csv('cust_trans.csv')


#adjust variable classes
cust_trans$Cust_ID <- as.character(cust_trans$Cust_ID)
cust_trans$Cov_ID <- as.character(cust_trans$Cov_ID)
cust_trans$Cov_Limit <- as.numeric(cust_trans$Cov_Limit)
cust_trans$Reward_R <- as.character(cut(cust_trans$Reward_R, c(0,100,199,299, 499, 549, 559,569, 579)))
cust_trans$Reward_A <- as.numeric(cust_trans$Reward_A)
cust_trans$Date <- as.Date(cust_trans$Date)

#explore non aggregated data
str(cust_trans)
summary(cust_trans)
ggplot(cust_trans) + geom_histogram(aes(Income)) + ggtitle("Histogram of Income") + geom_boxplot(aes(x = Income))
ggplot(cust_trans) + geom_boxplot(aes(factor(Transaction), Income, colour = factor(Transaction))) + ggtitle("Histogram of Income") 
ggplot(cust_trans) + geom_boxplot(aes(factor(Type), Income, colour = factor(Type))) + ggtitle("Histogram of Income") 


ggplot(cust_trans) + geom_histogram(aes(Reward_A)) + ggtitle("Histogram of Reward_Amount")
ggplot(cust_trans) + geom_boxplot(aes(factor(Type), Reward_A, colour = factor(Type))) + ggtitle("Reward Amount By Policy Type") 
ggplot(cust_trans) + geom_boxplot(aes(factor(Reward_R), as.numeric(Reward_A), colour = factor(Reward_R))) + ggtitle("Amount Rewarded vs Cause of Death") 


ggplot(cust_trans) + geom_histogram(aes(x = Cov_Limit)) + ggtitle("Histogram of Coverage Limit")
ggplot(cust_trans) + geom_boxplot(aes(factor(Transaction), Cov_Limit, colour = factor(Transaction))) + ggtitle("Coverage Limit By Transaction") 
ggplot(cust_trans) + geom_boxplot(aes(factor(Type), Cov_Limit, colour = factor(Type))) + ggtitle("Coverage Limit By Coverage Type") 


ggplot(cust_trans, aes(x = Type)) + geom_bar(aes(y = (..count..)/sum(..count..), fill=  Type)) +
    ylab("Percent occurance")

ggplot(cust_trans, aes(x = Transaction)) + geom_bar(aes(y = (..count..)/sum(..count..), fill=  Transaction)) +
    ylab("Percent occurance")

ggplot(cust_trans, aes(x = Reward_R)) + geom_bar(aes(y = (..count..)/sum(..count..), fill=  Reward_R))+
    ylab("Percent occurance")

hexbinplot(Income ~ Cov_Limit, data = cust_trans)


##Time Series Exploration
###########################################################

#1. Examine the categorical variables and how they change over time
cust_trans[is.na(cust_trans)] <- 0
cust_trans$Transaction <- factor(cust_trans$Transaction)
cust_trans$Type <- factor(cust_trans$Type)
cust_trans$Reward_R <- factor(cust_trans$Reward_R)


x <- data.frame(model.matrix(~ Transaction + Type + Reward_R, data=cust_trans,
                             contrasts.arg=list(Transaction=contrasts(cust_trans$Transaction, contrasts=F), 
                                                Type=contrasts(cust_trans$Type, contrasts=F),
                                                Reward_R=contrasts(cust_trans$Reward_R, contrasts=F))))
x <- x[,-c(1,9,17)]


names(x)[8:14] <- c('accident', 'criminal', 'health', 'danger', 'war', 'aviation', 'suicide')
plot_cust_trans <-cbind(cust_trans,x)

plot_cust_trans %>% 
  select(one_of(c("Date",names(x)))) %>% 
  mutate_each(funs(rollsum(.,5000, align = 'center', na.pad= T)/5000), -Date) %>% 
  na.omit() ->  plot_cust_trans

plot_cust_trans %>% 
  select(Date, TypeT, TypeV, TypeW) %>% 
  gather(key = 'Type', value = 'pct.occurance', TypeT, TypeV, TypeW) -> Type_plot

plot_cust_trans %>% 
  select(Date, TransactionCH, TransactionCL, TransactionIN, TransactionRE) %>% 
  gather(key = 'Type', value = 'pct.occurance', TransactionCH, 
         TransactionCL, TransactionIN, TransactionRE) -> Transaction_plot

plot_cust_trans %>% 
  select(Date, accident, criminal, health, danger, war, aviation, suicide ) %>% 
  gather(key = 'Type', value = 'pct.occurance', accident, criminal, 
         health, danger, war, aviation, suicide) -> Reward_A_plot


ggplot(Type_plot) + geom_line(aes(x = Date, y = pct.occurance, colour = Type)) + facet_grid(. ~ Type)

ggplot(Transaction_plot) + geom_line(aes(x = Date, y = pct.occurance, colour = Type)) + facet_grid(. ~ Type)

ggplot(Reward_A_plot) + geom_line(aes(x = Date, y = pct.occurance, colour = Type)) + facet_grid(. ~ Type)



#1. Examine the continuous variables and how they change over time
cust_trans %>% 
  group_by(Cust_ID) %>% 
  summarise(Date = max(Date)[1], 
            cust_income = mean(Income, na.rm = T),
            cust_cov_lim = mean(Cov_Limit, na.rm = T)) %>% 
  select(Date, cust_income, cust_cov_lim) %>% 
  na.omit()  -> hex_plot 
  hex_plot %>% 
  mutate_each(funs(rollmean(.,5000, align = 'center', na.pad= T)), -Date) %>% 
  na.omit() -> cust_trans_plot
# 
# hexbinplot(cust_income ~ as.numeric(Date), data = hex_plot)
# hexbinplot(cust_cov_lim ~ as.numeric(Date), data = hex_plot)

ggplot(cust_trans_plot) + geom_line(aes(x = Date, y = cust_income))
ggplot(cust_trans_plot) + geom_line(aes(x = Date, y = cust_cov_lim))
 #Income and Coverage limit are not changing very much over time

cust_trans %>% 
  select(Date, Reward_A) %>% 
  na.omit()  -> hex_plot 
hex_plot %>% 
  mutate_each(funs(rollmean(.,5000, align = 'center', na.pad= T)), -Date) %>% 
  na.omit() -> cust_trans_plot

hexbinplot(Reward_A ~ as.numeric(Date), data = hex_plot)


ggplot(cust_trans_plot) + geom_line(aes(x = Date, y = Reward_A))


#Other things to look at over time

  #1 Number coverage policies
  #2 Rate at which people enter and leave 
  #   ~ we can only look at the people who have whole and who have a reward
  #2 Number customers
  #3 Reward amounts and variation
  #4 Probability a person claims a reward
  #5 pct of term whole and variable over time


#1  and #2 of active coverage policies and customers

#problem with this is this only works for people who have whole life insurance.. not term
#we could use survival analysis to predict which people are dead which had term and make them
#leave to get a more accurate estimate/
#We could also take a max number of years in which it is not reasonable a person with term is still
#a customer

#for this, we will assume all term policies lasted 30 years (if they did not claim)
#variable will be forever

#for now only look at accounts with whole life policies...
cust_trans %>%  
  arrange(Date) %>% 
  mutate(cov_indicator = ifelse(Transaction == "IN", 1, 
                                (ifelse(Transaction == "RE",-1,0)))) %>% 
  mutate(Active_Policies = cumsum(cov_indicator))-> cov_flow
                                 
cust_trans %>% 
  group_by(Cust_ID) %>% 
  summarise(first_trans = min(Date), last_trans = max(Date[Transaction == 'RE'])) %>% 
  mutate(match_first = paste(first_trans, Cust_ID, sep = '_')) %>% 
  mutate(match_last = paste(last_trans, Cust_ID, sep = '_')) %>% 
  select(Cust_ID, match_first, match_last) -> find_cust 

cov_flow %>% 
  mutate(match = paste(Date, Cust_ID, sep = '_')) %>% 
  mutate(cust_indicator = ifelse(match %in% find_cust$match_first,1,
                          ifelse(match %in% find_cust$match_last,-1,0))) %>% 
  mutate(Active_Customers = cumsum(cust_indicator)) %>% 
  mutate(Ratio_Cust_to_Policy =  Active_Policies / Active_Customers ) %>% 
  select(Date, Active_Policies, Active_Customers, Ratio_Cust_to_Policy, cust_indicator) -> account_flow

time_series <- gather(account_flow, key = 'variable', value = 'acct_info', Active_Policies, Active_Customers)

ggplot(time_series) + 
  geom_line(aes(x = Date, y = acct_info, colour = variable), size = 1.5) +
  ggtitle('Number of Active Customers and Policies Over Time') + 
  ylab('Count')

ggplot(account_flow) + 
  geom_line(aes(x = Date, y = Ratio_Cust_to_Policy), size = 1.5) +
  ggtitle('Ratio of Active Policies to Customers Over Time') + 
  ylab('Ratio')




#3 Reward amounts and variation
# average cost per person : smoothed average over every 1000 people
# variance of cost per person : smoothed average over every 1000 people

cust_trans %>% 
  group_by(Cust_ID) %>% 
  summarise(reward_paid = sum(Reward_A, na.rm = T),
            Date = max(Date)) %>% 
  arrange(Date) %>%
  mutate(rolling_avg_cost = rollmean(reward_paid, 1000, na.pad = T, align = 'center'))  %>%
  mutate(rolling_var_cost = rollapply(reward_paid, width = 1000, FUN = sd, na.pad = TRUE, align = 'center')) %>% 
  select(Date, rolling_avg_cost, rolling_var_cost, reward_paid) %>% 
  filter(!is.na(rolling_avg_cost)) %>% 
  select(Date, rolling_avg_cost, rolling_var_cost) %>% 
  gather(key = 'key', value = 'value', rolling_avg_cost, rolling_var_cost) ->cust_cost

ggplot(cust_cost) + 
  geom_line(aes(x = Date, y = value, colour = key)) + 
  ggtitle('Rolling Average & Variance or amount rewarded per customer')


#4 Percent of people claiming over time  
#calculate the percent of people claiming over time
cust_trans %>% 
  group_by(Cust_ID) %>% 
  summarise(reward_paid = sum(Reward_A, na.rm = T),
            Date = max(Date)) %>% 
  arrange(Date) %>%
  mutate(recieved_reward = ifelse(reward_paid > 0, 1,0)) %>% 
  mutate(rolling_pct = rollapply(recieved_reward, width = 1000, FUN = sum, na.pad = T, align = 'center')/1000) %>% 
  filter(!is.na(rolling_pct)) %>% 
  select(Date, rolling_pct) -> claim_pct
  
ggplot(claim_pct) + 
  geom_line(aes(x = Date, y = rolling_pct)) + 
  ggtitle("Rolling Percent of People That Recieve a Reward") + 
  ylab("Percent Recieved Reward") 




#5 Rolling new and rolling out
cust_trans %>% 
  filter(cust_trans$Transaction %in% c("IN", "RE")) %>% 
  group_by(Cust_ID, Transaction) %>% 
  summarise(Date = max(Date)) %>% 
  arrange(Date) -> temp

temp %>% 
  mutate(year_month = paste(year(Date), month(Date), sep = '.')) %>% 
  group_by(year_month) %>% 
  summarise(Date = max(Date),
            num_in = sum(Transaction == 'IN'),
            num_out = sum(Transaction == 'RE')) %>% 
  mutate(rate = ifelse(num_in > num_out, (num_in + 1)/(num_out + 1),(-num_out - 1)/(num_in + 1))) -> monthly_rate 

temp <- data.frame(temp)
temp %>% 
  mutate(indicator = ifelse(Transaction == 'IN', 1, 0)) %>% 
  mutate(roll_rate = rollsum(indicator, 1000, na.pad = T, align = 'center')) -> monthly_rate

  
ggplot(monthly_rate) + geom_line(aes(y = roll_rate, x = Date))



cust_trans  %>% group_by(Cust_ID)  %>% (mutate)
##############################
  
#customer_transaction notes:
# No Nas for Date Cust_Id Cov_ID transaction or Type.
# There are NAs for Reward_R, Reward_A, Cov_Limit, and Income
# The Nas for Reward_A and Reward_R occur in the same observation
# The NAs for Cov_Limit and Income occur in the same observations
# We have data since the inception of the company

# People claim multiple times and get rewarded multiple times???
#Data checks: make sure things match up
##########

#shows that The NAs that occur between cov_limit and income occur at the same place
#sum(which(is.na(Cov_Limit)) - which(is.na(Income))) # it is = 0


#shows that The NAs that occur between Reward_R and Reward_A occur at the same place
#sum(which(is.na(Reward_A)) - which(is.na(Reward_R))) # it is = 0



cust_trans %>% 
  filter(Cust_ID %in% aggr_cust_trans$Cust_ID[aggr_cust_trans$trans_n_R > 1]) -> mult_reward


#group by customer_Id and generate useful variables
#(takes a few minutes to run.. the transition function it uses
#  is not very efficient)
cust_trans %>%  
  group_by(Cust_ID) %>%
  summarise(#Create variables for Transaction
            trans_n = sum(!is.na(Transaction)),
            trans_n_unique = length(unique(Transaction[!is.na(Transaction)])),
            trans_n_I = sum(Transaction == 'IN'),
            trans_n_CL = sum(Transaction == 'CL'),
            trans_n_CH = sum(Transaction == 'CH'),
            trans_n_R = sum(Transaction == 'RE'),
            trans_n_switch = transition(Transaction),
            
            #Create variables for Cov_ID
            cov_id = sum(!is.na(Cov_ID)),
            cov_id_n_unique = length(unique(Cov_ID[!is.na(Cov_ID)])),
            cov_id_n_switch = transition(Cov_ID),
            
            #Create variables for Date
            date_n = sum(!is.na(Date)),
            date_tot_num_years = as.numeric(max(Date) - min(Date))/365.25,
            date_mean_days_btwn_trans = mean(as.numeric(Date - min(Date))),
            date_max = max(Date),
            date_min = min(Date),
            date_var_days_btwn_trans = var(as.numeric(Date - min(Date))),
            
            #Create variables for Cov_Limit
            cov_lim_n = sum(!is.na(Cov_Limit)),
            cov_lim_n_unique = length(unique(Cov_Limit[!is.na(Cov_Limit)])),
            cov_lim_mean = mean(Cov_Limit,na.rm = T),
            cov_lim_max = max(Cov_Limit, na.rm = T),
            cov_lim_min = min(Cov_Limit, na.rm = T),
            cov_lim_var = var(Cov_Limit,na.rm = T),
            
            #create variables for Type
            type_n = sum(!is.na(Type)),
            type_n_unique = length(unique(Type[!is.na(Type)])),
            type_n_T = sum(Type == 'T'),
            type_n_W = sum(Type == 'W'),
            type_n_V = sum(Type == 'V'),
            type_n_switch = transition(Type),
            
            #create variables for Reward_R
            Reward_R_n = sum(!is.na(Reward_R)),
            Reward_R_sum = sum(Reward_R, na.rm = T),
            Reward_R_1 = Reward_R[!is.na(Reward_R)][1],
            Reward_R_2 = Reward_R[!is.na(Reward_R)][2],
            Reward_R_3 = Reward_R[!is.na(Reward_R)][3],
            
            #create variables for Reward_A
            Reward_A_n = sum(!is.na(Reward_A)),
            Reward_A_sum = sum(Reward_A, na.rm = T),
            Reward_A_1 = Reward_A[!is.na(Reward_A)][1],
            Reward_A_2 = Reward_A[!is.na(Reward_A)][2],
            Reward_A_3 = Reward_A[!is.na(Reward_A)][3],

            #Create variables for Income
            income_n = sum(!is.na(Income)),
            income_n_unique = length(unique(Income[!is.na(Income)])),
            income_var = var(Income,na.rm = T),
            income_mean = mean(Income, na.rm = T),
            income_min = min(Income, na.rm = T),
            income_max = max(Income, na.rm = T),
            income_n_switch = transition(Income)) -> aggr_cust_trans

aggr_cust_trans <- data.frame(aggr_cust_trans)
#write.csv(aggr_cust_trans, 'aggr_cust_trans.csv', row.names = F)
summary(aggr_cust_trans)

#exploring aggr_cust_trans and whether they are alive or not
ggplot2(aggr_cust_trans) = geom_density()




