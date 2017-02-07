#explore cust medical

#load libraries
libs <- c('ggplot2', 'plotly', 'dplyr', 'tidyr', 'zoo', 'psych','hexbin')
sapply(libs, require, character.only = TRUE)


#read data
cust_medical <- read.csv('cust_medical.csv', na.strings = '')
cust_medical$Date <- as.Date(cust_medical$Date)
cust_medical$Tobacco_Num <- as.numeric(cust_medical$Tobacco_Num)
cust_medical$Caffeine_Num <- as.numeric(cust_medical$Caffeine_Num)
cust_medical$Alcohol_Num <- as.numeric(cust_medical$Alcohol_Num)
cust_trans <- read.csv('cust_trans.csv')
cust_trans$Date <- as.Date(cust_trans$Date)

#can show if there are trends of certain variables over time:
#ex, does a person decrease smoking or tobacco use over time. 
# In this dataset that was not the case
summary(cust_medical)
cust_medical %>% 
  group_by(Cust_ID,Med_Gla) %>% 
  summarise(Date = Date[1],n = n()) %>% 
  filter(!is.na(Med_Gla)) %>% 
  mutate(tob_pct = n/sum(n))%>% 
  arrange(Cust_ID,Date) %>% 
  filter(tob_pct != 1)-> x



#create variable to calculate what percent of the variable existed in the medical history
cust_medical %>%
  select(-c(Date,Cov_ID)) %>% 
  na.omit %>% 
  mutate_each(funs(ifelse(. == 'Y',1,0)),
              -Tobacco_Num, -Caffeine_Num, -Alcohol_Num, -Cust_ID) %>%
  group_by(Cust_ID) %>% 
  summarise_each(funs(mean), -Cust_ID) -> aggr_cust_medical



#There are 13 people who signed up for two different policies on the same date
# and reported significantly different medical information
cust_medical %>% 
  group_by(Cov_ID) %>% 
  filter(Date == min(Date)) %>%  
  group_by(Cust_ID, Date) %>% 
  filter(n() > 1) -> different_info



#Is the medical information constant?
#The following section tests if customers 
#are consistent with the medical history they report over time
cust_trans %>% 
  mutate(merge = paste(Cov_ID, Date, sep = '.'))-> cust_trans
cust_medical %>% 
  mutate(merge = paste(Cov_ID, Date, sep = '.')) %>% 
  select(-c(Cov_ID, Date,Cust_ID)) -> cust_medical

trans_medical <- left_join(cust_trans, cust_medical, by = 'merge')

trans_medical %>% 
  filter(Transaction != 'RE') %>% 
  group_by(Cov_ID) %>% 
  filter("CL" %in% Transaction)  %>% 
  arrange(Date) %>%
  mutate(index = 1:n())%>% 
  top_n(2, index)%>% 
  select(starts_with('Med')) %>% 
  summarise_each(funs(sum(. == 'N')/n()))-> last_match

#if the number in each cell in last_match is either a 1 or 0 then whatever
#medical information was reported was consistant between the 
#initial and the claim or the change and the claim

#inded the only values are either zero or one, this checks out
apply(last_match[2:18],2,table)



#Now lets calculate the percent of customers that have inconsistent 
#Medical information. This is evident in the health condition columns
#If on a previous date a person says they have a history of heart attack,
#and then on a future date they report they don't have any history of a 
#heart attack.. then we know there is something wrong with the way medical
#information is recorded or the individual is not being truthful

#We are looking at cases that are not claims (since this medical info is automatically generated)
trans_medical %>% 
  filter(Transaction != 'RE' & Transaction != 'CL') %>%
  group_by(Cust_ID) %>% 
  arrange(Cust_ID, Date) %>% 
  filter(n() > 1) %>% #customers with only one observation can't be inconsistant
  select(Cust_ID, starts_with('Med')) %>% 
  summarise_each(funs(ifelse('Y' %in% .,   #If there is a Y in the sequence
                          ifelse(min(which(. == 'Y')) == n(), #and It is not the last value
                                     FALSE, #and a N comes after the first Y, Then True, Else False
                                     'N' %in% .[min(which(. == 'Y')):(n()-1)]),  
                          FALSE))) -> inconsistencies 
inconsistencies$mis_match <- apply(inconsistencies[2:18], 1, sum)
sum(inconsistencies$mis_match > 0)/nrow(inconsistencies)  #the approximate percent of customers with inconsistant information

#out of the accounts that had the ability to report medical 
#information that was contradictory 17.5% of the people did


#We are looking at cases that are not claims (since this medical info is automatically generated)
trans_medical %>% 
  filter(Transaction != 'RE' & Transaction != 'CL') %>%
  group_by(Cust_ID) %>% 
  filter(n_distinct(Cov_ID) > 1)%>% 
  arrange(Cust_ID, Date) %>% 
  filter(n() > 1) %>% #customers with only one observation can't be inconsistant
  select(Cust_ID, starts_with('Med')) %>% 
  summarise_each(funs(ifelse('Y' %in% .,   #If there is a Y in the sequence
                             ifelse(min(which(. == 'Y')) == n(), #and It is not the last value
                                    FALSE, #and a N comes after the first Y, Then True, Else False
                                    'N' %in% .[min(which(. == 'Y')):(n()-1)]),  
                             FALSE))) -> inconsistencies 
inconsistencies$mis_match <- apply(inconsistencies[2:18], 1, sum)
sum(inconsistencies$mis_match > 0)/nrow(inconsistencies)  #the approximate percent of customers with inconsistant information

#out of the accounts that had the ability to report medical 
#information that was contradictory 17.5% of the people did



