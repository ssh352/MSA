
#predict which people will be committing fraud based on number of transactions
#fraud defined as people changing their coverage policy multiple times within the same day and then 
#dying

cust_trans %>% 
  group_by(Cov_ID, Date) %>% 
  filter(Transaction == 'CH' & n() > 1) -> x
  
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
  select(-c(Birthday)) -> surv_cust_trans


  cust_trans %>% 
  filter(censored == 0) %>% 
  group_by(Cov_ID) %>% 
  mutate(date_before_CL = Date[n()-1], 
         CL_date = max(Date)) %>% 
  mutate(diff = as.numeric(CL_date - date_before_CL)/365.25) %>% 
  select(Cov_ID, policy_life, date_before_CL, CL_Date, diff)-> y

z <- data.frame(left_join(y,x, by ='Cov_ID' ))
