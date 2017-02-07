#answering questions asked:

#1. Can you use benfords law to detect if there are fraudulent addresses:
# https://en.wikipedia.org/wiki/Benford%27s_law
library(ggplot2)
library(tidyr)
library(ggthemes)

dat <- read.csv('cust_info.csv')
addr <- as.character(dat$StreetAddress)
num <- as.numeric(sapply(addr, function(x) substr(x,1,1)))
digit_freq<- data.frame(prop.table(table(num)))
names(digit_freq) <- c('Number', "Proportion_Occurred")
digit_freq$class <- 'Actual_Proportion'

benfords_law <- c(.301, .176,	.125, .097,	.079,	.067,	.058,	.051, .046)	
names(benfords_law) <- c(1,2,3,4,5,6,7,8,9)
benfords_law <- gather(data.frame(benfords_law))
benfords_law$key <- c(1:9)
names(benfords_law) <- c('Number', "Proportion_Occurred")
benfords_law$class  <- 'Benfords_Law' 

dat <- rbind(benfords_law,digit_freq)

ggplot(dat) + geom_bar(aes(x = Number, y=Proportion_Occurred, fill = class), stat = 'identity', position = 'dodge') +
  theme_wsj()
#### According to benfords law, there are likely falsely reported addresses




#2
#Check to see if adjuster and technicians matched up  
#often finish claims faster than ones are not often matched up.
#2013-07-01
#2013-06-04 = the min date of people that claimed but have not gotten a decision back
# 176 people still do not have a claim
emp <- read.csv('emp.csv')
cust_trans <- read.csv('cust_trans.csv')
cust_trans$Date <- as.Date(cust_trans$Date)
cust_trans$Cust_ID <- as.character(cust_trans$Cust_ID)
cust_trans$Cov_ID <- as.character(cust_trans$Cov_ID)
cust_trans$Transaction <- as.character(cust_trans$Transaction)
cust_trans$Type <- as.character(cust_trans$Type)
emp <- data.frame(apply(emp,2,as.character),stringsAsFactors = FALSE)
emp %>%  
  select(Cov_ID ,Claim_Info) %>% 
  group_by(Cov_ID) %>% 
  arrange(Cov_ID,Claim_Info) %>% 
  summarise(adjuster = Claim_Info[1], technician = Claim_Info[2]) -> x


cust_trans %>%
  filter(Transaction %in% c("RE", "CL")) %>% 
  arrange(Cov_ID, Date) %>% 
  select(Date, Cov_ID, Transaction) %>% 
  group_by(Cov_ID) %>% 
  summarise(min_date = min(Date), max_date = max(Date),
            Difference = as.numeric(max(Date) - min(Date))) -> temp


test <- merge(temp,x, by = 'Cov_ID')

test$pair <- paste(test$adjuster, test$technician, sep = '.')
tbl_pairs <- table(test$pair)
tbl_pairs <- data.frame(tbl_pairs[order(tbl_pairs, decreasing = T)])
names(tbl_pairs)[1] <-'pair'

test %>% 
  group_by(pair) %>% 
  summarise(mean_time = mean(Difference)) -> mean_times

test %>% 
  mutate(roll_mean_time = rollmean(Difference, 1000, align = 'center', na.pad = T)) -> try

final <- merge(mean_times, tbl_pairs, by = 'pair')

ggplot(final, aes(x = Freq, y = mean_time)) + stat_binhex()






#3 model and make of car and death occurence










