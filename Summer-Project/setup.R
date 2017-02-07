
#step 1  data setup


#download the R package sas7bdat to your computer: will allow R to read in sas data files
#(this only needs to be done one time)
install.packages('sas7bdat')

#load the newly downloaded package
#(this needs to be ran whenever you start a new R session)
library('sas7bdat')

#read in sas tables, may take a while to load
emp <- data.frame(read.sas7bdat('C:/Users/Will/Documents/Analytics/MSA/HW1/data/adjuster_technician.sas7bdat'))
fam_medical <- data.frame(read.sas7bdat('C:/Users/Will/Documents/Analytics/MSA/HW1/data/customer_family_medical.sas7bdat'))
cust_info <- data.frame(read.sas7bdat('C:/Users/Will/Documents/Analytics/MSA/HW1/data/customer_info.sas7bdat'))
cust_medical <- data.frame(read.sas7bdat('C:/Users/Will/Documents/Analytics/MSA/HW1/data/customer_medical.sas7bdat'))
cust_trans <- data.frame(read.sas7bdat('C:/Users/Will/Documents/Analytics/MSA/HW1/data/customer_transactions.sas7bdat'))

#convert dates to class date
cust_trans$Date <- as.Date('1960/01/01') + cust_trans$Date
cust_info$Birthday <- as.Date('1960/01/01') + cust_info$Birthday

#write sas tables to file, make sure you are either in the directory you want
#to write to, or specify the location in the function
#(this makes them faster to load)
write.csv(emp, 'emp.csv', row.names = F)
write.csv(fam_medical, 'fam_medical.csv', row.names = F)
write.csv(cust_info, 'cust_info.csv', row.names = F)
write.csv(cust_medical, 'cust_medical.csv', row.names = F)
write.csv(cust_trans, 'cust_trans.csv', row.names = F)






