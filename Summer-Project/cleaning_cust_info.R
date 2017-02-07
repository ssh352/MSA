#cleaning trans and cust info

#table nchar for cust id and cov_id;
table(nchar(as.character(cust_trans$Cov_ID)))
cust_trans[which(nchar(as.character(cust_trans$Cust_ID))== 7),]


cust_info <- read.csv('cust_info.csv')

#convert FeetInches to inches
height <- gsub("[[:punct:]]", "", cust_info$FeetInches)
height <- strsplit(height, split = ' ')
inches <- lapply(height,function(x){
  return(as.numeric(x[[1]]) * 12 + as.numeric(x[[2]]))
} )
inches <- unlist(inches)
cust_info$FeetInches <- inches

#convert dates to class Dates 
cust_info$Birthday <- as.Date('1960-01-01') + cust_info$Birthday
cust_info$CCExpires <- as.Date('1960-01-01') + cust_info$CCExpires
cust_info$ZipCode <- as.character(cust_info$ZipCode)
cust_info$CCNumber <- as.character(cust_info$CCNumber)


