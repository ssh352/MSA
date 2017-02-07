

#Homework 1 Logistic regression
#use alpha = .001 on the entire assignment

library(sas7bdat)
library(dplyr)
library(ggplot2)
library(tidyr)
# df <- read.sas7bdat('C:/Users/Will/Documents/MSA/Homeworks/HW 1 Logistic/insurance_hw1.sas7bdat')
# labels <- unlist(sapply(attr(df, "column.info"), function(x) x$label))
# labels<- gsub(" ","_", labels)
# colnames(df)[1:7] <- labels
# colnames(df) <- tolower(colnames(df))
# write.csv(df, 'insurance.csv', row.names = F)

#exploratory analysis
df <- read.csv('insurance.csv')
str(df)
summary(df)

for(colname in names(df)){
  print(ggplot(df) + geom_bar(aes(x = eval(eval(parse(text = paste("df$", colname, sep = "")))))) +
          xlab(colname) + ggtitle(paste("barplot of ", colname)))
  print(table(eval(parse(text=paste("df$", colname, sep = "")))))
}


response <- factor(df$insurance_product)


#Part 1:   Contingency Table Analysis#########################
#1. Create a table that displays all of the comparisons between each of the variables
# and our target variable. Your table should include only the test statistics for each
# comparison as well as the p-value. Rank these variables from smallest to largest
# p-value. Make sure you use the appropriate hypothesis tests!


chi_sq_tests <- df %>% select(-insurance_product)
chi_sq_tests <- apply(chi_sq_tests,2,function(x){ 
  x <- factor(x)
  chi <- chisq.test(x, response)
  chi_sq_statistic <- chi$statistic
  chi_p_value <- chi$p.value
  n_levels <- length(levels(x))
  return(data.frame('variable' = NA, n_levels, chi_sq_statistic, chi_p_value))
  }
)

chi_sq_tests <- do.call(rbind.data.frame, chi_sq_tests)
chi_sq_tests$variable <- row.names(chi_sq_tests)
row.names(chi_sq_tests) <- 1:nrow(chi_sq_tests)

chi_sq_tests <- chi_sq_tests[order(chi_sq_tests$chi_p_value),]

#2. Create a separate table similar to the above one with only the significant
# variables.


chi_sq_tests %>% 
  filter(chi_p_value < .001) -> significant_vars



#3. Create a table for the odds ratios from only the significant binary
# relationships with whether the customer bought our insurance product. Rank
# these variables from strongest to weakest relationship. Give an interpretation of
# the odds ratio for the strongest relationship.

significant_vars %>% 
  filter(n_levels == 2) -> bin_vars

bin_df <- df[,names(df) %in% bin_vars$variable]

#there is a perfect association between mmbal_bin and money_market variables
table(bin_df$money_market,bin_df$mmbal_bin)

#drop mmbal_bin, keep money_market
bin_df %>% 
  select(-c(mmbal_bin)) -> bin_df

# define function that calculates odds ratios
#' odds ratio = odds group A / odds group B
#' table(response, variable)
odds_ratio <- function(bin_tbl){
  tot_A <- sum(bin_tbl[,2])
  tot_B <- sum(bin_tbl[,1])
  p_A <- (bin_tbl[2,2]/tot_A)
  p_B <- (bin_tbl[2,1]/tot_B)
  odds_A <- p_A/(1-p_A)
  odds_B <- p_B/(1-p_B)
  ratio <- odds_A/odds_B
}

#calculate odds ratios for each variable
# and convert to table
odds_ratios <- sapply(bin_df, function(x) odds_ratio(table(response,x)))
odds_ratios <- data.frame(odds_ratios)
rownames_to_column(odds_ratios, "variable")  %>%
  arrange(desc(odds_ratios)) -> odds_ratios



#4.  For all of the significant, non-binary relationships, calculate the appropriate
# statistic to measure the strength of an association between the variables and
# whether the customer bought our insurance product. 

library(Deducer)

significant_vars %>% 
  filter(n_levels > 2) -> non_bin_vars

non_bin_df <- df[,names(df) %in% non_bin_vars$variable]

non_bin_df %>%
  mutate_each(funs(factor))->non_bin_df

#applied the likelihood-ratio chi-square test... see no reason to use this over 
# regular chi-square, just did it bc the homework expects us to use a different stat
likelihood_tbl <- apply(non_bin_df,2,function(x){
  likelihood <-  likelihood.test(x,response)
  test_stat <- likelihood$statistic
  p_value <- likelihood$p.value
  return(data.frame(test_stat, p_value))
})

likelihood_tbl <- do.call(rbind.data.frame, likelihood_tbl)
likelihood_tbl <- rownames_to_column(likelihood_tbl, "variable")
likelihood_tbl %>% 
  arrange(desc(test_stat)) -> likelihood_tbl

#########################################################################


# Part 2:  Stratified Analysis
# Conduct a stratified analysis on the relationship between having a savings account
# and buying our insurance product controlling for whether the customer has a money market account.
# Is there a significant relationship between having a savings account and whether or not individuals 
# buy our insurance product? Calculate the adjusted (common) odds ratio and compare this to your 
# previous results. Does a common odds ratio appear to be appropriate here?
# 
attach(df)
savings_vs_ins <- as.matrix(table(insurance_product,saving_account))
cont_savings_vs_ins <- matrix(table(insurance_product,saving_account, money_market), nrow = 2, ncol = 4)[1:2,3:4]


chisq.test(cont_savings_vs_ins)
#there is not a significant relationship between having a savings account and
# whether the customer buys the insurance product when controlling for money market account



# For the above stratified analysis, calculate the Breslow-Day-Tarone test statistic. Based
# on this analysis, is there any evidence of interaction between having a money market account
# and a savings account? What would an interaction between these two binary variables imply
# in terms of our problem?

breslowday.test(table(insurance_product,saving_account, money_market))
  #based on the breslowday test stat we should look into interactions
  # an interaction would imply that based on whether or not the person has a money market account,
  # the association between savings account and purchasing insurance changes
  

#create all possible one-way interactions

interactions_tbl <- bin_df
significance_tbl <- NULL

for(i in 1:(ncol(interactions_tbl)-1)){
  for(j in (i+1):ncol(interactions_tbl)){
    var1 <- interactions_tbl[,i]
    var1_name <- names(interactions_tbl)[i]
    var2 <- interactions_tbl[,j]
    var2_name <- names(interactions_tbl)[j]
    interactions <-glm(insurance_product~ var1 + var2 + I(as.numeric(var1)*as.numeric(var2)-1), family = binomial)
    interactions <- summary(interactions)
    interactions <- interactions$coefficients
    if(nrow(interactions) == 3){next}
    rownames(interactions) <- c('intercept', var1_name, var2_name, paste0(var1_name, '*', var2_name))
    interactions <- data.frame(interactions)[4,]
    interactions <- rownames_to_column(interactions, 'variable')
    significance_tbl <- rbind(significance_tbl,interactions)
  }
}

significance_tbl %>% 
  filter(Pr...z.. < .001) -> significance_tbl


#check for linearity between ordinal variables
#they all appear to have a linear relationship minus the first level for each variable
#I could statistically check for this using the mantel heanszel chi-sq test.
  df %>% 
  dplyr::select(one_of(c("ddabal_bin", "savbal_bin", "depamt_bin"))) -> ordinal

for(i in 1:3){
  y <- table(ordinal[,i], model_df$insurance_product)[,2]/table(ordinal[,i]) 
  x <- 1:length(y)
  plot_data <- data.frame(x = as.numeric(x), y = as.numeric(y))
  print(ggplot(plot_data, aes(x,y)) + geom_point() + xlab(names(ordinal)[i]))
  print(table(ordinal[,i], model_df$insurance_product))
}

# I could turn these into continuous variables but may not be worth the time to put into it
# since our data set is large enough to accurately estimate the error
 
 
  
  

# Part 3: Logistic Regression
# 
# 1. Build a logistic regression model. Use reference coding for all categorical variables.
# Binary variables should have “0” as the reference level, while categorical variables
# that are not binary should have the last category (in alpha-numeric order) as the 
# reference level. (Use various techniques, such as Backward Elimination [slstay=0.001], 
#                   Forward Selection [slentry=0.001], etc…..don’t forget model hierarchy).
#   


df %>%
  dplyr::select(-c(mmbal_bin)) %>% 
  mutate_each(funs(factor)) ->model_df

attach(model_df)

#stepwise formula based on AIC
#define full model
full_logistic_mod <- glm(insurance_product ~ . + 
                           I(as.numeric(checking_account)*as.numeric(retirement_account)) +
                           I(as.numeric(checking_account)*as.numeric(money_market)) + 
                           I(as.numeric(checking_account)*as.numeric(irabal_bin)) + 
                           I(as.numeric(saving_account)*as.numeric(money_market)) + 
                           I(as.numeric(retirement_account)*as.numeric(money_market)), data = model_df, family = binomial)
#define empty model
nothing <- glm(insurance_product~ 1, family = binomial)


#preform backwards, forwards, and stepwise selection, while optimizing AIC
backwards <-step(full_logistic_mod) # Backwards selection is the default
forwards <- step(nothing,
                 scope=list(lower=formula(nothing),upper=formula(full_logistic_mod)), direction="forward")
stepwise <- step(nothing, list(lower=formula(nothing),upper=formula(full_logistic_mod)),
                 direction="both",trace=1)



#terms in each
for_terms <- attr(forwards$terms, 'term.labels')
back_terms <- attr(backwards$terms, 'term.labels')
step_terms <- attr(stepwise$terms, 'term.labels')



#had to add terms to satisfy model heirarchy
forwards_model <- glm(insurance_product ~  savbal_bin + ddabal_bin + cdbal_bin + 
                        branch_of_bank + checks_bin + atmamt_bin + teller_bin + 
                        checking_account + money_market + retirement_account + 
                        saving_account + retirement_account + 
                        I(as.numeric(retirement_account) * as.numeric(money_market)) +
                        I(as.numeric(checking_account) * as.numeric(money_market)) + 
                        I(as.numeric(checking_account) * as.numeric(retirement_account)) + 
                        I(as.numeric(saving_account) * as.numeric(money_market)), data = model_df,
                        family = binomial) 

backwards_model <- glm(insurance_product ~  checking_account + retirement_account + 
                         money_market + branch_of_bank + ddabal_bin + checks_bin + 
                         teller_bin + savbal_bin + atmamt_bin + cdbal_bin +
                         saving_account + 
                         I(as.numeric(checking_account) * as.numeric(retirement_account)) + 
                         I(as.numeric(saving_account) * as.numeric(money_market)), data = model_df,
                      family = binomial) 


stepwise_model <- glm(insurance_product ~ savbal_bin + ddabal_bin + cdbal_bin + branch_of_bank + 
                        checks_bin + atmamt_bin + teller_bin + checking_account + money_market +
                        retirement_account + saving_account+
                        I(as.numeric(retirement_account) * as.numeric(money_market)) + 
                        I(as.numeric(checking_account) * as.numeric(retirement_account)) + 
                        I(as.numeric(saving_account) * as.numeric(money_market)), data = model_df,
                      family = binomial)


#roc curves for each model
calculate_ROC <- function(model, response, name){
  tp_rates <- NULL
  fp_rates <- NULL
    for(threshold in 0:100){
      probs <- predict(model, type= 'response')
      preds <- ifelse(probs > (threshold/100), 1,0)
      tp_rate <- sum(preds[preds == 1] == response[preds == 1])/sum(response == 1)
      tp_rates <- c(tp_rates, tp_rate)
      fp_rate <- sum(preds[response == 0] != response[response == 0])/sum(response == 0)
      fp_rates <- c(fp_rates, fp_rate)
    }
  return(data.frame(name,tp_rates, fp_rates))
}  



forwards_roc <- calculate_ROC(forwards_model, as.numeric(insurance_product)-1, 'forwards')
backwards_roc <-calculate_ROC(backwards_model, as.numeric(insurance_product)-1, 'backwards')
stepwise_roc <- calculate_ROC(stepwise_model, as.numeric(insurance_product)-1, 'stepwise')
rocs <- rbind(forwards_roc, backwards_roc, stepwise_roc)

ggplot(rocs, aes(x = fp_rates, y = tp_rates, colour = name)) + geom_line(size = 1) + geom_abline(slope = 1, intercept = 0)



#10-fold cross validation
library(caret)
set.seed(5)

fit_control <- trainControl(method = 'cv', number = 10, savePredictions = TRUE)
cv_for <- train(formula(forwards_model), data = model_df, method = 'glm', family = binomial,
            trControl = fit_control)
cv_back <- train(formula(backwards_model), data = model_df, method = 'glm', family = binomial,
            trControl = fit_control)
cv_step <- train(formula(stepwise_model), data = model_df, method = 'glm', family = binomial,
            trControl = fit_control)










#function used for breslow-day-terrone test

breslowday.test <- function(x) {
  #Find the common OR based on Mantel-Haenszel
  or.hat.mh <- mantelhaen.test(x)$estimate
  #Number of strata
  K <- dim(x)[3]
  #Value of the Statistic
  X2.HBD <- 0
  #Value of aj, tildeaj and Var.aj
  a <- tildea <- Var.a <- numeric(K)
  
  for (j in 1:K) {
    #Find marginals of table j
    mj <- apply(x[,,j], MARGIN=1, sum)
    nj <- apply(x[,,j], MARGIN=2, sum)
    
    #Solve for tilde(a)_j
    coef <- c(-mj[1]*nj[1] * or.hat.mh, nj[2]-mj[1]+or.hat.mh*(nj[1]+mj[1]),
              1-or.hat.mh)
    sols <- Re(polyroot(coef))
    #Take the root, which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
    tildeaj <- sols[(0 < sols) &  (sols <= min(nj[1],mj[1]))]
    #Observed value
    aj <- x[1,1,j]
    
    #Determine other expected cell entries
    tildebj <- mj[1] - tildeaj
    tildecj <- nj[1] - tildeaj
    tildedj <- mj[2] - tildecj
    
    #Compute \hat{\Var}(a_j | \widehat{\OR}_MH)
    Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)
    
    #Compute contribution
    X2.HBD <- X2.HBD + as.numeric((aj - tildeaj)^2 / Var.aj)
    
    #Assign found value for later computations
    a[j] <- aj ;  tildea[j] <- tildeaj ; Var.a[j] <- Var.aj
  }
  
  #Compute Tarone corrected test
  X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.aj) )
  
  #Compute p-value based on the Tarone corrected test
  p <- 1-pchisq(X2.HBDT, df=K-1)
  
  res <- list(X2.HBD=X2.HBD,X2.HBDT=X2.HBDT,p=p)
  class(res) <- "bdtest"
  return(res)
}

print.bdtest <- function(x) {
  cat("Breslow and Day test (with Tarone correction):\n")
  cat("Breslow-Day X-squared         =",x$X2.HBD,"\n")
  cat("Breslow-Day-Tarone X-squared  =",x$X2.HBDT,"\n\n")
  cat("Test for test of a common OR: p-value = ",x$p,"\n\n")
}

