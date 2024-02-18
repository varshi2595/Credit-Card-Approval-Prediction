ccApproval = read.csv("credit_card_approval.csv")

#changing categorical variables to factor
#gender 
ccApproval$CODE_GENDER = as.factor(ccApproval$CODE_GENDER)
ccApproval$FLAG_OWN_CAR = as.factor(ccApproval$FLAG_OWN_CAR)
ccApproval$FLAG_OWN_REALTY = as.factor(ccApproval$FLAG_OWN_REALTY)
ccApproval$NAME_EDUCATION_TYPE = as.factor(ccApproval$NAME_EDUCATION_TYPE)
ccApproval$NAME_HOUSING_TYPE = as.factor(ccApproval$NAME_HOUSING_TYPE)

#converting age to years
ccApproval$AGE = round(abs(ccApproval$DAYS_BIRTH)/365,0)

ccApproval$DAYS_EMPLOYED = abs(ccApproval$DAYS_EMPLOYED)
ccApproval$TARGET = as.factor(ccApproval$TARGET)

levels(as.factor(ccApproval$JOB))
ccApproval$JOB = as.factor(ccApproval$JOB)

#chance c to p to make better meaning out of it. p means paid.
ccApproval$STATUS = ifelse(ccApproval$STATUS=='C','P',ccApproval$STATUS)
ccApproval$STATUS = as.factor(ccApproval$STATUS)
ccApproval$BEGIN_MONTHS = abs(ccApproval$BEGIN_MONTHS)
ccApproval$CNT_CHILDREN = as.factor(ccApproval$CNT_CHILDREN)
ccApproval$NAME_FAMILY_STATUS = as.factor(ccApproval$NAME_FAMILY_STATUS)

ccApproval = ccApproval[,!names(ccApproval) %in% c("DAYS_BIRTH")]

library(Hmisc)



#create matrix of correlation coefficients and p-values
rcorr(as.matrix(ccApproval))