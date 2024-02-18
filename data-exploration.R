ccApproval = read.csv("credit_card_approval.csv")
ccApproval1 = na.omit(ccApproval)

save(ccApproval, "ccApproval.RDS")

summary(ccApproval)

library(ggplot2)
ggplot(ccApproval,aes(AMT_INCOME_TOTAL,NAME_EDUCATION_TYPE))+geom_boxplot()

eduLevels = as.factor(ccApproval$NAME_EDUCATION_TYPE)
levels(eduLevels)

academicSal = ccApproval[ccApproval$NAME_EDUCATION_TYPE=="Academic degree",]
riskUsers = ccApproval[ccApproval$TARGET==1,]
nrow(riskUsers)
(nrow(riskUsers)/nrow(ccApproval))*100
#0.03 percent users are risk users. need undersampling/oversampling?

ggplot(ccApproval,aes(abs(DAYS_BIRTH),AMT_INCOME_TOTAL))+geom_point(aes(color=NAME_EDUCATION_TYPE))
1962/537667
min(academicSal$AMT_INCOME_TOTAL)
max(academicSal$AMT_INCOME_TOTAL)
max(academicSal$AMT_INCOME_TOTAL)

g <- ggplot(ccApproval, aes(ccApproval$BEGIN_MONTHS, ccApproval$AMT_INCOME_TOTAL))
g + geom_point()  
g + geom_point(aes(color=ccApproval$CODE_GENDER))   # color the dots by gender

length(unique(ccApproval1$NAME_FAMILY_STATUS))
unique(ccApproval1$STATUS)

# Load ggplot2
library(ggplot2)

# Create side-by-side barchart of gender pertaining to marriage status by alignment
ggplot(ccApproval1, aes(x = ccApproval1$NAME_FAMILY_STATUS, fill = ccApproval1$CODE_GENDER)) + 
  geom_bar(position = "dodge")

# Create side-by-side barchart of gender pertaining to target status by alignment
ggplot(ccApproval1, aes(x = ccApproval1$TARGET, fill = ccApproval1$CODE_GENDER)) + 
  geom_bar(position = "dodge")

meanDaysEmployed = mean(ccApproval$DAYS_EMPLOYED)
sdDaysEmployed = sd(ccApproval$DAYS_EMPLOYED)

normalizedVal = data.frame(DaysEmployed = (ccApproval$DAYS_EMPLOYED - meanDaysEmployed)/sdDaysEmployed)
summary(normalizedVal)


#do clustering to see if I can apply smote oversampling. 
# if there is a overlap of the clusters, then there is a chance that the new records created might be ambiguous
