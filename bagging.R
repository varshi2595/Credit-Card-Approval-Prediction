boosting = function(data){
  library(adabag)
  library(rpart) 
  library(caret)
  
  set.seed(123)
  train = sample(1:nrow(data),(2/3)*nrow(data))
  train.df <- data[train,]
  test.df <- data[-train,]
  fit.boosting <- boosting(TARGET ~ CODE_GENDER+FLAG_OWN_CAR+FLAG_OWN_REALTY+CNT_CHILDREN+AMT_INCOME_TOTAL+NAME_EDUCATION_TYPE+NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+DAYS_EMPLOYED+JOB+BEGIN_MONTHS+AGE, data = train.df, mfinal = 20)
  pred <- predict(fit.boosting, test.df, type = "class")
  cm <- confusionMatrix(as.factor(pred$class), test.df$Personal.Loan)
  cm
}

?boosting

library(adabag)
library(rpart) 
library(caret)

set.seed(123)
train = sample(1:nrow(smoteLogitData),(2/3)*nrow(smoteLogitData))
train.df <- smoteLogitData[train,]
test.df <- smoteLogitData[-train,]
dataFormula = as.formula(TARGET ~ CODE_GENDER+FLAG_OWN_CAR+FLAG_OWN_REALTY+CNT_CHILDREN+AMT_INCOME_TOTAL+NAME_EDUCATION_TYPE+NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+DAYS_EMPLOYED+JOB+BEGIN_MONTHS+AGE)
fit.boosting <- boosting(formula=dataFormula, data=train.df, mfinal = 20)
pred <- predict(fit.boosting, test.df, type = "class")
cm <- confusionMatrix(as.factor(pred$class), test.df$TARGET)
cm

cm$byClass

(15726/(15726+162808))
