x <- data.frame(matrix(ncol=2,nrow=0))
for(i in colnames(ccApproval)){
  if(is.factor(ccApproval[,i])){
    new_col <- paste(i,"NUM",sep="_")
    x <- rbind(x,data.frame(oldCol=i,newCol=new_col))
    ccApproval[,new_col] <- as.numeric(ccApproval[,i]) 
  }
}
#skipping old cols since they are categorical
old_cols <- x$oldCol

library(smotefamily)
varsToSkipForSMOTE <- append(c("STATUS","ID"),old_cols)
smote <- SMOTE(ccApproval[,!names(ccApproval) %in% varsToSkipForSMOTE],ccApproval$TARGET_NUM)
smoteData <- smote$data

for(i in 1:nrow(x)){
  oldCol <- x[i,"oldCol"]
  newCol <- x[i,"newCol"]
  vec <- strsplit(levels(ccApproval[,oldCol]),"\"*\"")
  smoteData[,oldCol] <- unlist(vec[smoteData[,newCol]])
}

#remove the numeric vals for smote
smoteLogitData = smoteData[,!names(smoteData) %in% c(x$newCol,"STATUS","class")]
for(i in 1:nrow(x)){
  colName <- x[i,"oldCol"]
  if(colName != 'STATUS'){
    smoteLogitData[,colName] = as.factor(smoteLogitData[,colName]) 
  }
}
boosting(smoteLogitData)
logit(smoteLogitData)


fit = rpart(formula, 
            data = churn.train,
            method = "class",
            control = rpart.control(xval=0, minsplit = 1000),
            parms = list(split="gini"))

rm(list=ls())
cat('\014')
#smote and glm has better accuracy but the FNR is high is smote. almost double the rate as without using smote.

library(ROSE)
?ROSE
formula <- as.formula(TARGET ~ CODE_GENDER+FLAG_OWN_CAR+FLAG_OWN_REALTY+CNT_CHILDREN+AMT_INCOME_TOTAL+NAME_EDUCATION_TYPE
                      +NAME_FAMILY_STATUS+NAME_HOUSING_TYPE+DAYS_EMPLOYED+JOB+BEGIN_MONTHS+AGE)
rose = ROSE(formula, data=ccApproval, N=100000, p=0.80, seed=123)
roseData = rose$data
logit(roseData)


#Fit an XGBoost classifier model
library(xgboost)
library(caret)  
library(e1071)  

#Removning Status Variable

xgb_data=roseData
#xgb_data=ccApproval

colnames(xgb_data)
featuresToRemove = c("STATUS")
build_data = xgb_data[,!names(xgb_data) %in% featuresToRemove]
colnames(build_data)

#Train - Test Split
parts = createDataPartition(build_data$TARGET, p = 0.7, list = F)
train = build_data[parts, ]
test = build_data[-parts, ]
#View(test)


response = c("ID","TARGET")
X_train = data.matrix(train[,!names(train) %in% response])                  # independent variables for train
y_train = as.numeric(train$TARGET)-1                  # dependent variables for train

X_test = data.matrix(test[,!names(test) %in% response])                   # independent variables for test
y_test = as.numeric(test$TARGET)-1                       # dependent variables for test

# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)

# train a model using our training data
# Define the parameters for multinomial classification
num_class = length(unique(build_data$TARGET))-1

params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=3,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="binary:logistic"  )


model <- xgboost(params=params,data = xgboost_train,nrounds=100)
summary(model)


#use model to make predictions on test data
pred_test = predict(model, xgboost_test)
range(pred_test)

#By defauly threshold is 0.5. Round function automatically classifies values >0.5 as 1
#pred_test[(pred_test>0.5)] = 1

pred_y = as.factor(round(pred_test))
conf_mat = confusionMatrix(as.factor(y_test), pred_y)
print(conf_mat)
output <- as.data.frame(test$TARGET)
output$Predicted=as.numeric(pred_test)
colnames(output)=c("Actual","Predicted")

output$Actual=as.numeric(output$Actual)

head(output)
library(cvms)
library(rsvg)
library(ggimage)
eval <- evaluate(
  data = output,
  target_col = "Actual",
  prediction_cols = "Predicted",
  type = "binomial"
)
conf_mat <- eval$`Confusion Matrix`[[1]]
plot_confusion_matrix(conf_mat)


class(output$Actual)

#If you want to change the threshold
#threshold=0.3
#pred_test[(pred_test>threshold)] = 1
#pred_test[(pred_test<=threshold)] = 0
#pred_y = as.factor(as.numeric(pred_test)-1)

#confusionMatrix(as.factor(y_test), pred_y)$overall[1]





