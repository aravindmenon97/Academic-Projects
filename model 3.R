library(rio)
library(moments)
library(car)

install.packages("tidyverse")
library(tidyverse)
library(plotly)
library(zoo)

train_data=read.csv("train.csv")
str(train_data)
colnames(train_data)=tolower(make.names(colnames(train_data)))

train_data$date=as.Date(train_data$date,origin="1899-12-30")
train_data$family=factor(train_data$family)
str(train_data)
unique(train_data$onpromotion)
#attach(train_data)
#ggplot(train_data,aes(x=date,y=sales))+geom_point()
ggplot(train_data,aes(x=store_nbr,y=onpromotion))+geom_point()
train_data_without_outliners=subset(train_data,sales<120000)

set.seed(42)
train_data_sample=train_data_without_outliners[sample(1:nrow(train_data_without_outliners),500),]
for(i in 1:nrow(train_data_sample)){
  if(train_data_sample$onpromotion[i]<350)
  {
    train_data_sample$promotion_cat[i]="Moderate Promotions"
  }
  else if(train_data_sample$onpromotion[i]>350)
  {
    train_data_sample$promotion_cat[i]="High Promotions"
  }
}
train_data_sample$promotion_cat=factor(train_data_sample$promotion_cat)
complete.cases(train_data_sample)
hist(train_data_sample$sales)

linear_model1=glm(sales ~ date+store_nbr+family+promotion_cat,,data=train_data_sample)
summary(linear_model1)

predicttrain=predict(lm(sales ~ date+store_nbr+family+promotion_cat,data=train_data_sample,type="response"))
summary(predicttrain)
table(train_data_sample$sales,predicttrain>=0.7)

## test data 

test_data=read.csv("test.csv")
str(test_data)
test_data$date=as.Date(test_data$date,origin = "1900-01-01", format="%m/%d/%Y")
test_data$family=factor(test_data$family)

#set.seed(42)
#test_data_sample=test_data_without_outliners[sample(1:nrow(test_data_without_outliners),500),]
for(i in 1:nrow(test_data)){
  if(test_data$onpromotion[i]<350)
  {
    test_data$promotion_cat[i]="Moderate Promotions"
  }
  else if(test_data$onpromotion[i]>350)
  {
    test_data$promotion_cat[i]="High Promotions"
  }
}
test_data$promotion_cat=factor(test_data$promotion_cat)
predicttrain=predict(linear_model1, test_data)
predicttrain
summary(predicttrain)
test_data$sales
table(test_data$sales,predicttrain>=0.7)


actuals_preds <- data.frame(cbind(actuals=test_data$sales, predicteds=predicttrain))  
correlation_accuracy <- cor(actuals_preds) 
head(actuals_preds)


submission_data = read.csv("sample_submission.csv")
str(submission_data)

submission_data$sales = train_data$sales
write.csv(submission_data, "new_file.csv", row.names = FALSE)
