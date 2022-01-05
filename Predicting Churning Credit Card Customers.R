rm(list = ls())
setwd("C:/Users/aksha/Desktop/All Sem Assignments/3rd Sem Assignments/BML2/Project")
credit.data <- read.csv(file = "BankChurners.csv", sep = ",", header = T)

credit.new1 <- credit.data[!(credit.data$Education_Level == "Unknown"),]
credit.new2 <- credit.new1[!(credit.new1$Marital_Status == "Unknown"),]
credit.new3 <- credit.new2[!(credit.new2$Gender == "Unknown"),]
credit.new4 <- credit.new3[!(credit.new3$Income_Category == "Unknown"),]

#Retained 2 to 21 columns of the data frame.
credit.new4 <- credit.new4[c(2:21)]

#To check Null values.
colSums(is.na(credit.new4))

#Summary of the data frame.
summary(credit.new4)

library(dplyr)

#Sampling the data frame to randomly pick 4000 rows.
samplerow <- sample_n(credit.new4,4000)
attach(samplerow)
#Summary of the new data frame.
s <- summary(samplerow)

s1 <- data.frame(unclass(s), check.names = FALSE, stringsAsFactors = FALSE)

write.table(s1, file = "summary.csv", sep = ",", quote = FALSE, row.names = F)

##**Project Exploration**##

##** Pre-processing Steps**##
samplerow
View(samplerow)
nrow(samplerow)
class(samplerow$Attrition_Flag)


#Factoring the variables.
samplerow$Gender <- factor(samplerow$Gender, levels=c("M","F"), labels=c(0,1))
samplerow$Attrition_Flag <- factor(samplerow$Attrition_Flag, levels=c("Attrited Customer","Existing Customer"), labels=c(0,1))
samplerow$Marital_Status <- factor(samplerow$Marital_Status, levels=c("Single","Married","Divorced"), labels=c(0,1,2))
samplerow$Education_Level<-factor(samplerow$Education_Level,
                                  levels=c("Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate"),labels=c(0,1,2,3,4,5))
samplerow$Card_Category<-factor(samplerow$Card_Category,levels = c("Blue", "Silver", "Gold","Platinum"),labels=c(0,1,2,3))

##**converted income category to ordinal type**##
View(samplerow)
samplerow$Factored_Income_category <- samplerow$Income_Category
unique(samplerow$Factored_Income_category)
samplerow$Factored_Income_category<-factor(samplerow$Factored_Income_category,levels = c("Less than $40K", "$40K - $60K", "$60K - $80K","$80K - $120K", "$120K +"),labels=c(0,1,2,3,4))


##** Descriptive Statistics**##

#Grouping customers as per age group#
labs <- c(paste(seq(0, 95, by = 10), seq(0 + 10 - 1, 100 - 1, by = 10),
                sep = "-"), paste(100, "+", sep = ""))
labs
samplerow$AgeGroup <- cut(samplerow$Customer_Age, breaks = c(seq(0, 100, by = 10), Inf), labels = labs, right = FALSE)
samplerow<-samplerow[,-c(7)]
#-------------------------------------------------------------------
#Grouping customers based on age and attrition.
attr_data<-
  samplerow%>%
  group_by(AgeGroup,Attrition_Flag) %>%
  summarise(Attrited_Customer_Count = n())
attr_data
attr_data2<-attr_data[attr_data$Attrition_Flag=="Attrited Customer",]

#Bar plot for Atrrited customers vs age group#
barplot(attr_data2$Attrited_Customer_Count,names.arg = c("20-29","30-39","40-49","50-59","60-69"),xlab = "Age Group", ylab = "Attrited Customers",main = "Attrited Customers based on Age Group")

library(ggplot2)

#Grouping customers based on Income Category and attrition.
inc_data<-
  samplerow %>%
  group_by(Income_Category, Attrition_Flag) %>%
  summarise(AC=n())
inc2_data<-inc_data[inc_data$Attrition_Flag=="Attrited Customer",]

#GGplot for attritted customers vs income category.
ggplot(inc2_data, aes(y=AC, x=Income_Category))+ geom_bar(position="dodge",stat = "identity") 

#Grouping customers based on Card Category and Gender.
card_data<-
  samplerow %>%
  group_by(Gender, Card_Category) %>%
  summarise(G=n())

#GGplot for Income Category vs Gender.
ggplot(card_data, aes(y=G, x=Card_Category,fill= Gender))+ geom_bar(position="dodge",stat = "identity") 

#Grouping customers based on Education Level and Attrition.
e_data<-
  samplerow %>%
  group_by(Education_Level, Attrition_Flag) %>%
  summarise(AC=n())

#GG plot for Education Level and Attrition.
ggplot(e_data, aes(y=AC, x=Education_Level))+ geom_bar(position="dodge",stat = "identity") 

#Grouping customers based on Average Credit Limit and Age Group.
avg_customer_cr<-
  samplerow %>%
  group_by(AgeGroup) %>%
  summarise(Avg_creditLimit=mean(Credit_Limit))

#GG plot for Average Credit Limit vs Age Group.
ggplot(avg_customer_cr, aes(y=Avg_creditLimit, x=AgeGroup))+ geom_bar(position="dodge",stat = "identity")


installed.packages("caret")
library(caret)


##**Clustering (Unsupervised Learning)**##

##**Clustering on 1st set of input variable**##
input_values <- samplerow[c("Customer_Age", "Total_Trans_Ct", "Credit_Limit")]
pre_values <- preProcess(input_values, method = c("range"))
pre_values
predict_values <- predict(pre_values, input_values)
predict_values
Cl_3 <- kmeans(predict_values,3) 
Cl_4 <- kmeans(predict_values,4)
Cl_5 <- kmeans(predict_values,5)
Cl_3$betweenss/Cl_3$tot.withinss
Cl_4$betweenss/Cl_4$tot.withinss
Cl_5$betweenss/Cl_5$tot.withinss
plot(x= predict_values$Total_Trans_Ct,y = predict_values$Customer_Age, xlab="Total_Trans_Ct", ylab="Customer's Age", col= Cl_3$cluster)
plot(x= predict_values$Total_Trans_Ct,y = predict_values$Customer_Age, xlab="Total_Trans_Ct", ylab="Customer's Age", col= Cl_4$cluster)
plot(x= predict_values$Total_Trans_Ct,y = predict_values$Customer_Age, xlab="Total_Trans_Ct", ylab="Customer's Age", col= Cl_5$cluster)
installed.packages("cluster")
library(cluster)
sil_3<-mean(silhouette(Cl_3$cluster,dist(predict_values[c("Total_Trans_Ct","Credit_Limit")]))[,3])
sil_4<-mean(silhouette(Cl_4$cluster,dist(predict_values[c("Total_Trans_Ct","Credit_Limit")]))[,3])
sil_5<-mean(silhouette(Cl_5$cluster,dist(predict_values[c("Total_Trans_Ct","Credit_Limit")]))[,3])
sil_3
sil_4
sil_5
k <- 3:5
sil_values <- c(sil_3,sil_4,sil_5)
plot(k,sil_values,type="b",frame=FALSE,xlab="Number of Clusters K", ylab="Silhouette Measure")

table(Attrition_Flag,Cl_3$cluster)
input_vars <- samplerow[c("Total Trans Count", "Customer_Age")]
cluster_3 <- kmeans(input_vars,3)
cluster_3$centers


##**Clustering on 2nd set of input variable**##
#Elbow method:
input_val2 <- samplerow[c("Total_Revolving_Bal", "Avg_Utilization_Ratio", "Total_Trans_Amt")]
data_val2 <- scale(input_val2, center = F, scale = T)
data_val2

cl_3_2 <- kmeans(data_val2,3)
cl_4_2 <- kmeans(data_val2,4)
cl_5_2 <- kmeans(data_val2,5)
cl_6_2 <- kmeans(data_val2,6)


plot(x = input_val2$Avg_Utilization_Ratio, y = input_val2$Total_Revolving_Bal, xlab="Average Utilization Ratio", ylab="Total Revolving Balance", col= cl_4_2$cluster)
plot(x=c(3,4,5,6),y=c(cl_3_2$tot.withinss,cl_4_2$tot.withinss,cl_5_2$tot.withinss,cl_6_2$tot.withinss),
     xlab="K Value",ylab="Within Cluster Distance", type="b")

table(Attrition_Flag,cl_4_2$cluster)
cluster_4 <- kmeans(input_val2,4)
cluster_4$centers 

##**Clustering on 3rd set of input variable**##

input_vars3 <- samplerow[c("Total_Relationship_Count","Months_Inactive_12_mon", "Contacts_Count_12_mon")]
clus_3<- kmeans(input_vars3,3)
clus_4<- kmeans(input_vars3,4)
clus_5<- kmeans(input_vars3,5)
plot(x=c(3,4,5),y=c(clus_3$tot.withinss,clus_4$tot.withinss,clus_5$tot.withinss),
     xlab="K Value",ylab="Within Cluster Distance", type="b")

table(Attrition_Flag,clus_4$cluster)
clus_4$centers
#----------------------------------------------------------------------------------
##**Supervised Learning**## 

#10 fold cross validation
df<-samplerow

df$Attrition_Flag<-factor(df$Attrition_Flag)
levels(df$Attrition_Flag) <- make.names(levels(factor(df$Attrition_Flag)))
View()

#70:30 split
set.seed(123)
ti<-sample(1:nrow(df), size = (0.7*nrow(df)),replace = FALSE)
train_data<- df[ti,]
test_data<- df[-ti,]

##**Logistic regression model**##

response.test<-test_data[,"Attrition_Flag"]
test_data$Attrition_Flag<-NA

lr_model<- glm(Attrition_Flag~.,
               family = binomial,
               data = train_data)
summary(lr_model)


predicted_probability <- predict(lr_model, newdata = data.frame(test_data), type ="response")
predict_output <- ifelse(predicted_probability >= 0.5, 1, 0)
predict_output<-as.factor(predict_output)
mat<-table(Actual=predict_output, Predicted=response.test)
mat
accuracy <-(mat[1,1]+mat[2,2])/sum(mat)
accuracy
precision <-mat[2,2]/sum(mat[2,])
precision
recall <-mat[2,2]/sum(mat[,2])
recall


library(caret)
library(pROC)
library(mlbench)
ctrl<-trainControl(method="cv",
                   number=10, 
                   savePredictions=TRUE,
                   classProbs=TRUE,
                   summaryFunction = twoClassSummary)
model<-train(Attrition_Flag~.,
             method='glm',
            family="binomial",
             data = df,trControl=ctrl)  

print(model)
model$results

##**Random forest**Validating by 10-fold cross validation**##------------------

mod_rf<-train(Attrition_Flag~.,method = 'rf',importance = TRUE, data=train_data,trControl = ctrl)
print(mod_rf$results)
mod_rf$finalModel

#find top 10 important variables#
plot(varImp(mod_rf), top = 10)

#predicting test data and building confusion matrix#
rf.predict<-predict(mod_rf , test_data)
print(rf.predict)
confusionMatrix(rf.predict, response.test)

##**Random Forest model**##-------------------------------------------------------------


##**Decision Tree**##--------------------------------------------------------------------------
View(samplerow)
install.packages("party")
library(party)
install.packages("caret")
library(caret)
install.packages("pROC")
library(pROC)
install.packages("rpart")
library(rpart)

#Building decision tree model
decisionTree <- ctree(data=train_data, Attrition_Flag~.)
decisionTree

ImpVariables <- rpart(Attrition_Flag~., data=train_data, method="class")
ImpVariables$variable.importance

plot(varImp(decisionTree), top = 10)

#prediction on train data
trainPredict <- predict(decisionTree, train_data)
trainPredict

#prediction on test data
testPredict <- predict(decisionTree, test_data)
testPredict

#Building confusion matrix
confMat<-as.matrix(table(Actual=test_data$Attrition_Flag, Predicted=testPredict))
confMat

#Finding accuracy,precision and recall values
accuracy <- (confMat[1,1]+confMat[2,2])/sum(confMat)
accuracy
precision <- confMat[2,2]/sum(confMat[2,])
precision
recall <- confMat[2,2]/sum(confMat[,2])
recall

F1_DT<- (2*precision*recall)/(precision+recall)
F1_DT

#Finding ROC values for train and validation sets.
predict_train=as.numeric(trainPredict)
predict_test=as.numeric(testPredict)
roc(train_data$Attrition_Flag,predict_train)
roc(test_data$Attrition_Flag,predict_test)

#printing and plotting the decision tree to know variable importance.
print(decisionTree)
plot(decisionTree)

##**Decision tree**##

##*Naive Bayes*##
# create response and feature data
feature <- setdiff(names(train_data), "Attrition_Flag")
x <- train_data[, features]
y <- train_data$Attrition_Flag

# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = ctrl
)

#results
confusionMatrix(nb.m1)

#predicting test data and building confusion matrix#
pred_train<- predict(nb.m1, train_data)
pred_test <- predict(nb.m1, test_data)

pred_data<-confusionMatrix(pred_test, response.test)
table(pred_test,response.test)
pred_data

##*Naive Bayes*##


