# Week 3: Module 3 Assignment
# Class: ALY6015-21788 
# Student: Yu Qiu
# Date: 01/30/2022

#To library the packages needed
library(dplyr)
library(psych)
library(ISLR)
library(kableExtra)
library(RColorBrewer)
library(ggplot2)
library(readxl)
library("gridExtra")
library(InformationValue)
library(pscl)
library(car)
library(caret)
library(plotROC)
library(caret)
library(pROC)

#Request the data set
data(College)

#Checking structure and missing value
dim(College)
str(College)
sum(is.na(College))

#Remove scientific notation
options(scipen = 999)

#Descriptive Statistics
des <- psych::describe(College, omit = T) %>%
  select(n, mean, sd, median, min, max, range)

#Create 3-line table
round(des, 1) %>%
  kbl(caption = "Table 1") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Subgroup Descriptive Statistics
nopri <- table(College$Private)
barplot(nopri)

private <- subset(College, Private == "Yes")
nprivate <- subset(College, Private == "No")

prides <- psych::describe(private, omit = T)%>%
  select(n, mean, sd, median, range)
nprides <- psych::describe(nprivate, omit = T)%>%
  select(n, mean, sd, median, range)

total <- cbind(prides, nprides)
round(total, 0) %>%
  kbl(caption = "Table 3") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  add_header_above(c("" , "Private" = 5, "Public" = 5))

#Bar chart of Average Number of Applications
par(mfrow = c(3, 2),cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))
appmean <- tapply(College$Apps, INDEX = College$Private, mean)
plot1 <- barplot(appmean, ylim = c(0, 7000), col = c("gold", "lightblue"), xlab = "Private School", ylab = "Average Number of Applications", las =1, cex.axis = 1, cex.names = 1, main = "Applications")
text(y = round(appmean,0),
     plot1,
     round(appmean,0),
     cex = 1,
     pos = 3)

#Bar chart of Average Number of Applications Accepted
acptmean <- tapply(College$Accept, INDEX = College$Private, mean)
plot2 <- barplot(acptmean, ylim = c(0, 5000), col = c("gold", "lightblue"), xlab = "Private School", ylab = "Average Number of Applications Accepted", las =1, cex.axis = 1, cex.names = 1, main = "Applications Accepted")
text(y = round(acptmean,0),
     plot2,
     round(acptmean,0),
     cex = 1,
     pos = 3)

#Bar chart of Average Number of New Students Enrolled
enrollmean <- tapply(College$Enroll, INDEX = College$Private, mean)
plot3 <- barplot(enrollmean, ylim = c(0, 2000), col = c("gold", "lightblue"), xlab = "Private School", ylab = "Average Number of New Students Enrolled", las =1, cex.axis = 1, cex.names = 1, main = "New Students Enrolled")
text(y = round(enrollmean,0),
     plot3,
     round(enrollmean,0),
     cex = 1,
     pos = 3)

#Bar chart of Average Number of Fulltime Undergrads
fundermean <- tapply(College$F.Undergrad, INDEX = College$Private, mean)
plot4 <- barplot(fundermean, ylim = c(0, 10000), col = c("gold", "lightblue"), xlab = "Private School", ylab = "Average Number of Fulltime Undergrads", las =1, cex.axis = 1, cex.names = 1, main = "Fulltime Undergrads")
text(y = round(fundermean,0),
     plot4,
     round(fundermean,0),
     cex = 1,
     pos = 3)

#Bar chart of Average Number of Parttime Undergrads
pundermean <- tapply(College$P.Undergrad, INDEX = College$Private, mean)
plot5 <- barplot(pundermean, ylim = c(0, 2500), col = c("gold", "lightblue"), xlab = "Private School", ylab = "Average Number of Parttime Undergrads", las =1, cex.axis = 1, cex.names = 1, main = "Parttime Undergrads")
text(y = round(pundermean,0),
     plot5,
     round(pundermean,0),
     cex = 1,
     pos = 3)
dev.off()

#Box plots of Out-of-state tuition, Estimated Book Cost, Instructional Expenditure per Student
par(mfrow = c(2, 2),cex=0.8, mai=c(4,4,2,1), mar=c(4,4,2,1))

boxplot(College$Outstate ~ College$Private, col = c("gold", "lightblue"), xlab = "Private University", ylab = "Out-of-state tuition", main = "Out-of-state tuition", pch = 16)

boxplot(College$Books ~ College$Private, col = c("gold", "lightblue"), xlab = "Private University", ylab = "Estimated Book Cost", main = "Estimated Book Cost", pch = 16)

boxplot(College$Expend ~ College$Private, col = c("gold", "lightblue"), xlab = "Private University", ylab = "Instructional Expenditure per Student", main = "Instructional Expenditure per Student", pch = 16)        

#Scatter Plots
g1 <- ggplot(College, aes(x= Outstate, y = Books, color = Private)) + geom_point() + theme_bw()
g2 <- ggplot(College, aes(x= Expend, y = Books, color = Private)) + geom_point() + theme_bw()
g3 <- ggplot(College, aes(x= Outstate, y = Expend, color = Private)) + geom_point()+ theme_bw()
grid.arrange(g1 , g2 , g3, nrow=3)


#Split the data into a train and test set
set.seed(1)
train <- sort(sample(x=nrow(College), size = nrow(College)*0.7))
sample_train <- College[train,]
sample_test <- College[-train,]


#Fit the model
model1 <- glm(Private ~ Apps + Accept + Enroll  + F.Undergrad + P.Undergrad + Outstate + Books + perc.alumni + Expend, data = sample_train, family = binomial(link = "logit"))
summary(model1)

model2 <- glm(Private ~ Apps + Accept +  F.Undergrad + Outstate, data = sample_train, family = binomial(link = "logit"))
summary(model2)

model3 <- glm(Private ~Accept +  F.Undergrad + Outstate, data = sample_train, family = binomial(link = "logit"))
summary(model3)
vif(model3)

model4 <- glm(Private ~ F.Undergrad + Outstate, data = sample_train, family = binomial(link = "logit"))
summary(model4)
vif(model4)


#To calculate the log odds and odds ratios 
coef(model4)
exp(coef(model4))


#Create data set to see how probability changes for different values of out-of-state tuition
test <- data.frame(Outstate = c(min(College$Outstate), mean(College$Outstate), max(College$Outstate)), F.Undergrad =  c(mean(College$F.Undergrad)))

test$probs <- predict(model4, test, type = "response")                                
test                   


#Train set predictions
trainpredicted <- predict(model4, newdata = sample_train, type = "response")
trainpredictmin <- as.factor(ifelse(trainpredicted >= 0.5, "Yes", "No"))
#Confusion Matrix for model accuracy 
confusionMatrix(trainpredictmin, sample_train$Private, positive = "Yes")


#Test set predictions
testpredicted <- predict(model4, newdata = sample_test, type = "response")
testpredictmin <- as.factor(ifelse(testpredicted >= 0.5, "Yes", "No"))
#Confusion Matrix for model accuracy
confusionMatrix(testpredictmin, sample_test$Private, positive = "Yes")


#Plot the ROC 
test2 <- as.factor(ifelse(sample_test$Private == "Yes", "1", "0"))
plotROC(test2, testpredicted)

roc <- roc(test, testpredicted)
plot(roc, col = "blue", las = 1)

#Calculate AUC
auc(roc)

