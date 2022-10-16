

getRversion()

write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)


Sys.which("make")
## "C:\\rtools40\\usr\\bin\\make.exe"
install.packages("Rcpp")
library("Rcpp")
evalCpp("1 + 1") # confirms Rtools was installed correctly

  #Required Libraries
install.packages("tidyverse")
install.packages("lifecycle")
install.packages("ggthemr")
install.packages("ggpubr")
install.packages("scales")
install.packages("skimr")
install.packages("GGally")
install.packages("corrr")
install.packages("corrplot")
install.packages("brglm2")
install.packages("ROSE")
install.packages("ROCR")
install.packages("caret")
#install.packages("plotROC")
install.packages("plotly")
install.packages("yardstick")
install.packages("partykit")
install.packages("pastecs")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot")
install.packages("ggplot2")
install.packages("cowplot")
library(tidyverse)
library(lifecycle)
library(ggthemr)
library(ggpubr)
library(scales)
library(skimr)
library(GGally)
library(corrr)
library(corrplot)
library(brglm2)
library(ROSE)
library(ROCR)
library(caret)
library(plotROC)
library(plotly)
library(yardstick)
library(partykit)
library(pastecs)
library(dplyr)
library(magrittr)
#library(ggplot)
library(ggplot2)
library(cowplot)



remove.packages("rlang")
remove.packages("dplyr")

install.packages("rlang")
install.packages("dplyr")

library(rlang)
library(dplyr)

?lifecycle

#######################################################################
#loading Data
########################

VerifyData <- read.csv('G:/SNHU Capstone/Module 7/DAT 690 Attrition Verification Data/DAT 690 Attrition-Proj1EmpAttrVerify.csv', header = TRUE)

Employee_Attrition <- read.csv('G:/SNHU Capstone/Employee Attrition Data.csv', header = TRUE)

#verifying Data and printing first five line from the top
head(Employee_Attrition,5)

#Checking the dimension of data (observations, variables)
dim(Employee_Attrition)

#printing the column names of the data set
names(Employee_Attrition)

#Preview the data type of each column
str(Employee_Attrition)

##################################################################################################
#Data Cleaning
Count_Missing_Values <- sum(is.na(Employee_Attrition))#counting missing values
cat('there is',Count_Missing_Values, 'missing values')

Count_Duplicate_Values <- sum(duplicated(Employee_Attrition))#counting duplicated values
cat('there is',Count_Duplicate_Values, 'duplicated values')

Count_Missing_Values <- sum(is.na(VerifyData))#counting missing values
cat('there is',Count_Missing_Values, 'missing values')

Count_Duplicate_Values <- sum(duplicated(VerifyData))#counting duplicated values
cat('there is',Count_Duplicate_Values, 'duplicated values')

###################################################################################################
# Exploratory Data Analysis
#################################

#Display a table of basic descriptive statistics of some important variables
attach(Employee_Attrition)
options(scipen=100, digits=2)
#options(digits=2)
Numeric_Variables <- cbind(Age,DistanceFromHome,Education,YearsAtCompany,YearsInCurrentRole,
                           MonthlyIncome, PercentSalaryHike,JobSatisfaction)
stat.desc(Numeric_Variables)


attach(VerifyData)
options(scipen=100, digits=2)
#options(digits=2)
Numeric_Variables2 <- cbind(Age,DistanceFromHome,Education,YearsAtCompany,YearsInCurrentRole,
                           MonthlyIncome, PercentSalaryHike,JobSatisfaction)
stat.desc(Numeric_Variables2)

########################################################################################################################

#######################
#Data Visualization
#####################

t = table(Employee_Attrition$Attrition) #creating table of Employee Attrition
t# printing employee attrition table

#Creating a Pie Chart
labels = c("Attrition: No", "Attrition: Yes")

piepercent<- round(100*t/sum(t), 1)

pie(t, labels = piepercent, main = " Pie Chart of GE Employee Attrition",
    col = rainbow(length(t)))
legend("topright", labels, fill = rainbow(length(t)), cex = 0.9)


##---##

t = table(VerifyData$Attrition) #creating table of Employee Attrition
t# printing employee attrition table

#Creating a Pie Chart
labels = c("Attrition: No", "Attrition: Yes")

piepercent2<- round(100*t/sum(t), 1)

pie(t, labels = piepercent2, main = " Pie Chart of GE Employee Attrition",
    col = rainbow(length(t)))
legend("topright", labels, fill = rainbow(length(t)), cex = 0.9)



#########################################################################################
#Creating histogram of age distribution
#This block of code display the three different histograms of overall age distribution in the company,
#male age distribution, and female age distribution

#Overall age distribution
plot1 <- Employee_Attrition %>%
  ggplot(aes(x=Age)) +
  geom_density(fill = "green", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = " Employee Age Distribution")

#Male age distribution

Employee_Attrition$Gender <- as.factor(Employee_Attrition$Gender) #need to convert to factor to avoid "Error in filter(., Gender == "Male") : object 'Gender' not found"

plot2 <- Employee_Attrition %>%
  #filter(Gender == "Male") %>%   #kept getting error --> object 'Gender' not found
  ggplot(aes(x=Age)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = "Male Age Distribution")

#Female age distribution
plot3 <- Employee_Attrition %>%
  filter(Gender == "Female") %>% #kept getting error --> object 'Gender' not found
  ggplot(aes(x=Age)) +
  geom_density(fill = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = "Female Age Distribution")

#Arranging all three figure in a single plot
ggarrange(plot1,
          ggarrange(plot2, plot3),
          nrow = 2)

##--##

#Overall age distribution
plot1a <- VerifyData %>%
  ggplot(aes(x=Age)) +
  geom_density(fill = "green", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = " Employee Age Distribution")

#Male age distribution

VerifyData$Gender <- as.factor(VerifyData$Gender) #need to convert to factor to avoid "Error in filter(., Gender == "Male") : object 'Gender' not found"

plot2a <- VerifyData %>%
  #filter(Gender == "Male") %>%   #kept getting error --> object 'Gender' not found
  ggplot(aes(x=Age)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = "Male Age Distribution")

#Female age distribution
plot3a <- VerifyData %>%
  filter(Gender == "Female") %>% #kept getting error --> object 'Gender' not found
  ggplot(aes(x=Age)) +
  geom_density(fill = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(Age))) +
  labs(title = "Female Age Distribution")

#Arranging all three figure in a single plot
ggarrange(plot1a,
          ggarrange(plot2a, plot3a),
          nrow = 2)

############################################################################################

#Creating the bar graph of attrition by gender
dist_attr_gender <- Employee_Attrition %>%
  group_by(Attrition, Gender) %>%
  summarise(Total = n())
print(dist_attr_gender)
#visualizing attrition by Gender
dist_attr_gender %>%
  ggplot(aes(x=Attrition, y=Total, fill=Gender)) +
  geom_col(position="dodge") +
  labs(title = "Attrition By Gender")

##--##

dist_attr_gendera <- VerifyData %>%
  group_by(Attrition, Gender) %>%
  summarise(Total = n())
print(dist_attr_gender)
#visualizing attrition by Gender
dist_attr_gendera %>%
  ggplot(aes(x=Attrition, y=Total, fill=Gender)) +
  geom_col(position="dodge") +
  labs(title = "Attrition By Gender")

##################################

#Histogram of monthly income
x <- Employee_Attrition$MonthlyIncome
h<-hist(x, breaks=100, col="gray", xlab="Monthly Income",
        main="Histogram of Monthly Income with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)


dist_attr_gender <- Employee_Attrition %>%
  group_by(Attrition, Gender) %>%
  summarise(Total = n())
print(dist_attr_gender)

##--##

x <- VerifyData$MonthlyIncome
h<-hist(x, breaks=100, col="gray", xlab="Monthly Income",
        main="Histogram of Monthly Income with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)


dist_attr_gendera <- VerifyData %>%
  group_by(Attrition, Gender) %>%
  summarise(Total = n())
print(dist_attr_gendera)

#############################################

dist_attr_MthIncome <- Employee_Attrition %>%
  group_by(Attrition, MonthlyIncome) %>%
  summarise(Total = n())
print(dist_attr_MthIncome)

#visualizing attrition by Monthly Income
dist_attr_MthIncome %>%
  ggplot(aes(x=Attrition, y=Total, fill=MonthlyIncome,)) +
  geom_col(position="dodge") +
  labs(title = "Attrition By Monthly Income")

##--##

dist_attr_MthIncomea <- VerifyData %>%
  group_by(Attrition, MonthlyIncome) %>%
  summarise(Total = n())
print(dist_attr_MthIncomea)

#visualizing attrition by Monthly Income
dist_attr_MthIncomea %>%
  ggplot(aes(x=Attrition, y=Total, fill=MonthlyIncome,)) +
  geom_col(position="dodge") +
  labs(title = "Attrition By Monthly Income")


#################################################

#Creating the bar graph of attrition by gender
dist_attr_gender <- Employee_Attrition %>%
  group_by(Attrition, Gender) %>%
  summarise(Total = n())
print(dist_attr_gender)
#visualizing attrition by Gender
dist_attr_gender %>%
  ggplot(aes(x=Attrition, y=Total, fill=Gender)) +
  geom_col(position="dodge") +
  labs(title = "Attrition By Gender")

##--##

#Creating the bar graph of attrition by gender
dist_attr_gendera <- VerifyData %>%
  group_by(Attrition, Gender) %>%
  summarise(Total = n())
print(dist_attr_gendera)
#visualizing attrition by Gender
dist_attr_gendera %>%
  ggplot(aes(x=Attrition, y=Total, fill=Gender)) +
  geom_col(position="dodge") +
  labs(title = "Attrition By Gender")

##################################
#Histogram of monthly income
x <- Employee_Attrition$MonthlyIncome
h<-hist(x, breaks=100, col="gray", xlab="Monthly Income",
        main="Histogram of Monthly Income with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)


dist_attr_MonthlyInc <- Employee_Attrition %>%
  group_by(Attrition, MonthlyIncome) %>%
  summarise(Total = n())
print(dist_attr_MonthlyInc)
#visualizing attrition by MOnthly Income
dist_attr_MonthlyInc %>%
  ggplot(aes(x=Attrition, y=Total, fill=MonthlyIncome)) +
  geom_col(position="dodge") +
  labs(title = "Attrition By Monthly Income")

###########################################################################################
#Pie chart of attrition by gender (Male)
pie_attr_male <- dist_attr_gender %>%
  filter(Gender == "Male") %>%
  ggplot(aes(x="", y=Total, fill=Attrition)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) +
  ggtitle("Pie Chart \nAttrition Male") +
  geom_text(aes(y = Total/2 + c(5, 10),
                label = percent(Total/sum(Total))), size=5)

#Pie chart of attrition by gender (Female)
pie_attr_female <- dist_attr_gender %>%
  filter(Gender == "Female") %>%
  ggplot(aes(x="", y=Total, fill=Attrition)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) +
  ggtitle("Pie Chart \nAttrition Female") +
  geom_text(aes(y = Total/2 + c(5, 10),
                label = percent(Total/sum(Total))), size=5)

#Arranging two pie chart in a single plot
ggarrange(pie_attr_male, pie_attr_female)

##--##

#Pie chart of attrition by gender (Male)
pie_attr_malea <- dist_attr_gendera %>%
  filter(Gender == "Male") %>%
  ggplot(aes(x="", y=Total, fill=Attrition)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) +
  ggtitle("Pie Chart \nAttrition Male") +
  geom_text(aes(y = Total/2 + c(5, 10),
                label = percent(Total/sum(Total))), size=5)

#Pie chart of attrition by gender (Female)
pie_attr_femalea <- dist_attr_gendera %>%
  filter(Gender == "Female") %>%
  ggplot(aes(x="", y=Total, fill=Attrition)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) +
  ggtitle("Pie Chart \nAttrition Female") +
  geom_text(aes(y = Total/2 + c(5, 10),
                label = percent(Total/sum(Total))), size=5)

#Arranging two pie chart in a single plot
ggarrange(pie_attr_malea, pie_attr_femalea)
#################################################################################

#Data Modelling
##########################################################
#Before developing a model, we need to format the dataset. It is the process of removing unused variables from the data set.
#It is also possible that some data variables are not required in the analysis and should be removed from the data set.
#I removed the employee number, count, standard hours, stock options level from the data set.
#These variables do not help predict employee turnover, so it is a good idea to remove them from the dataset.

##########################################################

#Removing unused variables
Final_Attrition_Data <- Employee_Attrition[ , ! names(Employee_Attrition) %in% c("X", "EmployeeCount", "EmployeeNumber", "Over18", "StandardHours", "StockOptionLevel")]

Final_Attrition_Data2 <- VerifyData[ , ! names(Employee_Attrition) %in% c("X", "EmployeeCount", "EmployeeNumber", "Over18", "StandardHours", "StockOptionLevel")]

##############################################################################
#Checking correlation between numeric variables
corrplot(cor(Numeric_Variables),
         method = "number",
         type = "upper")# show only upper side
##--##
corrplot(cor(Numeric_Variables2),
         method = "number",
         type = "upper")# show only upper side

####################################################################

#Converting Categorical variable into numeric format
Final_Attrition_Data$Attrition[Final_Attrition_Data$Attrition=="Yes"]=1
Final_Attrition_Data$Attrition[Final_Attrition_Data$Attrition=="No"]=0
Final_Attrition_Data$Attrition=as.numeric(Final_Attrition_Data$Attrition)

##--##

Final_Attrition_Data2$Attrition[Final_Attrition_Data2$Attrition=="Yes"]=1
Final_Attrition_Data2$Attrition[Final_Attrition_Data2$Attrition=="No"]=0
Final_Attrition_Data2$Attrition=as.numeric(Final_Attrition_Data2$Attrition)

#########################################
#Converting categorical (character) variables into factors
Final_Attrition_Data[,c(2,4,7,9,13,15,19)]=lapply(Final_Attrition_Data[,c(2,4,7,9.13,15,19)],as.factor)
head(Final_Attrition_Data)

##--##

#Converting categorical (character) variables into factors
Final_Attrition_Data2[,c(2,4,7,9,13,15,19)]=lapply(Final_Attrition_Data2[,c(2,4,7,9.13,15,19)],as.factor)
head(Final_Attrition_Data2)

###################################################################################################

#Splitting the data set into "training" and "testing"
set.seed(1000)
ranuni=sample(x=c("Training","Testing"),size=nrow(Final_Attrition_Data),replace=T,prob=c(0.7,0.3))
TrainingData=Final_Attrition_Data[ranuni=="Training",]
TestingData=Final_Attrition_Data[ranuni=="Testing",]
nrow(TrainingData)
nrow(TestingData)


###########################################################

#Setting independent variables into formula
formula = Attrition ~ BusinessTravel + DistanceFromHome + Education + EnvironmentSatisfaction  +
  HourlyRate + JobLevel + JobRole + JobSatisfaction + MaritalStatus+MonthlyIncome+
  NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating +
  RelationshipSatisfaction+TotalWorkingYears+
  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole +
  YearsSinceLastPromotion + YearsWithCurrManager

#Creating a data model
Trainingmodel = glm(formula = formula, data = TrainingData, family = "binomial")
summary(Trainingmodel)
summary(Trainingmodel3)

Testingmodel = glm(formula = formula, data = TestingData, family = "binomial")
summary(Trainingmodel)
summary(Testingmodel)

ModelP = glm(Attrition ~ BusinessTravel + DistanceFromHome + Education + EnvironmentSatisfaction  + HourlyRate + JobLevel + JobRole + JobSatisfaction + MaritalStatus+MonthlyIncome+
  NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction+TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole +
  YearsSinceLastPromotion + YearsWithCurrManager, data = TrainingData, family = "binomial")
summary(ModelP)

coef(summary(ModelP))[,4]

####################################################
####################################################

## skip this part? ##

ll.null <- Final_Attrition_Data$null.deviance/-2
ll.proposed <- Final_Attrition_Data$deviance/-2

(ll.null - ll.proposed) / ll.null

1 - pchisq(2*(ll.proposed - ll.null), df=(length(Final_Attrition_Data$coefficients)-1))

Final_Attrition_Data$Attrition <- as.factor(Final_Attrition_Data$Attrition)

       with(summary(TestingModel2), 1 - deviance/null.deviance)

summary(Employee_Attrition)$r.squared

####################################################
####################################################
# SOURCE: https://www.statology.org/confusion-matrix-in-r/
# Create a confusion matrix 
#use model to predict probability of Attrition
predicted <- predict(Trainingmodel, TrainingData3, type="response")

##convert defaults from "Yes" and "No" to 1's and 0's
##TestingData$Attrition <- ifelse(TestingData$Attrition=="Yes", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(TrainingData3$Attrition, predicted)[1]

#create confusion matrix
confusionMatrix(TestingData3$Attrition, predicted)

#########################################################
# SOURCE: https://www.geeksforgeeks.org/how-to-calculate-auc-area-under-curve-in-r/
# create roc curve
roc_object <- roc(TestingData3$Attrition, predicted)

# calculate area under curve
auc(roc_object)

####################################################

EAD <- read.csv('G:/SNHU Advanced Data Analytics/Employee_Attrition_Data.csv', header = TRUE)


library(rattle)
rattle()

library(pROC)
Trainingmodel = glm(formula = formula, data = TrainingData, family = "binomial")
Trainingmodel3 = glm(formula = formula, data = TrainingData3, family = "binomial")

#Lift Curve
# SOURCE: https://www.projectpro.io/recipes/plot-lift-chart-r-logistic-regression
pred_test <- predict(Trainingmodel3,TestingData3,type="response")
ROCR_pred_test <- prediction(pred_test,TestingData3$Attrition)
perf <- performance(ROCR_pred_test,"lift","rpp")
plot(perf, main="Lift curve", colorize=T) 


pred_test <- predict(Trainingmodel,TestingData,type="response")
ROCR_pred_test <- prediction(pred_test,TestingData$Attrition)
perf <- performance(ROCR_pred_test,"lift","rpp")
plot(perf, main="Lift curve", colorize=T) 


###########################################
###########################################
###########################################

#Attempt at s-curve graph
#SOURCE: https://www.youtube.com/watch?v=C4N3_XJJ-jU&list; at time 16:42

predicted.data <- data.frame(
  probability.of.Attrition=Trainingmodel$fitted.values,
  Attrition=TrainingData$Attrition)

predicted.data <- Predicted.data[
  order(predicted.data$probability.of.Attrition, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.Attrition)) + 
  geom_point(aes(color=Attrition), alpha=1, shape=4, stroke=2) +
               xlab("Index") + ylab("Predicted probability of attrition")
