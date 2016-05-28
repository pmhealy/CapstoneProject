#########################################
##########   Set up the Working Directory
#########################################
setwd("C:/Users/Mark's/Desktop/Data Science/CapstoneProject")
getwd()

##############################
##########   Read in the Data
##############################
CensusData = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                        header=FALSE, sep=",",
                        col.names=c("Age","WorkType","FnlWgt","Education","EducationNum","MaritalStatus",
                                    "Occupation", "Relationship", "Race", "Sex","CapGain","CapLoss","HoursPerWeek",
                                    "NativeCountry","Salary"))

## Check Dimension of Dataset
dim(CensusData)

## Laod dplyr and create local dataframe
library(dplyr)
CensusDF<-tbl_df(CensusData)

##############################
##########   Data Cleaning
##############################

## Remove Eduacation from Dataset - EducationNum provides same info in integer form
CensusDF$Education<-NULL
## Remove FnlWgt from Dataset - Weighting for how many people this observation represents
CensusDF$FnlWgt<-NULL

## Remove CapGain from Dataset
length(CensusDF$CapGain)
CensusDF$CapGain<-NULL

## Remove CapLoss from Dataset
table(CensusDF$CapLoss)
CensusDF$CapLoss<-NULL

## Check Dimension of Dataset (4 variables less than previously)
dim(CensusDF)
names(CensusDF)
###################################
### Plot the  data

par(mfrow=c(1,3))
hist(CensusDF$Age, col="cornflowerblue",main="Fig 2.1: Age", xlab="Years")
hist(CensusDF$EducationNum, col="cornflowerblue",main="Fig 2.2: Years of Education", xlab="Years")
hist(CensusDF$HoursPerWeek, col="cornflowerblue",main="Fig 2.3: Hours worked per Week", xlab="Hours")

median_age<-median(CensusDF$Age) ; siqr_age<-IQR(CensusDF$Age)/2
median_age; siqr_age

median_age<-median(CensusDF$Age) ; siqr_age<-IQR(CensusDF$Age)/2
median_age; siqr_age

median_HoursPerWeek<-median(CensusDF$HoursPerWeek) ; siqr_HoursPerWeek<-IQR(CensusDF$HoursPerWeek)/2
median_HoursPerWeek; siqr_HoursPerWeek



## Tabulate Categorical Variables to investgate possible groupings
table(CensusDF$WorkType)    
table(CensusDF$MaritalStatus)
table(CensusDF$Occupation)  
table(CensusDF$Relationship)
table(CensusDF$Race)
table(CensusDF$Sex)
table(CensusDF$NativeCountry)
table(CensusDF$Salary)

#################################################
###### Recode WorkType into fewer categories
#################################################

CensusDF$WorkType = as.character(CensusDF$WorkType)

CensusDF$WorkType = gsub("Private","Private",CensusDF$WorkType)
CensusDF$WorkType = gsub("Self-emp-inc","Self-Employed",CensusDF$WorkType)
CensusDF$WorkType = gsub("Self-emp-not-inc","Self-Employed",CensusDF$WorkType)
CensusDF$WorkType = gsub("Without-pay","Not-Working",CensusDF$WorkType)
CensusDF$WorkType = gsub("Never-worked","Not-Working",CensusDF$WorkType)

CensusDF$WorkType = factor(CensusDF$WorkType)

table(CensusDF$WorkType)
str(CensusDF$WorkType)

#################################################
###### Recode MaritalStatus into fewer categories
#################################################

CensusDF$MaritalStatus = as.character(CensusDF$MaritalStatus)

CensusDF$MaritalStatus = gsub("Never-married","Not-Married",CensusDF$MaritalStatus)
CensusDF$MaritalStatus = gsub("Married-AF-spouse","Married",CensusDF$MaritalStatus)
CensusDF$MaritalStatus = gsub("Married-civ-spouse","Married",CensusDF$MaritalStatus)
CensusDF$MaritalStatus = gsub("Married-spouse-absent","Not-Married",CensusDF$MaritalStatus)
CensusDF$MaritalStatus = gsub("Separated","Not-Married",CensusDF$MaritalStatus)
CensusDF$MaritalStatus = gsub("Divorced","Not-Married",CensusDF$MaritalStatus)
CensusDF$MaritalStatus = gsub("Widowed","Widowed",CensusDF$MaritalStatus)

CensusDF$MaritalStatus = factor(CensusDF$MaritalStatus)

table(CensusDF$MaritalStatus)
str(CensusDF$MaritalStatus)



#################################################
###### Recode Occupation into fewer categories
#################################################

CensusDF$Occupation = as.character(CensusDF$Occupation)
CensusDF$Occupation <- gsub("?",NA,CensusDF$Occupation, fixed = TRUE)

CensusDF$Occupation = gsub("Adm-clerical","Admin",CensusDF$Occupation)
CensusDF$Occupation = gsub("Armed-Forces","Military",CensusDF$Occupation)
CensusDF$Occupation = gsub("Craft-repair","Skilled-Manual",CensusDF$Occupation)
CensusDF$Occupation = gsub("Exec-managerial","Professional",CensusDF$Occupation)
CensusDF$Occupation = gsub("Farming-fishing","Skilled-Manual",CensusDF$Occupation)
CensusDF$Occupation = gsub("Handlers-cleaners","Manual",CensusDF$Occupation)
CensusDF$Occupation = gsub("Machine-op-inspct","Manual",CensusDF$Occupation)
CensusDF$Occupation = gsub("Other-service","Service",CensusDF$Occupation)
CensusDF$Occupation = gsub("Priv-house-serv","Service",CensusDF$Occupation)
CensusDF$Occupation = gsub("Prof-specialty","Professional",CensusDF$Occupation)
CensusDF$Occupation = gsub("Protective-serv","Other-Occupations",CensusDF$Occupation)
CensusDF$Occupation = gsub("Sales","Sales",CensusDF$Occupation)
CensusDF$Occupation = gsub("Tech-support","Professional",CensusDF$Occupation)
CensusDF$Occupation = gsub("Transport-moving","Skilled-Manual",CensusDF$Occupation)

table(CensusDF$Occupation)
nlevels(CensusDF$Occupation)
str(CensusDF$Occupation)

CensusDF$Occupation = factor(CensusDF$Occupation)


#################################################
###### Recode NativeCountry into fewer categories
#################################################

CensusDF$NativeCountry = as.character(CensusDF$NativeCountry)
## http://econ.worldbank.org/WBSITE/EXTERNAL/DATASTATISTICS/0,,contentMDK:20421402~menuPK:64133156~pagePK:64133150~piPK:64133175~theSitePK:239419,00.html#Low_income

CensusDF$NativeCountry <- gsub("?",NA,CensusDF$NativeCountry, fixed = TRUE)
CensusDF$NativeCountry[CensusDF$NativeCountry==" Cambodia"] = "Low-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Canada"] = "High-Income"    
CensusDF$NativeCountry[CensusDF$NativeCountry==" China"] = "Lower-Middle-Income"       
CensusDF$NativeCountry[CensusDF$NativeCountry==" Columbia"] = "Upper-Middle-Income"    
CensusDF$NativeCountry[CensusDF$NativeCountry==" Cuba"] = "Upper-Middle-Income"        
CensusDF$NativeCountry[CensusDF$NativeCountry==" Dominican-Republic"] = "Upper-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Ecuador"] = "Lower-Middle-Income"     
CensusDF$NativeCountry[CensusDF$NativeCountry==" El-Salvador"] = "Lower-Middle-Income" 
CensusDF$NativeCountry[CensusDF$NativeCountry==" England"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" France"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Germany"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Greece"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Guatemala"] = "Lower-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Haiti"] = "Low-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Holand-Netherlands"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Honduras"] = "Lower-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Hong"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Hungary"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" India"] = "Lower-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Iran"] = "Lower-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Ireland"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Italy"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Jamaica"] = "Upper-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Japan"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Laos"] = "Low-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Mexico"] = "Upper-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Nicaragua"] = "Lower-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Outlying-US(Guam-USVI-etc)"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Peru"] = "Upper-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Philippines"] = "Lower-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Poland"] = "Upper-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Portugal"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Puerto-Rico"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Scotland"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" South"] = " ?"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Taiwan"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Thailand"] = "Lower-Middle-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Trinadad&Tobago"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" United-States"] = "High-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Vietnam"] = "Low-Income"
CensusDF$NativeCountry[CensusDF$NativeCountry==" Yugoslavia"] = "Upper-Middle-Income"



CensusDF$NativeCountry = factor(CensusDF$NativeCountry)
str(CensusDF$NativeCountry)
table(CensusDF$NativeCountry)






######################################################################
######################################################################
## Recoded categorical data plotted

CensusCleaned<-na.omit(CensusDF)
dim(CensusCleaned)

par(mfrow=c(3,1))
counts_MaritalStatus <- table(CensusCleaned$MaritalStatus)
barplot(counts_MaritalStatus, main="Fig 3.1: Marital Status", 
        xlab="Marital Status", col="cornflowerblue")

counts_Occupation <- table(CensusCleaned$Occupation)
barplot(counts_Occupation, main="Fig 3.2: Occupation", 
        xlab="Occupation", col="cornflowerblue")

counts_NativeCountry <- table(CensusCleaned$NativeCountry)
barplot(counts_NativeCountry, main="Fig 3.3: Native Country", 
        xlab="Native Country", col="cornflowerblue")

##############################################################################
#### Split into Test and Train Data (30% and 70% respectively)
##############################################################################

## Set seed to ensure reproducible results
set.seed(888)
## Sample Indexes
indexes = sample(1:nrow(CensusCleaned), size=0.3*nrow(CensusCleaned))

# Split the data into Test and Train
Test = CensusCleaned[indexes,]
dim(Test)  # 9027 11
Train = CensusCleaned[-indexes,]
dim(Train) # 21064 11

TestX<-Test %>% select(-Salary)
TestY<-Test %>% select(Salary)

TrainX<-Train %>% select(-Salary)
TrainY<-Train %>% select(Salary)


## Export the Datasets
write.csv(TestX, "TestX.csv")
write.csv(TestY, "TestY.csv")
write.csv(TrainX, "TrainX.csv")
write.csv(TrainY, "TrainY.csv")


library(e1071)
## Build a base SVM on TrainX and TrainY, Predict on TestX
#svmfit=svm(Train$Salary~., data=Train, kernel="radial",  gamma=1, cost=1)
#TestPred<-predict(svmfit, Test[,-11])
#length(TestPred) # 9027

## Build an SVM on TrainX and TrainY, Predict on TrainX
#svmfit=svm(Train$Salary~., data=Train, kernel="radial",  gamma=1, cost=1)
TrainPred<-predict(svmfit, Train[,-11])
length(TrainPred) # 21,064

## Calculate Train Error

## Contingency Tables   
#tablePred<-table(pred=TrainPred, true=unlist(TrainY))
#table(pred=TestPred, true=unlist(TestY))

## Confusion Matrices for Train and Test
install.packages("caret")
#caret::confusionMatrix(TrainPred,unlist(TrainY))
#caret::confusionMatrix(TestPred,unlist(TestY))


######################################################################
#### Use tune.svm to pick optimal values for Cost and Gamma parameters
######################################################################
str(Train)


#obj <- tune.svm(Salary~., data = Train, gamma =seq(0.5, 2, by = 0.5), cost = seq(1,8, by = 2))
#obj
#summary(obj)
#class(obj) 

### the smallest combo was best in the above tuning
#### - best performance: 0.164546 

###- Detailed performance results:
###        gamma cost     error  dispersion
###   1    0.5    1     0.1645460 0.007409911
### so will edit Cost and Gamma to search a lower bound

###########################################################################
###########################################################################
### Second Tune

### - sampling method: 10-fold cross validation 

### - best parameters:
###  gamma cost
###  0.3  1.1

obj <- tune.svm(Salary~., data = Train, gamma =seq(0.1, 0.5, by = 0.1), cost = seq(0.1,1.1, by = .2))
obj
summary(obj)
class(obj)

### Run singular model with optimal parameter values inputted

svm_model_after_tune <- svm(Salary~., data = Train, gamma =0.3, cost = 1.1, probability=TRUE)
summary(svm_model_after_tune)

trainpred <- predict(svm_model_after_tune,TrainX)
#table(trainpred,Train$Salary)

testpred <- predict(svm_model_after_tune,TestX)
#table(testpred,Test$Salary)

########################################
#Tabulate Test and Train Errors

caret::confusionMatrix(trainpred,unlist(TrainY))
caret::confusionMatrix(testpred,unlist(TestY))

########################################

#Using Test Data here

###########################################
TestSalary<-Test$Salary
TestX ### Variable Salary discarded
table(Test$Salary)

# obsolete i think
##svm.model <- svm(TestSalary~., data = TestX, gamma =0.3, cost = 1.1,probability=TRUE)

### replace below (svm_model_after_tune) with svm.model from above

# obsolete i think
#svm.pred<-predict(svm.model, TestX, decision.values = TRUE, 
 #                 probability = TRUE) ###### re-run from here

svm.pred<-predict(svm_model_after_tune, TestX, decision.values = TRUE, 
                  probability = TRUE) ###### re-run from here

library(ROCR) 
svm.roc <- prediction(attributes(svm.pred)$decision.values, TestSalary, label.ordering=c(" >50K", " <=50K"))
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
par(mfrow=c(1,1))
plot(svm.auc, main="Fig 4.1: ROC Curve for SVM model") 
text(0.47, 0.75, "AUC =0.87", cex=1)
abline(a=0, b= 1)

summary(svm.auc)

performance(svm.roc, "auc")

##########################################################
###################################
###################################

