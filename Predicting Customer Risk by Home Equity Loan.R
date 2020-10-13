
data <- read.csv("../input/hmeq-data/hmeq.csv")
str(data)

#DATA PRE-PROCESSING
# 1.Convert The Classes

# Convert 'BAD' from integer to factor
data$BAD <- as.factor(data$BAD)
                  
# Convert integer variables to numeric variables
data$LOAN <- as.numeric(data$LOAN)
data$NINQ <- as.numeric(data$NINQ)
data$CLNO <- as.numeric(data$CLNO)
data$DEROG <- as.numeric(data$DEROG)
data$DELINQ <- as.numeric(data$DELINQ)
str(data)

#Check For Missing Values
summary(data)
data<-na.omit(data)
dim(data)

#Check For Outliers
par(mfrow = c(2,3))
boxplot(data$LOAN, main = "LOAN")
boxplot(data$MORTDUE, main = "MORTDUE")
boxplot(data$VALUE, main = "VALUE")
boxplot(data$YOJ, main = "YOJ")
boxplot(data$DEROG, main = "DEROG")
boxplot(data$DELINQ, main = "DELINQ")
boxplot(data$CLAGE, main = "CLAGE")
boxplot(data$NINQ, main = "NINQ")
boxplot(data$CLNO, main = "CLNO")
boxplot(data$DEBTINC, main = "DEBTINC")

#remove the outliers
outliers_remover <- function(a){
  df <- a
  aa<-c()
  count<-1
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])){
      Q3 <- quantile(df[,i], 0.75, na.rm = TRUE)
      Q1 <- quantile(df[,i], 0.25, na.rm = TRUE) 
      IQR <- Q3 - Q1  #IQR(df[,i])
      upper <- Q3 + 1.5 * IQR
      lower <- Q1 - 1.5 * IQR
      for(j in 1:nrow(df)){
        if(is.na(df[j,i]) == TRUE){
          next
        }
        else if(df[j,i] > upper | df[j,i] < lower){
          aa[count]<-j
          count<-count+1                  
        }
      }
    }
  }
  df<-df[-aa,]
}

data <- outliers_remover(data)
boxplot(data)
dim(data)

#check the typographical errors in the factor type
par(mfrow = c(2,2))
barplot(table(data$REASON), main = "REASON")
barplot(table(data$JOB), main = "JOB")
barplot(table(data$BAD), main = "BAD")

#correct the errors
data <- data[!(data$REASON ==""),]
data <- data[!(data$JOB ==""),]
par(mfrow = c(1,2))
barplot(table(data$REASON), main = "REASON")
barplot(table(data$JOB), main = "JOB")
dim(data)

#Check if there is any multicollinearity between variables.
cor(data[c("LOAN", "MORTDUE", "VALUE", "YOJ", "DEROG", "DELINQ", "CLAGE", "NINQ", "CLNO", "DEBTINC")])
data$DEROG <- NULL
data$DELINQ <- NULL
# there is multicollinearity between MORTDUE and VALUE
data$MORTDUE <- NULL # delete MORTDUE
dim(data)

#Split the Dataset into Training Data and Test Data
#create training data
input_ones <- data[which(data$BAD == 1), ] #all 1's
input_zeros <- data[which(data$BAD == 0), ] # all 0's
set.seed(100) # for repeatability of sample
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7 * nrow(input_ones)) #1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7 * nrow(input_zeros)) #0's for training
#pick as many as 0's and 1's
training_ones <- input_ones[input_ones_training_rows, ]
training_zeros <- input_zeros[input_zeros_training_rows, ]
#row bind the 1's and 0's
trainingData <- rbind(training_ones, training_zeros)

#create test data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
#row bind the 1's and 0's
testData <- rbind(test_ones, test_zeros)

#Check the dependent variable whether imbalance
#check table
table(trainingData$BAD)
#check classe distrubution
prop.table(table(trainingData$BAD))

#Resampling the Training Set
library(rpart)
treeMod <- rpart(BAD ~., data = trainingData)
pred_treeMod <- predict(treeMod, newdata = testData)

library(ROSE)
accuracy.meas(testData$BAD, pred_treeMod[,2])
# check the accuracy using ROC curve
roc.curve(testData$BAD, pred_treeMod[,2], plotit = F)

#Let's compare various sampling methods.
# Under-sampling
data_balanced_under <- ovun.sample(BAD ~., data = trainingData, method = "under", N = 144)$data
table(data_balanced_under$BAD)

# Over-sampling
data_balanced_over <- ovun.sample(BAD ~., data = trainingData, method = "over", N = 2912)$data
table(data_balanced_over$BAD)

# Do both under-sampling and over-sampling
# In this case, the minority class is oversampled with replacement and majority class is undersampled without replacement.
data_balanced_both <- ovun.sample(BAD ~., data = trainingData, method = "both", p = 0.5, N = 1528)$data
table(data_balanced_both$BAD)

# ROSE helps us to generate data synthetically as well. 
data_rose <- ROSE(BAD ~., data =  trainingData)$data
table(data_rose$BAD)

#Let's compute the model using each data and evaluate its accuracy.
# built decision tree models
tree.under <- rpart(BAD ~., data = data_balanced_under)
tree.over <- rpart(BAD ~., data = data_balanced_over)
tree.both <- rpart(BAD ~., data = data_balanced_both)
tree.rose <- rpart(BAD~., data = data_rose)

# make predictions on test data
pred_tree.under <- predict(tree.under, newdata = testData)
pred_tree.over <- predict(tree.over, newdata = testData)
pred_tree.both <- predict(tree.both, newdata = testData)
pred_tree.rose <- predict(tree.rose, newdata = testData)

# AUC
par(mfrow = c(2,2))
roc.curve(testData$BAD, pred_tree.under[,2], col = "RED", main = "ROC curve of under")
roc.curve(testData$BAD, pred_tree.over[,2], col = "BLUE", main = "ROC curve of over")
roc.curve(testData$BAD, pred_tree.both[,2], col = "ORANGE", main = "ROC curve of both")
roc.curve(testData$BAD, pred_tree.rose[,2], col = "BLACK", main = "ROC curve of rose")

ROSE.holdout <- ROSE.eval(BAD ~., data = trainingData, learner = rpart, method.assess = "holdout", extr.pred = function(obj)obj[,2])
ROSE.holdout

#Bulid Logit Models and Predict
logitMod <- glm(BAD ~., data = data_rose, family = binomial(link = "logit"))
pred_logit <- predict(logitMod, testData)

#Model Diagnostics
summary(logitMod)

library(car)
vif(logitMod)

library(InformationValue)
optCutoff  <- optimalCutoff(testData$BAD, pred_logit)[1] # returns cutoff that gives minimum misclassification error.
optCutoff

misClassError(testData$BAD, pred_logit, threshold = optCutoff)

misClassError(testData$BAD, pred_logit, threshold = 0.5)

confusionMatrix(testData$BAD, pred_logit, optCutoff)
# The columns are actuals, while rows are predicteds.

specificity(testData$BAD, pred_logit, optCutoff)

sensitivity(testData$BAD, pred_logit, optCutoff)

# Precision, recall, F
accuracy.meas(testData$BAD, pred_logit, optCutoff)

plotROC(testData$BAD, pred_logit)

Concordance(testData$BAD, pred_logit)

#Let's look at the feature importance
library('caret')
varImp(logitMod)

