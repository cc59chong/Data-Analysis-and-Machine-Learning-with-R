#Preparing the breast cancer data
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc,ds,sep="")

breast <- read.table(url, sep=",", header=FALSE, na.strings = "?")
names(breast) <- c("ID", "clumpThickness", "sizeUniformity", "shapeUniformity",
                   "maginalAdhesion", "singleEpithelialCellSize", "bareNuclei",
                   "blandChromatin", "normalNucleoli", "mitosis", "class")

df <- breast[-1]
df$class <- factor(df$class, levels = c(2,4), labels=c("benign", "malignant"))
str(df)
#randomly divided into a training sample(70%) and a validate sample(30%)
set.seed(1234)
train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate<-df[-train,]
table(df.train$class)
table(df.validate$class)

#Logistic regression with glm()
fit.logit <- glm(class~., data=df.train, family = binomial())
summary(fit.logit)
#Classifies new cases
prob <- predict(fit.logit, df.validate, type = "response")
logit.pred <- factor(prob > .5, levels = c(FALSE, TRUE), labels=c("benign", "malignant"))
#Evaluates the predictive accuracy
logit.perf <- table(df.validate$class, logit.pred, dnn=c("Actual", "Predicted"))
logit.perf
#           Predicted
#Actual      benign malignant
#benign       118         2
#malignant      4        76
# The accurary was (76+118)/200 = 97% in the calidation sample

# **********************************************************************
# > summary(fit.logit)
# 
# Call:
#   glm(formula = class ~ ., family = binomial(), data = df.train)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.75813  -0.10602  -0.05679   0.01237   2.64317  
# 
# Coefficients:           Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -10.42758    1.47602  -7.065 1.61e-12 ***
# clumpThickness             0.52434    0.15950   3.287  0.00101 ** 
# sizeUniformity            -0.04805    0.25706  -0.187  0.85171    
# shapeUniformity            0.42309    0.26775   1.580  0.11407    
# maginalAdhesion            0.29245    0.14690   1.991  0.04650 *  
# singleEpithelialCellSize   0.11053    0.17980   0.615  0.53871    
# bareNuclei                 0.33570    0.10715   3.133  0.00173 ** 
# blandChromatin             0.42353    0.20673   2.049  0.04049 *  
# normalNucleoli             0.28888    0.13995   2.064  0.03900 *  
# mitosis                    0.69057    0.39829   1.734  0.08295 .  
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 612.063  on 482  degrees of freedom
# Residual deviance:  71.346  on 473  degrees of freedom
# (6 observations deleted due to missingness)
# AIC: 91.346
# 
# Number of Fisher Scoring iterations: 8
#**********************************************************************

# There are three variables "sizeUniformity", "shapeUniformity", "singleEpithelialCellSize" have not coefficients
# can try to remove them
#New Logistic regression with glm()
fit.logit_1 <- glm(class ~ clumpThickness+maginalAdhesion+bareNuclei+blandChromatin+normalNucleoli+mitosis,data=df.train, family = binomial())
summary(fit.logit_1)
#Classifies new cases
prob_1 <- predict(fit.logit_1, df.validate, type = "response")
logit.pred_1 <- factor(prob_1 > .5, levels = c(FALSE, TRUE), labels=c("benign", "malignant"))
#Evaluates the predictive accuracy
logit.perf_1 <- table(df.validate$class, logit.pred_1, dnn=c("Actual", "Predicted"))
logit.perf_1
# Predicted
# Actual      benign malignant
# benign       118         2
# malignant      3        77

# or use stepwise logistic regression
logit.fit.reduced <- step(fit.logit)
#reduced  Logistic regression with glm()
fit.logit_reduced <- glm(class ~ clumpThickness + shapeUniformity + maginalAdhesion + 
                           bareNuclei + blandChromatin + normalNucleoli + mitosis, data=df.train, family = binomial())
summary(fit.logit_reduced)
#Classifies new cases
prob_reduced  <- predict(fit.logit_reduced, df.validate, type = "response")
logit.pred_reduced  <- factor(prob_reduced  > .5, levels = c(FALSE, TRUE), labels=c("benign", "malignant"))
#Evaluates the predictive accuracy
logit.perf_reduced  <- table(df.validate$class, logit.pred_reduced , dnn=c("Actual", "Predicted"))
logit.perf_reduced 
# Predicted
# Actual      benign malignant
# benign       118         2
# malignant      3        77

#Creating a classical decison tree with rpart()
library(rpart)
set.seed(1234)
dtree <- rpart(class~., data=df.train, method = "class", parms = list(split="information"))
print(dtree)
summary(dtree)

dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree, cp=.0125)

library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104, fallen.leaves = TRUE, main = "Decision Tree")

dtree.pred <- predict(dtree.pruned, df.validate, type = "class")
dtree.perf <- table(df.validate$class, dtree.pred, dnn = c("Actual", "Predicted"))
dtree.perf


#Creating a conditional inference tree with ctree()
install.packages("party")
library(party)
fit.ctree <- ctree(class~., data= df.train)
plot(fit.ctree, main="conditional Inference Tree")

ctree.pred <- predict(fit.ctree, df.validate, type="response")
ctree.perf <- table(df.validate$class, ctree.pred, dnn = c("Actual","Predicted"))
ctree.perf

#Random forest
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class~., data = df.train, na.action=na.roughfix, importance= TRUE)
fit.forest

importance(fit.forest, type = 2)

forest.pred <- predict(fit.forest, df.validate)
forest.perf <- table(df.validate$class, forest.pred, dnn=c("Actual","Predicted"))
forest.perf

#Support vector machines
library(e1071)
set.seed(1234)
fit.svm <- svm(class~., data=df.train)
fit.svm

svm.pred <- predict(fit.svm, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class, svm.pred, dnn=c("Actual","Predicted"))
svm.perf

#Tuning an RBF support vectormachine
set.seed(1234)
#Varies the parameters
tuned <- tune.svm(class~., data=df.train, gamma = 10^(-6:1), cost = 10^(-10:10))
#Prints the best model
tuned

fit.svm <- svm(class~., data = df.train, gamma=.01,cost=1)
svm.pred <- predict(fit.svm, na.omit(df.validate))
svm.perf <- table(na.omit(df.validate)$class, svm.pred, dnn=c("Actual","Predicted"))
svm.perf

#Function for assessing binary classification accuracy
performance <- function(table, n=2){
  if(!all(dim(table)== c(2,2)))
    stop("Must be a 2*2 table")
#extracts frequencise
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  #calculates statistics
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate=(tp+tn)/(tp+tn+fp+fn)
  #print result
  result <- paste("Sensitivity = ", round(sensitivity,n), "\nSpecificity = ", round(specificity,n),
                  "\nPositive Predictive Value = ", round(ppp,n), "\nNegative Predictive Value = ", round(npp,n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}

#Performance of breast cancer data classifiers
performance(logit.perf)
performance(dtree.perf)
performance(ctree.perf)
performance(forest.perf)
performance(svm.perf)
