insurance <- read.csv("../input/insurance/insurance.csv")
str(insurance)

summary(insurance$charges)
par(mfrow = c(1,2)) # combine the two plots
hist(insurance$charges, main = "Histogram of charges", col = "lightblue")
plot(density(insurance$charges), main = "Density plot of charges")
polygon(density(insurance$charges), col = "orange")

insurance$charges <- log(insurance$charges)
summary(insurance$charges)
par(mfrow = c(1,2)) 
hist(insurance$charges, main = "Histogram of charges", col = "lightblue")
plot(density(insurance$charges), main = "Density plot of charges")
polygon(density(insurance$charges), col = "orange")

par(mfrow = c(1,3))
barplot(table(insurance$sex), main = "sex")
barplot(table(insurance$smoker), main = "smoker")
barplot(table(insurance$region), main = "region")

par(mfrow = c(1,3))
hist(insurance$age, main = "Histogram of age", col = "lightblue")
hist(insurance$bmi, main = "Histogram of bmi", col = "lightblue")
hist(insurance$children, main = "Histogram of children", col = "lightblue")

par(mfrow = c(1,3))
boxplot(insurance$age, main = "Histogram of age")
boxplot(insurance$bmi, main = "Histogram of bmi")
boxplot(insurance$children, main = "Histogram of children")

outliers_remover <- function(a){
  df <- a
  aa <- c()
  count <- 1
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
          aa[count] <- j
          count <- count+1                  
        }
      }
    }
  }
  
  df <- df[-aa,]
}

insurance_new <- outliers_remover(insurance)
str(insurance_new)

cor(insurance_new[c("age", "bmi", "children")])

library(psych)
pairs.panels(insurance_new[c("age", "sex", "bmi", "children", "smoker", "region")], digits = 2, cor = TRUE)

sl_model <- lm(charges ~ age, data = insurance_new)
summary(sl_model)

mul_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance_new)
# mul_model_ <- lm(charges ~ ., data = insurance_new)
summary(mul_model)

library(relaimpo)
relative_importance <- calc.relimp(mul_model, type = "lmg", rela = TRUE)
sort(relative_importance$lmg, decreasing = TRUE)

#backward stepwise
mul_model1 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance_new)
mul_model2 <- lm(charges ~ age + sex + bmi + children + smoker,          data = insurance_new)
mul_model3 <- lm(charges ~ age + sex + bmi + children,                   data = insurance_new)
#.........
library(rcompanion)
com_mod <- compareLM(mul_model1, mul_model2, mul_model3)
com_mod

#Check the AIC. 
#Create a line chart for AIC values with model numbers on the x axis, and AIC values on the y axis. 
com_model <- com_mod$Fit.criteria
com_model[order(com_model$AIC),]
plot(com_model$AIC, type = "b", xlab = "model number", ylab = "AICc value")


base_mod <- lm(charges ~ 1 , data= insurance_new)  # base intercept only model

all_mod <- lm(charges ~ . , data= insurance_new) # full model with all predictors

stepMod <- step(base_mod, scope = list(lower = base_mod, upper = all_mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm

stepMod


library(MASS)
mul_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance_new)
stepAIC(mul_model, direction = "backward")

library(car)
vif(mul_model)
sqrt(vif(mul_model)) > 2

library(leaps)
subsets <- regsubsets(charges ~., data = insurance_new, nbest = 2)
subsets

plot(subsets, scale = "r2") # regsubsets plot based on R-sq
abline(h=4, v=0, col="red")
abline(h=10, v=0, col="green")


mod_re <- residuals(mul_model)
hist(mod_re)

par(mfrow=c(2,2))
plot(mul_model)

insurance_new$age2 <- insurance_new$age^2
insurance_new$bmi30 <- ifelse(insurance_new$bmi >= 30, 1, 0)
im_model <- lm(charges ~ age + age2 + children +bmi + sex + bmi30*smoker + region, data = insurance_new)
summary(im_model)

par(mfrow=c(2,2))
plot(im_model)

insurance <- read.csv("../input/insurance/insurance.csv")
insurance$charges <- log(insurance$charges)
insurance_new <- outliers_remover(insurance)

# Create Training and Test data -- model1 (before the improvments)
trainingRowIndex <- sample(1:nrow(insurance_new), 0.7 * nrow(insurance_new)) #row indices for training data
trainingData <- insurance_new[trainingRowIndex,] #training data
testData <- insurance_new[-trainingRowIndex,] #test data
# Build the model on training data
model1 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = trainingData)
# predict
pred1 <- predict(model1, testData)
actuals_preds1 <- data.frame(cbind(actuals = testData$charges, predicted = pred1))
cor(actuals_preds1)

insurance <- read.csv("../input/insurance/insurance.csv")
insurance$charges <- log(insurance$charges)
insurance_new <- outliers_remover(insurance)
insurance_new$age2 <- insurance_new$age^2
insurance_new$bmi30 <- ifelse(insurance_new$bmi >= 30, 1, 0)

# Create Training and Test data -- model2 (after the improvments)
trainingRowIndex <- sample(1:nrow(insurance_new), 0.7 * nrow(insurance_new)) #row indices for training data
trainingData <- insurance_new[trainingRowIndex,] #training data
testData <- insurance_new[-trainingRowIndex,] #test data
# Build the model on training data
model2 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = trainingData)
# predict
pred2 <- predict(model2, testData)
actuals_preds2 <- data.frame(cbind(actuals = testData$charges, predicted = pred2))
cor(actuals_preds2)