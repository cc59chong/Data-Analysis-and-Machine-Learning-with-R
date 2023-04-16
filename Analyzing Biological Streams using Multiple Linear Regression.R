stream0 <- read.csv("stream.csv")
str(stream0)

stream0$Stream <- NULL
stream0$Acerage <- NULL
str(stream0)
boxplot(stream0)


boxplot(stream0$Longnose, main = "Longnose")
boxplot(stream0$Acerage, main = "Acerage")
boxplot(stream0$DO2, main = "DO2")
boxplot(stream0$Maxdepth, main = "Maxdepth")
boxplot(stream0$NO3, main = "NO3")
boxplot(stream0$SO4, main = "SO4")
boxplot(stream0$Temp,main = "Temp")

outliers_remover <- function(a){
  df <- read.csv(a)
  aa<-c()
  count<-1
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])){
      Q3 <- quantile(df[,i], 0.75)
      Q1 <- quantile(df[,i], 0.25) 
      IQR <- Q3 - Q1  #IQR(df[,i])
      upper <- Q3 + 1.5 * IQR
      lower <- Q1 - 1.5 * IQR
      for(j in 1:nrow(df)){
        if(df[j,i] > upper | df[j,i] < lower){
          aa[count]<-j
          count<-count+1                  
        }
      }
    }
  }
  
  df<-df[-aa,]
}

stream1 <- outliers_remover("stream.csv")
str(stream1)

# boxplot.stats(stream0$Longnose)$out
# boxplot.stats(stream0$Acerage)$out
# boxplot.stats(stream0$DO2)$out
# boxplot.stats(stream0$Maxdepth)$out
# boxplot.stats(stream0$NO3)$out
# boxplot.stats(stream0$SO4)$out
# boxplot.stats(stream0$Temp)$out


#b******************************
#Check the normality of the dependent variable. Log transform the dependent variable if necessary.¶
summary(stream1$Longnose)
par(mfrow = c(1,2))
hist(stream1$Longnose, main = "Histogram of Longnose")
plot(density(stream1$Longnose), main = "Density of Longnose")

#Then log and check the dependent variable Longnose.
stream1$Longnose <- log(stream1$Longnose)
summary(stream1$Longnose)
par(mfrow = c(1,2))
hist(stream1$Longnose, main = "Histogram of Longnose(log)")
plot(density(stream1$Longnose), main = "Density of Longnose(log)")

#c******************************
#Check the correlation coefficient

#1.Using cor() to see table
cor(stream1[c("Longnose", "Acerage", "DO2", "Maxdepth", "NO3", "SO4", "Temp")])


#d******************************
#Using paris.pannels
library(psych)
library(psych)
pairs.panels(stream1[c("Longnose", "Acerage", "DO2", "Maxdepth", "NO3", "SO4", "Temp")],
             pch =1,
             lm = TRUE, 
             cex.cor = 1,
             smoother = F,
             stars = T)

#Using chart.Correlation
library(PerformanceAnalytics)
chart.Correlation(stream1[c("Longnose", "Acerage", "DO2", "Maxdepth", "NO3", "SO4", "Temp")],
                  method="pearson",
                  histogram=TRUE,
                  pch=1,
                  main = "Scatterplot Matrix")

#Using ggpairs
library(ggplot2)
library(GGally)
ggpairs(stream1[c("Longnose", "Acerage", "DO2", "Maxdepth", "NO3", "SO4", "Temp")])


#e******************************
model_stream <- lm(Longnose ~ Acerage + DO2 + Maxdepth + NO3 + SO4 + Temp, data = stream1) 
model_stream

#f******************************
#Check the model assumptions
hist(residuals(model_stream))
mod_re <- residuals(model_stream)
hist(mod_re)
#II

#III
plot(model_stream)

#g******************************
install.packages("rcompanion")
library(rcompanion )
model_1  <- lm(Longnose ~ Acerage,                          data=stream1)
model_2  <- lm(Longnose ~ Maxdepth,                         data=stream1)
model_3  <- lm(Longnose ~ NO3,                              data=stream1)
model_4  <- lm(Longnose ~ Acerage  + Maxdepth,              data=stream1)
model_5  <- lm(Longnose ~ Acerage  + NO3,                   data=stream1)
model_6  <- lm(Longnose ~ Maxdepth + NO3,                   data=stream1) 
model_7  <- lm(Longnose ~ Acerage  + Maxdepth + NO3,        data=stream1)
model_8  <- lm(Longnose ~ Acerage  + Maxdepth + NO3 + DO2,  data=stream1)                
model_9  <- lm(Longnose ~ Acerage  + Maxdepth + NO3 + SO4,  data=stream1)                       
model_10 <- lm(Longnose ~ Acerage  + Maxdepth + NO3 + Temp, data=stream1) 

com_mod <- compareLM(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10)
com_mod

com_model <- com_mod$Fit.criteria
com_model[order(com_model$AIC),]


plot(com_model$AIC, type = "b", xlab = "model number", ylab = "AIC value")



#h******************************
set.seed(100)
stream_7 <- stream1[c("Longnose","Acerage","Maxdepth", "NO3")]

trainingRowIndex <- sample(1:nrow(stream_7), 0.8*nrow(stream_7)) 
# row indices for training data

trainingData <- stream_7[trainingRowIndex, ]  # model training data

testData  <- stream_7[-trainingRowIndex, ]   # test data

# Build the model on training data -

str_model7 <- lm(Longnose ~ Acerage + Maxdepth + NO3, data=trainingData)  # build the model

long_Pred7 <- predict(str_model7, testData)  # predict distance

actuals_preds7 <-data.frame(cbind(actuals=testData$Longnose, predicteds=long_Pred7))

correlation_accuracy7 <- cor(actuals_preds7)
correlation_accuracy7

set.seed(100)
stream_8<- stream1[c("Longnose","Acerage","Maxdepth", "NO3", "DO2")]

trainingRowIndex <- sample(1:nrow(stream_8), 0.8*nrow(stream_8)) 
# row indices for training data

trainingData <- stream_8[trainingRowIndex, ]  # model training data

testData <- stream_8[-trainingRowIndex, ]   # test data

# Build the model on training data -

str_model8 <- lm(Longnose ~ Acerage + Maxdepth + NO3 + DO2, data=trainingData)  # build the model

long_Pred8 <- predict(str_model8, testData)  # predict distance

actuals_preds8 <-data.frame(cbind(actuals=testData$Longnose, predicteds=long_Pred8))

correlation_accuracy8 <- cor(actuals_preds8)
correlation_accuracy8




