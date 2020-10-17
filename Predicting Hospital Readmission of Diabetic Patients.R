library(dplyr)
library(skimr)
library(stringr)
library(psych)
library(ROSE)
library(ggplot2)
library(caret)

hospData <- read.csv("../input/diabetes/diabetic_data.csv")
skim(hospData)
#summary(hospData)

#Change the data type of "Admission type", "Discharge disposition" and "Admission source" from numeric to factor.
hospData$admission_type_id <- as.factor(hospData$admission_type_id)
hospData$discharge_disposition_id <- as.factor(hospData$discharge_disposition_id)
hospData$admission_source_id <- as.factor(hospData$admission_source_id)

#count the missing value with mark"?" and "Unknown/Invalid"
count <- 0
for(i in 1:ncol(hospData)){
  if(is.factor(hospData[,i])){
    for(j in 1:nrow(hospData)){
      if(hospData[j,i]== "?" | hospData[j,i]== "Unknown/Invalid" ){
        count <- count + 1
        hospData[j,i] <- NA  #replace "?" and "Unknown/Invalid" values with NA
      }
    }
    if(count > 0){
      print(c(colnames(hospData)[i],count))
    }
  }
  count <- 0
}

#other method: replace "?" and "Unknown/Invalid"values with NA
# library(naniar)
# replace_with_na_all(data = hospDate, condition = ~.x %in% c("?", "Unknown/Invalid"))

#Due to the large amount of data and long running time, I archive the converted data first for convenience of later call.
write.csv(hospData, file = "hospData_NA.csv")
hospD <- read.csv("./hospData_NA.csv")
hospD$X <- NULL

#delete columns "weight", "payer_code", "medical_specialty"
hospD$weight <- NULL
hospD$payer_code <- NULL
hospD$medical_specialty <- NULL
dim(hospD)

#delete columns "encounter_id"
hospD$encounter_id <- NULL
#delete columns "diag_2", "diag_3", only use the primary diagnosis(diag_1)
hospD$diag_2 <- NULL
hospD$diag_3 <- NULL
dim(hospD)

#"examide" and "citoglipton" only have 1 value, remove
hospD$examide <- NULL
hospD$citoglipton <- NULL
dim(hospD)

#remove missing value--"race","gender","diag_1"
hospD <- na.omit(hospD)
dim(hospD)

par(mfrow = c(1,2))
barplot(table(hospD$discharge_disposition_id), main = "Before")
#"discharge__disposition_id" tells us where the patient went after the hospitalization.
#11,13,14,19,20,21 related to death or hospice, which cannot be readmitted
#remove
hospD <- hospD[!hospD$discharge_disposition_id %in% c(11,13,14,19,20,21), ]
barplot(table(hospD$discharge_disposition_id), main = "After")

#change the name of column "admission_type_id" to "admission_type" 
colnames(hospD)[5] <- "admission_type"
barplot(table(hospD$admission_type))

#collapsing some other variable
hospD$admission_type <- replace(hospD$admission_type,hospD$admission_type == 2, 1)
hospD$admission_type <- replace(hospD$admission_type,hospD$admission_type == 7, 1)
hospD$admission_type <- replace(hospD$admission_type,hospD$admission_type == 6, 5)
hospD$admission_type <- replace(hospD$admission_type,hospD$admission_type == 8, 5)

barplot(table(hospD$admission_type), main = "After collapsing")

#change the name of variables
hospD$admission_type <- str_replace(hospD$admission_type,"1","Emergency")
hospD$admission_type <- str_replace(hospD$admission_type,"5","Other")
hospD$admission_type <- str_replace(hospD$admission_type,"3","Elective")
hospD$admission_type <- str_replace(hospD$admission_type,"4","Newborn")

hospD$admission_type <- as.factor(hospD$admission_type)
barplot(table(hospD$admission_type))

#change the name of column "admission_source_id" to "admission_source" 
colnames(hospD)[7] <- "admission_source"
barplot(table(hospD$admission_source))

#collapsing some other variable and change the name of variables
hospD$admission_source <- case_when(hospD$admission_source %in% c("1","2","3") ~ "Physician   Referral",
                                    hospD$admission_source %in% c("4","5","6","8","9","10","11","12","13","14","15","17","18","19","20","21","22","23","24","25","26") ~   "Other",  
                                    TRUE ~ "Emergency Room")                                          

hospD$admission_source <- as.factor(hospD$admission_source)
barplot(table(hospD$admission_source), main = "After collapsing and changing the type")

#change the name of column "discharge_disposition_id" to "discharge_disposition" 
colnames(hospD)[6] <- "discharge_disposition"
barplot(table(hospD$discharge_disposition))

#collapsing some other variable and change the name of variables
hospD$discharge_disposition <- case_when(hospD$discharge_disposition %in% "1" ~ "Home",
                                         TRUE ~ "Other")

hospD$discharge_disposition <- as.factor(hospD$discharge_disposition)
barplot(table(hospD$discharge_disposition), main = "After collapsing and changing the type")

hospD<- mutate(hospD, primary_diagnosis =
                 ifelse(str_detect(diag_1, "V") | str_detect(diag_1, "E"),"Other", 
                        # disease codes starting with V or E are in "other" category;
                        ifelse(str_detect(diag_1, "250"), "Diabetes",
                               ifelse((as.integer(diag_1) >= 390 & as.integer(diag_1) <= 459) | as.integer(diag_1) == 785, "Circulatory",
                                      ifelse((as.integer(diag_1) >= 460 & as.integer(diag_1) <= 519) | as.integer(diag_1) == 786, "Respiratory", 
                                             ifelse((as.integer(diag_1) >= 520 & as.integer(diag_1) <= 579) | as.integer(diag_1) == 787, "Digestive", 
                                                    ifelse((as.integer(diag_1) >= 580 & as.integer(diag_1) <= 629) | as.integer(diag_1) == 788, "Genitourinary",
                                                           ifelse((as.integer(diag_1) >= 140 & as.integer(diag_1) <= 239), "Neoplasms",  
                                                                  ifelse((as.integer(diag_1) >= 710 & as.integer(diag_1) <= 739), "Musculoskeletal",          
                                                                         ifelse((as.integer(diag_1) >= 800 & as.integer(diag_1) <= 999), "Injury",                    
                                                                                "Other"))))))))))


#**********************************other method*************************************#
#hospD <-mutate(hospD, primary_diagnosis = case_when (
#                                                   str_detect(diag_1, "V") | str_detect(diag_1, "E") ~ "Other",
#                                                    str_detect(diag_1, "250") ~ "Diabetes",
#                                                    (as.integer(diag_1) >= 390 & as.integer(diag_1) <= 459) | as.integer(diag_1) == 785 ~ "Circulatory",
#                                                    (as.integer(diag_1) >= 460 & as.integer(diag_1) <= 519) |as.integer(diag_1) == 786 ~ "Respiratory", 
#                                                    (as.integer(diag_1) >= 520 & as.integer(diag_1) <= 579) | as.integer(diag_1) == 787~ "Digestive", 
#                                                    (as.integer(diag_1) >= 580 & as.integer(diag_1) <= 629) | as.integer(diag_1) == 788 ~ "Genitourinary", 
#                                                    as.integer(diag_1) >= 140 & as.integer(diag_1) <= 239 ~ "Neoplasms",
#                                                    as.integer(diag_1) >= 710 & as.integer(diag_1) <= 739  ~ "Musculoskeletal", 
#                                                    as.integer(diag_1) >= 800 & as.integer(diag_1) <= 999 ~ "Injury",                    
#                                                    TRUE ~ "other"))


hospD$primary_diagnosis <- as.factor(hospD$primary_diagnosis)
table(hospD$primary_diagnosis)

#remove "diag_1"
hospD$diag_1 <- NULL

barplot(table(hospD$age))

#regroup the "age" to [0-40],[40-50],[50-60],[60-70],[70-80],[80-100]
hospD$age <- case_when(hospD$age %in% c("[0-10)","[10-20)","[20-30)","[30-40)") ~ "[0-40]",
                       hospD$age %in% c("[80-90)","[90-100)") ~ "[80-100]",
                       hospD$age %in% "[40-50)" ~ "[40-50]",
                       hospD$age %in% "[50-60)" ~ "[50-60]",
                       hospD$age %in% "[60-70)" ~ "[60-70]",
                       TRUE ~ "[70-80]")
barplot(table(hospD$age), main = "Regroup Age")

hospD$age <- as.factor(hospD$age)

#rename "A1Cresult" to "HbA1c"
colnames(hospD)[17] <- "HbA1c"

#remove some features medications, just keep 7 features
hospD$repaglinide <- NULL
hospD$nateglinide <- NULL
hospD$chlorpropamide <-NULL
hospD$acetohexamide <- NULL
hospD$tolbutamide <- NULL
hospD$acarbose <- NULL
hospD$miglitol <- NULL
hospD$troglitazone <- NULL
hospD$tolazamide <- NULL
hospD$glyburide.metformin <- NULL
hospD$glipizide.metformin <- NULL
hospD$glimepiride.pioglitazone <- NULL
hospD$metformin.rosiglitazone <- NULL
hospD$metformin.pioglitazone <- NULL

dim(hospD)

#categorize "readmitted" to 1 --patient was readmitted within 30 days, 0-- readmission after 30 days and no readmission
hospD$readmitted <- case_when(hospD$readmitted %in% c(">30","NO") ~ "0",
                              TRUE ~ "1")
hospD$readmitted <- as.factor(hospD$readmitted)
levels(hospD$readmitted)

#remove patients who had multiple encounters (remove duplicated rows by a column)
hospD <- hospD[!duplicated(hospD$patient_nbr),]
#remove "patient_nbr"
hospD$patient_nbr <- NULL

dim(hospD)

#archive the converted data first for convenience of later call.
write.csv(hospD, file = "hospD_bef_outlier.csv")

par(mfrow = c(2,4))
boxplot(hospD$time_in_hospital, main = "time_in_hospital")
boxplot(hospD$num_lab_procedures, main = "num_lab_procedures")
boxplot(hospD$num_procedures, main = "num_procedures")
boxplot(hospD$num_medications, main = "num_medications")
boxplot(hospD$number_outpatient, main = "number_outpatient")
boxplot(hospD$number_emergency, main = "number_emergency")
boxplot(hospD$number_inpatient, main = "number_inpatient")
boxplot(hospD$number_diagnoses, main = "number_diagnoses")

hospD$number_emergency <- NULL
hospD$number_inpatient <- NULL
hospD$number_outpatient <- NULL
dim(hospD)

#remove the outliers
outliers_remover <- function(a){
  df <- a
  aa <- c()
  count <- 1
  for(i in 1:ncol(df)){
    if(is.integer(df[,i])){
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
hospD <- outliers_remover(hospD)

pairs.panels(hospD[c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_diagnoses")])

dim(hospD)
table(hospD$readmitted)

# ensure results are repeatable
set.seed(100)
load the library
library(Boruta)

boruta <- Boruta(readmitted ~., data = hospD, doTrace = 2)
plot(boruta, las = 2, cex.axis = 0.5)
plotImpHistory(boruta)
attStats(boruta)
boruta

#Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)


#set random seed
set.seed(100)
train <- createDataPartition(hospD$readmitted, p = 0.8, list = FALSE)
training <- hospD[train, ]
testing <- hospD[-train, ]
#check dependent variable(training set)
table(training$readmitted)

#balane dataset
data_rose <- ROSE(readmitted ~., data = training)$data
table(data_rose$readmitted)

#10-Fold Cross-validation
trCntl <- trainControl(method = "CV",number = 10)

#Logistic regression model with 10-folds cross validation
logitMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                       time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                       max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                     data = data_rose, trControl = trCntl, method = "glm", family = "binomial")

logit_pred_CV <- predict(logitMod_CV, testing)

confusionMatrix(logit_pred_CV, testing$readmitted)

#Decision Tree model with 10-folds cross validation
DTMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                    time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                    max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                  data = data_rose, trControl = trCntl, method = "rpart")

DT_pred_CV <- predict(DTMod_CV, testing)

confusionMatrix(DT_pred_CV, testing$readmitted)

# Random Forest model with 10-folds cross validation
RFMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                 time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                 max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis,    
                 data = data_rose, trControl = trCntl, method = "rf")

RF_pred_CV <- predict(RFMod_CV, testing)

confusionMatrix(RF_pred_CV, testing$readmitted)

#NaiveBayesa model with 10-folds cross validation
NBMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                  time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                 max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                 data = data_rose, trControl = trCntl, method = "nb")

NB_pred_CV <- predict(NBMod_CV, testing)

confusionMatrix(NB_pred_CV, testing$readmitted)

#Model Comparison

model_list <- list(LR = logitMod2_CV, DT = DTMod_CV, RF = RFMod_CV, NB = NBMod_CV)
res <- resamples(model_list)
summary(res)

roc.curve(testing$readmitted, logit_pred2CV, plotit = T, col = "blue")
roc.curve(testing$readmitted, DT_pred_CV, plotit = T, add.roc = T, col = "red")
roc.curve(testing$readmitted, NB_pred_CV, plotit = T, add.roc = T, col = "yellow")
roc.curve(testing$readmitted, RF_pred_CV, plotit = T, add.roc = T, col = "green")

legend(.8, .4, legend = c("LG", "DT", "NB", "RF"),
       col = c("blue", "red", "yellow", "green"),
      lty = c(1,2,3,4), ncol = 1)
bwplot(res)


Variable Importance -- RF!
varImp(RFMod_CV)
ggplot(varImp(RFMod_CV))


plot(varImp(RFMod_CV), col = "red", lwd = 10)

ggplot(varImp(RFMod_CV)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  coord_flip() + 
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")


library(ggplot2)

ggplot(hospD,aes(x=num_procedures,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=1,binwidth=1)+theme_bw()

ggplot(hospD,aes(x=time_in_hospital,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=1,binwidth=1)+theme_bw()

ggplot(hospD,aes(number_diagnoses,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=1,binwidth=1)+theme_bw()

ggplot(hospD,aes(num_lab_procedures,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=1,binwidth=1)+theme_bw()

ggplot(hospD,aes(num_medications,group=readmitted,fill=readmitted))+
  geom_histogram(position="identity",alpha=1,binwidth=1)+theme_bw()


#------------------------------------------------------------------------------------------
set.seed(100)
#dividing the dataset into train and test
#Create the training and test set for hospD with 80:20 proportion, respectively.
#Name the training set hospD_train, and the test set hospD_test.
trainingRowIndex <- sample(1:nrow(hospD), 0.8*nrow(hospD)) # row indices for training data
hospD_train <- hospD[trainingRowIndex, ]  # model training data
hospD_test  <- hospD[-trainingRowIndex, ]   # test data
#check dependent variable(training set)
table(hospD_train$readmitted)
#balane dataset
library(ROSE)
data_balance_under <- ovun.sample(readmitted ~., data = hospD_train, method = "under", N = 8954, seed = 1)$data
table(data_balance_under$readmitted)
###################
logitMod <- glm(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + time_in_hospital +
                  num_lab_procedures + num_procedures + num_medications + number_diagnoses + max_glu_serum + 
                  HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, data = data_balance_under, 
                family=binomial(link="logit"))
summary(logitMod)

logit_pred <- predict(logitMod, newdata = hospD_test)

install.packages("InformationValue")
library(InformationValue)
optCutoff <- optimalCutoff(hospD_test$readmitted, logit_pred)[1]
optCutoff
misClassError(hospD_test$readmitted, logit_pred, threshold = optCutoff)
confusionMatrix(hospD_test$readmitted, logit_pred, threshold = optCutoff)
plotROC(hospD_test$readmitted, logit_pred)
#install.packages("concordance")
library(concordance)
Concordance(hospD_test$readmitted, logit_pred)
sensitivity(hospD_test$readmitted, logit_pred, threshold = optCutoff)
specificity(hospD_test$readmitted, logit_pred, threshold = optCutoff)
#***************************************Method 2************************************************
#loading dataset
hospD <- read.csv("hospDataOld_cleaned.csv")
hospD$X <- NULL
hospD$readmitted <- as.factor(hospD$readmitted)
#checking dimensions of data
glimpse(hospD)
#set random seed
set.seed(100)
library(caret)
train <- createDataPartition(hospD$readmitted, p = 0.8, list = FALSE)
training <- hospD[train, ]
testing <- hospD[-train, ]
#check dependent variable(training set)
table(training$readmitted)
#balane dataset
library(ROSE)
data_bal_under <- ovun.sample(readmitted ~., data = training, method = "under", N = 8950, seed = 1)$data
table(data_bal_under$readmitted)

logitMod2 <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + time_in_hospital 
                   + num_lab_procedures + num_procedures + num_medications + number_diagnoses + max_glu_serum + HbA1c + metformin 
                   + insulin + change + diabetesMed + primary_diagnosis, data = data_bal_under, method = "glm", family = "binomial")
summary(logitMod2)
logitMod2
confusionMatrix(logitMod2)

logit_pred2 <- predict(logitMod2, newdata = testing)

confusionMatrix(logit_pred2, reference=testing$readmitted)

library(concordance)
#?????
concordance(testing$readmitted, logit_pred2)
#plotROC(testing$readmitted, logit_pred2)
#checking accuracy
accuracy.meas(logit_pred2, testing$readmitted)
roc.curve(testing$readmitted, logit_pred2, plotit = F)

####################Logistic regression model with 10-folds cross validation###############################
trCntl <- trainControl(method = "CV",number = 10)
logitMod2_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                        time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                        max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                      data = data_bal_under, trControl = trCntl, method = "glm", family = "binomial")
summary(logitMod2_CV)
logitMod2_CV
confusionMatrix(logitMod2_CV)

logit_pred2CV <- predict(logitMod2_CV, testing)
confusionMatrix(logit_pred2CV, testing$readmitted)
#plotROC(testing$readmitted, logit_pred2CV)
#checking accuracy
accuracy.meas(logit_pred2CV, testing$readmitted)
roc.curve(testing$readmitted, logit_pred2CV, plotit = F)

####################Decision Tree model with 10-folds cross validation##########################################
trCntl <- trainControl(method = "CV",number = 10)
DTMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                    time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                    max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                  data = data_bal_under, trControl = trCntl, method = "rpart")
summary(DTMod_CV)
DTMod_CV
confusionMatrix(DTMod_CV)

DT_pred_CV <- predict(DTMod_CV, testing)
confusionMatrix(DT_pred_CV, testing$readmitted)
#checking accuracy
accuracy.meas(DT_pred_CV, testing$readmitted)
roc.curve(testing$readmitted, DT_pred_CV, plotit = F)

####################Random Forest model with 10-folds cross validation##########################################
trCntl <- trainControl(method = "CV",number = 10)
RFMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                    time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                    max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                  data = data_bal_under, trControl = trCntl, method = "rf")
summary(RFMod_CV)
RFMod_CV
confusionMatrix(RFMod_CV)

RF_pred_CV <- predict(RFMod_CV, testing)
confusionMatrix(RF_pred_CV, testing$readmitted)
#checking accuracy
accuracy.meas(RF_pred_CV, testing$readmitted)
ROSE::roc.curve(testing$readmitted, RF_pred_CV, plotit = F)

###################NaiveBayesa model with 10-folds cross validation############################################
library(e1071)
folds <- createFolds(hospD$readmitted, k = 10)
str(folds)
NBMod_CV <- lapply(folds, function(x){
  set.seed(100)
  train <- hospD[-x,]
  test <- hospD[x,]
  data_bal_ROSE <- ROSE(readmitted ~., data = train, seed = 1)$data
  #data_bal_under <- ovun.sample(readmitted ~., data = train, method = "under", N = 8950, seed = 1)$data
  model <- naiveBayes(readmitted~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                        time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                        max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                      data = data_bal_ROSE)
  pred <- predict(model, test)
  #cm <- confusionMatrix(pred, test$readmitted, positive = "0")
  conf_matrx <- table(pred, test$readmitted)
  # acc <- (conf_matrx[1,1] + conf_matrx[2,2])/(conf_matrx[1,1] + conf_matrx[2,2] + conf_matrx[1,2] + conf_matrx[2,1])
  # return(acc)
  # sens <- sensitivity(conf_matrx)
  # return(sens)
  spec <- specificity(pred, test$readmitted)
  return(spec)
})
NBMod_CV
unlist(NBMod_CV)
mean(unlist(NBMod_CV))

#***************************************Method 2************************************************
# trCntl <- trainControl(method = "CV",number = 10)
# NBMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
#                     time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
#                     max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
#                     data = data_bal_under, trControl = trCntl, method = "nb")
# summary(NBMod_CV)
# NBMod_CV
# confusionMatrix(NBMod_CV)
# 
# NB_pred_CV <- predict(NBMod_CV, testing)
# confusionMatrix(NB_pred_CV, testing$readmitted)
# #checking accuracy
# accuracy.meas(NB_pred_CV, testing$readmitted)
# ROSE::roc.curve(testing$readmitted, NB_pred_CV, plotit = F)
################## Compare Models###################
model_list <- list(LR = logitMod2_CV, DT = DTMod_CV, RF = RFMod_CV, NB = NBMod_CV)
res <- resamples(model_list)
summary(res)
bwplot(res)
#Best Model

########################################################ROSE####################
#loading dataset
hospD <- read.csv("hospDataOld_cleaned.csv")
hospD$X <- NULL
hospD$readmitted <- as.factor(hospD$readmitted)
#checking dimensions of data
glimpse(hospD)
#set random seed
set.seed(100)
library(caret)
train <- createDataPartition(hospD$readmitted, p = 0.8, list = FALSE)
training <- hospD[train, ]
testing <- hospD[-train, ]
#check dependent variable(training set)
table(training$readmitted)
#balane dataset
library(ROSE)
data_bal_ROSE <- ROSE(readmitted ~., data = training, seed = 1)$data
table(data_bal_ROSE$readmitted)
#************************************************************************************************************************************

####################Logistic regression model with 10-folds cross validation###############################
trCntl <- trainControl(method = "CV",number = 10)
logitMod2_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                        time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                        max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                      data = data_bal_ROSE, trControl = trCntl, method = "glm", family = "binomial")
summary(logitMod2_CV)
logitMod2_CV
confusionMatrix(logitMod2_CV)

logit_pred2CV <- predict(logitMod2_CV, testing)
confusionMatrix(logit_pred2CV, testing$readmitted)
#plotROC(testing$readmitted, logit_pred2CV)
#checking accuracy
accuracy.meas(logit_pred2CV, testing$readmitted)
roc.curve(testing$readmitted, logit_pred2CV, plotit = F)

####################Decision Tree model with 10-folds cross validation##########################################
trCntl <- trainControl(method = "CV",number = 10)
DTMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                    time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                    max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                  data = data_bal_ROSE, trControl = trCntl, method = "rpart")
summary(DTMod_CV)
DTMod_CV
confusionMatrix(DTMod_CV)

DT_pred_CV <- predict(DTMod_CV, testing)
confusionMatrix(DT_pred_CV, testing$readmitted)
#checking accuracy
accuracy.meas(DT_pred_CV, testing$readmitted)
roc.curve(testing$readmitted, DT_pred_CV, plotit = F)

####################Random Forest model with 10-folds cross validation##########################################
trCntl <- trainControl(method = "CV",number = 10)
RFMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                    time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                    max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                  data = data_bal_ROSE, trControl = trCntl, method = "rf")
summary(RFMod_CV)
RFMod_CV
confusionMatrix(RFMod_CV)

RF_pred_CV <- predict(RFMod_CV, testing)
confusionMatrix(RF_pred_CV, testing$readmitted)

confusionMatrix(RF_pred_CV, testing$readmitted, positive = "1")

#checking accuracy
accuracy.meas(RF_pred_CV, testing$readmitted)
ROSE::roc.curve(testing$readmitted, RF_pred_CV, plotit = F)
###################NaiveBayesa model with 10-folds cross validation############################################
trCntl <- trainControl(method = "CV",number = 10)
NBMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                    time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                    max_glu_serum + HbA1c + metformin + insulin + change + diabetesMed + primary_diagnosis, 
                  data = data_bal_ROSE, trControl = trCntl, method = "nb")
summary(NBMod_CV)
NBMod_CV
confusionMatrix(NBMod_CV)

NB_pred_CV <- predict(NBMod_CV, testing)
confusionMatrix(NB_pred_CV, testing$readmitted)
confusionMatrix(NB_pred_CV, testing$readmitted, positive = "1")

#checking accuracy
accuracy.meas(NB_pred_CV, testing$readmitted)
ROSE::roc.curve(testing$readmitted, NB_pred_CV, plotit = F)
