
# Write a function (name it outlier_checker) that will count the number of values beyond ±2 standard deviation of the mean
# for each numeric variable in a .csv data file. Note that if a variable has a data type other than numeric type, 
# function should not perform any operation for that variable. 
# (Note: Statistically,  values beyond ±3 standard deviation of the mean would be considered as outliers, 
#   but for practice purposes we will use ±2 as a benchmark)

#-------------------------------------------------------------------------------------------------------------------------
# The Empirical Rule

outlier_checker <- function(a){
  df <- read.csv(a)
  count <- 0
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])){
      for(j in 1:nrow(df)){
        if(df[j,i] > mean(df[,i], na.rm = TRUE) + 2*sd(df[,i], na.rm = TRUE) | df[j,i] < mean(df[,i], na.rm = TRUE) - 2*sd(df[,i], na.rm = TRUE)){
          count <- count + 1
        }
      }
      print(count)
    } else {
      
      print("Character")
    }
    
    count<-0
  }
}
#-------------------------------------------------------------------------------------------\

outlier_checker <- function(a){
  df <- read.csv(a)
  
  for(i in 1:ncol(df)){
    count <- 0
    if(is.numeric(df[,i])){
      upper <- (mean(df[,i], na.rm = TRUE) + 2*sd(df[,i], na.rm = TRUE))
      lower <- (mean(df[,i], na.rm = TRUE) - 2*sd(df[,i], na.rm = TRUE))
      for(j in 1:nrow(df)){
        if(df[j,i] > upper | df[j,i] < lower){
          count <- count + 1
        }
      }
      print(count)
    }
  }
}      


#Import promotion.csv data set and check its structure. Convert Oral, Written and Combine variables into numeric type, if needed. 
df <- read.csv("Promotion.csv")
str(df)
#'data.frame':	118 obs. of  5 variables:

#we can see Oral, Written and Combine variables are numeric type. (Note: integer is belong to numeric) So, we need not convert the type. 


#Check how many values are beyond ±2 standard deviation of the mean in the promotion.csv file using outlier_checker function. 
outlier_checker("Promotion.csv")

# output:
# [1] "Character"
# [1] "Character"
# [1] 1
# [1] 7
# [1] 4


# Write a function (name it outlier_remover) that will first detect the values beyond ±2 standard deviation of the mean
# and then delete the rows including these values in each numeric variable. 

outlier_remover <- function(a){
  df <- read.csv(a)
  aa<-c()
  count<-1
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])){
      for(j in 1:nrow(df)){
        if(df[j,i] > mean(df[,i], na.rm = TRUE) + 2*sd(df[,i], na.rm = TRUE) | df[j,i] < mean(df[,i], na.rm = TRUE) - 2*sd(df[,i], na.rm = TRUE)){
          
          aa[count]<-j
          count<-count+1                  
        }
      }
    }
  }
  
  df<-df[-aa,]
}
#-----------------------------------------------------------------------------------------------------------
outlier_remover <- function(a){
  df <- read.csv(a)
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])){
      upper <- (mean(df[,i], na.rm = TRUE) + 2*sd(df[,i], na.rm = TRUE))
      lower <- (mean(df[,i], na.rm = TRUE) - 2*sd(df[,i], na.rm = TRUE))
      for(j in 1:nrow(df)){
        if(df[j,i] > upper | df[j,i] < lower){
           df[j,i] <- NA
        }
      }
    }
  }
  print(na.omit(df))
}

#Use promotion.csv file to test your function. After removing the outliers, how many observations do you have in your data set? 
df_new <- outlier_remover("Promotion.csv") 
str(df_new)
# 'data.frame':	109 obs. of  5 variables: