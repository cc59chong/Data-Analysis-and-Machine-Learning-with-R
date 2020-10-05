Titanic<-read.csv("Titanic_Passengers_New.csv")
library(dplyr)
glimpse(Titanic)
# or
# str(Titanic)
# or
# dim(Titanic)

library(tidyr)

T1<- separate(Titanic, Name, into = c("last_name", "title_name"), sep = ",")

T2 <- separate(T1, title_name, into = c("Title", "first_name"), sep = ". ", remove = TRUE, convert = FALSE, extra = "merge", fill = "warn")

Titanic <- unite(T2, Name_Last, c("first_name","last_name"), sep = "_")

head(Titanic, 2)


Titanic$Ticket..<- gsub(" ", "",Titanic$Ticket..)
Titanic$Ticket..[11:13]


library(stringr)

Titanic$Cabin <- gsub(" ", "",Titanic$Cabin)

Titanic$Cabin <- str_pad(Titanic$Cabin,  width = 9, side = "left", pad = 0)

head(Titanic)


Titanic <- separate(Titanic, Home...Destination, into = c("Home", "Destination"), sep = "/", remove = TRUE, convert = FALSE, extra = "merge", fill = "warn")
head(Titanic, 5)


barplot(table(Titanic$Passenger.Class))
Titanic$Passenger.Class <- str_replace(Titanic$Passenger.Class, "33", "3")
barplot(table(Titanic$Passenger.Class))

barplot(table(Titanic$Sex))
Titanic$Sex <- str_replace(Titanic$Sex, "femalee", "female")
Titanic$Sex <- str_replace(Titanic$Sex, "mmale", "male")
barplot(table(Titanic$Sex))


Titanic$Sex <- str_replace(Titanic$Sex, "female", "F")
Titanic$Sex <- str_replace(Titanic$Sex, "male", "M")
Titanic$Sex


Titanic$Title <- NULL
Titanic$Body <- NULL
Titanic$Destination <- NULL
Titanic$Midpoint.age <- NULL
head(Titanic, 2)


summary(Titanic)
Titanic <- na.omit(Titanic)
summary(Titanic)


str(Titanic)
hist(Titanic$Age)
boxplot(Titanic$Age, horizontal = TRUE)
boxplot(as.numeric(Titanic$Lifeboat), horizontal = TRUE)
