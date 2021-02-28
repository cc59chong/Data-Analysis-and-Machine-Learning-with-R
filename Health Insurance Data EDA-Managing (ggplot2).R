library(ggplot2)
library(scales)
library(hexbin)
library(dplyr)

custdata <- read.table('../input/customer-data/custdata.tsv', head = T, sep = '\t')
summary(custdata)

#Visually checking distributions for a single variable¶
ggplot(custdata) + geom_histogram(aes(x=age), binwidth=5, fill="gray")

ggplot(custdata) + geom_density(aes(x=income)) + scale_x_continuous(labels=dollar) #Set the x-axis labels to dollars.

ggplot(custdata) + geom_density(aes(x=income)) + scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) + annotation_logticks(sides="bt") #Add log-scaled tick marks to the top and bottom of the graph

ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")

ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill="gray") +
  coord_flip() + #Flip the x and y axes: state.of.res is now on the y-axis.
  theme(axis.text.y=element_text(size=rel(0.8)))

statesums <- table(custdata$state.of.res)
statef <- as.data.frame(statesums)
colnames(statef)<-c("state.of.res", "count") #Rename the columns for readability.
summary(statef)

statef <- transform(statef, state.of.res=reorder(state.of.res, count)) 
#Use the reorder() function to set the state.of.res variable to be count ordered. 
#Use the transform() function to apply the transformation to the state.of.res data frame.
summary(statef)

ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
                         stat="identity",
                         fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))


#Visually checking relationships between two variables
custdata2 <- subset(custdata,(custdata$age > 0 & custdata$age < 100 & custdata$income > 0))
cor(custdata2$age, custdata2$income)
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + ylim(0, 200000)
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + stat_smooth(method="lm") + ylim(0, 200000)
ggplot(custdata2, aes(x=age, y=income)) + geom_point() + geom_smooth() + ylim(0, 200000)

ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) + geom_point(position=position_jitter(w=0.05, h=0.05)) + geom_smooth()

ggplot(custdata2, aes(x=age, y=income)) + geom_hex(binwidth=c(5, 10000)) + geom_smooth(color="white", se=F) + ylim(0,200000)

#Stacked bar chart, the default
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins))


#Side-by-side bar chart
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins), position="dodge")

#Filled bar chart
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins), position="fill")


#Plotting data with a rug
ggplot(custdata, aes(x=marital.stat)) +
  geom_bar(aes(fill=health.ins), position="fill") +
  geom_point(aes(y=-0.05), size=0.75, alpha=0.3,
             position=position_jitter(h=0.01))

#Distribution of marital status by housing type: side-by-side bar chart

ggplot(custdata2) +
  geom_bar(aes(x=housing.type, fill=marital.stat ),
           position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Distribution of marital status by housing type: faceted side-by-side bar chart
ggplot(custdata2) +
  geom_bar(aes(x=marital.stat), position="dodge",
           fill="darkgray") +
  facet_wrap(~housing.type, scales="free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Cleaning data¶
summary(custdata[is.na(custdata$housing.type), c("recent.move","num.vehicles")])
summary(custdata$is.employed)
#Remapping NA to a level
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed), "missing", ifelse(custdata$is.employed==T, "employed", "not employed"))

summary(as.factor(custdata$is.employed.fix))

#Data Transformations
custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)

#Converting age into ranges
brks <- c(0, 25, 65, Inf)
custdata$age.range <- cut(custdata$age, breaks=brks, include.lowest=T)
summary(custdata$age.range)

# Centering on mean age
summary(custdata$age)
meanage <- mean(custdata$age)
custdata$age.normalized <- custdata$age/meanage
summary(custdata$age.normalized)

# Summarizing age
summary(custdata$age)
meanage <- mean(custdata$age)
stdage <- sd(custdata$age)
meanage
stdage

custdata$age.normalized <- (custdata$age-meanage)/stdage
summary(custdata$age.normalized)

# how to calculate signed log base 10, in R:
signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

custdata$gp <- runif(dim(custdata)[1]) # dim(custdata) returns the number of rows and columns of the data frame as a vector, 
# so dim(custdata)[1] returns the number of rows.
testSet <- subset(custdata, custdata$gp <= 0.1) # generate a test set of about 10% of the data (93 customers-a little over 9%, actually) 

trainingSet <- subset(custdata, custdata$gp > 0.1) # and train on the remaining 90%.

dim(testSet)[1]

dim(trainingSet)[1]

