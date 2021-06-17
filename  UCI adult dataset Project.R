library(dplyr)
library(Amelia)
library(tidyr)
library(ggplot2)
library(caTools)
require(gdata)


#read CSV file 
adult <- read.csv('adult_sal.csv')
head(adult)

#drop the repteated index col  
adult <- select(adult,-X)
head(adult)
str(adult)
summary(adult)

#Data Cleaning
colSums(is.na(adult))
sum(is.na(adult$type_employer))
table(adult$type_employer)
is.na(adult$type_employer)
missmap(adult, main="Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
min(adult$type_employer)

#combine cols into unemployed
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)
table(adult$type_employer)

# combine col into Sl_gov
sl_e <- function(emp){
  if (emp == 'State-gov' | emp == 'Local-gov'){
    return('SL-gov')
  }else{
    return(emp) 
  }
}        

adult$type_employer <- sapply(adult$type_employer,sl_e)
table(adult$type_employer)

#combine cols into self-emp 
self_emp <- function(self){
  if (self == 'Self-emp-inc' | self == 'Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(self)
  }
}

adult$type_employer <- sapply(adult$type_employer,self_emp)
table(adult$type_employer)

# Reduce Marital col to three groups
table(adult$marital)

mar_stute <- function(mar){
  if (mar == 'Married-civ-spouse' | mar ==  'Married-spouse-absent' | mar ==  'Married-AF-spouse')
    return ('Married') else
      if (mar == 'Widowed'| mar == 'Separated'| mar == 'Divorced')
        return('Not-Married') else
          if (mar == 'Never-married')
            return('Never-married')
}

adult$marital <- sapply(adult$marital, mar_stute)
table(adult$marital)

# grouping countries baesd on continents
table(adult$country)
Asia <- c('Cambodia', 'China', 'Hong', 'Japan', 'Philippines', 'India', 'Vietnam', 'Taiwan', 'Thailand', 'Iran', 'Laos')
Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
North.America <- c('Canada','United-States','Puerto-Rico' )
Other <- c('South')
new.country <- function(ctr){
  if(ctr %in% Asia)
    return ('Asia')else
      if (ctr %in% Europe)
        return ('Europe')else
          if (ctr %in% Latin.and.South.America)
            return('Latin.and.South.America')else
              if (ctr %in% North.America)
                return('North.America')else
                  return('Other')
  
}

adult$country <- sapply(adult$country,new.country)
table(adult$country)

#factor columns
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

#chang na Value into NA
table(adult$type_employer)
table(adult$occupation)
which(is.na(adult))
adult[adult == '?'] <- NA

#check tables
table(adult$type_employer)
table(adult$country)
table(adult$marital)

#dealing with missing data
missmap(adult, main="Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
adult <- na.omit(adult)
adult$occupation <- factor(adult$occupation)
table(adult$occupation)
adult$type_employer <- factor(adult$type_employer)
table(adult$type_employer)
missmap(adult, main="Missings Map", 
        col=c("white", "blue"), legend=FALSE)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('white','blue'))

#3EDA
str(adult)

#reorder income variable levels
adult$income <- with(adult, factor(income, levels(income)[2:1]))
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

# plot hist on hour per week
ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()  + scale_x_continuous(breaks = seq(10, 110, by=10))

#rename country col to region and barplot it
names(adult)[names(adult) == 'country'] <- 'region'
str(adult)
ggplot(adult, aes(region, fill = income)) + geom_bar()

#split data into train and test data
head(adult)
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.70) 
head(adult)
train = subset(adult, sample == TRUE)
test = subset(adult, sample == FALSE)

#explore glm()
help(glm)
model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)

#step function on model
help(step)
new.model <- step(model)
summary(new.model)

#onfusion matrix using the predict function
test$predicted.income <- predict(model,newdata=test,type='response')
table(test$income, test$predicted.income  > 0.5)

# accuracy of model
(6372+1423)/(6372+1423+548+872)

#other measure of perfromance
#recall
6732/(6372+548)
#precision
6732/(6372+872)
