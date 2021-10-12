
### IMPORT LIBRARIES ###

library(tidyverse)
library(forcats)
library(stringr)
library(caTools)
library(DT)

library(data.table)
library(pander)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(corrplot)
library(VIM) 
library(knitr)
library(vcd)
library(caret)

library(xgboost)
library(MLmetrics)
library('randomForest') 
library('rpart')
library('rpart.plot')
library('car')
library('e1071')
library(vcd)
library(ROCR)
library(pROC)
library(VIM)
library(glmnet) 

### IMPORT DATA ###

gendersubmission <- read.csv("data/gender_submission.csv")
train <- read.csv("data/train.csv", na.strings="")
test <- read.csv("data/test.csv", na.strings="")

### EXPLORATORY DATA ANALYSIS ###

train$set <- "train"
test$set  <- "test"
test$Survived <- NA
full <- rbind(train, test)

# CHECK DATA #

str(full)
dim(full)
lapply(full, function(x) length(unique(x)))
missing_values <- full %>% summarize_all(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()


checkColumn = function(df,colname){
  testData = df[[colname]]
  numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)
  if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime' | class(testData) == 'integer'){
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = sum(is.infinite(testData)), 'avgVal' = mean(testData,na.rm=TRUE), 'minVal' = round(min(testData,na.rm = TRUE)), 'maxVal' = round(max(testData,na.rm = TRUE)))
  } else{
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = NA,  'avgVal' = NA, 'minVal' = NA, 'maxVal' = NA)
  }
}
checkAllCols = function(df){
  resDF = data.frame()
  for (colName in names(df)){
    resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
  }
  resDF
}
data.table(checkAllCols(full), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


miss_pct <- map_dbl(full, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
miss_pct <- miss_pct[miss_pct > 0]
data.frame(miss=miss_pct, var=names(miss_pct), row.names=NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss)) + 
  geom_bar(stat='identity', fill='red') +
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# DATA MANIPULATION #

full <- full %>%
  mutate(
    Age = ifelse(is.na(Age), mean(full$Age, na.rm=TRUE), Age),
    `Age Group` = case_when(Age < 13 ~ "Age.0012", 
                            Age >= 13 & Age < 18 ~ "Age.1317",
                            Age >= 18 & Age < 60 ~ "Age.1859",
                            Age >= 60 ~ "Age.60Ov"))

full$Embarked <- replace(full$Embarked, which(is.na(full$Embarked)), 'S')

names <- full$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)
full$title <- title
table(title)

full$title[full$title == 'Mlle']        <- 'Miss' 
full$title[full$title == 'Ms']          <- 'Miss'
full$title[full$title == 'Mme']         <- 'Mrs' 
full$title[full$title == 'Lady']          <- 'Miss'
full$title[full$title == 'Dona']          <- 'Miss'

full$title[full$title == 'Capt']        <- 'Officer' 
full$title[full$title == 'Col']        <- 'Officer' 
full$title[full$title == 'Major']   <- 'Officer'
full$title[full$title == 'Dr']   <- 'Officer'
full$title[full$title == 'Rev']   <- 'Officer'
full$title[full$title == 'Don']   <- 'Officer'
full$title[full$title == 'Sir']   <- 'Officer'
full$title[full$title == 'the Countess']   <- 'Officer'
full$title[full$title == 'Jonkheer']   <- 'Officer'  