library('dplyr')
library('stringr')
library(ggplot2)
library(tidyr)
rm(list = ls())
setwd('C:/Users/chirag/Desktop/Kaggle/sf-salaries-release-2015-12-21-03-21-32/output')
list.files()

#Need to check how to figure out the na string

x <- read.csv('Salaries.csv',na=c('Not Provided',''),stringsAsFactors = FALSE)

str(x)

#change names to lower case
x$EmployeeName <- str_to_lower(x$EmployeeName)
x$JobTitle <- str_to_lower(x$JobTitle)

#Findout the non numeric columns
non_numeric_columns <- names(x)[!sapply(x, is.numeric)]

#Get the summary of numeric column

x %>% select(-one_of(non_numeric_columns)) %>% summary()

#Get unique values in each numeric columns
x %>% select(-one_of(non_numeric_columns)) %>% summarise_each(funs(length(unique(.))))

#Get na values in each columns
x %>% select(-one_of(non_numeric_columns)) %>% summarise_each(funs(sum(is.na(.))))

#Get unique values in each non numeric columns
x %>% select(one_of(non_numeric_columns)) %>% summarise_each(funs(length(unique(.))))

#removing the Notes and Agency columns as they just have a single value
x <- x %>% select(-Notes,-Agency)

str(x)

#Get na values in each columns
x  %>% summarise_each(funs(sum(is.na(.))))

x %>% filter(!is.na(Status)) %>% group_by(Year) %>% summarise(mean(TotalPay))

unique((x %>% filter(!is.na(Status)))$Year)

#Studying Job titles
length(unique(x$JobTitle))

x %>% 
  group_by(JobTitle) %>% 
  summarise(freq=n()) %>%  
  mutate(OccursOnce = freq==1) %>%
  group_by(OccursOnce) %>% 
  summarise(count=n())

#plot the totalPay for unique as well as duplicated job titles
x %>%
  mutate(OccursOnce=!duplicated(JobTitle)) %>%
  ggplot(aes(x=OccursOnce,y=TotalPay)) + geom_boxplot(aes(fill=OccursOnce))

x %>%
  mutate(OccursOnce=!duplicated(JobTitle)) %>%
  ggplot(aes(x=TotalPay)) + geom_density(aes(fill=OccursOnce),alpha=0.6)

#understanding for what years we have the values of status variable
x %>%
  group_by(Status,Year) %>%
  summarise(Frequency = n()) %>%
  spread(Year,Frequency)


#analysing total pay of FT and PT employees
x %>% 
  filter(!is.na(Status)) %>% 
  ggplot(aes(x=Status,y=TotalPay)) +geom_boxplot(aes(fill=Status))

#analysing other benifits for FT and Pt employee
x %>% 
  filter(!is.na(Status)) %>% 
  ggplot(aes(x=Status)) +geom_boxplot(aes(y=OvertimePay,fill=Status)) +
  geom_boxplot(aes(y=Benefits))

#
x %>% 
  filter(!is.na(Status)) %>% 
  ggplot(aes(x=Status)) +geom_boxplot(aes(y=OtherPay,fill=Status))

x %>%
  group_by(Status,Year) %>%
  summarise(Frequency = n()) %>%
  spread(Year,Frequency)

# get the first word of job title and summarise by it
x %>%
  filter(Status=='FT') %>%
  mutate(first_word = str_extract(JobTitle, "^.+?(?=\\s)")) %>%
  group_by(first_word) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))



