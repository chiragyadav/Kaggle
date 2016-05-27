library(gender)
library(genderdata)
library(stringr)
library(dplyr)
gender(c("Madison", "Hillary"), years = 2000, method = "ssa")$gender

EmpNames <- x$EmployeeName
pattern <- '^(.*)?\\s'
FirstNames <- str_trim(str_extract(EmpNames,pattern))
sum(is.na(FirstNames))

genderdf<-gender(FirstNames,years = c(1980,2010), method = 'ssa',countries = c('United States'))
str(genderdf)
genderdf <- genderdf %>% select(name,gender)

x$name <- FirstNames
str(FirstNames)

gendercol <- sapply(x$EmployeeName, function(x){
  gender(x,years = c(1980,2010), method = 'ssa',countries = c('United States'))$gender
})


length(unique(genderdf$name))
length(unique(FirstNames))

x %>% 
  left_join(genderdf, by = c("name" = "name"))


gendercol <- sapply(x$name, function(empname){
       subset(genderdf,name=empname)$gender
   })

gendercol


