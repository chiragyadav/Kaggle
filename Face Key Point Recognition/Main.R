rm(list = ls())
setwd('C:/Users/chirag/Desktop/Kaggle/Face Key Point Recognition')
train <- read.csv('Data/training.csv', stringsAsFactors = FALSE)
test <- read.csv('Data/test.csv', stringsAsFactors = FALSE)
str(train)
img.train =  train$Image
img.test = test$Image
train$Image = NULL
test$image = NULL


library(doSNOW)
library(foreach)
c1 <- makeCluster(2)
registerDoSNOW(c1)


img.train=foreach(image=img.train, .combine=rbind) %dopar% {
    as.integer(unlist(strsplit(image," ")))
}

img.test=foreach(image=img.test, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(image," ")))
}

stopCluster(c1)

?rev

for(i in 1:1){
  im <- matrix(data=rev(img.train[i,]), nrow=96, ncol=96)
  image(1:96,1:96,im, col=gray((0:255)/255))
}


c1 <- makeCluster(2)
registerDoSNOW(c1)


for(i in 1:nrow(train)) {
  points(96- train$nose_tip_x[i],96-train$nose_tip_y[i], col='red')
}

#Plotting the xtreme case for nose coordinates to check is there is spme mislabelling

idx_maxnose= which.max(train$nose_tip_x)

im <- matrix(data=rev(img.train[idx_maxnose,]), nrow=96, ncol=96)
image(1:96,1:96,im, col=gray((0:255)/255))
points(96- train$nose_tip_x[idx_maxnose],96-train$nose_tip_y[idx_maxnose], col='red')

source('PlotExtremeCaseCol.R')

PlotExtremeCaseCol('nose_tip_y')


