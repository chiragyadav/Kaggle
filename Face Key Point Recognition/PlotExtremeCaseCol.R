PlotExtremeCaseCol <- function(col_name) {
  RowMaxIndex <- which.max(train[,col_name])
  im <- as.matrix(rev(train[RowMaxIndex,]),96,96)
  image(1:96,1:96,im, col=gray((0:255)/255))
}