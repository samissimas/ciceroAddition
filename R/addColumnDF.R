###function which adds a numeric column to a data frame
addColumnDF <- function(peaks.data){
  length = dim(peaks.data)[1]
  access <- rep(0, length)
  peaksReturn.data <- data.frame(peaks.data, access)
  return(peaksReturn.data)

}
