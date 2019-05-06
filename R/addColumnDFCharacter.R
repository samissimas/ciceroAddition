###function which adds a character column to a data frame
addColumnDF_character <- function(peaks.data){
  lengthVec = dim(peaks.data)[1]
  connections <- rep("empty",lengthVec)
  peaksReturn.data <- data.frame(peaks.data, connections)
  peaksReturn.data$connections <- as.character(peaksReturn.data$connections)
  return(peaksReturn.data)

}
