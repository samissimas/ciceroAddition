#'Dataframe Charcter Column Adder
#'
#'function which adds a character column to a data frame
#'
#' @param peaks.data the dataframe which you wish to add a column too
#'
#' @return the same dataframe with an added character column
#'
#' @examples
#' newdataframe.data = addcolumnDFCharacter(peak.data)
#'
#' @export
addColumnDF_character <- function(peaks.data){
  lengthVec = dim(peaks.data)[1]
  connections <- rep("empty",lengthVec)
  peaksReturn.data <- data.frame(peaks.data, connections)
  peaksReturn.data$connections <- as.character(peaksReturn.data$connections)
  return(peaksReturn.data)

}
