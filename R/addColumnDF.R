#'Dataframe Numeric Column Adder
#'
#'function which adds a numeric column to a data frame
#'
#' @param peaks.data the dataframe which you wish to add a column too
#'
#' @return the same dataframe with an added numeric column
#'
#' @examples
#' newdataframe.data = addcolumnDF(peak.data)
#'
#' @export
addColumnDF <- function(peaks.data){
  length = dim(peaks.data)[1]
  access <- rep(0, length)
  peaksReturn.data <- data.frame(peaks.data, access)
  return(peaksReturn.data)

}
