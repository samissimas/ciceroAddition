#'Peak Bed Dataframe Builder
#'
#'fills an empty data frame with only peak data
#'
#' @param uniquePeaks a vector containing all uniquepeaks
#'
#' @return a new dataframe filled with given peak data
#'
#' @examples
#' peak.data = buildDF_bed(uniquePeaks)
#'
#' @export
buildDF_matrix <- function(uniquePeaks){
  vectorsize <- length(uniquePeaks)
  i = 1
  chr <- rep(NA, vectorsize)
  start <- rep(NA, vectorsize)
  end <- rep(NA, vectorsize)
  peaks.data <- data.frame(chr, start, end)
  while(i <= vectorsize)
  {
    loopvector <- cicero_split(uniquePeaks[i])
    peaks.data[i,1] = loopvector[1]
    peaks.data[i,2] = loopvector[2]
    peaks.data[i,3] = loopvector[3]
    i = i + 1
  }
  return(peaks.data)
  
}
  
