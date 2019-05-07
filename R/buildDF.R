#'Peak Dataframe Builder
#'
#'function which fills empty data frame with given peak data
#'
#' @param uniquePeaks the vector containing uniquepeaks from a Conn file
#' @param peaks.data the dataframe which you wish to add a column too
#'
#' @return the same dataframe now filled with peak data
#'
#' @examples
#' peak.data = buildDF(uniquePeaks, peak.data)
#'
#' @export
buildDF <- function(uniquePeaks, peaks.data){
  vectorsize <- length(uniquePeaks)
  i = 1
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
