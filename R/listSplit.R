#'Create Peak Dataframe from List
#'
#'given a comma seperated list of peaks, creates a peak dataframe
#'
#' @param cicString the string containing a list of peaks
#'
#' @return a peak dataframe
#'
#' @examples
#' peaks.data = list_string(cicString)
#'
#' @export
list_split <- function(cicString){
  string = cicString
  peakvector = unlist(strsplit(string, split=","))
  length = length(peakvector)
  peaks.data = createDF(length)
  i = 1
  while(i<=length)
  {
    loopVector = cicero_split(peakvector[i])
    peaks.data[i,1] = loopVector[1]
    peaks.data[i,2] = loopVector[2]
    peaks.data[i,3] = loopVector[3]

    i = i +1
  }
  return(peaks.data)
}
