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
buildDF_bed <- function(uniquePeaks){
  vectorsize <- length(uniquePeaks)
  i = 1
  chr <- rep(NA, vectorsize)
  start <- rep(NA, vectorsize)
  end <- rep(NA, vectorsize)
  peaks.data <- data.frame(chr, start, end)
  unVector <- numeric()

  while(i <= vectorsize)
  {
    loopvector <- cicero_split_space(uniquePeaks[i])

    cicString = loopvector[1]
    chromnum <- regexpr("r",cicString)
    number <- substring(cicString, chromnum+1)

    finder = "_"
    if(grepl(finder, number))
    {

      cutnum <- regexpr("_",number)
      number <- substr(number, 1, cutnum-1)

    }
    if(number=="X")
    {
      number = "18"
    }
    if(number=="Y")
    {
      number = "19"
    }


    if(number=="Un")
    {
      unVector <- c(unVector,i)
      peaks.data[i,1] = number
      peaks.data[i,2] = loopvector[2]
      peaks.data[i,3] = loopvector[3]
      i = i + 1
    }
    else
    {
      peaks.data[i,1] = as.numeric(number)
      peaks.data[i,2] = loopvector[2]
      peaks.data[i,3] = loopvector[3]
      i = i + 1
    }


  }

  peaks.data = peaks.data[-c(unVector),]
  peaks.data$chr <- as.numeric(peaks.data$chr)
  peaks.data = peaks.data[order(peaks.data$chr),]

  return(peaks.data)

}
