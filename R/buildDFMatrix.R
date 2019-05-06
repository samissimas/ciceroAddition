###function which fills an empty data frame with the given peak data
buildDFmatrix <- function(uniquePeaks){

  vectorsize <- length(uniquePeaks)

  chr <- rep(NA, vectorsize)
  start <- rep(NA, vectorsize)
  end <- rep(NA, vectorsize)
  peaks.data <- data.frame(chr, start, end)


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
