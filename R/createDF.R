#   function which creates a N/A filled data frame of a requested size
createDF <- function(vectorsize){

  chr <- rep(NA, vectorsize)
  start <- rep(NA, vectorsize)
  end <- rep(NA, vectorsize)
  peaks.data <- data.frame(chr, start, end)
  return(peaks.data)
}

