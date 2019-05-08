#'Unique Peak Vector
#'
#'takes every unique peak from a conns file and returns a vector of them
#'
#' @param conns a conns file
#'
#' @return a vector containing the peaks
#'
#' @examples
#' vector = unique_peak(conns)
#'
#' @export
unique_peak <- function(conns){

  size <- dim(conns)
  x = conns[1,1]
  x = toString(x)
  uniquePeaks <- c(x)
  i = 2
  while(i <= size[1])
  {
    y = conns[i,1]
    y = toString(y)
    if(y != x)
    {
      uniquePeaks <- c(uniquePeaks, y)
      x = y
    }

    i = i+ 1;
  }
  return(uniquePeaks)
}
