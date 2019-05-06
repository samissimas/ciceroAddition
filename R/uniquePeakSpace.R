#   function which simplifies the connections matrix into a
#   matrix only containing the unique peaks, using spaces
unique_peak_space <- function(conns){

  size <- length(conns)
  x = conns[1]
  x = toString(x)
  uniquePeaks <- c(x)
  i = 2
  while(i <= size)
  {
    y = conns[i]
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
