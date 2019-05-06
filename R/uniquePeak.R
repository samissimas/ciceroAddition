#   function which simplifies the connections matrix into a
#   matrix only containing the unique peaks

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
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
