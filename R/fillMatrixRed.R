#'Fill A Matrix With Conns
#'
#'Fills a graphable matrix based on a given peakvector
#'
#' @param KO_conns a conns file
#' @param emptymatrix an empty matrix of the same dimension as the peaks vector
#' @param peaksVector a vector of the peak bins that the conns will be sorted into
#' @param thresh the threshold over which a connection will be accepted
#'
#' @return a completed matrix filled with connsInt
#'
#' @examples
#' matrix = fillMatrixRed(KO_conns, emptymatrix, peaksVector, thresh)
#'
#' @export
fillMatrixRed <- function(KO_conns, emptymatrix, peaksVector, thresh){
  lengthMat = dim(KO_conns)[1]
  i= 1
  startSearch = 1
  print(peaksVector)
  while(i <= lengthMat)
  {
    if(!is.na(KO_conns[i,3]))
    {
      if(as.numeric(KO_conns[i,3]) > as.numeric(thresh))
      {
        x1 = findCoord(KO_conns[i,1], peaksVector, startSearch)
        if(startSearch>1000)
        {
          y2 = findCoord(KO_conns[i,2], peaksVector, startSearch-100)
        }
        else if(startSearch<=1000)
        {
          y2 = findCoord(KO_conns[i,2], peaksVector, 1)
        }
        startSearch = x1


        emptymatrix[x1, y2] = emptymatrix[x1, y2] + 1

      }

    }
    i = i + 1



  }

  return(emptymatrix)

}
