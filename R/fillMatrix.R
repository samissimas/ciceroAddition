###function which fills a matrix with the co-accessibilty values
fillMatrix <- function(KO_conns, emptymatrix, peaksVector, thresh){
  lengthMat = dim(KO_conns)[1]
  i= 1
  startSearch = 1
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


        emptymatrix[x1, y2] = as.numeric(KO_conns[i,3])

      }

    }
    i = i + 1



  }

  return(emptymatrix)

}
