###function which finds coordinates along a peak vector
findCoord <- function(connsString,peaksVector, startSearch){
  j=startSearch
  found = FALSE
  while(found == FALSE)
  {
    vectorsplit <- cicero_split(connsString)
    vectorsplit[2] = as.numeric(vectorsplit[2])
    vectorsplit[3] = as.numeric(vectorsplit[3])

    peakvectorsplit <- cicero_split(peaksVector[j])

    if(as.numeric(vectorsplit[2]) <= as.numeric(peakvectorsplit[3]))
    {
      if(as.numeric(vectorsplit[3]) >= as.numeric(peakvectorsplit[2]))
      {

        return(j)
        found = TRUE

      }
      else if(as.numeric(vectorsplit[3]) < as.numeric(peakvectorsplit[2]))
      {
        j = j - 1

      }
    }
    else
    {
      j = j + 1

    }

  }

}
