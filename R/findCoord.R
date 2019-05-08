#'Finds Place of Peak Along PeakVetor
#'
#'given a starting location and a list of peaks, finds where a given connString would fit (i.e. a coordinate)
#'
#' @param connString the conn format peak you are looking to locate
#' @param peaksVector the numerically sorted list of peaks you're searching through
#' @param startSearch the number guess of where the peak may be near too
#'
#' @return a number corresponding to it's location
#'
#' @examples
#' location = findCoord(connString, peaksVector, startSearch)
#'
#' @export
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
