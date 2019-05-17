#'Returns a Location Based on A Given Coordinate
#'
#'returns the position along a unique peak vector whcih coresponds to a given location along a chromosone
#'
#' @param uniquepeaks a vector containing all uniweu peaks in a conns file
#' @param location the coordiante you are conserned with
#'
#' @return a integer coordinate
#'
#' @examples
#' locater = locaterBelow(uniquepeaks, location)
#'
#' @export
locaterBelow <- function(uniquepeaks, location){
  
  UP.data = buildDF_matrix(uniquepeaks)
  startCount = 0
  peakcutter = 1
  while(peakcutter <= length(uniquepeaks))
  {
    if(as.numeric(UP.data[peakcutter,2])<=as.numeric(location))
    {
      if(as.numeric(UP.data[peakcutter,3])>=as.numeric(location))
      {
        startCount = peakcutter
        peakcutter = length(uniquepeaks) + 1
      }
      else if(as.numeric(UP.data[peakcutter,3])<as.numeric(location))
      {
        peakcutter = peakcutter + 1
      }
    }
    else if(as.numeric(UP.data[peakcutter,2]) >= as.numeric(location))
    {
        startCount = peakcutter - 1
        peakcutter = length(uniquepeaks) + 1
      
    }
  }
  return(startCount)
}