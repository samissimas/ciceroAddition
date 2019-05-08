#'Signifigant Peak Finder
#'
#'creates a dataframe which mirrors a reduced matrix to help in interpretation
#'
#' @param SUM.data a peak file containing two conn files connection data
#' @param thresh the amount of total connections in at least one conn file which the user determines is signifigant, only peaks above this will be taken 
#' @param percent the percent change in amount of connection between each conn file which the user deems is signifigant
#'
#' @return a dataframe containing the peak information of your signifigant peaks
#'
#' @examples
#' peaks.data = sigPeakFinder(SUM.data, 20, 50)
#'
#' @export
sigPeakFinder <- function(SUM.data, thresh, percent){
  length = dim(SUM.data)[1]
  i=1
  j=1
  while(i <= length)
  {
    if(as.numeric(SUM.data[i,5]) >= as.numeric(thresh) | as.numeric(SUM.data[i,6]) >= as.numeric(thresh))
    {
      if(as.numeric(SUM.data[i,5]) > as.numeric(SUM.data[i,6]))
      {
        drop = (((as.numeric(SUM.data[i,5])-as.numeric(SUM.data[i,6]))/(as.numeric(SUM.data[i,5])))*100)
      }
      else{
        drop = (((as.numeric(SUM.data[i,6])-as.numeric(SUM.data[i,5]))/(as.numeric(SUM.data[i,6])))*100)
      }



      if(drop >= as.numeric(percent) | drop <= (as.numeric(percent)*(-1)))
      {
        if(j==1)
        {
          returnSum.data <- SUM.data[i,]
          j = j + 1
        }
        else
        {
          returnSum.data = rbind(returnSum.data, SUM.data[i,])

        }


      }

    }
    i = i +1;
  }
  return(returnSum.data)
}
