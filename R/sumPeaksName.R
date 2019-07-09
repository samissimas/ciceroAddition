#'Peak DataFrame Combination
#'
#'creates a dataframe which adds two peak dataframe by combining like and independant peaks and marks each as such
#'
#' @param WTpeaks.data a peak file containing the peaks of one conn file
#' @param KOpeaks.data a peak file containing the peaks of one conn file 
#' @param firstString the character you want attached with the first conns file
#' @param secondString the character you want attached with the second conns file
#'
#' @return a dataframe containing the peak information of your signifigant peaks
#'
#' @examples
#' peaks.data = sigPeakFinder(SUM.data, 20, 50)
#'
#' @export
sumPeaksName <- function(WTpeaks.data, KOpeaks.data, firstString, secondString){
  SUMpeaks.data = WTpeaks.data
  
  

  length = dim(SUMpeaks.data)[1]
  location <- rep(secondString, length)
  SUMpeaks.data <- data.frame(SUMpeaks.data, location)
  SUMpeaks.data <- data.frame(lapply(SUMpeaks.data, as.character), stringsAsFactors=FALSE)

  length2 = dim(KOpeaks.data)[1]
  location <- rep(firstString, length2)
  KOpeaks.data <- data.frame(KOpeaks.data, location)
  KOpeaks.data <- data.frame(lapply(KOpeaks.data, as.character), stringsAsFactors=FALSE)


  vectorsizeWT <- length(SUMpeaks.data)
  l=1
  while(l <= vectorsizeWT)
  {
    SUMpeaks.data[l,2] = as.numeric(SUMpeaks.data[l,2])
    SUMpeaks.data[l,3] = as.numeric(SUMpeaks.data[l,3])
    l = l + 1
  }

  i=1
  j=1
  vectorsizeKO <- dim(KOpeaks.data)[1]
  while(i <= vectorsizeKO)
  {
    row = KOpeaks.data[i,]
    row[1,2] = as.numeric(row[1,2])
    row[1,3] = as.numeric(row[1,3])

    if(as.numeric(row[1,2]) >= as.numeric(SUMpeaks.data[j,2]))
    {
      if(as.numeric(row[1,2]) < as.numeric(SUMpeaks.data[j,3]))
      {
        if(as.numeric(row[1,3]) > as.numeric(SUMpeaks.data[j,3]))
        {
          SUMpeaks.data[j,3] = row[1,3]
          SUMpeaks.data[j,4] = "B"
          i = i + 1
        }
        else if(as.numeric(row[1,3]) <= as.numeric(SUMpeaks.data[j,3]))
        {
          SUMpeaks.data[j,4] = "B"
          i = i + 1
        }
      }
      else if(as.numeric(row[1,2]) > as.numeric(SUMpeaks.data[j,3]))
      {
        if(j == dim(SUMpeaks.data)[1])
        {
          
          SUMpeaks.data = rbind(SUMpeaks.data, row)

          i = i+1
        }
        else if(j < dim(SUMpeaks.data)[1])
        {
          j = j+1
        }
      }
    }
    else if(as.numeric(row[1,2]) < as.numeric(SUMpeaks.data[j,2]))
    {

      if(as.numeric(row[1,3]) < as.numeric(SUMpeaks.data[j,2]))
      {
        if(j == 1)
        {
         
          SUMpeaks.data = rbind(row, SUMpeaks.data)

          i = i+1
        }
        else if(j > 1)
        {
          
          SUMpeaks.data = rbind(SUMpeaks.data[1:(j-1),], row, SUMpeaks.data[-(1:(j-1)),])

          i = i + 1
        }
      }
      else if(as.numeric(row[1,3]) >= as.numeric(SUMpeaks.data[j,2]))
      {
        if(as.numeric(row[1,3]) < as.numeric(SUMpeaks.data[j,3]))
        {
          SUMpeaks.data[j,2] = row[1,2]
          SUMpeaks.data[j,4] = "B"
          i = i +1
        }
        else if(as.numeric(row[1,3]) >= as.numeric(SUMpeaks.data[j,3]))
        {
          SUMpeaks.data[j,2] = row[1,2]
          SUMpeaks.data[j,3] = row[1,3]
          SUMpeaks.data[j,4] = "B"
          i = i + 1
        }
      }
    }
    else
    {
      print("bad")
      i = i + 1
    }
  }

  return(SUMpeaks.data)
}
