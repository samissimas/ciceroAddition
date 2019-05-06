###function which finds all changes in access over a threshold, and over a certain percent
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
