###function which combines two peak files
sumPeaks <- function(WTpeaks.data, KOpeaks.data, vectorsizeKO){
  SUMpeaks.data = WTpeaks.data

  length = dim(SUMpeaks.data)[1]
  location <- rep("W", length)
  SUMpeaks.data <- data.frame(SUMpeaks.data, location)
  SUMpeaks.data <- data.frame(lapply(SUMpeaks.data, as.character), stringsAsFactors=FALSE)

  length2 = dim(KOpeaks.data)[1]
  location <- rep("K", length2)
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
