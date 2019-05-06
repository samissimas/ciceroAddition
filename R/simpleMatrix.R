###Creates a matrix which reduces dimension by a given factor and cuts around certain give values
simpleMatrix <- function(KO_conns, uniquepeaks, dim, thresh, startNum, endNum){

  if(endNum<=startNum)
  {
    print("End must be greater than start")
    break
  }
  UP.data = buildDFmatrix(uniquepeaks)

  startCount = 0
  peakcutter = 1
  while(peakcutter <= length(uniquepeaks))
  {
    if(as.numeric(UP.data[peakcutter,2])<=as.numeric(startNum))
    {
      if(as.numeric(UP.data[peakcutter,3])>=as.numeric(startNum))
      {
        startCount = peakcutter - 1
        peakcutter = length(uniquepeaks) + 1
      }
      else if(as.numeric(UP.data[peakcutter,3])<as.numeric(startNum))
      {
        peakcutter = peakcutter + 1
      }
    }
    else if(as.numeric(UP.data[peakcutter,2]) >= as.numeric(startNum))
    {
      if(peakcutter ==1)
      {
        peakcutter = length(uniquepeaks)  + 1
      }
      else if(peakcutter != 1)
      {
        startCount = peakcutter
        peakcutter = length(uniquepeaks) + 1
      }
    }

  }

  endCount = 0
  peakcutter = 1
  endFound = FALSE



  while(peakcutter <= length(uniquepeaks))
  {
    if(as.numeric(UP.data[peakcutter,2])<=as.numeric(endNum))
    {
      if(as.numeric(UP.data[peakcutter,3])>=as.numeric(endNum))
      {
        endCount = peakcutter+1
        peakcutter = length(uniquepeaks) + 1
      }
      else if(as.numeric(UP.data[peakcutter,3])<as.numeric(endNum))
      {
        if(as.numeric(UP.data[length(uniquepeaks), 3]) <= endNum)
        {
          peakcutter = length(uniquepeaks)+1
          endCount = length(uniquepeaks)+1
          endFound = TRUE

        }
        peakcutter = peakcutter + 1
      }
    }
    else if(as.numeric(UP.data[peakcutter,2]) >= as.numeric(endNum))
    {
      if(peakcutter == 1)
      {
        peakcutter = length(uniquepeaks) + 1
      }
      else if(peakcutter != 1)
      {
        endCount = peakcutter - 1
        peakcutter = length(uniquepeaks) + 1
      }
    }

  }
  if(endCount==0)
  {
    print("Region end number is too small")
    break
  }

  if(dim > (endCount-startCount))
  {
    print("Dimension Too Large: Must Be Half Or Less Of Region Length")
    break
  }
  if(startCount == 0 & endCount != length(uniquepeaks) + 1)
  {
    vector2 <- UP.data[1,]
    vector2[1,2] = UP.data[endCount,2]
    vector2[1,3] = UP.data[length(uniquepeaks), 3]

    UP2.data <- UP.data[-c(endCount:dim(UP.data)[1]),]

    UP3.data = rbind(UP2.data, vector2)
    print("Cutting Complete")
  }
  else if(startCount != 0 & endCount == length(uniquepeaks) + 1)
  {
    vector <- UP.data[1,]
    vector[1,3] = UP.data[startCount,3]

    UP2.data <- UP.data[-c(1:startCount),]

    UP3.data = rbind(vector,UP2.data)
    print("Cutting Complete")
  }
  else if(startCount != 0 & endCount != length(uniquepeaks) + 1){
    vector <- UP.data[1,]
    vector[1,3] = UP.data[startCount,3]


    vector2 <- UP.data[1,]
    vector2[1,2] = UP.data[endCount,2]
    vector2[1,3] = UP.data[length(uniquepeaks), 3]

    UP2.data <- UP.data[-c(1:startCount, endCount:dim(UP.data)[1]),]

    UP3.data = rbind(vector,UP2.data, vector2)
    print("Cutting Complete")

  }
  else if(startCount == 0 & endCount == length(uniquepeaks) + 1)
  {
    UP3.data = UP.data
    print("Cutting Complete")
  }


  uniquepeaks = sumtoPeaks(UP3.data)


  fullLength = length(uniquepeaks)
  runSize = floor((fullLength/dim))
  print(runSize)
  if(runSize<2)
  {
    print("Dimension Too Large: Must Be Half Or Less Of Unique Peak Length")
    break

  }
  else if(runSize >= 2)
  {
    if(fullLength%%dim == 0)
    {
      mm <- matrix(0, runSize, runSize)
    }
    else if(fullLength%%dim != 0)
    {
      mm <- matrix(0, runSize+1, runSize+1)
      runSize = runSize+1
    }
  }
  print("Built Matrix")

  chr <- rep(NA, runSize)
  start <- rep(NA, runSize)
  end <- rep(NA, runSize)
  peaks.data <- data.frame(chr, start, end)


  i=1
  while(i<=runSize)
  {
    if(i<runSize)
    {
      if(i==1)
      {
        loopvector <- cicero_split(uniquepeaks[1])
        loopvector2 <- cicero_split(uniquepeaks[dim])
        peaks.data[i,1] = loopvector[1]
        peaks.data[i,2] = loopvector[2]
        peaks.data[i,3] = loopvector2[3]
        i=i+1

      }
      else if(i!=1)
      {
        loopvector <- cicero_split(uniquepeaks[((i-1)*dim)+1])
        loopvector2 <- cicero_split(uniquepeaks[(i*dim)])
        peaks.data[i,1] = loopvector[1]
        peaks.data[i,2] = loopvector[2]
        peaks.data[i,3] = loopvector2[3]
        i=i+1

      }
    }
    else if(i==runSize)
    {
      loopvector <- cicero_split(uniquepeaks[((i-1)*dim)+1])
      loopvector2 <- cicero_split(uniquepeaks[fullLength])
      peaks.data[i,1] = loopvector[1]
      peaks.data[i,2] = loopvector[2]
      peaks.data[i,3] = loopvector2[3]
      i=i+1

    }

  }
  print("Generated new peak file")
  vector <- sumtoPeaks(peaks.data)
  print("Filling Matrix")

  finalMatrix = fillMatrixRed(KO_conns, mm, vector, thresh)


  print("Cutting Matrix")
  if(startCount != 0)
  {
    finalMatrix = finalMatrix[-1,]
    finalMatrix = finalMatrix[,-1]
  }

  if(endFound != TRUE)
  {
    finalMatrix = finalMatrix[-dim(finalMatrix)[1],]
    finalMatrix = finalMatrix[,-dim(finalMatrix)[2]]
  }

  return(finalMatrix)
}
