#'Creates a Reduced Graphable Matrix of Conns values
#'
#'Creates a reduced graphable matrix based on a given conns file
#'
#' @param KO_conns a conns file
#' @param dim the size of each bin
#' @param startNum the start coordinate of your desired region
#' @param endNum the end coordinate of your desired region
#'
#' @return a reduced matrix filled with connsInt
#'
#' @examples
#' matrix = simpleMatrix(KO_conns, 70, 1200, 300000000)
#'
#' @export
#' #' @export
simpleMatrixEven <- function(KO_conns, dim, thresh, startNum, endNum){
  
  
  
  if(endNum<=startNum)
  {
    print("End must be greater than start")
    break
  }
  uniquepeaks <- unique_peak(KO_conns)
  UP.data = buildDF_matrix(uniquepeaks)
  
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
        startCount = peakcutter - 1
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
        endFound = TRUE
      }
      else if(as.numeric(UP.data[peakcutter,3])<as.numeric(endNum))
      {
        if(as.numeric(UP.data[length(uniquepeaks), 3]) <= endNum)
        {
          peakcutter = length(uniquepeaks)+1
          endCount = length(uniquepeaks)+1
          
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
        endCount = peakcutter
        peakcutter = length(uniquepeaks) + 1
        endFound = TRUE
      }
    }
    
  }
  if(endCount==0)
  {
    print("Region end number is too small")
    break
  }
  
  if(dim > (endNum-startNum))
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
  print(UP3.data)
  
  fullLength = as.numeric(endNum) - as.numeric(startNum)
  runSize = ceiling((as.numeric(fullLength)/as.numeric(dim)))
  print(runSize)
  ###if((locaterBelow(uniquepeaks, endNUM)-locaterBelow(uniquepeaks,startNum)) < runSize)
  ###{
  ###print("The dimension is too large. There will be empty columns and rows within the final matrix")
  ###break
  
  ###}
  if(runSize<2)
  {
    print("Dimension Too Large: Must Be Half Or Less Of Unique Peak Length")
    break
    
  }
  
  
  
  if(startCount == 0 & endFound == TRUE)
  {
    chr <- rep(NA, runSize+1)
    start <- rep(NA, runSize+1)
    end <- rep(NA, runSize+1)
    peaks.data <- data.frame(chr, start, end)
    
    loopvector <- cicero_split(uniquepeaks[length(uniquepeaks)])
    peaks.data[runSize+1,1] = loopvector[1]
    peaks.data[runSize+1,2] = loopvector[2]
    peaks.data[runSize+1,3] = loopvector[3]
    i=1
    cutter = startNum + dim
    locator = locaterBelow(uniquepeaks, cutter)
    locator2 = locator
  }
  else if(startCount != 0 & endFound == FALSE)
  {
    chr <- rep(NA, runSize+1)
    start <- rep(NA, runSize+1)
    end <- rep(NA, runSize+1)
    peaks.data <- data.frame(chr, start, end)
    
    loopvector <- cicero_split(uniquepeaks[1])
    peaks.data[1,1] = loopvector[1]
    peaks.data[1,2] = loopvector[2]
    peaks.data[1,3] = loopvector[3]
    i=2
    runSize = runSize + 1
    cutter = startNum + dim
    locator = locaterBelow(uniquepeaks, cutter)
    locator2 = 1
  }
  else if(startCount != 0 & endFound == TRUE)
  {
    chr <- rep(NA, runSize+2)
    start <- rep(NA, runSize+2)
    end <- rep(NA, runSize+2)
    peaks.data <- data.frame(chr, start, end)
    loopvector <- cicero_split(uniquepeaks[1])
    peaks.data[1,1] = loopvector[1]
    peaks.data[1,2] = loopvector[2]
    peaks.data[1,3] = loopvector[3]
    
    loopvector <- cicero_split(uniquepeaks[length(uniquepeaks)])
    peaks.data[runSize+2,1] = loopvector[1]
    peaks.data[runSize+2,2] = loopvector[2]
    peaks.data[runSize+2,3] = loopvector[3]
    
    runSize = runSize + 1
    i=2
    cutter = startNum + dim
    locator = locaterBelow(uniquepeaks, cutter)
    locator2 = 1
    
  }
  else if(startCount == 0 & endFound == FALSE)
  {
    chr <- rep(NA, runSize)
    start <- rep(NA, runSize)
    end <- rep(NA, runSize)
    i=1
    cutter = startNum + dim
    locator = locaterBelow(uniquepeaks, cutter)
    locator2 = locator
  }
  print("Creating New Peak Dataframe")
  while(i<=runSize)
  {
    if(i<runSize)
    {
      if(i==1)
      {
        loopvector <- cicero_split(uniquepeaks[i])
        loopvector2 <- cicero_split(uniquepeaks[locator])
        peaks.data[i,1] = loopvector[1]
        peaks.data[i,2] = loopvector[2]
        peaks.data[i,3] = loopvector2[3]
        i=i+1
        cutter = cutter + dim
        locator2 = locator
        locator = locaterBelow(uniquepeaks, cutter)
        while(locator2 == locator && i < runSize)
        {
          i=i+1
          cutter = cutter + dim
          locator = locaterBelow(uniquepeaks, cutter)
          runSize = runSize-1
          print("Matrix dimensions have been altered to accomidate large dimension size, results are now skewed, consider using a larger dimension")
          
        }
        
      }
      else if(i!=1)
      {
        
        loopvector <- cicero_split(uniquepeaks[locator2+1])
        loopvector2 <- cicero_split(uniquepeaks[locator])
        peaks.data[i,1] = loopvector[1]
        peaks.data[i,2] = as.numeric(loopvector[2])
        print(locator)
        print(i)
        print(runSize)
        print(loopvector2)
        peaks.data[i,3] = loopvector2[3]
        i=i+1
        cutter = cutter + dim
        locator2 = locator
        locator = locaterBelow(uniquepeaks, cutter)
        if(i < runSize)
        {
          while(locator2 == locator && i < runSize)
          {
            i=i+1
            cutter = cutter + dim
            locator = locaterBelow(uniquepeaks, cutter)
            runSize = runSize-1
            print("Matrix dimensions have been altered to accomidate large dimension size, results are now skewed, consider using a smaller dimension")
            
          }
          
        }
        
        
      }
    }
    else if(i==runSize)
    {
      loopvector <- cicero_split(uniquepeaks[locator2+1])
      loopvector2 <- cicero_split(uniquepeaks[locator])
      
      if(as.numeric(loopvector[2]) > endNum)
      {
        peaks.data[i,1] = NA
        peaks.data[i,2] = NA
        peaks.data[i,3] = NA
      }
      else if(as.numeric(loopvector[2]) <= endNum)
      {
        peaks.data[i,1] = loopvector[1]
        peaks.data[i,2] = as.numeric(loopvector[2])
        peaks.data[i,3] = endNum
      }
      i=i+1
      
    }
    
  }
  
  peaks.data = na.omit(peaks.data)
  mm <- matrix(0, runSize, runSize)

  print("Built Matrix")
  
  print("Generated new peak file")
  print(peaks.data)
  vector <- sumtoPeaks(peaks.data)
  print("Filling Matrix")
  
  finalMatrix = fillMatrixRed(KO_conns, mm, vector, thresh)
  
  
  print("Cutting Matrix")
  if(startCount != 0)
  {
    finalMatrix = finalMatrix[-1,]
    finalMatrix = finalMatrix[,-1]
  }
  
  if(endFound == TRUE)
  {
    finalMatrix = finalMatrix[-dim(finalMatrix)[1],]
    finalMatrix = finalMatrix[,-dim(finalMatrix)[2]]
  }
  
  return(finalMatrix)
}