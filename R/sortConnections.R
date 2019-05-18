#'Sort Connections of a completed dataframe
#'
#'Takes a completed dataframe with KO and WT conns and sorts them based on a given bed file containing peaks
#'
#' @param loop.data the generated dataframe whose connections you want to sort
#' @param bed the peaks you want to search for and keep within the conns
#' 
#' @return the loop.data dataframe with newly sorted peaks
#'
#' @examples
#' loop.data = sortConnections(loop.data, bed)
#'
#' @export
sortConnections <- function(loop.data, bed){
  
  ###create a peaks file of the bed file
  i = 1
  
  chr <- rep(NA, dim(bed)[1])
  start <- rep(NA, dim(bed)[1])
  end <- rep(NA, dim(bed)[1])
  peaks.data <- data.frame(chr, start, end)
  
  while(i <= dim(bed)[1])
  {
    
    unVector <- numeric()
    cicString = bed[i, 1]
    chromnum <- regexpr("r",cicString)
    number <- substring(cicString, chromnum+1)
    if(number=="X")
    {
      number = "18"
      peaks.data[i, 1] = number
      peaks.data[i, 2] = bed[i, 2]
      peaks.data[i, 3] = bed[i, 3]
      i = i + 1
    }
    else if(number=="Y")
    {
      number = "19"
      peaks.data[i, 1] = number
      peaks.data[i, 2] = bed[i, 2]
      peaks.data[i, 3] = bed[i, 3]
      i = i + 1
    }
    else
    {
      peaks.data[i, 1] = number
      peaks.data[i, 2] = bed[i, 2]
      peaks.data[i, 3] = bed[i, 3]
      i = i + 1
    }
    
    
    
  }
  
  peaks.data$chr <- as.numeric(peaks.data$chr)
  peaks.data = peaks.data[order(peaks.data$chr),]
  
  
  loop.data$KO_connections <- as.character(loop.data$KO_connections)
  loop.data$WT_connections <- as.character(loop.data$WT_connections)
  
  WTCount = 0
  KOCount = 0
  
  ###Analyze the KO and WT conns
  j=1
  while(j<=dim(loop.data)[1])
  {
    
    ###Build Dataframes out of conns
    WTCount = 0
    KOCount = 0
    
    if(loop.data[j,8] != "empty")
    {
      string = loop.data[j,8]
      peakvector = unlist(strsplit(string, split=","))
      length = length(peakvector)
      WT.data = createDF(length)
      b = 1
      while(b<=length)
      {
        loopVector = cicero_split(peakvector[b])
        WT.data[b,1] = loopVector[1]
        WT.data[b,2] = loopVector[2]
        WT.data[b,3] = loopVector[3]
        b = b +1
      }
      
      
      ###Analyze the WT data to see if they are contained in the bed
      k =1
      l = 1
      WTvector <- vector()
      while(k<=dim(WT.data)[1])
      {
        cicString2 = WT.data[k,1]
        chromnum2 <- regexpr("r",cicString2)
        number2 <- substring(cicString2, chromnum2+1)
        if(as.numeric(number2) == peaks.data[l,1])
        {
          if(as.numeric(peaks.data[l,2]) <= as.numeric(WT.data[k,3]))
          {
            if(as.numeric(peaks.data[l,3]) >= as.numeric(WT.data[k,2]))
            {
              WTvector = c(WTvector, paste(WT.data[k,1], WT.data[k,2], WT.data[k,3],sep="_"))
              WTCount = WTCount + 1
              k = k + 1
            }
            else if(as.numeric(peaks.data[l,3]) < as.numeric(WT.data[k,2]))
            {
              l = l + 1
            }
          }
          else if(as.numeric(peaks.data[l,2]) > as.numeric(WT.data[k,3]))
          {
            k = k + 1
          }
        }
        else
        {
          if(as.numeric(number2) > peaks.data[l,1])
          {
            l = l + 1
          }
          else if(as.numeric(number2) < peaks.data[l,1])
          {
            k = k + 1
          }
        }
      }
      
      ###Turn the selected WT peaks into a string
      i=1
      string = ""
      while(i<=length(WTvector))
      {
        if(i==1)
        {
          string = WTvector[1]
        }
        else if(i!=1)
        {
          string = paste(string, WTvector[i], sep=",")
        }
        i = i + 1
      }
      
      loop.data[j,8] = string
      loop.data[j,6] = WTCount
    }
    
    if(loop.data[j,7] != "empty")
    {
      string = loop.data[j,7]
      peakvector = unlist(strsplit(string, split=","))
      length = length(peakvector)
      KO.data = createDF(length)
      i = 1
      while(i<=length)
      {
        loopVector = cicero_split(peakvector[i])
        KO.data[i,1] = loopVector[1]
        KO.data[i,2] = loopVector[2]
        KO.data[i,3] = loopVector[3]
        i = i +1
      }
      
      ###Analyze the KO data to see if they are contained in the bed
      k =1
      l = 1
      KOvector <- vector()
      while(k<=dim(KO.data)[1])
      {
        cicString2 = KO.data[k,1]
        chromnum2 <- regexpr("r",cicString2)
        number2 <- substring(cicString2, chromnum2+1)
        if(as.numeric(number2) == peaks.data[l,1])
        {
          if(as.numeric(peaks.data[l,2]) <= as.numeric(KO.data[k,3]))
          {
            if(as.numeric(peaks.data[l,3]) >= as.numeric(KO.data[k,2]))
            {
              KOvector = c(KOvector, paste(KO.data[k,1], KO.data[k,2], KO.data[k,3],sep="_"))
              KOCount = KOCount + 1
              k = k + 1
            }
            else if(as.numeric(peaks.data[l,3]) < as.numeric(KO.data[k,2]))
            {
              l = l + 1
            }
          }
          else if(as.numeric(peaks.data[l,2]) > as.numeric(KO.data[k,3]))
          {
            k = k + 1
          }
        }
        else
        {
          if(as.numeric(number2) > peaks.data[l,1])
          {
            l = l + 1
          }
          else if(as.numeric(number2) < peaks.data[l,1])
          {
            k = k + 1
          }
        }
        
        
      }
      
      ###Turn the selected KO peaks into a string
      i=1
      string = ""
      while(i<=length(KOvector))
      {
        if(i==1)
        {
          string = KOvector[1]
        }
        else if(i!=1)
        {
          string = paste(string, KOvector[i], sep=",")
        }
        i = i + 1
        
      }
      
      loop.data[j,7] = string
      loop.data[j,5] = KOCount
    }
    
    j = j + 1
    if(j%%500==0)
    {
      jstring = paste("Completed", j, "of", dim(loop.data)[1], sep=" ")
      print(jstring)
    }
  }
  
}