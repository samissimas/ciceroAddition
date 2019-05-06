###compare two conns files
compareConns <- function(WT_conns, KO_conns)
{
  name1="K"
  name2="W"

  uniquePeaksKO = unique_peak(KO_conns)

  uniquePeaksWT = unique_peak(WT_conns)

  vectorsizeWT <- length(uniquePeaksWT)
  vectorsizeKO <- length(uniquePeaksKO)

  WTpeaks.data <- createDF(vectorsizeWT)
  KOpeaks.data <- createDF(vectorsizeKO)

  WTpeaks.data <- buildDF(uniquePeaksWT, WTpeaks.data)
  KOpeaks.data <- buildDF(uniquePeaksKO, KOpeaks.data)

  SUMpeaks.data = sumPeaksName(WTpeaks.data, KOpeaks.data, vectorsizeKO, name1, name2)

  SUMpeaksblend.data = peakBlend(SUMpeaks.data)

  SUMpeaksWT.data <- connblendInt(SUMpeaksblend.data, WT_conns, .3)

  SUMpeaksKO.data <- connblendInt(SUMpeaksblend.data, KO_conns, .3)

  access1 = paste("WT","_access",sep="")
  access2 = paste("KO","_access",sep="")

  connections1 = paste("WT","_connections",sep="")
  connections2 = paste("KO","_connections",sep="")

  colnames(SUMpeaksWT.data)[colnames(SUMpeaksWT.data)=="access"] <- access1
  colnames(SUMpeaksKO.data)[colnames(SUMpeaksKO.data)=="access"] <- access2

  colnames(SUMpeaksWT.data)[colnames(SUMpeaksWT.data)=="connections"] <- connections1
  colnames(SUMpeaksKO.data)[colnames(SUMpeaksKO.data)=="connections"] <- connections2

  SUMpeaksPreSort.data <- cbind(SUMpeaksKO.data, SUMpeaksWT.data[!names(SUMpeaksKO.data) %in% names(SUMpeaksWT.data)])

  SUMpeaksPreSortB.data = SUMpeaksPreSort.data[SUMpeaksPreSort.data$location == "B",]

  percentPeaks = ((dim(SUMpeaksPreSort.data)[1]-dim(SUMpeaksPreSortB.data)[1])/dim(SUMpeaksPreSort.data)[1])*100

  if(sum(SUMpeaksPreSort.data$WT_access) >= sum(SUMpeaksPreSort.data$KO_access))
  {
    percentConn = ((sum(SUMpeaksPreSort.data$WT_access) -sum(SUMpeaksPreSort.data$KO_access)) /sum(SUMpeaksPreSort.data$WT_access))*100

  }
  if(sum(SUMpeaksPreSort.data$WT_access) < sum(SUMpeaksPreSort.data$KO_access))
  {
    percentConn = ((sum(SUMpeaksPreSort.data$KO_access) -sum(SUMpeaksPreSort.data$WT_access)) /sum(SUMpeaksPreSort.data$KO_access))*100

  }

  results <- data.frame("Percent Peaks" = percentPeaks, "Percent Conns" = percentConn)

  return(results)
}
