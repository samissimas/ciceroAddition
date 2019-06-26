#'Blend two conns files
#'
#'Takes in two conns files and combines like peaks and adds peaks not included in each others
#'
#' @param KO_conns the first dataframe you want to combine
#' @param WT_conns the second dataframe you want to combine
#' @param namefirst the label you want to give to the first conns file, i.e. "A"
#' @param namesecond the label you want to give to the second conns file, i.e. "B"
#' 
#' @return a blended dataframe
#'
#' @examples
#' loop.data = sortConnections(loop.data, bed)
#'
#' @export
connBlender <- function(KO_conns, WT_conns, namefirst, namesecond){
  
  uniquePeaksKO = unique_peak(KO_conns) 
  
  print("unique peaks for KO done")
  
  uniquePeaksWT = unique_peak(WT_conns)
  
  print("unique peaks for WT done")
  
  vectorsizeKO <- length(uniquePeaksKO)
  vectorsizeWT <- length(uniquePeaksWT)
  
  KOpeaks.data <- createDF(vectorsizeKO)
  WTpeaks.data <- createDF(vectorsizeWT)
  
  KOpeaks.data <- buildDF(uniquePeaksKO, KOpeaks.data)
  WTpeaks.data <- buildDF(uniquePeaksWT, WTpeaks.data)
  
  SUMpeaks.data = sumPeaksName(WTpeaks.data, KOpeaks.data, namefirst, namesecond)
  
  print("sum peaks done")
  
  SUMpeaksblend.data = peakBlend(SUMpeaks.data)
  
  return(SUMpeaksblend.data)
  
}