#'Create a conns files
#'
#'Takes in a cds files and creates a conns files
#'
#' @param WT_cicero_cds the first cds you want to convert to a conns file
#' @param KO_cicero_cds the second cds you want to convert to a conns file
#' @param vector the vector containing which chromosones you are interested in
#' @param mm10.chr the genome file you are referencing to create the conns files
#' @param namefirst the label you want to give to the first conns file, i.e. "A"
#' @param namesecond the label you want to give to the second conns file, i.e. "B"
#' @param thresh the threshold for peaks you are interested in
#' @param changePerc the percent difference between Ko and WT conns you want to use as a threshold
#' @param viablethresh the threshold for peaks you consider viable
#' 
#' @return a blended dataframe
#'
#' @examples
#' loop.data = sortConnections(loop.data, bed)
#'
#' @export
connInteger <- function(WT_cicero_cds, KO_cicero_cds, vector, mm10.chr, namefirst, namesecond, thresh, changePerc, viablethresh){
  
  if(length(vector)==0)
  {
    print("You must have a vector with real numbers corresponding to chromosones you are interested in")
    
  }
  
  fullGenomeSorted <- data.frame()
  
  for(i in vector)
  {
    print("Creating the first conns file")
    KO_conns = connCreator(KO_cicero_cds, i, mm10.chr)
    
    print("Creating the second conns file")  
    WT_conns = connCreator(WT_cicero_cds, i, mm10.chr)
    
    print("Beginning to blend conns files")
    SUMpeaks.data = connBlender(KO_conns, WT_conns, namefirst, namesecond)
    
    SUMpeaksKO.data <- connblendInt(SUMpeaks.data, KO_conns, viablethresh)
    
    print("KO connections tabulated")
    
    SUMpeaksWT.data <- connblendInt(SUMpeaks.data, WT_conns, viablethresh)
    
    print("WT connections tabulated")
    
    colnames(SUMpeaksKO.data)[colnames(SUMpeaksKO.data)=="access"] <- "KO_access"
    colnames(SUMpeaksWT.data)[colnames(SUMpeaksWT.data)=="access"] <- "WT_access"
    colnames(SUMpeaksKO.data)[colnames(SUMpeaksKO.data)=="connections"] <- "KO_connections"
    colnames(SUMpeaksWT.data)[colnames(SUMpeaksWT.data)=="connections"] <- "WT_connections"
    
    
    SUMpeaksPreSort.data <- cbind(SUMpeaksKO.data, SUMpeaksWT.data[!names(SUMpeaksKO.data) %in% names(SUMpeaksWT.data)])
    
    SUMpeaksPreSort.data = SUMpeaksPreSort.data[,c(1,2,3,4,5,7,6,8)]
    
    SUMpeaksSorted.data <- sigPeakFinder(SUMpeaksPreSort.data, thresh, changePerc)
    fullGenomeSorted  = rbind(fullGenomeSorted, SUMpeaksSorted.data)
    
  }
  return(result)
  
}