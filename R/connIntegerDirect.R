#'Analyzes two conns files
#'
#'Takes in two conns files and analyzes them fully with integer weighting
#'
#' @param WT_conns the first conns file
#' @param KO_conns the second conns file
#' @param mm10.chr the genome file you are referencing to create the conns files
#' @param namefirst the label you want to give to the first conns file, i.e. "A"
#' @param namesecond the label you want to give to the second conns file, i.e. "B"
#' @param thresh the threshold for peaks you are interested in
#' @param changePerc the percent difference between Ko and WT conns you want to use as a threshold
#' @param viablethresh the threshold for peaks you consider viable
#' 
#' @return a completed, sorted, and eleaborated dataframe
#'
#' @examples
#' SUM.data = connWeight(WT_conns, KO_conns, namefirst, namesecond, thresh, changPerc)
#'
#' @export
connIntegerDirect <- function(WT_conns, KO_conns, namefirst, namesecond, thresh, changePerc, viablethresh){
  

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
    return(SUMpeaksPreSort.data)
  
}