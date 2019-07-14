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
#' 
#' @return a blended dataframe
#'
#' @examples
#' loop.data = sortConnections(loop.data, bed)
#'
#' @export
connInteger <- function(WT_cicero_cds, KO_cicero_cds, vector, mm10.chr, namefirst, namesecond){
  
  if(length(vector)==0)
  {
    print("You must have a vector with real numbers corresponding to chromosones you are interested in")
    
  }
  
  for(i in vector)
  {
    print(is.Numeric(i))
    print("Creating the first conns file")
    KO_conns = connCreator(KO_cicero_cds, i, mm10.chr)
    
    print("Creating the second conns file")  
    WT_conns = connCreator(WT_cicero_cds, i, mm10.chr)
    
    print("Beginning to blend conns files")
    SUMpeaks.data = connBlender(KO_conns, WT_conns, namefirst, namesecond)
    
    return(SUMpeaks.data)
  }
  
  
}