#'Create a conns files
#'
#'Takes in a cds files and creates a conns files
#'
#' @param WT_cicero_cds the cds you want to convert to a conns file
#' @param number the chromosone you want a conns file for
#' @param mm10.chr the genome file you are referencing to create the conns file
#' 
#' @return a conns file
#'
#' @examples
#' KO_conns = connCreator(T_cicero_cds, number, mm10.chr)
#'
#' @export
connCreator <- function(WT_cicero_cds, number, mm10.chr){
  
  
  sample_genome <- subset(mm10.chr, V1==number)
  WT_conns <- run_cicero(WT_cicero_cds, sample_genome)
  return(WT_conns)
  
}