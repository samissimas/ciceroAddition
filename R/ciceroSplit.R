#'Cicero Output Splitter
#'
#'takes cicero output string and returns a vector with: end, start, chromosone
#'
#' @param cicString a string in the form Chr'_'''''''_''''''''
#'
#' @return a vector containg start, end, and chromosone
#'
#' @examples
#' vector = cicero_split(cicString)
#'
#' @export
cicero_split <- function(cicString){
  chromnum <- regexpr("_",cicString)
  chrom <- substr(cicString, 1, chromnum-1)
  peak <- substring(cicString, chromnum+1)
  peakStart = gsub("_.*", "", peak)
  peakEnd = gsub(".*_", "", peak)
  peakVector <- c(chrom,peakStart,peakEnd)
  return(peakVector)
}
