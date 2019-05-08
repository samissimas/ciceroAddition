#'Cicero Output Splitter Space
#'
#'takes tab dilemited output string and returns a vector with: end, start, chromosone
#'
#' @param cicString a string in the form Chr' ''''''' ''''''''
#'
#' @return a vector containg start, end, and chromosone
#'
#' @examples
#' vector = cicero_split(cicString)
#'
#' @export
cicero_split_space <- function(cicString){
  chromnum <- regexpr(" ",cicString)
  chrom <- substr(cicString, 1, chromnum-1)
  peak <- substring(cicString, chromnum+1)
  peakStart = gsub(" .*", "", peak)
  peakEnd = gsub(".* ", "", peak)
  peakVector <- c(chrom,peakStart,peakEnd)
  return(peakVector)
}
