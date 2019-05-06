#   function which takes the combined cicero ooutput string
#   and returns a vector
#   containing the end, start, and chromosone of a peak, using
#   spaces

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
cicero_split_space <- function(cicString){
  chromnum <- regexpr(" ",cicString)
  chrom <- substr(cicString, 1, chromnum-1)
  peak <- substring(cicString, chromnum+1)
  peakStart = gsub(" .*", "", peak)
  peakEnd = gsub(".* ", "", peak)
  peakVector <- c(chrom,peakStart,peakEnd)
  return(peakVector)
}
