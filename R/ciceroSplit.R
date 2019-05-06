#   function which takes the combined cicero ooutput string and returns a vector
#   containing the end, start, and chromosone of a peak
#   connections and returns a cicero split list

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
cicero_split <- function(cicString){
  chromnum <- regexpr("_",cicString)
  chrom <- substr(cicString, 1, chromnum-1)
  peak <- substring(cicString, chromnum+1)
  peakStart = gsub("_.*", "", peak)
  peakEnd = gsub(".*_", "", peak)
  peakVector <- c(chrom,peakStart,peakEnd)
  return(peakVector)
}
