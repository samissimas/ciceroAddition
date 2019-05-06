###function which turns a sum peak dataframe back into a peak vector
sumtoPeaks <- function(SUM.data){
  length = dim(SUM.data)[1]
  peaks <- vector(mode="character", dim(SUM.data)[1])
  i= 1
  while(i <= length)
  {
    char1 = toString(SUM.data[i,1])
    char2 = toString(SUM.data[i,2])
    char3 = toString(SUM.data[i,3])
    char4 =
      peaks[i] = paste(char1, "_", char2, "_", char3, sep = "")

    i = i + 1
  }
  return(peaks)
}
