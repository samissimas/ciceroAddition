#   function which takes the comma seperated string of

#   connections and returns a cicero split list
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
list_split <- function(cicString){
  string = cicString
  peakvector = unlist(strsplit(string, split=","))
  length = length(peakvector)
  peaks.data = createDF(length)
  i = 1
  while(i<=length)
  {
    loopVector = cicero_split(peakvector[i])
    peaks.data[i,1] = loopVector[1]
    peaks.data[i,2] = loopVector[2]
    peaks.data[i,3] = loopVector[3]

    i = i +1
  }
  return(peaks.data)
}
