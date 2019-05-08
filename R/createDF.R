#'Create Empty Peak Dataframe
#'
#'Creates an empty peak dataframe of a given length
#'
#' @param vectorsize the length you want the dataframe to be
#'
#' @return an empty dataframe of the correct length
#'
#' @examples
#' peak.data = createDF(400)
#'
#' @export
createDF <- function(vectorsize){

  chr <- rep(NA, vectorsize)
  start <- rep(NA, vectorsize)
  end <- rep(NA, vectorsize)
  peaks.data <- data.frame(chr, start, end)
  return(peaks.data)
}

