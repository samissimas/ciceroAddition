#'Calculate Total Connections Weighted
#'
#'Calculate total weight of connections for each peak in a given conns file over a threshold
#'
#' @param SUMpeaksblend.data a data frame containting all the unique peaks
#' @param KO_conns a conns file
#' @param thresh the decimal certainty above which you will accept peaks
#'
#' @return a dataframe containing the results of the conn weights
#'
#' @examples
#' SUMpeaksKO.data = connblendInt(SUMpeaksblend.data, KO_conns, thresh)
#'
#' @export
connblendweight <- function(SUMpeaksblend.data, KO_conns, thresh){
  SUMpeaksKO.data <- addColumnDF(SUMpeaksblend.data)
  SUMpeaksKO.data <- addColumnDF_character(SUMpeaksKO.data)
  i=1
  j=1
  length = dim(KO_conns)[1]



  while(i <= length)
  {
    if(!(is.na(KO_conns[i,3])))
    {
      if(as.numeric(KO_conns[i,3]) > as.numeric(thresh))
      {
        vectorsplit <- cicero_split(KO_conns[i,1])
        vectorsplit[2] = as.numeric(vectorsplit[2])
        vectorsplit[3] = as.numeric(vectorsplit[3])



        if(as.numeric(vectorsplit[2]) <= as.numeric(SUMpeaksKO.data[j,3]))
        {
          if(as.numeric(vectorsplit[3]) >= as.numeric(SUMpeaksKO.data[j,2]))
          {
            if(!(is.na(KO_conns[i,3])))
            {
              if(as.numeric(KO_conns[i,3]) > as.numeric(thresh))
              {
                SUMpeaksKO.data[j,5] = SUMpeaksKO.data[j,5] + as.numeric(KO_conns[i,3])
                if(SUMpeaksKO.data[j,6] == "empty")
                {
                  SUMpeaksKO.data[j,6] = as.character(KO_conns[i,2])
                }
                else
                {
                  SUMpeaksKO.data[j,6] = paste(SUMpeaksKO.data[j,6],",",KO_conns[i,2], sep="")
                }
              }
            }
            i = i + 1
          }
          else if(as.numeric(vectorsplit[2]) < as.numeric(SUMpeaksKO.data[j,2]))
          {
            print(i)

            break

          }

        }
        else
        {
          j = j + 1

        }
      }
      else
      {
        i = i + 1

      }
    }
    else
    {
      i = i + 1
    }
  }

  return(SUMpeaksKO.data)

}
