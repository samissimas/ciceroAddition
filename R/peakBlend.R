###function which combines overlapping peaks
peakBlend <- function(SUMpeaks.data){
  i=1
  length = dim(SUMpeaks.data)[1]
  while(i < length)
  {
    SUMpeaks.data[i,2] = as.numeric(as.character(SUMpeaks.data[i,2]))
    SUMpeaks.data[i,3] = as.numeric(as.character(SUMpeaks.data[i,3]))
    SUMpeaks.data[i+1,2] = as.numeric(as.character(SUMpeaks.data[i+1,2]))
    SUMpeaks.data[i,3] = as.numeric(as.character(SUMpeaks.data[i,3]))


    if(as.numeric(SUMpeaks.data[i,3]) >= as.numeric(SUMpeaks.data[i+1,2]))
    {
      while(as.numeric(SUMpeaks.data[i,3]) >= as.numeric(SUMpeaks.data[i+1,2]) & i != dim(SUMpeaks.data)[1])
      {
        if(as.numeric(SUMpeaks.data[i,3]) > as.numeric(SUMpeaks.data[i+1,3]))
        {
          if(as.numeric(SUMpeaks.data[i,2]) <= as.numeric(SUMpeaks.data[i+1,2]))
          {
            if(SUMpeaks.data[i,4] == "B" | SUMpeaks.data[i+1,4] == "B")
            {
              SUMpeaks.data[i,4] = "B"
            }
            else if(SUMpeaks.data[i,4] != SUMpeaks.data[i+1,4])
            {
              SUMpeaks.data[i,4] = "B"
            }
            SUMpeaks.data <- SUMpeaks.data[-(i+1),]
            length = dim(SUMpeaks.data)[1]

          }
          else if (as.numeric(SUMpeaks.data[i,2]) >= as.numeric(SUMpeaks.data[i+1,2]))
          {
            if(as.numeric(SUMpeaks.data[i,2]) <= as.numeric(SUMpeaks.data[i+1,3]))
            {
              if(SUMpeaks.data[i,4] == "B" | SUMpeaks.data[i+1,4] == "B")
              {
                SUMpeaks.data[i,4] = "B"
              }
              else if(SUMpeaks.data[i,4] != SUMpeaks.data[i+1,4])
              {
                SUMpeaks.data[i,4] = "B"
              }
              SUMpeaks.data[i,2] = SUMpeaks.data[i+1,2]
              SUMpeaks.data <- SUMpeaks.data[-(i+1),]
              length = dim(SUMpeaks.data)[1]

            }

          }

        }
        else if(as.numeric(SUMpeaks.data[i,3]) <= as.numeric(SUMpeaks.data[i+1,3]))
        {
          if(as.numeric(SUMpeaks.data[i,2]) < as.numeric(SUMpeaks.data[i+1,2]))
          {
            if(SUMpeaks.data[i,4] == "B" | SUMpeaks.data[i+1,4] == "B")
            {
              SUMpeaks.data[i,4] = "B"
            }
            else if(SUMpeaks.data[i,4] != SUMpeaks.data[i+1,4])
            {
              SUMpeaks.data[i,4] = "B"
            }
            SUMpeaks.data[i,3] = SUMpeaks.data[i+1,3]
            SUMpeaks.data <- SUMpeaks.data[-(i+1),]
            length = dim(SUMpeaks.data)[1]

          }
          else if(as.numeric(SUMpeaks.data[i,2]) >= as.numeric(SUMpeaks.data[i+1,2]))
          {
            if(SUMpeaks.data[i,4] == "B" | SUMpeaks.data[i+1,4] == "B")
            {
              SUMpeaks.data[i+1,4] = "B"
            }
            else if(SUMpeaks.data[i,4] != SUMpeaks.data[i+1,4])
            {
              SUMpeaks.data[i+1,4] = "B"
            }
            SUMpeaks.data <- SUMpeaks.data[-(i),]
            length = dim(SUMpeaks.data)[1]
          }

        }
      }
      i = i + 1
    }
    else if(as.numeric(SUMpeaks.data[i,3]) < as.numeric(SUMpeaks.data[i+1,2]))
    {
      i = i + 1

    }
  }
  return(SUMpeaks.data)

}
