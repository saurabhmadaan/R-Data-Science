MyMode <- function(myVector)
{
  uniqValues <- unique(myVector) 
  #uniqCounts <- tabulate(myVector)
  uniqCounts <- tabulate(match(myVector,uniqValues)) #tabulate fills in missing nums with 0 count, if unmatched
  mode <- uniqValues[which.max(uniqCounts)]
  return(mode)
}