MySamplingDistribution <- function(myVector)
{
  splmeans<-replicate(1000,mean(sample(myVector,10)))
  return(splmeans)
}