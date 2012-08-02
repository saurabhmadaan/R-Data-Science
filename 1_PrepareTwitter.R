# EnsurePackage -  Installs and loads package if necessary
EnsurePackage<-function(x)
{
  x<-as.character(x)
  if (!require(x,character.only=TRUE))
  {
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# PrepareTwitter -  Loads packages for working with Twitter
PrepareTwitter<-function()
{
  EnsurePackage('bitops')
  EnsurePackage('RCurl')
  EnsurePackage('RJSONIO')
  EnsurePackage('twitteR')
}


# TweetFrame() - return a data frame based on a Twitter search, sorted by time of creation
TweetFrame<-function(searchTerm,maxTweets)
{
  tweetList<-searchTwitter(searchTerm,n=maxTweets)  
  df<-do.call('rbind',lapply(tweetList,as.data.frame))
  sorteddf<-df[order(as.integer(df$created)),]
  return(sorteddf)
}


# ArrivalProbability() - Given a list of arrival times calculates delays between them using lagged
# differences, then computes a list of cumulative probabilities of arrival for sequential list
# of time increments
ArrivalProbability<-function(timesList,increment,max)
{
  plist<-NULL
  timeLen<-length(timesList)
  if(increment>max){return(NULL)}
  for(i in seq(increment,max,by=increment))
  {
    plist<-c(plist,(sum(as.integer(diff(timesList))<i))/timeLen)
  }
  return(plist)
}
# tf<-TweetFrame('oprah',500)
# pl<-ArrivalProbability(tf$created,1,40)
# plot(pl)


#of<-TweetFrame('#oprah',500)
#gf<-TweetFrame('#ladygaga',500)


#ofEventDelays<-as.integer(diff(of$created))
#gfEventDelays<-as.integer(diff(gf$created))

#mean(ofEventDelays)
#mean(gfEventDelays)

#sum(gfEventDelays<=43)
# [1] 320
# poisson.test(320,500)$conf.int

#sum(ofEventDelays<=43)
#[1] 82
#poisson.test(82,500)$conf.int

# no overlap between oprah and gaga conf intervals. significantly differ


PoissComp<-function(String1,String2,n)
{
  tf1<-TweetFrame(String1,n)
  tf2<-TweetFrame(String2,n)
  v1<-diff(as.integer(tf1$created))
  v2<-diff(as.integer(tf2$created))
  AvgArr1<-mean(v1)
  AvgArr2<-mean(v2)
  TweetsBelowThreshold1<-sum(v1<=AvgArr1)
  TweetsBelowThreshold2<-sum(v2<=AvgArr1) #using avg arrival of 1st dist as threshold
  pt<-poisson.test(c(TweetsBelowThreshold1,TweetsBelowThreshold2),c(n,n))
  return(pt)
}

CleanTweets<-function(tweetList)
{
  tweetList<-str_replace_all(tweetList,'@[a-z,A-Z]*','')
  tweetList<-str_replace_all(tweetList,'http://t.co/[a-z,A-Z,0-9]{8}','')
  tweetList<-str_replace(tweetList,'RT @[a-z,A-Z]*:','')
  tweetList<-str_replace_all(tweetList,'#[a-z,A-Z]*','')
  tweetList<-str_replace_all(tweetList,'@[a-z,A-Z]*','')
  tweetList<-str_replace_all(tweetList,'  ',' ')
  return(tweetList)
}

# tdf<-TweetFrame('#solar',100)
# attach(tdf)
# ctext<-CleanTweets(text)
# EnsurePackage('tm')
# tc<-Corpus(VectorSource(ctext))

GenWordCloud<-function(searchTerm,n)
{
  tf<-TweetFrame(searchTerm,n)
  tl<-CleanTweets(tf$text)
  tc<-Corpus(VectorSource(tl))
  # additional processing of tweets
  tc<-tm_map(tc,tolower)
  tc<-tm_map(tc,removePunctuation)
  tc<-tm_map(tc,removeWords,stopwords('english'))
  #build doc matrix: words vs its binary presence in each tweet
  tdm<-TermDocumentMatrix(tc)
  tcMatr<-as.matrix(tdm)
  sortedMatr<-sort(rowSums(tcMatr),decreasing=TRUE)
  cloudFrame<-data.frame(word=names(sortedMatr),freq=sortedMatr)
  cloudFrame
  wordcloud(cloudFrame$word,cloudFrame$freq)
  return(tf)
}

# convert factors column/vector to numeric column/vector
Numberize<-function(inputVector)
{
  inputVector<-str_replace_all(inputVector,',','')
  inputVector<-str_replace_all(inputVector,' ','')
  return(as.numeric(inputVector))
}

#testFr<-subset(testFr, select = -c(July10Base,July10Census))


#library(RMySQL)
#con <-dbConnect(dbDriver('MySQL'),dbname='test')
#dbListTables(con)
#dbWriteTable(con,'census',testFr,overwrite=TRUE)




