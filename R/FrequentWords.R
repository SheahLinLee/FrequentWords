FrequentWords <- function(Text, k){
  Text1 <- strsplit(Text, split=NULL)[[1]]
  FrequentPatterns <- c()
  Pattern1 <- c()
  Count <- integer()
  for (i in 1:(length(Text1)-k+1)){
    Pattern<-paste(Text1[i:(i+k-1)],collapse="")
    Pattern1[i]<-Pattern
    Count[i]<-PatternCount(Text,Pattern)
  }
  maxCount<-max(Count)
  for (i in 1:(length(Text1)-k+1)){
    if(Count[i]==maxCount){
      FrequentPatterns <- c(FrequentPatterns, Pattern1[i])
    }
  }
  FrequentPatterns<-FrequentPatterns[!duplicated(FrequentPatterns)]
  return(list(FrequentPatterns, maxCount))
}
