#Function for making a Wordcloud

# By default, the function makes a wordcloud with 150 words (max) with a min frequency o 5

# Usage in local R: 
#   if (!require(RCurl)) {install.packages("RCurl")}; library(RCurl)
#   build_wordcloud = getURL("insert raw GIT URL", ssl.verifypeer=FALSE)
#   eval(parse(text=buid_wordcloud))
#   BUILD_WORDCLOUD(x_dtm, plot.title="title", max.words=<number>, min.frequency=<number>)


BUILD_WORDCLOUD <- function(dtm, 
                            max.words1=150,     # max no. of words to accommodate
                            min.freq=5,       # min.freq of words to consider
                            plot.title="wordcloud"){          # write within double quotes
  
  if (!require(wordcloud)) {install.packages("wordcloud")}; library(wordcloud)
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)] # terms in decreasing order of freq
  head(tsum);    tail(tsum)
  
  wordcloud(names(tsum), tsum, # words, their freqs 
            scale = c(3.5, 0.5), # range of word sizes
            min.freq, # min.freq of words to consider
            max.words = max.words1, # max #words
            colors = brewer.pal(8, "Dark2")) # Plot results in a word cloud 
  title(sub = plot.title) # title for the wordcloud display
  
} # func ends
