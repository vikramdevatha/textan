#Function for making a DTM

# By default, the function makes a DTM using TF IDF. The alternative is to make a DTM without weights. 

# Usage in local R: 
#   if (!require(RCurl)) {install.packages("RCurl")}; library(RCurl)
#   DTM_Builder = getURL("insert raw GIT URL", ssl.verifypeer=FALSE)
#   eval(parse(text=DTM_Builder))
#   x_clean = DTM_Builder(x, dtm=TRUE\FALSE, tfidf=TRUE\FALSE)
# Note: only one argument can be TRUE - either DTM or tfidf
# Returns a DTM with the largest cell on the top left

DTM_Builder = function(text.input, dtm=FALSE, tfidf=TRUE){
  
  if (!require(tidytext)) {install.packages("tidytext")}; library(tidytext)
  if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
  if (!require(tibble)) {install.packages("tibble")}; library(tibble)
  if (!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
  if (!require(tidyr)) {install.packages("tidyr")}; library(tidyr)

  text.input = data_frame(text=text.input)
  
  text.input %>% 
    mutate(doc = row_number()) %>% #adds row numbers to the df
    unnest_tokens(word, text) %>% #output is a word, input is a text
    anti_join(stop_words) %>% #remove stop words from the textdf
    group_by(doc) %>% #gives the document number
    count(word, sort=TRUE) -> #counts the number of times the word has occured
    text.input
 
  if (tfidf == "TRUE"){
    text.input %>%
      group_by(doc) %>% 
      count(word, sort=TRUE) %>% 
      ungroup() %>% 
      bind_tf_idf(word, doc, nn) -> #makes a DTM using TF-IDF
      text.input.dtm
    
    text.input.dtm = text.input.dtm[order(-text.input.dtm$tf_idf),]
    }
    
  if (dtm=="TRUE"){
    text.input %>% 
      cast_sparse(doc, word, n) -> #making a document-term matrix
      text.input.dtm
  }

  return(text.input.dtm)
}
