#Function for making a DTM

# By default, the function makes a DTM using TF IDF. The alternative is to make a DTM without weights. 

# Usage in local R: 
#   if (!require(RCurl)) {install.packages("RCurl")}; library(RCurl)
#   DTM_Builder = getURL("https://raw.githubusercontent.com/vikramdevatha/textan/master/DTM_Builder.R", ssl.verifypeer=FALSE)
#   eval(parse(text=DTM_Builder))
#   x_clean = DTM_Builder(x, tfidf=TRUE\FALSE)

DTM_Builder = function(text.input, tfidf=TRUE){
  
  if (!require(tidytext)) {install.packages("tidytext")}; library(tidytext)
  if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
  if (!require(tibble)) {install.packages("tibble")}; library(tibble)
  if (!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
  if (!require(tidyr)) {install.packages("tidyr")}; library(tidyr)

  # converting raw corpus to tibble to tidy DF
  textdf = data_frame(text = text.input);    textdf  
  
  tidy_df = textdf %>%   
    mutate(doc = row_number()) %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    group_by(doc) %>%
    count(word, sort=TRUE)
  tidy_df
  
  # evaluating IDF wala DTM
  if (tfidf == "TRUE") {
    textdf1 = tidy_df %>% 
      group_by(doc) %>% 
      count(word, sort=TRUE) %>% ungroup() %>%
      bind_tf_idf(word, doc, nn) %>%   # 'nn' is default colm name
      rename(value = tf_idf)} else { textdf1 = tidy_df %>% rename(value = n)  } 
  
  dtm = textdf1 %>% cast_sparse(doc, word, value)
  
  # order rows and colms putting max mass on the top-left corner of the DTM
  colsum = apply(dtm, 2, sum)    
  col.order = order(colsum, decreasing=TRUE)
  row.order = order(rownames(dtm) %>% as.numeric())
  dtm1 = dtm[row.order, col.order]
  return(dtm1)
}
