Text_Clean = function(text.input,
                      remove_numbers = TRUE,
                      remove_stopwords = TRUE,
                      remove_punc = FALSE,
                      stem_document = FALSE) {
  
  if (!require(tm)) {install.packages("tm")}; library(tm)
  if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
  
  text.input = gsub("<*.?>", " ", text.input) #removing HTML tags
  text.input = iconv(text.input, "latin1", "ASCII", sub=" ") #keep only ASCII characters
  text.input = tolower(text.input)
  text.input = stripWhitespace(text.input)
  text.input = gsub("^\\s+|\\s+$", " ", text.input) #removing space at the beginning and at end
  
  if(remove_stopwords){
    #combining stop words from two sources - GIT and TM package
    stop_words = readLines("https://raw.githubusercontent.com/vikramdevatha/textan/master/stopwords.txt")
    stop_words2 = tm::stopwords(kind='en')
    common_stop_words = unique(c(stop_words, stop_words2))
    common_stop_words = unique(gsub("'", "", common_stop_words)) #removing the apostrophy
    
    text.input %>% 
      removeWords(common_stop_words) %>% 
      stripWhitespace() ->
      text.input
  }
  
  if(remove_numbers){
    text.input %>%
      removeNumbers() ->
      text.input
  }
  
  if(remove_punc){
    text.input = removePunctuation(text.input, preserve_intra_word_dashes = TRUE) #removing punctuation from the document
  }
  
  if(stem_document){
    text.input = stemDocument(text.input) #stems the document
  }
  
  return(text.input)
}
