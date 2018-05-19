# Usage in local R: 
#   if (!require(RCurl)) {install.packages("RCurl")}; library(RCurl)
#   WORDCLOUD_COG = getURL("insert raw GIT URL", ssl.verifypeer=FALSE)
#   eval(parse(text=WORDCLOUD_COG))
#   WORDCLOUD_COG(x.text, max_edges=<number>, drop.stopwords=<TRUE/FALSE>, new.stopwords=NULL)


WORDCLOUD_COG <- function(corpus, # text colmn only
                             max_edges = 150, 
                             drop.stop_words=TRUE,
                             new.stopwords=NULL){
  
  # invoke libraries
  if (!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
  if (!require(tidytext)) {install.packages("tidytext")}; library(tidytext)
  if (!require(widyr)) {install.packages("widyr")}; library(widyr)
  if (!require(ggraph)) {install.packages("ggraph")}; library(ggraph)
  if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
  
  # build df from corpus
  corpus_df = data.frame(docID = seq(1:length(corpus)), text = corpus, stringsAsFactors=FALSE)
  
  # eval stopwords condn
  if (drop.stop_words == TRUE) {
    stop.words = unique(c(stop_words$word, new.stopwords)) %>%
      as_tibble() %>%
      rename(word=value)} 
  else {stop.words = stop_words[2,]}
  
  corpus_df %>% 
    unnest_tokens(word, text) %>% # tokenize into words 
    anti_join(stop.words) -> # drop stop_words
    tokens   # list of tokens in the corpus
  
  tokens %>%
    count(word,sort = T) %>%
    dplyr::rename(wordfr = n) ->
    word_counts
  
  tokens %>%
    pairwise_count(word, docID, sort = TRUE, upper = FALSE) %>% #counts token-pairs co-occuring in docs
    left_join(word_counts, by = c("item1" = "word")) ->
    word_pairs #counts token pairs cooccuring in docs
  
  row_thresh = min(nrow(word_pairs), max_edges)
  
  set.seed(1234) #setting a seed so results are replicable
  word_pairs %>%
    filter(n >= 3) %>%
    top_n(row_thresh) %>%
    igraph::graph_from_data_frame() ->
    plot_d
  
  dfwordcloud = data_frame(vertices = names(V(plot_d))) %>% left_join(word_counts, by = c("vertices"= "word"))
  
  plot_obj = plot_d %>% # graph object built
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4")  +
    # geom_node_point(size = 5) +
    geom_node_point(size = log(dfwordcloud$wordfr)) +
    geom_node_text(aes(label = name), repel = TRUE, 
                   point.padding = unit(0.2, "lines"),
                   size = 1 + log(dfwordcloud$wordfr)) +
    theme_void()
  
  return(plot_obj)    # must return func output
  
}  # func ends

