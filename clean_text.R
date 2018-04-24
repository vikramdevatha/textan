#function to clean the text, remove any non character and  non digits, and split it into words

clean_text <- function(text){
  
  library(stringr)
  clean.text6 = character()
  for (i in 1:length(text)){
    clean.text1 = tolower(text[i]) #converting entire string to lower case
    clean.text2 = stringr::str_replace_all(clean.text1, "[^a-zA-Z\\s]", "") #replace any non character and non digit with space
    clean.text3 = stringr::str_replace_all(clean.text2, "[\\s]+", " ") #replace multiple spaces into single space
    clean.text4 = str_split(clean.text3, "\\s")[[1]]  #splitting the string into words, using space as a token-sepeartor
    clean.text5 = unique(clean.text4)  #capture only the unique words
    clean.text6 = append(clean.text6, clean.text5) #appending the cleaned up text
  }
  
  to_return = list(num_tokens = length(clean.text6), text = clean.text6)
  return(to_return)
}