make.barchart <- function(df, tokens=15, color="Grey", x="Axis title", y=2006){
  df = head(df, tokens)
  p = ggplot(data=df, aes(x = df$lemma, y = df$n)) + 
    geom_bar(stat = "identity", fill = "Grey") +
    geom_text(aes(label = n), vjust= -0.30) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = y) +
    labs(title = paste0(x,y))
  plot(p)
}