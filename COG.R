# Usage in local R: 
#   if (!require(RCurl)) {install.packages("RCurl")}; library(RCurl)
#   COG = getURL("insert raw GIT URL", ssl.verifypeer=FALSE)
#   eval(parse(text=COG))
#   COG(x_dtm, title=<text>, central.nodes=<colour>, max.connexns=<number>)

COG = function(dtm, # input dtm
                       title="COG", # title for the graph
                       central.nodes=4, # no. of central nodes
                       max.connexns = 5){ # max no. of connections  
  
  if (!require(igraph)) {install.packages("igraph")}; library(igraph) #network plotting library
  
  # first convert dtm to an adjacency matrix i.e. 1 is there is a connection, 0 if there is not
  dtm1 = as.matrix(dtm) # need regular matrix for matrix opertions
  adj.mat = t(dtm1) %*% dtm1 # making a square symmatric term-term matrix 
  diag(adj.mat) = 0 # no self-references. So diagonal is 0
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  mat1 = as.matrix(adj.mat[a0[1:50], a0[1:50]]) # take the top 50 items
  
  a = colSums(mat1) # collect column sums into a vector obj a
  b = order(-a) # order the vector in descending order  
  
  mat2 = mat1[b, b] # order both rows and columns along vector b  
  diag(mat2) =  0 # no self-references. So diagonal is 0.
  
  ## find top k adjacencies, row-by-row ##
  
  wc = NULL
  
  for (i1 in 1:central.nodes){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[max.connexns]]
    mat2[i1, mat2[i1,] < thresh1] = 0
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc, word) #appends word
  } # i1 loop ends
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  
  # build and plot a network object
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T) # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:central.nodes] = "green"
  V(graph)$color[(central.nodes+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singles
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
}
