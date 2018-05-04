# Dependeancy Libraries for Basic Text Analytics

# Install the required pckages, and load them
if (!require(udpipe)){install.packages("udpipe")}; library(udpipe)
if (!require(textrank)){install.packages("textrank")}; library(textrank)
if (!require(lattice)){install.packages("lattice")}; library(lattice)
if (!require(igraph)){install.packages("igraph")}; library(igraph)
if (!require(ggraph)){install.packages("ggraph")}; library(ggraph)
if (!require(wordcloud)){install.packages("wordcloud")}; library(wordcloud)
if (!require(RCurl)) {install.packages("RCurl")}; library(RCurl)
if (!require(tm)) {install.packages("tm")}; library(tm)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if (!require(tidytext)) {install.packages("tidytext")}; library(tidytext)
if (!require(tibble)) {install.packages("tibble")}; library(tibble)
if (!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
if (!require(tidyr)) {install.packages("tidyr")}; library(tidyr)
if (!require(reticulate)) {install.packages("reticulate")}; library(reticulate)
