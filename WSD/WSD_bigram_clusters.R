## Ruben Ros December 2018 ## r.s.ros@students.uu.nl | rubenros@live.nl ##

## Code for clustering bigrams and plotting the combined relative frequency of clusters
## across time. Works for English and Dutch bigrams. Bigram .csv need to come in the standard format


library(wordVectors)
library(ggplot2)
library(udpipe)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(gridExtra)


#### IMPORT VECTOR SPACE MODEL ###
setwd("~/GitHub/TheForeign/SCC/output-data") #setwd("~/path/to/models") 

model <- read.vectors("model-whole.bin") #model <- read.vectors("model-english.bin")

#### IMPORT POS TAGGER ###
dl <- udpipe_download_model(language = 'dutch')
udmodel_l <- udpipe_load_model(file = dl$file_model)

tag_f <- function(arg1) {
  x <- as.data.frame(udpipe_annotate(udmodel_l, x = arg1, tagger = "default", parser = "none") )
  if(nrow(x) > 1){x = as.data.frame(x[1,])}
  tags <- paste(x$upos)
  tags
}



#### IMPORT BIGRAMS FOR EXTRACTION WORDS ####
setwd("~/Scriptie/Data/kwic_dfs")
df <- read.csv("buitenlandsche_mogendheden_1815_1914_tw_c48d0.csv", sep = ",")
colnames(df)[1] = 'ngram'

get_words <- function(input_df, input_model){
  input_df[is.na(input_df)] = 0
  input_df$type = ""
  
  for(i in 1:nrow(input_df)){
    input_df$type[i] = tag_f(input_df$ngram[i])
  }
  input_df = input_df[input_df$type == "NOUN",]
  
  total_words <- c()
  
  for(i in 2:ncol(input_df)){
    tmp = input_df[,c(1,i)]
    colnames(tmp) = c('ngram', 'freq')
    tmp <- tmp[tmp$freq > 0,]
    tmp = tmp[order(tmp$freq, decreasing = T),]
    tmp <- as.character(tmp$ngram[1:20])
    total_words <- c(total_words, tmp)
    total_words <- unique(total_words)
    
  }  
  
  list_words <- c()
  for(i in 1:as.numeric(length(total_words))){
    tmp <- as.data.frame(input_model[[total_words[i]]])
    tmp <- as.character(sum(tmp[1,]))
    if(tmp == "NaN"){
      next
    } else {list_words <- c(list_words, total_words[i])}
  }
  
  list_words
  
} 
list_words <- get_words(df, model)


#### GET DISTANCE MATRIX ###
get_distance_matrix <- function(list_of_words, input_model){
  matrix_df <- data.frame(matrix(0,length(list_of_words),0))
  
  for(y in seq(1,length(list_of_words),1)){
    tmp_col <- data.frame(matrix(0,0,1))
    for(z in seq(1,length(list_of_words),1)){
      tmp <- as.data.frame(cosineDist(input_model[[list_of_words[y]]], input_model[[list_of_words[(z)]]]))
      rownames(tmp) <- list_of_words[(z)]
      tmp_col <- rbind(tmp_col, tmp)}
    
    colnames(tmp_col) <- list_of_words[y]
    matrix_df <- cbind(matrix_df, tmp_col)
  } 
  
  matrix_df
  
}

matrix_df <- get_distance_matrix(list_words, model)





#### GET CLUSTER PLOTS

get_cluster <- function(clusn){
  clust <- kmeans(matrix_df, clusn, nstart = 20)
  clust_centroids = clust$centers
  clust <- as.data.frame(clust$cluster)
  
  clust$names <- rownames(matrix_df)
  colnames(clust) <- c("cluster", "name")
  clust$cluster <- as.numeric(clust$cluster)
  
  # Get Names
  names_col <- c()
  for(i in seq(1,as.numeric(length(unique(clust$cluster))),1)){
    tmp <- clust$name[clust$cluster == i]
    tmp <- which(colnames(clust_centroids) %in% tmp)
    tmp <- clust_centroids[,c(tmp)]
    tmp <- as.data.frame(t(tmp))
    tmp$name <- rownames(tmp)
    tmp <- melt(tmp, id.vars = "name")
    tmp <- tmp[order(tmp$value, decreasing = T),]
    tmp <- unique(as.character(tmp$name[1:15]))
    tmp <- paste0(tmp, collapse = " | ")
    names_col <- c(names_col, tmp)
  }
  
  
  #Get Frequency Distrubtion
  
  output <- data.frame(matrix(0,100,0))

  for(i in seq(1,clusn,1)){
    tmp <- clust[clust$cluster == i,]
    tmp <- as.character(tmp$name)
    tmp <- as.data.frame(df[df$ngram %in% tmp,])
    names <- names_col[i]
    cs <- as.data.frame(colSums(tmp[,c(2:as.numeric(ncol(tmp)))]))
    
    colnames(cs) <- names
    output <- cbind(output, cs)
  }
  
  output$total <- rowSums(output)
  
  for(i in seq(1,as.numeric(ncol(output)),1)){
    output[,i] <- output[,i] / output$total * 100
  }
  
  output$total <- NULL
  output$years <- as.numeric(gsub("X","", rownames(output)))
  output <- melt(output, id.vars = "years")
  output$years <- as.numeric(output$years)
  output$value <- as.numeric(output$value)
  
  
  cols <- colorRampPalette(brewer.pal(12, "Set3"))
  myPal <- cols(clusn)
  
  main <- ggplot(output, aes(years, value, fill = variable)) + 
    geom_area() + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values = myPal) +
    theme(legend.position = "bottom", 
          legend.text=element_text(size=12),
          legend.title = element_blank(),
          #axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.background = element_rect(fill="grey95", 
                                           size=0.5, linetype="solid", colour ="grey95")) +
    guides(fill=guide_legend(title="Clusters", nrow=clusn,byrow=TRUE))
  
  
  main
} 

get_cluster(12)

