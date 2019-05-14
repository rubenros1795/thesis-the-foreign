## Ruben Ros February 2019 ## r.s.ros@students.uu.nl | rubenros@live.nl ##

## Code for clustering bigrams and plotting the combined relative frequency of clusters
## across time. 

library(wordVectors)
library(ggplot2)
library(udpipe)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(gridExtra)

# Set Working Directory
setwd("~/GitHub/TheForeign/SCC/output-data")
# Import objects

df = read.csv('bigrams_vreemde.csv')
model = read.vectors('model-whole.bin')
#model_vocabulary = as.character(read.table('model-whole-vocab.txt')$V1)
matrix_df <- as.data.frame(read.csv("bigrams_vreemde_dm.csv"))
rownames(matrix_df) = matrix_df$X
matrix_df = matrix_df[,-1]


### Make sure colnames are integers (years)
colnames(df)[2:ncol(df)] = as.numeric(gsub("X", '', colnames(df)[2:ncol(df)]))

#### Define functions
get_cluster <- function(clusn, input_matrix){
  clust <- kmeans(input_matrix, clusn, nstart = 20)
  clust <- as.data.frame(clust$cluster)
  
  clust$names <- rownames(input_matrix)
  colnames(clust) <- c("cluster", "name")
  clust$cluster <- as.numeric(clust$cluster)
  
  # Get Names
  names_col <- c()
  for(i in seq(1,clusn,1)){
    tmp <- clust$name[clust$cluster == i]
    
    tmp_rs = df
    #tmp_rs$ngram = word(tmp_rs$ngram,2)
    tmp_rs = tmp_rs[tmp_rs$ngram %in% tmp,]
    tmp_rs$rs = rowSums(tmp_rs[,c(2:100)])
    
    tmp_rs = tmp_rs[order(tmp_rs$rs, decreasing = T),]
    
    tmp_rs = tmp_rs$ngram[1:15]
    
    
    tmp <- paste0(tmp_rs, collapse = " | ")
    names_col <- c(names_col, tmp)
  }
  
  clust$centroid = ""
  
  for(i in 1:clusn){
    clust$centroid[clust$cluster == i] = names_col[i]
  }
  
  clust
  
}


get_clusters_period_tt <- function(input_df, start_year, end_year, clusn, model, matrix){
  df = input_df[, which(names(input_df) %in% c(colnames(input_df[1]), start_year:end_year))]
  
  clust_df = get_cluster(clusn, matrix)
  
  output <- data.frame(matrix(0,as.numeric(ncol(df) - 1),0))
  
  for(i in seq(1,clusn,1)){
    tmp <- clust_df[clust_df$cluster == i,]
    name_clus = as.character(unique(tmp$centroid))
    tmp <- as.character(tmp$name)
    tmp <- as.data.frame(df[df$ngram %in% tmp,])
    tmp[is.na(tmp)] = 0
    cs <- as.data.frame(colSums(tmp[,c(2:as.numeric(ncol(tmp)))]))
    
    colnames(cs) <- name_clus
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
    theme(legend.position = "bottom", 
          legend.text=element_text(size=12),
          legend.title = element_blank(),
          #axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.background = element_rect(fill="grey95", 
                                           size=0.5, linetype="solid", colour ="grey95")) +
    guides(fill=guide_legend(title="Clusters", nrow=clusn,byrow=TRUE)) +
    scale_fill_manual(values = myPal)
  main
}



get_clusters_period_int <- function(input_df, start_year, end_year, clusn, model, matrix){
  df = input_df[, which(names(input_df) %in% c(colnames(input_df[1]), start_year:end_year))]
  
  clust_df = get_cluster(clusn, matrix)

  output <- data.frame(matrix(0,as.numeric(ncol(df) - 1),0))
  
  for(i in seq(1,clusn,1)){
    tmp <- clust_df[clust_df$cluster == i,]
    name_clus = as.character(unique(tmp$centroid))
    tmp <- as.character(tmp$name)
    tmp <- as.data.frame(df[df$ngram %in% tmp,])
    tmp[is.na(tmp)] = 0
    cs <- as.data.frame(colSums(tmp[,c(2:as.numeric(ncol(tmp)))]))
    
    colnames(cs) <- name_clus
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
    theme(legend.position = "bottom", 
          legend.text=element_text(size=12),
          legend.title = element_blank(),
          #axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.background = element_rect(fill="grey95", 
                                           size=0.5, linetype="solid", colour ="grey95")) +
    guides(fill=guide_legend(title="Clusters", nrow=clusn,byrow=TRUE)) +
    scale_fill_manual(values = myPal)
  main
}



get_clusters_period_tt(df, 1815, 1914, 10, model, matrix_df)


