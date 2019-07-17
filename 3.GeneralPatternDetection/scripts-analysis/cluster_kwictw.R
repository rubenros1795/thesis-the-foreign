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


setwd("~/Scriptie/Data/kwic_dfs/top_bigrams_top_words")
df <- read.csv("het_buitenland_1815_1914_c48.csv")
df[is.na(df)] = 0
colnames(df)[1] = "ngram"
colnames(df)[2:ncol(df)] = as.numeric(gsub("X", "", colnames(df)[2:ncol(df)]))
model_vocabulary = as.character(read.table('~/Scriptie/Data/kwic_dfs/top_bigrams_top_words/het_buitenland_dm.txt')$V1)


model = read.vectors('~/GitHub/TheForeign/SCC/output-data/model-whole.bin')

matrix_df <- as.data.frame(read.csv('~/Scriptie/Data/kwic_dfs/top_bigrams_top_words/het_buitenland_dm.csv'))
rownames(matrix_df) = as.character(matrix_df$X)
matrix_df = matrix_df[,-1]
colnames(matrix_df) = rownames(matrix_df)

token_total = read.csv('~/GitHub/TheForeign/SCC/output-data/tokens_year.csv')

### Modify Files and Set Function

get_cluster <- function(clusn, input_matrix){
  clust <- kmeans(input_matrix, clusn, nstart = 20)
  clust_centroids = clust$centers
  clust <- as.data.frame(clust$cluster)
  
  clust$names <- rownames(input_matrix)
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
    tmp <- unique(as.character(tmp$name[1:12]))
    tmp <- paste0(tmp, collapse = " | ")
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
  tt = token_total[token_total$year >= start_year,]
  tt = tt[tt$year <= end_year,]
  
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
  
  for(i in seq(1,ncol(output) -1)){
    output[,i] = output[,i] / tt$tokens * 100
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
          legend.background = element_rect(fill="grey95", 
                                           size=0.5, linetype="solid", colour ="grey95")) +
    guides(fill=guide_legend(title="Clusters", nrow=clusn,byrow=TRUE)) +
    scale_fill_manual(values = myPal)
  main
  #list(main, output, clust_df)
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

### Plot
get_clusters_period_int(df, 1815, 1914, 12, model, matrix_df)

#### Plot Most Similar

l = "dutch"
dl <- udpipe_download_model(language = l)
udmodel_pos <- udpipe_load_model(file = dl$file_model)
tag_f <- function(arg1) {
  x <- as.data.frame(udpipe_annotate(udmodel_pos, x = arg1, tagger = "default", parser = "none") )
  tags <- paste(x$token, sep = '_', x$upos)
  tags
}

setwd("~/Scriptie/Data/kwic_dfs/top_bigrams_top_words")

tfd <- function(input,input_df, s_year, e_year, model){
  df <- read.csv(input_df, sep = ",")
  df[is.na(df)] <- 0
  
  w <- as.character(closest_to(model, input, 30)$word[1:30])
  
  for(i in seq(1,30,1)){
    tmp <- as.character(closest_to(model, w[i], 30)$word[1:30])
    w <- c(w,tmp)
  }
  
  w <- tag_f(w)
  w <- w[grepl("NOUN", w)]
  w <- gsub("_NOUN", "", w)
  w <- w[duplicated(w)]
  wc <- as.numeric(length(unique(w)))
  w <- unique(w)
  
  
  colnames(df)[1] = "ngram"
  
  totals = as.data.frame(colSums(df[,c(2:as.numeric(ncol(df)))]))
  totals$year = as.numeric(gsub("X","", rownames(totals)))
  colnames(totals) = c("total", "year")
  
  ss <- df[df[,1] %in% w,]
  words <- as.character(ss$ngram)
  
  ss <- setNames(data.frame(t(ss[,-1])), ss[,1])
  ss$year <- totals$year
  
  for(i in 1:as.numeric(ncol(ss) - 1)){
    ss[,i] = ss[,i] / totals$total * 100
  }
  
  ss <- melt(ss, id.vars = "year")
  ss[is.na(ss)] <- 0
  ss$value <- as.numeric(ss$value)
  ss <- ss[ss$year >= s_year & ss$year <= e_year,]
  
  
  graph_max <- as.numeric(max(ss$value))
  
  cols <- colorRampPalette(brewer.pal(12, "Set3"))
  myPal <- cols(as.numeric(length(unique(ss$variable))))
  
  ggplot(ss, aes(year, value, fill = variable)) + geom_area() + 
    xlab('years') +
    ylab('Relative Frequency (compared to all bigrams in that year)') +
    ggtitle(paste0("Combined Relative Frequency of Words Most Similar to: ", input)) +
    guides(fill=guide_legend(title="Words")) +
    scale_x_continuous(expand = c(0,0), limits = c(s_year,e_year)) + 
    scale_y_continuous(expand = c(0,0), limits = c(0,graph_max)) +
    scale_fill_manual(values = myPal)
  
} #input_df must be a file name: "total-en-bigrams.csv" or "total-ne-bigrams.csv"

tfd("vereeniging", "het_buitenland_1815_1914_c48.csv", 1815, 1915, model)
