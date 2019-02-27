library(stringr)
library(reshape2)
library(wordVectors)
library(RColorBrewer)

setwd("~/GitHub/TheForeign/SCC/output-data")

dfc = read.csv('creativity_bg_buitenlandsche.csv')
dfb = read.csv('bigrams_buitenlandsche.csv')


### Remove Sparse Words & Construct List of Words
dfb$sum = ""

for(i in 1:2522){
  dfb$sum[i] = as.numeric(rowSums(dfb[i,2:101]))
}
dfb = dfb[dfb$sum > 4,]
list_all = c()
for(i in seq(1,100)){
  tmp = as.character(dfc[,i])
  tmp = tmp[!tmp == "."]
  tmp = tmp[tmp %in% as.character(dfb$ngram)]
  list_all = unique(c(list_all, tmp))
}
list_words <- c()
for(i in seq(1,as.numeric(length(list_all)),1)){
  tmp <- as.data.frame(model[[list_all[i]]])
  tmp <- as.character(sum(tmp[1,]))
  if(tmp == "NaN"){
    next
  } else {list_words <- c(list_words, list_all[i])}
}
list_words = unlist(str_extract_all(list_words, "(\\b[^\\s\\d]+\\b)"))
list_words = unlist(str_extract_all(list_words, '\\w{4,}'))
list_words = list_words[!list_words %in% tm::stopwords('dutch')]
list_words = as.character(rownames(matrix_dff))

### Create Distance Matrix
model = read.vectors('model-whole.bin')
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
matrix_df = get_distance_matrix(list_words, model)

### Get Cluster File
nclus = 15
clust <- kmeans(matrix_dff, nclus, nstart = 20)
clust_centroids = clust$centers

clust <- as.data.frame(clust$cluster)
colnames(clust) = 'clust'


### Clean original DF

long_crea = data.frame(matrix(0,0,3))

for(i in seq(1,100)){
  year_column = dfc[,i]
  
  for(word in year_column){
    if(word %in% list_words){
      tmp = data.frame(word)
      tmp$year = colnames(dfc)[i]
      tmp$clust = clust$clust[as.numeric(which(rownames(clust) == word))]
      long_crea = rbind(long_crea, tmp)
    }
    else{next}}
}


long_crea$year = as.numeric(gsub("X", '', long_crea$year))
long_crea$year = long_crea$year - long_crea$year %% 10

df = data.frame(matrix(0,0,3))

for(i in seq(1810,1910,10)){
  
  year_df = long_crea[long_crea$year == i,]
  
  for(c in unique(year_df$clust)){
    year_clust_df = year_df[year_df$clust == c,]
    
    if(nrow(year_clust_df == 0)){
      tmp = as.data.frame(i)
      tmp$clust = c
      tmp$count = 0
      df = rbind(df, tmp)
    }
    
    if(nrow(year_clust_df > 0)){
      tmp = as.data.frame(i)
      tmp$clust = c
      tmp$count = as.numeric(nrow(year_clust_df))
      df = rbind(df, tmp)
    }
    
  }
}


df$clust = as.character(df$clust)

ggplot(df, aes(x = i, y = count,fill=clust)) +
  geom_bar(stat='identity')


# Get Names
clust$name = rownames(clust)

names_col <- c()
for(i in seq(1,nclus,1)){
  tmp <- clust$name[clust$clust == i]
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


cols <- colorRampPalette(brewer.pal(12, "Set3"))
myPal <- cols(nclus)

ggplot(df, aes(x = i, y = count,fill=clust)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = myPal)


#### Relative to Yearly Counts
library(data.table)

dff = aggregate(. ~  i + clust, data = df, sum)

# Set Names
dff$centroid = ""
for(i in seq(1,as.numeric(nrow(dff)))){
  clustno = as.numeric(dff$clust[i])
  dff$centroid[i] = names_col[clustno]
  
  
}

# Create DF with yearly sums
output = data.frame(matrix(0,0,4))

for(i in seq(1810,1910,10)){
  tmp_df = dff[dff$i == i,]
  tmp_df$count = tmp_df$count / sum(tmp_df$count) * 100
  output = rbind(output,tmp_df)
}



ggplot(output, aes(x = i, y = count,fill=centroid)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = myPal)
