## Ruben Ros December 2018 ## r.s.ros@students.uu.nl | rubenros@live.nl ##


library(wordVectors)
library(ggplot2)
library(udpipe)
library(stringr)
library(reshape2)
library(RColorBrewer)


setwd("~/GitHub/TheForeign/SCC/output-data")
w2v_model <- read.vectors("model-whole.bin", binary = T)

## SET LANGUAGE AND DOWNLOAD POS TAGGER ##

l = "dutch"
dl <- udpipe_download_model(language = l)
udmodel_pos <- udpipe_load_model(file = dl$file_model)
tag_f <- function(arg1) {
  x <- as.data.frame(udpipe_annotate(udmodel_pos, x = arg1, tagger = "default", parser = "none") )
  tags <- paste(x$token, sep = '_', x$upos)
  tags
}

df <- read.csv("bigrams_buitenlandsche.csv", sep = ",")

tfd <- function(input,input_df, s_year, e_year, model, maxv){
  df <- read.csv(input_df, sep = ",")
  df[is.na(df)] <- 0
  
  w <- as.character(closest_to(model, input, 15)$word[2:15])

  for(i in seq(1,19,1)){
    tmp <- as.character(closest_to(model, w[i], 15)$word[2:15])
    w <- c(w,tmp)
  }
  
  w <- tag_f(w)
  w <- w[grepl("NOUN", w)]
  w <- gsub("_NOUN", "", w)
  w <- w[duplicated(w)]
  wc <- as.numeric(length(unique(w)))
  w <- unique(w)
  
  
  
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
  
  
  graph_max <- maxv
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


tfd("soorten", "bigrams_vreemde.csv", 1815, 1914, w2v_model, 20)

##### CUSTOM WORDS

tfdv <- function(input_vector,input_df, s_year, e_year, model){
  df <- read.csv(input_df, sep = ",")
  df[is.na(df)] <- 0
  
  totals = as.data.frame(colSums(df[,c(2:as.numeric(ncol(df)))]))
  totals$year = as.numeric(gsub("X","", rownames(totals)))
  colnames(totals) = c("total", "year")
  
  ss <- df[df[,1] %in% input_vector,]
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
    #ggtitle(paste0("Combined Relative Frequency of Words Most Similar to: ", input)) +
    guides(fill=guide_legend(title="Words")) +
    scale_x_continuous(expand = c(0,0), limits = c(s_year,e_year)) + 
    scale_y_continuous(expand = c(0,0), limits = c(0,graph_max)) +
    scale_fill_manual(values = myPal)
  
} #input_df must be a file name: "total-en-bigrams.csv" or "total-ne-bigrams.csv"


search_ts = c("vorst", "vorsten", "hertog", "hertogen", "prins", "prinsen", "koning", "koningen", "koningin", "koninginen", "keizer", "keizers", "regent", "prinsregenten")
search_ts2 = c("gezant", "gezanten", "afgevaardigde", "afgevaardigden", "vertegenwoordiger","vertegenwoordigers")
search_ts3 = c('vraagstukken', 'vraagstuk', 'quaestie', 'aangelegenheid', 'aangelegenheden')

tfdv(c('landen', 'mogendheden', 'natien'), "bigrams_buitenlandsche.csv", 1815, 1915, w2v_model)



