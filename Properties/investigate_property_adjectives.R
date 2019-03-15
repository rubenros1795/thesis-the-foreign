library(stringr)
library(reshape2)

# POS tagging - Load Model (Package UDPIPE)
library(udpipe)
dl <- udpipe_download_model(language = "dutch")
udmodel_dutch <- udpipe_load_model(file = dl$file_model)

# Set functions for applying POST to dataframe
tag_f <- function(arg1) {
  x <- as.data.frame(udpipe_annotate(udmodel_dutch, x = arg1, tagger = "default", parser = "none") )
  tags <- paste(x$upos)
  tags
}

df$first = word(df$ngram, 1)

df$tag = ""

for(i in 1:2221){
  df$tag[i] = tag_f(df$first[i])
}

dfa = df[df$tag == "ADJ",]
#dfa = as.data.frame(t(dfa))
dfa = dfa[,c(1:101)]

get_productivity <- function(df, start_year, end_year){
  df <- df[,-1]
  df[is.na(df)] <- 0
  p <- as.data.frame(colSums(df != 0))
  p$years <- gsub("X","", colnames(df))
  colnames(p) <- c(paste0("prod"), "years")
  p <- p[p$years >= start_year & p$years <= end_year,]
  p <- melt(p, id.vars = "years")
  p
}

dfp = get_productivity(dfa, 1815, 1914)
