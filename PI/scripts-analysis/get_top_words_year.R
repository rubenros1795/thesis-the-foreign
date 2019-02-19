library(readr)

lines= read_lines('1831_lines.txt')

library(tm)

library(tidyverse)

b_lines = lines %>% 
  str_subset(pattern = "buitenland")

library(quanteda)

df_kwic = as.data.frame(quanteda::kwic(b_lines, 'polen', window = 12))

lines_kwic = c(as.character(df_kwic$pre), as.character(df_kwic$post))
lines_kwic = paste0(lines_kwic, collapse = ' ')

myStopwords = read.table("C:/Users/Ruben/Documents/Artikelen/Joris/stopwords-nl.txt")$V1
corpuz = VCorpus(VectorSource(lines_kwic))

chunk <- 500
n <- length(myStopwords)
r <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
d <- split(myStopwords,r)

for (i in 1:length(d)) {
  corpuz <- tm_map(corpuz, removeWords, c(paste(d[[i]])))
}

dtm = DocumentTermMatrix(corpuz)

dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 30)

library(reshape2)
library(ggplot2)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
fig <- fig + xlab("Word in Corpus")
fig <- fig + ylab("Count")
print(fig)
