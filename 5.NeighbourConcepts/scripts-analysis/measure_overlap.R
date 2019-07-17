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
library(zoo)
library(dplyr)

# Set Working Directory and Import Dfs
setwd("~/GitHub/TheForeign/SCC/output-data")

dv1 = read.csv("bigrams_internationale.csv")
dv2 = read.csv("bigrams_buitenlandsche.csv")

nrows = 71
start_year = 1845
end_year = 1914



### Percentage of words in dv1 also in dv2
overlap_year = c()
top_overlap = as.data.frame(matrix(0,0,3))

for(i in 2:nrows){
  dv1a = dv1[,c(1,i)]
  dv1a = dv1a[dv1a[,2] > 0,]
  dv2a = dv2[,c(1,i)]
  dv2a = dv2a[dv2a[,2] > 0,]
  
  v1 = as.character(dv1a$ngram)
  v2 = as.character(dv2a$ngram)
  
  overlapping_words = v1[v1 %in% v2]
  ovpc = as.numeric(length(overlapping_words)) / as.numeric(length(v1)) * 100
  overlap_year = c(overlap_year, ovpc)
}

dfv1 = data.frame(overlap_year)
dfv1$year = c(start_year:end_year)
colnames(dfv1) = c("overlap1in2", "year")

### Percentage of words in dv2 also in dv1
overlap_year = c()
top_overlap = as.data.frame(matrix(0,0,3))

for(i in 2:nrows){
  dv1a = dv1[,c(1,i)]
  dv1a = dv1a[dv1a[,2] > 0,]
  dv2a = dv2[,c(1,i)]
  dv2a = dv2a[dv2a[,2] > 0,]
  
  v1 = as.character(dv1a$ngram)
  v2 = as.character(dv2a$ngram)
  
  overlapping_words = v2[v2 %in% v1]
  ovpc = as.numeric(length(overlapping_words)) / as.numeric(length(v2)) * 100
  overlap_year = c(overlap_year, ovpc)
}

dfv2 = data.frame(overlap_year)
dfv2$year = c(start_year:end_year)
colnames(dfv2) = c("overlap2in1", "year")

#### Melt and Plot

df = dfv1
df$overlap2in1 = dfv2$overlap2in1
df = melt(df, id.vars = "year")


ggplot(df, aes(x = year, y = value, col = variable)) +
  geom_point(alpha = 0.5) + 
  geom_line(data = df %>%
              group_by(variable) %>%
              mutate(pmi = rollmean(value, 5, na.pad=TRUE)), size =1)
