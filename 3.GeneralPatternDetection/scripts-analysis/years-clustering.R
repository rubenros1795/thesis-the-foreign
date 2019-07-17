setwd("~/GitHub/TheForeign/SCC/output-data")

df <- read.csv("bigrams_buitenlandsche.csv", sep = ",")
df[is.na(df)] <- 0


get_bigrams_year <- function(x){
  tmp1 <- df[,c(1,x)]
  tmp1 <- tmp1[tmp1[,2] != 0,]
  as.character(tmp1$ngram)
}

matrix_df <- data.frame(matrix(0,100,0))

for(i in seq(2, 101, 1)){
  tmp <- get_bigrams_year(i)
  
  tmp_col <- data.frame(matrix(0,0,1))
  
  
  for(z in seq(1,100, 1)){
    
    tmp_2 <- get_bigrams_year(z)
    tmp_2 <- as.data.frame(as.numeric(length(tmp[tmp %in% tmp_2])))
    tmp_col <- rbind(tmp_col, tmp_2)
    
  }
  
  matrix_df <- cbind(matrix_df, tmp_col)
}

colnames(matrix_df) <- colnames(df)[2:101]
rownames(matrix_df) <- colnames(df)[2:101]

d <- dist(matrix_df) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# Make Df of results

dfres = as.data.frame(fit$points)
dfres$V3 = as.numeric(gsub('X', '', rownames(dfres)))
dfres$V3 = dfres$V3 - dfres$V3 %% 10
dfres$V4 = rownames(dfres)

dfres = dfres[c(2:100),]

ggplot(dfres, aes(x=V1, y=V2, fill = V3)) +geom_text(aes(label=V4, colour = V4)) + theme(legend.position="none")

