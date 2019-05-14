



df <- read.csv('bigrams_buitenlandsche.csv')
rownames(df) <- df[,1]
df <- df[,-1]
df[is.na(df)] <- FALSE
df[df == 0] <- FALSE
df[df > 0] <- TRUE
df$first <- colnames(df)[ifelse(rowSums(df)==0, NA, max.col(df, "first"))]
df$bigram <- row.names(df)
  
# Get no. of years in which bigram occurs after introduction
df$nylength <- ""
  
for(i in seq(1,as.numeric(nrow(df)),1)){
  bigram_name <- df$bigram[i]
  first_year <- df$first[i]
  if(first_year != colnames(df)[as.numeric(ncol(df) - 2)]){
      df_bg <- df[df$bigram == bigram_name,]
      col_year <- match(first_year,colnames(df))
      df_bg <- df_bg[col_year:as.numeric(ncol(df) - 2)]
      total_span <- as.numeric(ncol(df_bg) - 1)
      df_bg <- as.data.frame(df_bg[df_bg != 0])
      nylength <- nrow(df_bg) - 1
      df$nylength[i] <- nylength / total_span * 100
    } else {df$nylength[i] <- 0}
    
  }

dft = df[df$nylength > 80,]


cy = c()
cl = c()

for(i in unique(dft$first)){
  tmp = dft[dft$first == i,]
  tmp = as.numeric(nrow(dft))
  
  cy = c(cy,i)
  cl = c(cl, tmp)
}

dfr = data.frame(cl,cy)
