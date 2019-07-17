df = read.csv('bigrams_buitenlandsche.csv')

dfm = melt(df, id.vars = 'ngram')
dfm$variable = as.numeric(gsub('X', '', dfm$variable))

op_y = c()
op_w = c()
op_c = c()

for(ngram in unique(dfm$ngram)){
  ss = dfm[dfm$ngram == ngram,]
  ss = ss[!ss$value == 0,]
  sy = as.numeric(min(ss$variable))
  c = as.numeric(ss[ss$variable == sy,]$value)
  
  op_y = c(op_y, sy)
  op_w = c(op_w, ngram)
  op_c = c(op_c,c)
}

dfo = as.data.frame(op_y)
dfo$ngram = op_w
dfo$count = op_c

tot = data.frame(matrix(0,150,0))

for(year in seq(1815,1914)){
  ss = dfo[dfo$op_y == as.numeric(year),]
  ss = ss[order(ss$count, decreasing = T),]
  nss= as.numeric(nrow(ss))
  
  if(nss < 150){
  ss = as.character(ss$ngram)
  n = 150 - as.numeric(length(ss))
  n = replicate(n, ".")
  ss = c(ss,n)}
  if(nss >= 150){
    ss=ss$ngram[1:150]
  }
  
  
  ss = data.frame(ss)
  tot = cbind(tot,ss)
}

colnames(tot) = 1815:1914

write.csv(tot, "creativity_bg_buitenlandsche.csv")
get_productivity <- function(file_name, start_year, end_year, csv_delimiter){
  df <- read.csv(file_name, sep = csv_delimiter)
  df <- df[,-1]
  df[is.na(df)] <- 0
  p <- as.data.frame(colSums(df != 0))
  p$years <- gsub("X","", colnames(df))
  colnames(p) <- c('prod', 'years')
  p <- p[p$years >= start_year & p$years <= end_year,]
  p <- melt(p, id.vars = "years")
}
