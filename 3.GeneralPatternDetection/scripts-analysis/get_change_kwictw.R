


df[is.na(df)] = 0

tt = c()

for(i in 3:101){
  tmp = df[,c(1,i)]
  colnames(tmp) = c('w', 'c')
  tmp = tmp[tmp$c > 0,]
  tmp = as.character(tmp$w)
  
  tmpp = df[,c(1,i-1)]
  colnames(tmpp) = c('w', 'c')
  tmpp = tmpp[tmpp$c > 0,]
  tmpp = as.character(tmpp$w)
  
  tmpr = as.numeric(length(setdiff(tmp, tmpp)))
  tmpr = tmpr / length(tmp) * 100
  
  tt = c(tt,tmpr)
}

dfr = as.data.frame(tt)
dfr$y = as.numeric(colnames(df)[3:101])
colnames(dfr) = c('c', 'y')


library(ggplot2)

ggplot(dfr, aes(x=y,y=c)) + geom_bar(stat = 'identity')
