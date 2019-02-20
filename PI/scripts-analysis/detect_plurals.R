df$type = ""

for(i in 1:2522){
  word = df$ngram[i]
  
  if(paste0(word, "en") %in% df$ngram){
    rn_ev = which(df$ngram == word)
    df$type[rn_ev] = "ev"
    
    rn_mv = which(df$ngram == paste0(word, "en"))
    df$type[rn_mv] = "mv"
    
    }
  
}

tmp_mv = df[df$type == "mv",]
tmp_ev = df[df$type == "ev",]

tmp_mv= colSums(tmp_mv[,c(2:101)])
tmp_ev= colSums(tmp_ev[,c(2:101)])
tmp_tot = colSums(df[,c(2:101)])

dfc = as.data.frame(colnames(df)[2:101])
dfc$mv = tmp_mv
dfc$ev = tmp_ev
dfc$tot = tmp_tot

dfc$mv = dfc$mv / dfc$tot * 100
dfc$ev = dfc$ev / dfc$tot * 100
dfc = melt(dfc, id.vars = 'year')
ggplot(dfc, aes(x=year, y=value, fill = variable)) + geom_area()