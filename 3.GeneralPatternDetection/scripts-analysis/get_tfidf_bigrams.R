yearly = c()

for(i in 2:101){
  tmp_df = df[,c(1,i)]
  colnames(tmp_df) = c('ngram', 'year')
  tmp_df = tmp_df[tmp_df$year > 0,]
  
  year_char = c()
  
  
  for(x in 1:as.numeric(nrow(tmp_df))){
    ngram = tmp_df$ngram[x]
    times = tmp_df$year[x]
    tmp = paste(replicate(times, ngram), collapse = " ")
    year_char = c(year_char, tmp)
    
  }
  
  year_char = paste0(year_char, collapse = " ")
  yearly = c(yearly, year_char)
}

dfy = as.data.frame(yearly)







corpus = VCorpus(VectorSource(yearly))
dtm = DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))


termz = c()
for(i in apply(dtm, 1, function(x) {
  x2 <- sort(x, TRUE)
  x2[x2 >= x2[3]]
})){
  termz = c(termz, i)
}



x = apply(dtm, 1, function(x) {
  x2 <- sort(x, TRUE)
  x2[x2 >= x2[3]]
})[1][[1]]








