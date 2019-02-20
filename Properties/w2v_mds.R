df = data.frame(matrix(0,0,150))

for(i in seq(1840,1910,5)){
  m = read.vectors(paste0(i, "_static_aligned.bin"))
  tmp = as.data.frame(t(as.numeric(m[['binnenland']])))
  rownames(tmp) = paste0(i)
  df = rbind(df, tmp)
  
  
}





d <- dist(df) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric MDS", type="n")
text(x, y, labels = row.names(df), cex=.7)
