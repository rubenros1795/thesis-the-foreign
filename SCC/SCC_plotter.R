library(reshape2)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

setwd("~/GitHub/TheForeign/SCC/output-data")
df = read.csv('tokens_npp_year.csv', sep = ",")

dfm = melt(df, id.vars = 'npp')
dfm$value = as.numeric(as.character(dfm$value))
dfm$variable = as.numeric(gsub('X', '', dfm$variable))

library(ggplot2)
cols <- colorRampPalette(brewer.pal(12, "Set3"))
myPal <- cols(21)
ggplot(dfm, aes(x=variable, y=value, fill=npp)) + 
  geom_bar(stat='identity') + 
  scale_fill_manual(values = myPal) + 
  scale_x_continuous(breaks = round(seq(1815,1914,10),1), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal')


dfss = dfm[dfm$variable < 1877,]
ggplot(dfss, aes(x=variable, y=value, fill=npp)) + geom_area() + scale_fill_manual(values = myPal) + scale_x_continuous(breaks = round(seq(1815,1914,10),1))
