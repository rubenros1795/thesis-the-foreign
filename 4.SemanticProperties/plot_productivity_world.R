df = read.csv("unigrams_wereld_clean.csv")
df = df[df$tag == "NOUN",]
library(reshape2)
library(ggplot2)
library(udpipe)

#### IMPORT POS TAGGER ###
dl <- udpipe_download_model(language = "dutch")
udmodel_l <- udpipe_load_model(file = dl$file_model)

tag_f <- function(arg1) {
  x <- as.data.frame(udpipe_annotate(udmodel_l, x = arg1, tagger = "default", parser = "none") )
  if(nrow(x) > 1){x = as.data.frame(x[1,])}
  tags <- paste(x$upos)
  tags
}

for(i in 1:1648){
  df$tag[i] = tag_f(df$ngram[i])
  
}

get_productivity <- function(df, start_year, end_year){
  df <- df[,-1]
  df[is.na(df)] <- 0
  p <- as.data.frame(colSums(df != 0))
  p$years <- gsub("X","", colnames(df))
  colnames(p) <- c("prod", "years")
  p <- p[p$years >= start_year & p$years <= end_year,]
  p <- melt(p, id.vars = "years")
  p
}

d = get_productivity(df, 1815,1914)

theme_cus <- function (base_size = 20, base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22){
  half_line <- base_size/2
  theme_grey(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white",colour = NA), 
          panel.border = element_rect(fill = NA,colour = "grey70", size = rel(1)), 
          panel.grid = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.ticks = element_line(colour = "grey63", size = rel(0.5)),
          axis.text.x =  element_text(color = "grey63"),
          axis.text.y =  element_text(color = "grey63"),
          legend.key = element_rect(fill = "white", colour = NA),
          legend.text= element_text(size=20),
          legend.position="bottom",
          legend.direction="horizontal",
          #legend.position = c(.1,.8),
          legend.title=element_blank(),
          strip.background = element_rect(fill = "grey60", 
                                          colour = NA), strip.text = element_text(colour = "white", 
                                                                                  size = rel(0.8), margin = margin(0.8 * half_line, 
                                                                                                                   0.8 * half_line, 0.8 * half_line, 0.8 * half_line)), 
          complete = TRUE)
}

d$years = as.numeric(d$years)





ggplot(data = d, aes(x = years, y = value)) +
  geom_bar(stat='identity')
