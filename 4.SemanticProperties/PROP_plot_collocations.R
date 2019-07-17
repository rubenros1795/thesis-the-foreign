df = pmi_scale2

dft = df[df$w1 == "buitenlandsche",]
dft = dft[dft$w2 == "onrustbarend",]

dft = df


dft$year = as.numeric(dft$year)

dft = dft[dft$year > 1815,]
library(zoo)

#ggplot(dft, aes(x=year, y=pmi, colour = w2)) + 
#  geom_point(colour = "grey77") + 
#  geom_line(aes(y=rollmean(pmi, 10, na.pad=TRUE)), group = 1) +
#  ggtitle("Perc. of words following buitenlandsch(e) that is also preceded by vreemd(e)")


ggplot(dft, aes(x = year, y = pmi, col = w2)) +
  geom_point(alpha = 0.1) + 
  geom_line(data = dft %>%
              group_by(w2) %>%
              mutate(pmi = rollmean(pmi, 5, na.pad=TRUE)), size =1)
