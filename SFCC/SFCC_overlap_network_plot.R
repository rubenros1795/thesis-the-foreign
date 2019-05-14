library(igraph)
library(tidygraph)
library(tidyr)
library(tidyverse)
library(ggraph)
setwd("C:/Users/Ruben/Desktop")
op_list = list()

list_years = c("1840", "1855", "1870", "1885", "1900", "1914")

for(i in seq(1,6)){
  fn = paste0("ntw_", list_years[i], ".csv")
  df = read_csv(fn)
  df$weight = round(df$weight)
  
  sources <- df %>%
    distinct(source) %>%
    rename(label = source)
  
  targets <- df %>%
    distinct(target) %>%
    rename(label = target)
  
  nodes = full_join(sources, targets, by = "label")
  nodes <- nodes %>% rowid_to_column("id")
  
  per_route <- df %>%  
    group_by(source, target) %>%
    summarise(weight = weight) %>% 
    ungroup()
  
  edges <- per_route %>% 
    left_join(nodes, by = c("source" = "label")) %>% 
    rename(from = id)
  
  edges <- edges %>% 
    left_join(nodes, by = c("target" = "label")) %>% 
    rename(to = id)
  
  
  edges <- select(edges, from, to, weight)
  
  library(network)
  
  routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
  par(mar=c(1,1,1,1))
  
  plot(routes_network, vertex.cex = 3)
  
  routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
  
  routes_tidy %>% 
    activate(edges) %>% 
    arrange(desc(weight))
  
  p = ggraph(routes_tidy,layout = 'linear', circular = T) +
    geom_node_point(size = 7, shape = 21, stroke = 1, fill = 'grey44', color = 'black') +
    geom_edge_link(start_cap = circle(4, 'mm'),
                   end_cap = circle(4, 'mm'),
                   aes(color = weight, width = weight),
                   show.legend = F) +
    geom_node_text(aes(label = label), repel = T, size = 5) +
    scale_edge_width(range = c(0, 4)) +
    coord_fixed() +
    scale_x_continuous(expand = c(0.3,0.3)) +
    scale_y_continuous(expand = c(0.1,0.1)) +
    scale_colour_gradient2() +
    scale_edge_colour_gradient(low = "#FFDB6D", high = "#00AFBB") + 
    theme_graph() +
    labs(title = paste0("Vocabulary overlap in ", list_years[i]))
  
  assign(paste0("g", i), p)
  
}
  
  

library(gridExtra)
grid.arrange(g1, g2, g3, g4,g5, g6, nrow = 3, ncol = 2)

