library(igraph)

df = testnw[testnw$clust_year == 1835,]

edgez=c()
for(i in 1:900){
  tmp = df$source[i]
  edgez=c(edgez, tmp)
  tmp = df$target[i]
  edgez=c(edgez, tmp)
}

p = graph(edges = edgez, directed = F)
plot(p, vertex.label = NA, edge.arrow.size=.1)

adj_matrix = get.adjacency(graph.edgelist(as.matrix(df[,c(1,3)]), directed = F))

NET = graph.adjacency(adj_matrix, mode = "undirected", weighted = NULL)

#plot(NET, edge.arrow.size = .01, vertex.label = NA)
#net.sym <- as.undirected(NET, mode = "collapse", edge.attr.comb = list(weight ="sum", "ignore"))
#cliques(net.sym)
#sapply(cliques(net.sym), length)
#largest.cliques(net.sym)
#vcol <- rep("grey80", vcount(net.sym))
#vcol[unlist(largest_cliques(net.sym))] <- "gold"
#plot(as.undirected(net.sym), vertex.label=NA, vertex.color=vcol)

#### COMMUNITY DETECTION

clp <- cluster_label_prop(net)
plot(clp,net, vertex.label = NA)
plot(clp, net, vertex.size=NA, vertex.label.cex = 0.6, vertex_color = membership(clp))

length(clp)
dfmms = as.data.frame(as.numeric(membership(clp)))


adj_matrix = get.adjacency(graph.edgelist(as.matrix(df[,c(1,3)]), directed = F))
# build graph from the adjacency matrix
g = graph.adjacency(adj_matrix, mode="undirected", diag=FALSE)
V(g)$name

# remove loop and multiple edges
g = simplify(g)
wt = walktrap.community(g, steps=5) # default steps=2
table(membership(wt))

# set vertex color & size
nodecolor = rainbow(length(table(membership(wt))))[as.vector(membership(wt))]
nodesize = as.matrix(round((log2(10*membership(wt)))))
nodelayout = layout.fruchterman.reingold(g,niter=1000,area=vcount(g)^1.1,repulserad=vcount(g)^10.0, weights=NULL)

par(mai=c(0,0,1,0)) 
plot(g, 
     layout=nodelayout,
     vertex.size = nodesize,
     #vertex.label=NA,
     vertex.label.cex = 0.8,
     vertex.color = nodecolor,
     edge.arrow.size=0.2,
     edge.color="grey",
     edge.width=1)

                      