# Week 8 Social Networks

## 8.1 Social Network and Social Analysis

### Just lecture no code in this section

## 8.2.1 Visualizing Social Networks

library(igraph)

setwd("J:/My Drive/Graduate School/IST719 Vizualization/")

link.data <- read.csv("Files for Class from 2U/links-421-719network.csv", header = T, stringsAsFactors = F)

node.data <- read.csv("Files for Class from 2U/nodes-421-719network.csv",
                      header = T, stringsAsFactors = F)


colnames(link.data)
colnames(link.data) <- gsub("\\.","",colnames(link.data))
link.data$X <- gsub(" |-", "", link.data$X)
cbind(link.data$X, colnames(link.data)[-1])

node.data$Name <- gsub(" |-","",node.data$Name)
cbind(link.data$X, node.data$Name)

M <- as.matrix(link.data[,-1])
rownames(M) <- colnames(M)

any(is.na(M))
M[is.na(M)] <- 0
M[M>1]

g <- graph_from_adjacency_matrix(M)

#8.2.3

vcount(g)
ecount(g)
plot.igraph(g)

g <- simplify(g)
par(mar=c(0,0,0,0))
plot.igraph(g, edge.arrow.size = 0, edge.arrow.width = 0)

E(g)$arrow.size <- 0
E(g)$arrow.width <- 0
plot.igraph(g)

V(g)$color <- 'gold'
V(g)$frame.color <- 'white'
V(g)$label.color <- 'black'
E(g)$color <- 'cadetblue'
V(g)$size <- 5
plot.igraph(g)
?igraph.plotting

E(g)$curved <- .4
plot.igraph(g)

#8.3.1
par(mar = c(3,10,1,1))
barplot(sort(degree(g)), horiz = T, las = 2)

V(g)$degree <- degree(g)
V(g)$deg.out <- degree(g, mode = "out")
V(g)$deg.in <- degree(g, mode = "in")

barplot(V(g)$deg.out, horiz = T
        , las = 2, names.arg = V(g)$name)

barplot(V(g)$deg.in, horiz = T
        , las = 2, names.arg = V(g)$name)

g.bak <- g

g2 <- g

V(g2)$close <- closeness(g2, normalized = T, mode ='all')
V(g2)$bet <- betweenness(g2, directed = F)

library(plotrix)
my.pallet <- colorRampPalette(c('steelblue1','violet','tomato','red','red'))

V(g2)$color <- rev(
  my.pallet(200))[round(1+rescale(V(g2)$close, c(1,199)),0)]

plot.igraph(g2)

V(g2)$size <- 2+rescale(V(g)$degree, c(0,13))
V(g2)$label.cex <- .7 + rescale(V(g2)$bet, c(0,1.25))

#8.3.3
cbind(V(g2)$name, node.data$Name)
g3 <- g2

V(g3)$class <- node.data$Class
V(g3)$country <- node.data$Country
V(g3)$year <- node.data$year

g3 <- delete.vertices(g3, "JoHunter")

plot.igraph(g3)

V(g3)$shape <- "circle"
V(g3)$shape[V(g3)$class=='Wednesday'] <- "square"
V(g3)$shape[V(g3)$class=='Both'] <- "rectangle"

plot.igraph(g3)

V(g3)$color <- 'gold'
V(g3)$color[V(g3)$country=='India'] <- 'springgreen4'
V(g3)$color[V(g3)$country=='China'] <- 'red'
V(g3)$color[V(g3)$country=='Both'] <- 'purple'

plot.igraph(g3)

V(g3)$label.color <- 'blue'
V(g3)$label.color[V(g3)$year==1] <- 'black'
plot.igraph(g3)

fc <- cluster_fast_greedy(as.undirected(g3))
print(modularity(fc))
membership(fc)

g4 <- g3

V(g4)$cluster <- membership(fc)
length(fc)
sizes(fc)
par(mar=c(0,0,0,0))
plot_dendrogram(fc, palette= rainbow(7))

# 8.4.1
g5 <- g4
l <- layout_in_circle(g5)
V(g5)$x <- l[,1]
V(g5)$y <- l[,2]
plot.igraph(g5)

g6 <- g5
l3 <- layout_as_star(g6, center = 'LeelaDeshmukh')
E(g6)$color <- 'gray'
E(g6)[from('LeelaDeshmukh')]$color <- 'red'
V(g6)$x <- l3[,1]
V(g6)$y <- l3[,2]
plot.igraph(g6)

g7 <- g6
l4 <- layout_with_kk(g7)
V(g7)$x <- l4[,1]
V(g7)$y <- l4[,2]
plot.igraph(g7)

g8 <- g
V(g8)$x <- 0
V(g8)$y <- 0
plot.igraph(g8)
coord <- cbind(V(g8)$x, V(g8)$y)
iteration <- c(500, 100, 20, 5,3, 2, 1)
for(i in 1:length(iteration)){
  l <- layout_with_fr(g8, coords = coord, dim = 2, niter = iteration[i])
  V(g8)$x <- l[,1]
  V(g8)$y <- l[,2]
  plot.igraph(g8)
  mtext(paste0('Layout FR:', iteration[i]),
        side = 3, line = 0, cex = 1.5, adj = 0)
}

#8.4.3
g9 <- g3

my.linked.list <- data.frame(person = V(g9)$name, event = V(g9)$country)

g9 <- graph_from_data_frame(my.linked.list, directed = F)
V(g9)$type <- F
V(g9)$type[V(g9)$name %in% node.data$Name] <- TRUE
l <- layout_as_bipartite(g9, types = V(g9)$type)
V(g9)$x <- l[,2]
V(g9)$y <- l[,1]
par(mar = c(0,0,0,0))
plot.igraph(g9)
V(g9)$size = 0
plot.igraph(g9)



#Graphs to turn in
pdf("Week 8/Lab8_KW.pdf")
plot.igraph(g)
barplot(sort(degree(g)), horiz = T,
        las = 2, main = "connections")
barplot(V(g)$deg.out, horiz = T
        , las = 2, names.arg = V(g)$name, main = "most friendly")
barplot(V(g)$deg.in, horiz = T
        , las = 2, names.arg = V(g)$name, main = "most popular")
plot.igraph(g2)


plot_dendrogram(fc, palette= rainbow(7))
plot.igraph(g9)

dev.off()

