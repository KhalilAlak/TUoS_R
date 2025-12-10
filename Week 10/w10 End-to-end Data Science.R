library(igraph)
library(ggnetwork)
library(intergraph)
library(sna)
set.seed(1)
# simulate education data similar structure
n <- 20
education <- data.frame(iso2c=LETTERS[1:n], country=paste0('C',1:n), year=2014,
                        SE.PRM.ENRR=runif(n,50,100), SE.SEC.ENRR=runif(n,50,100),
                        SE.TER.ENRR=runif(n,20,80), SE.SEC.PROG.ZS=runif(n,50,100), SE.PRM.CMPT.ZS=runif(n,60,100),
                        region=rep(c('X','Y','Z','W'), length.out=n), income=rep(c('High','Low'), length.out=n))
education.features <- education[,4:8]
education.features_scaled <- scale(education.features)
thresholds <- c(0.75, 1.0, 1.25)
distance_methods <- c('euclidean','manhattan','maximum')
for (d_method in distance_methods) {
  education.distance_matrix <- as.matrix(dist(education.features_scaled, method = d_method))
  for (thresh in thresholds) {
    education.adjacency_matrix <- education.distance_matrix < thresh
    g1 <- graph_from_adjacency_matrix(education.adjacency_matrix, mode='undirected')
    g1 <- set_vertex_attr(g1, name='Region', value=as.character(education$region))
    g1 <- set_vertex_attr(g1, name='Income', value=as.character(education$income))
    print(ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend)) + geom_edges(colour='grey70') + geom_nodes(aes(colour=Income)))
  }
}
# layout section
layouts <- list(circle=sna::gplot.layout.circle, fruchtermanreingold=sna::gplot.layout.fruchtermanreingold)
education.adjacency_matrix <- education.distance_matrix < 1.0
for (layout_name in names(layouts)) {
  coords <- layouts[[layout_name]](education.adjacency_matrix, NULL)
  g1 <- graph_from_adjacency_matrix(education.adjacency_matrix, mode='undirected')
  g1 <- set_vertex_attr(g1, name='Region', value=as.character(education$region))
  g1 <- set_vertex_attr(g1, name='Income', value=as.character(education$income))
  g1 <- set_vertex_attr(g1, name='x', value=coords[,1])
  g1 <- set_vertex_attr(g1, name='y', value=coords[,2])
  print(ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend)) + geom_edges(colour='grey70') + geom_nodes(aes(colour=Income)))
}
