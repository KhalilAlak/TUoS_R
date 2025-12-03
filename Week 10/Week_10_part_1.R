library(WDI)
new_wdi_cache <- WDIcache()
library(tidyverse)
airports<-read_csv("/Users/khalilalakbarzade/Documents/TUoS/TUoS_R/Week 10/airports.dat", col_types = "icccccdddccccc")
routes<-read_csv("/Users/khalilalakbarzade/Documents/TUoS/TUoS_R/Week 10/routes.dat")

install.packages("igraph")
library(igraph)

nodes<-airports %>% select(IATACode, Name, City, Country, longitude, latitude) %>%
filter(IATACode != "\\N") %>%
filter(Country=="United Kingdom")

links<-routes %>% select(Source, Destination, Airline, Plane) %>%
filter(Source %in% nodes$IATACode) %>%
filter(Destination %in% nodes$IATACode)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)

install.packages("intergraph")
library(intergraph)
install.packages("ggnetwork")
library(ggnetwork)
install.packages("sna")
library(sna)
ggplot(net, aes(x=x, y=y, xend=xend, yend=yend))+
geom_edges(colour="red")+
geom_nodes(size=3)+
geom_text(aes(x=x, y=y, label=City))+
theme_blank()+labs(caption='Airport routes')

education<-WDI(indicator=c("SE.PRM.ENRR","SE.SEC.ENRR",
"SE.TER.ENRR","SE.SEC.PROG.ZS","SE.PRM.CMPT.ZS"),
start=2014,
end=2014,
extra= TRUE,
cache=new_wdi_cache)
education<-education[education$region!="Aggregates",]
education<-na.omit(education)
education.features <- education[,7:11]

education.features_scaled <-scale(education.features)
education.distance_matrix <- as.matrix(dist(education.features_scaled))
education.adjacency_matrix <- education.distance_matrix < 1.0

g1<-graph_from_adjacency_matrix(education.adjacency_matrix, mode="undirected")

ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend))+
geom_edges(colour="blue")+
geom_nodes(size=5, colour="grey")+
theme_blank()+
labs(caption='WDI School enrollment and progression datasets')

g1 <- set_vertex_attr(g1, name="Region", value=as.character(education$region))
ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend))+
geom_edges(colour="grey")+
geom_nodes(size=5,aes(colour=Region ))+
theme_blank()+
labs(caption='WDI School enrollment and progression datasets')

# Execise 1
thresholds <- c(0.75, 1.0, 1.25)
distance_methods <- c("euclidean", "manhattan", "maximum")

for (d_method in distance_methods) {
  education.distance_matrix <- as.matrix(dist(education.features_scaled, method = d_method))
  for (thresh in thresholds) {
    education.adjacency_matrix <- education.distance_matrix < thresh
    g1 <- graph_from_adjacency_matrix(education.adjacency_matrix, mode="undirected")
    g1 <- set_vertex_attr(g1, name="Region", value=as.character(education$region))
    print(
      ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend))+
        geom_edges(colour="grey70")+
        geom_nodes(size=4,aes(colour=Region ))+
        theme_blank()+
        labs(caption=paste0('Method: ', d_method, ' | Threshold: ', thresh))
    )
  }
}

#End of Exercise 1

new.g1<-ggnetwork(g1, layout = igraph::layout.random(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend))+
geom_edges(colour="grey")+
geom_nodes(size=5,aes(colour=Region))+
theme_blank()+
labs(caption='WDI School enrollment and progression datasets')

new.g1<-ggnetwork(g1, layout = igraph::layout.fruchterman.reingold(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend))+
geom_edges(colour="grey")+
geom_nodes(size=5,aes(colour=Region))+
theme_blank()+
labs(caption='WDI School enrollment and progression datasets')

new.g1<-ggnetwork(g1, layout = igraph::layout.kamada.kawai(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend))+
geom_edges(colour="grey")+
geom_nodes(size=5,aes(colour=Region))+
theme_blank()+
labs(caption='WDI School enrollment and progression datasets')

# Try different layouts and colour by region/income ----
layouts <- list(
  circle = sna::gplot.layout.circle,
  fruchtermanreingold = sna::gplot.layout.fruchtermanreingold,
  kamadakawai = sna::gplot.layout.kamadakawai
)

for (layout_name in names(layouts)) {
  coords <- layouts[[layout_name]](education.adjacency_matrix, NULL)
  g1 <- graph_from_adjacency_matrix(education.adjacency_matrix, mode="undirected")
  g1 <- set_vertex_attr(g1, name="Region", value=as.character(education$region))
  g1 <- set_vertex_attr(g1, name="Income", value=as.character(education$income))
  net_df <- ggnetwork(g1, layout = coords)  
  print(
    ggplot(net_df, aes(x=x, y=y, xend=xend, yend=yend))+
      geom_edges(colour="grey70")+
      geom_nodes(size=4,aes(colour=Region ))+
      theme_blank()+
      labs(caption=paste0('Layout: ', layout_name, ' | coloured by Region'))
  )
  print(
    ggplot(net_df, aes(x=x, y=y, xend=xend, yend=yend))+
      geom_edges(colour="grey70")+
      geom_nodes(size=4,aes(colour=Income ))+
      theme_blank()+
      labs(caption=paste0('Layout: ', layout_name, ' | coloured by Income'))
  )
}

#End of Execise 2

install.packages("ggdendro")
library(ggdendro)
hc<-hclust(dist(education.features_scaled))
ggdendrogram(hc, rotate=T)+coord_flip()

hc<-hclust(dist(education.features_scaled))
dd.row <- as.dendrogram(hc)
ddata <- dendro_data(dd.row)
ggplot(ddata$segments) +
geom_segment(aes(x=x, y=y, xend=xend, yend=yend))+
geom_point(data=ddata$labels,
aes(x=x, y=y,
colour=education$region[as.numeric(as.character(label))]))+
theme_dendro()+
labs(colour="region", caption='WDI School enrollment and progression datasets')

# Exercise 3
labour_indicators <- c(
  "SL.AGR.EMPL.ZS",   
  "SL.IND.EMPL.ZS",   
  "SL.SRV.EMPL.ZS",   
  "SL.UEM.TOTL.ZS",   
  "SL.UEM.1524.ZS",   
  "SL.TLF.CACT.ZS"    
)

labour <- WDI(
  indicator = labour_indicators,
  start     = 2014,
  end       = 2014,
  extra     = TRUE,
  cache     = new_wdi_cache
)
labour <- labour[labour$region != "Aggregates", ]
labour <- na.omit(labour)

labour_cols <- which(colnames(labour) %in% labour_indicators)
labour_scaled <- scale(labour[, labour_cols])
labour_dist <- dist(labour_scaled, method = "euclidean")
labour_hc <- hclust(labour_dist, method = "complete")

plot(labour_hc, labels = labour$country, main = "Labour indicators dendrogram (2014)")

# End of Exercise 3

install.packages("ggalluvial")
library(ggalluvial)
education %>% group_by(income, region) %>% summarise(count=n()) %>%
ggplot(aes(y=count, axis1=income, axis2=region))+
geom_alluvium(aes(fill=region))+
geom_stratum()+
geom_label(stat="stratum", aes(label = after_stat(stratum)))+
scale_x_discrete(limits=c("Income", "Region"))+
theme(panel.background = element_blank(), axis.line.y = element_blank(),
axis.text.y = element_blank(), axis.ticks = element_blank())+
labs(x="", y="", fill="Region")

data(majors)
ggplot(majors,
aes(x=semester, stratum=curriculum, alluvium=student,
fill=curriculum, label=curriculum))+
geom_stratum()+
geom_flow(stat="alluvium", colour='darkgrey')+
labs(fill="Curriculum")+
theme(legend.position = "bottom", panel.background = element_blank())

#End of Exercise 4

install.packages("networkD3")
library(networkD3)

titanic <- as.data.frame(Titanic)
node_names <- c(levels(titanic$Class), levels(titanic$Sex), levels(titanic$Age))
nodes <- data.frame(name = node_names)

class_to_sex <- titanic %>%
  group_by(Class, Sex, Survived) %>%
  summarise(value = sum(Freq), .groups = "drop") %>%
  mutate(source = match(as.character(Class), node_names) - 1,
         target = match(as.character(Sex), node_names) - 1,
         group  = Survived)

sex_to_age <- titanic %>%
  group_by(Sex, Age, Survived) %>%
  summarise(value = sum(Freq), .groups = "drop") %>%
  mutate(source = match(as.character(Sex), node_names) - 1,
         target = match(as.character(Age), node_names) - 1,
         group  = Survived)

links <- bind_rows(class_to_sex, sex_to_age)

survival_palette <- 'd3.scaleOrdinal().domain(["No","Yes"]).range(["#d73027", "#1a9850"])'

sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value  = "value",
  NodeID = "name",
  LinkGroup = "group",
  colourScale = survival_palette,
  sinksRight = FALSE,
  fontSize = 12,
  nodeWidth = 20
)

# End of Exercise 5

# Final exercise: 
combined <- inner_join(
  education %>% select(iso2c, country, region, income, all_of(colnames(education.features))),
  labour %>% select(iso2c, country, region, income, all_of(labour_indicators)),
  by = c("iso2c", "country", "region", "income")
)

combined_features <- combined %>% select(all_of(colnames(education.features)), all_of(labour_indicators))
combined_scaled <- scale(combined_features)

# Network from distance threshold (adjust threshold if too sparse/dense)
combined_dist <- as.matrix(dist(combined_scaled, method = "euclidean"))
combined_adj <- combined_dist < 1.0

g1 <- graph_from_adjacency_matrix(combined_adj, mode = "undirected")
g1 <- set_vertex_attr(g1, name = "Region", value = as.character(combined$region))
g1 <- set_vertex_attr(g1, name = "Income", value = as.character(combined$income))

ggplot(g1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(colour = "grey70") +
  geom_nodes(size = 4, aes(colour = Region)) +
  theme_blank() +
  labs(caption = "Combined People indicators network (coloured by Region)")

ggplot(g1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(colour = "grey70") +
  geom_nodes(size = 4, aes(colour = Income)) +
  theme_blank() +
  labs(caption = "Combined People indicators network (coloured by Income)")

# Dendrogram on the same combined indicators
combined_hc <- hclust(dist(combined_scaled, method = "euclidean"), method = "complete")
plot(combined_hc, labels = combined$country, main = "Combined People indicators dendrogram")

#End of Final exercise
