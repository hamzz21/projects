#installing package 
install.packages("igraph")

#loading the library
library(igraph)

######### Task One ###############

##importing files in r studio
gang_nodes <- read.csv("./Data Set-20240522\\StreetGangNodes.csv")
gang_links <- read.csv("./Data Set-20240522\\StreetGangLinks.csv")

##this is viewing the data frame of the two files that have been imported
head(gang_nodes)
head(gang_links)

##Converting data frames into igraph objects
gang_network <- graph_from_data_frame(d=gang_links,vertices = gang_nodes, directed = FALSE)

##Inspecting nodes attributes of the network
gang_nodes.attributes<- attributes(V(gang_network))
head(attributes(V(gang_network)))

##Inspecting links attributes of the network
gang_links.attributes <- attributes(E(gang_network))
head(attributes(E(gang_network)))

##plotting the network with default settings 
plot(gang_network)

##Setting the links width to weight attribute
gang_links.width <- E(gang_network)$weight

##Setting the node size to Age attribute which is divided by 2
gang_nodes.size <- gang_nodes$Age / 2 

##Plotting the network with attribute settings
plot(gang_network, gang_links.width = gang_links.width, vertex.size = gang_nodes.size)

######### Task Two ###############

##Calculating the centrality measures in the network

##Degree centrality
deg_centrality <- degree(gang_network) 
head(degree(gang_network))

##Betweenness centrality
bet_centrality <- betweenness(gang_network) 
head(betweenness(gang_network))

##Closeness centrality
clo_centrality <- closeness(gang_network) 
head(closeness(gang_network))

##Display nodes in a descending order of the centrality measures

##Degree  
sort_degree <- sort(deg, decreasing = TRUE)
##The top three nodes by degree centrality
degree_top_3 <- names(sort_degree)[1:3] 
head(degree_top_3)

##Betweenness 
sort.betweenness <- sort(bet, decreasing = TRUE)
#the top three nodes by betweenness centrality
betweenness_top_3 <- names(sort.betweenness)[1:3] 
head(betweenness_top_3)

##Closeness
sort.closeness <- sort(clo, decreasing = TRUE)
##the top three nodes by closeness centrality
closeness_top_3 <- names(sort.closeness)[1:3] 
head(closeness_top_3)

######### Task Three ###############

##Simplifying the network

##Removing the nodes with degree < 15
remove_nodes <- V(gang_network)[deg_centrality < 15]

##Removing the nodes in network
gang_network.simple <- delete.vertices(gang_network, remove_nodes)

##Identifying the links with the weight attribute less than 3
remove_links <- E(gang_network.simple)[E(gang_network.simple)$weight < 3]

##Removing the links in network
gang_network.simple <- delete.edges(gang_network.simple,  remove_links)

##Adjusting the network using 'layout_nicely' function
plot(gang_network.simple, layout=layout_nicely(gang_network.simple))

######### Task Four ###############

## Defining the colours for rankings
color_ranks <- c("lightblue", "pink", "lightgreen", "lightyellow", "purple") 
## Assigning the colours based on the ranking
color_nodes <- color_ranks[V(gang_network)$Ranking]  
plot(gang_network, vertex.color = color_nodes)
## Adding legend
legend("topright", legend = unique(V(gang_network)$Ranking), fill = color_ranks, bty = "n")  

##Setting the node colour based on 'Birthplace' attribute and simplify network
##Define colors for birthplaces
birthplace_colors <- c("hotpink", "lightgreen", "lightblue", "yellow", "lavender")
## Assign colors based on birthplace
node_colors <- birthplace_colors[V(gang_network)$Birthplace] 
## Remove nodes where Prison == 0
gang_network.prison <- delete.vertices(gang_network,V(gang_network)$Prison == 0)
plot(gang_network.prison, vertex.color = node_colors[V(gang_network.prison)$Birthplace])
## Add legend
legend("topright", legend = unique(V(gang_network)$Birthplace), fill = birthplace_colors, bty = "n") 

##Delete nodes with ranking < 3, calculate hub scores, and plot network
##Remove nodes with ranking < 3
gang_network.high_rank <- delete.vertices(gang_network.prison, V(gang_network.prison)$Ranking < 3)

# Creating the two-panel plot
par(mfrow=c(1,2))

# Panel 1: Plotting network with node sizes proportional to hub scores
## Calculating the hub scores
hub_scores <- hub_score(gang_network.high_rank)$vector 

plot(gang_network.high_rank, vertex.size = 15 * hub_scores, main = "The Node Size by Hub Score")


# Panel 2: Displaying the communities within the network
## Plotting communities within the network
communities <- cluster_optimal(gang_network.high_rank)
plot(communities, gang_network.high_rank, main = "Communities in the Network")

# Resetting the plotting layout
par(mfrow=c(1,1))



######### Task Five ###############

##Simplifying the network based on the 'Ranking' attribute
network_ranks <- list()
for (rank in unique(V(gang_network)$Ranking)) {
  network_ranks[[as.character(rank)]] <- induced.subgraph(gang_network, V(gang_network)[V(gang_network)$Ranking == rank])
  plot(network_ranks[[as.character(rank)]], main = paste("Ranking", rank)) }

######### Task Six ###############

##creating networks for UK interactions with other the ethnicities

nodes_uk <- V(gang_network)[V(gang_network)$Birthplace == 3]$name
nodes_westafrican <- V(gang_network)[V(gang_network)$Birthplace == 1]$name
nodes_caribbean <- V(gang_network)[V(gang_network)$Birthplace == 2]$name
nodes_eastafrican <- V(gang_network)[V(gang_network)$Birthplace == 4]$name

uk_westafrican <- induced.subgraph(gang_network, c(nodes_uk, nodes_westafrican ))
uk_westafrican <- delete_edges(uk_westafrican,E(uk_westafrican)[weight==1])
# Resetting plotting layout
par(mfrow=c(1,1))
vertex_colors <- birthplace_colors[V(uk_westafrican)$Birthplace]
plot(uk_westafrican, vertex.color = vertex_colors, main = "UK and West African Interactions")

uk_caribbean <- induced.subgraph(gang_network, c(nodes_uk, nodes_caribbean))
uk_caribbean <- delete_edges(uk_caribbean,E(uk_caribbean)[weight==1])
vertex_colors <-  birthplace_colors[V(uk_caribbean)$Birthplace]
plot(uk_caribbean, vertex.color = vertex_colors, main = "UK and Caribbean Interactions")

uk_eastafrican <- induced.subgraph(gang_network, c(nodes_uk, nodes_eastafrican))
uk_eastafrican <- delete_edges(uk_eastafrican ,E(uk_eastafrican )[weight==1])
vertex_colors <-  birthplace_colors[V(uk_eastafrican)$Birthplace]
plot(uk_eastafrican, vertex.color = vertex_colors, main = "UK and East African Interactions")

# Creating the two-panel plotting window
par(mfrow = c(1, 2))
plot(uk_caribbean, vertex.size = node_sizes, vertex.color = vertex_colors, main = "UK and Caribbean Interactions
(Node Size by Authority Score)")
communities <- cluster_optimal(uk_caribbean)
plot(communities, uk_caribbean, main = "Communities within
UK and Caribbean Interactions")


