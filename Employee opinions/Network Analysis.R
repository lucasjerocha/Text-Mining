### Network Analysis ###

### Co-occurence Networks and Social Networks ###

library(tidyverse)
library(tidytext)
library(ggraph)
library(widyr)
library(igraph)
library(stopwords)
library(textstem)

opinions <- read_csv2("employee_opinions.csv") %>% select(-topic)
opinions
opinions$assessment <- as.factor(opinions$assessment)
glimpse(opinions)

opinion_tok1 <- opinions %>% unnest_tokens(word, comment) %>%
  mutate(word = str_remove_all(word, "[^[:alpha:]]")) %>%
  filter(!word %in% stopwords("en")) %>% 
  mutate(word = lemmatize_words(word)) %>% filter(word != "")
opinion_tok1

# Word co-occurrence networks (co-word analysis) 

co_occurrence <- opinion_tok1 %>%
  pairwise_count(word, commentID, sort = TRUE) # Count pairs of items within a group 
co_occurrence

# Create graph 

graph <- co_occurrence %>%
  filter(n > 2) %>% # To reduce noise in the network 
  graph_from_data_frame(directed = FALSE) # An undirected graph 

# Plot graph 

ggraph(graph, layout = "fr") + # Fruchterman-Reingold layout
  geom_edge_link(aes(width = n), edge_colour = "lightblue", show.legend = FALSE) + # To add the edges to the graph
  geom_node_point(size = 3) + # Adds the nodes of the graph as points
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = Inf) # Adds labels

### Centrality metrics ###
# ---Degree centrality: Measures the number of connections a node has in a network
# ---Eigenvector centrality: Evaluates the importance of a node based on the connections it has with other well-connected nodes
# ---Betweenness centrality: Quantifies the extent to which a node lies on the shortest paths between other nodes in the network
# ---Closeness centrality: Assesses how close a node is to all other nodes in the network, based on the shortest path lengths

centrality <- data.frame(
  degree = degree(graph),
  closeness = closeness(graph, normalized = TRUE), # normalized to be between 0 and 1
  betweenness = betweenness(graph, normalized = TRUE),
  eigenvector = eigen_centrality(graph)$vector) # $vector extracts the eigenvector centrality vector
centrality

# Display top 10 nodes based on centrality metrics
centrality %>% select(degree) %>% arrange(desc(degree)) %>% head(10)
centrality %>% select(closeness) %>% arrange(desc(closeness)) %>% head(10)
centrality %>% select(betweenness) %>% arrange(desc(betweenness)) %>% head(10)
centrality %>% select(eigenvector) %>% arrange(desc(eigenvector)) %>% head(10)

### Social Networks ###

# Read social network data 

data <- read.csv("network.csv")
glimpse(data)

# Create graph

net <- graph.data.frame(data, directed = FALSE)
net

# Plot social network 

plot(net, vertex.size = 15, vertex.label.cex = 1)
# Oscar is a bridge in the network

centrality <- data.frame(
  degree = degree(net),
  closeness = closeness(net, normalized = TRUE),
  betweenness = betweenness(net, normalized = TRUE),
  eigenvector = eigen_centrality(net)$vector)
centrality

# Display centrality metrics for social network

centrality %>% select(degree) %>% arrange(desc(degree))
centrality %>% select(eigenvector) %>% arrange(desc(eigenvector))
centrality %>% select(closeness) %>% arrange(desc(closeness))
centrality %>% select(betweenness) %>% arrange(desc(betweenness))
