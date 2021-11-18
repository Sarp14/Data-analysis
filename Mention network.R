# Mreža spominjanja

library(ggplot2)
library(rtweet)
library(httpuv)
library(tidyr)
library(dplyr)
library(dils)
library(visNetwork)
library(igraph)
library(twitteR)
library(ROAuth)
library(CINNA)
library(corrplot)
library(ggcorrplot)
library(RColorBrewer)
library(networktools)
library(cluster)
library(qgraph)
library(GGally)
library(network)
library(lsa)
library(ggraph)
library(networkD3)
library(hrbrthemes)
library(signnet)
library(devtools)
library(stringr)
library(nortest)
library(tidyverse)
library(tidytext)
library(tidygraph)
library(pageviews)


# Lista spominjanja

statues_tweets_users <- unnest(statues_tweets_users, replied_to, mentioned)
View(statues_tweets_users)

mentioned_edgelist <- statues_tweets_users %>%
  select(-replied_to) %>%
  filter(complete.cases(.)) %>%
  group_by(sender, mentioned) %>%
  summarise(weight = n()) %>%
  ungroup()

# Mreža spominjanja  

men_graph <- graph_from_data_frame(mentioned_edgelist)
men_graph

men_graph <- simplify(men_graph)

no_edges_nodes <- which(degree(men_graph) == 0)
men_graph <- delete_vertices(men_graph, no_edges_nodes)

nodes_men <- data.frame(id = V(men_graph)$name, stringsAsFactors = F)
edges_men <- data.frame(as_edgelist(men_graph), stringsAsFactors = F)
colnames(edges_men) <- c("from", "to")


men_visn <- toVisNetworkData(men_graph)
men_visn$edges$value <- men_visn$edges$weight

men_network <- visNetwork(nodes = men_visn$nodes, 
                          edges = men_visn$edges, 
                          main = 'Mreža spominjanja') %>% 
  visNodes(color = "#00CC33") %>% 
  visEdges(arrows = "to", color = "darkgreen", smooth = T) %>%
  visOptions(highlightNearest = T, nodesIdSelection = T)

men_network

# Merenje centralnosti stepena 
# Ulazna i izlazna centralnost za mrežu spominjanja

deg_in_men <- degree(men_graph, mode = "in", normalized = T)
summary(deg_in_men)

deg_out_men <- degree(men_graph, mode = "out", normalized = T)
summary(deg_out_men)

deg_men_df <- data.frame(node_id = as.integer(V(men_graph)), 
                         in_degree = deg_in_men,
                         out_degree = deg_out_men)



deg_men_df_long <- pivot_longer(data = deg_men_df, 
                                cols = in_degree:out_degree,
                                names_to = 'degree_type', 
                                names_ptypes = list(degree_type = factor()),
                                values_to = 'degree_value')
View(deg_men_df_long)

ggplot(data = deg_men_df_long, 
       mapping = aes(x = node_id, y = degree_value, fill = degree_type)) +
  geom_col(width = 100, position = 'dodge') +
  ggtitle( "In adn Out Degree for Mention network") +
  labs(x = "Node ID", y = "In- and Out-Degrees") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max(deg_men_df_long$node_id),30)) +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram distribucija centralnosti stepena za mrežu spominjanja

max_degree <- max(deg_men_df_long$degree_value)

ggplot(deg_men_df_long, aes(x = degree_value, fill = degree_type)) + 
  geom_histogram(binwidth = 20,  position = "dodge") + 
  scale_fill_discrete(name = "Tip stepena") +
  labs(title = "Raspodela stepena za mrežu spominjanja", x = "Vrednost stepena", y = "Frekvencija") +
  scale_x_continuous(breaks = seq(0,max_degree,200)) +
  theme(plot.title = element_text(hjust = 0.5))


# Vizalizacija ulaznog stepena po boji i izlaznog po velièini èvora


in_degree_colors_men = attr_based_color_gradient(g_attr = deg_men_df$in_degree, 
                                                 pal_end_points = c('#99FF33', '#003300'))

ggnet2(men_graph, directed = T,
       node.color = in_degree_colors_men,
       size = 'outdegree', 
       max_size = 8,
       arrow.size = 5, 
       arrow.gap = 0.005,
       edge.color = '#333333',
       segmet.alpha + 4) + 
  ggtitle('Mention network by In and Out Degree') + 
  theme(plot.title = element_text(hjust = 0.5))


# Centralnost intermedijalnosti 

men_betweenness <- data.frame(node_id = as.integer(V(men_graph)),
                              betwenness = betweenness(men_graph))

summary(men_betweenness$betwenness)

# Èvorovi sa najveæom intermedijalnošæu 

max(men_betweenness$betwenness)
which(men_betweenness$betwenness == max(men_betweenness$betwenness))

# Vizualizacija intermedijalnosti 

ggnet2(men_graph, directed = T,
       node.color = 'orange3',
       size = men_betweenness$betwenness,
       max_size = 15,
       arrow.size = 5, 
       arrow.gap = 0.005,
       edge.color = 'grey46') + 
  ggtitle('Mention betweenness') + 
  theme(plot.title = element_text(hjust = 0.5))

# Centranost ranga

page_men <- page.rank(men_graph, directed = T, weights = E(men_graph)$weight)$vector
page_men <- round(page_men, digits = 1)

summary(page_men)
which(page_men == max(page_men))

# Korelacija mera centralnosti 

men_centr_all <- data.frame(node_id = V(men_graph)$name,
                            in_degree = degree(men_graph, mode = 'in'),
                            out_degree = degree(men_graph, mode = 'out'),
                            betweenness = men_betweenness$betwenness,
                            rank = page_men)

View(men_centr_all)

apply(men_centr_all[0:5000,-1], 2, shapiro.test)
centr_corr_men <- cor(men_centr_all[,-1],
                      use = 'complete.obs',
                      method = 'spearman')


ggcorrplot(centr_corr_men, 
           title = "Korelaciona matrica metrika centralnosti za mrežu spominjanja",
           ggtheme = theme_grey(),
           colors = c("coral3", "white", "dodgerblue4"),
           hc.order = T,
           lab = T,
           method = "square",
           type ="lower")

# Proseèna putanja, gustina ivica i tranzitivnost

transitivity(men_graph, type = 'global')
edge_density(men_graph)

# Raèunanje proseka najkraæe putanje za èvorore u mreži spominjanja

men_dist_in <- distances(men_graph, mode = 'in', 
                         weights = E(men_graph)$weight)
men_dist_out <- distances(men_graph, mode = 'out', 
                          weights = E(men_graph)$weight)
avg_dist_men <- mean_distance(men_graph, directed = T)
avg_dist_men

men_dist_in[is.infinite(men_dist_in)] <- NA
men_dist_in <- apply(men_dist_in, 1, function(x) mean(x, na.rm = T))
summary(men_dist_in)

men_dist_out[is.infinite(men_dist_out)] <- NA
men_dist_out <- apply(men_dist_out, 1, function(x) mean(x, na.rm = T))
summary(men_dist_out)

# Najudaljeniji èvorovi 

most_apart_men <- farthest_vertices(men_graph, weights = men_graph$weight)$vertices
most_apart_men
big_dist_men <- shortest_paths(men_graph, 
                               from = most_apart_men[1], 
                               to = most_apart_men[2],
                               weights = E(men_graph)$weight,
                               output = 'epath')
big_dist_men

# Najveæa distanca u mreži 

diameter(men_graph, directed = T, unconnected = T, weights = NA)

# Izdvajanje najvažnijih èvorova po metrikama centralnosti

most_important_nodes_men <- list()
most_important_nodes_men$in_degree <- men_centr_all %>% 
  select(node_id, in_degree) %>%
  arrange(desc(in_degree))

most_important_nodes_men$out_degree <-  men_centr_all %>%
  select(node_id, out_degree) %>%
  arrange(desc(out_degree))

most_important_nodes_men$betweenness <- men_centr_all %>% 
  select(node_id, betweenness) %>%
  arrange(desc(betweenness))

most_important_nodes_men$rank <-  men_centr_all %>%
  select(node_id, rank) %>%
  arrange(desc(rank))

most_important_nodes_men$in_degree[1:10, ]
most_important_nodes_men$out_degree[1:10, ]
most_important_nodes_men$betweenness[1:10, ]
most_important_nodes_men$rank[1:10, ]

# Klaster analiza uz pomoæ algoritama za detekciju zajednica u mreži 

is_connected(men_graph, mode = 'strong')
men_und <- as.undirected(men_graph, 
                         mode = 'collapse',
                         edge.attr.comb = 'max')
men_giant <- decompose(men_und, mode = 'weak')
biggest_cluster_id_men <- which.max(men_giant$csize)
biggest_cluster_id_men
men_giant <- men_giant[[1]]
men_giant

edge_men <- cluster_edge_betweenness(men_giant, weights = NA, directed = F)

men_louvain <- cluster_louvain(as.undirected(men_giant),weights = E(men_giant)$weight)
men_louvain

men_walktrap <- cluster_walktrap(men_giant, weights = E(men_giant)$weight, steps = 5)
men_walktrap

men_info <- infomap.community(men_giant, e.weights = E(men_giant)$weight)
men_info

modularity_scores_men <- list()
modularity_scores_men$men_louvain <- modularity(men_louvain)
modularity_scores_men$men_walktrap <- modularity(men_walktrap)
modularity_scores_men$men_info <- modularity(men_info)

modularity_scores_men

men_grd <- cluster_louvain(men_giant)
V(men_giant)$community <- men_grd$membership
men_grd

nodes <- data.frame(id = V(men_giant)$name, title = V(men_giant)$name, group = V(men_giant)$community)
nodes <- nodes[order(nodes$id, decreasing = F),]
edges <- get.data.frame(men_giant, what="edges")[1:2]

visNetwork(nodes, edges, main = 'Klasteri mreže spominjanja') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)


ggplot(data = deg_men_df_long, 
       mapping = aes(x = degree_value, fill = degree_type)) +
  geom_histogram(bins = 200, position = 'dodge') +
  geom_density(alpha = 0.2) +
  labs(x = "Degree value", title = "Degree distribution for the Advice network") +
  scale_fill_discrete(name = "Degree type",
                      labels = c("In-degree", "Out-degree")) +
  scale_x_continuous(breaks = seq(0,max_degree,40)) +
  theme_bw()

deg_men <- degree(men_graph)
summary(deg_men)

ggplot(data_frame(y=degree_distribution(men_graph), x=1:length(y))) +
  geom_segment(aes(x, y, xend=x, yend=0), color="slateblue") +
  scale_y_continuous(expand=c(0,0), trans="sqrt") +
  labs(x="Stepen", y="Gustina (sqrt scale)", title="Raspodela stepena za mrežu spominjanja") +
  theme_ipsum_rc(grid="Y", axis="x") 
  



deg_men_df <- data.frame(node_id = as.integer(V(men_graph)),
                         degree_dist = degree(men_graph))

ggplot(deg_men_df,aes(x = log1p(degree_dist))) +
geom_density() +
geom_histogram(binwidth = 0.5, position = "dodge", color = 'darkgreen', fill = 'lightgreen') +
xlab("Stepen") + ylab("Frekvencija") + 
ggtitle("Raspodela stepena za mrežu spominjanja") + 
theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent)  



