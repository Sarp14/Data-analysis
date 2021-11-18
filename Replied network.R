# Mreža odgovora 

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

# Replied list

statues_tweets_users <- unnest(statues_tweets_users,sender, replied_to)

replied_to_edgelist <- statues_tweets_users %>%
  select(sender, replied_to) %>%
  filter(complete.cases(.)) %>%
  group_by(sender, replied_to) %>%
  summarise(weight = n()) %>%
  ungroup()

# Replied network

rep_graph <- graph_from_data_frame(replied_to_edgelist)
rep_graph

rep_graph <- simplify(rep_graph, remove.multiple = F, remove.loops = T)

no_edges_nodes <- which(degree(rep_graph) == 0)
rep_graph <- delete_vertices(rep_graph, no_edges_nodes)

nodes_rep <- data.frame(id = V(rep_graph)$name, stringsAsFactors = F)
edges_rep <- data.frame(as_edgelist(rep_graph), stringsAsFactors = F)
colnames(edges_rep) <- c("from", "to")


rep_visn <- toVisNetworkData(rep_graph)
rep_visn$edges$value <- rep_visn$edges$weight
rep_network <- visNetwork(nodes = rep_visn$nodes, 
                          edges = rep_visn$edges, 
                          main = 'Mreža odgovora') %>% 
  visNodes(color = "#990000") %>% 
  visEdges(arrows = "to", color = "darkred", smooth = T) %>%
  visOptions(highlightNearest = T, nodesIdSelection = T)

rep_network

# Measuring degree centrality for replied network
# In and out degree for replied network

deg_in_rep <- degree(rep_graph, mode = "in")
summary(deg_in_rep)

deg_out_rep <- degree(rep_graph, mode = "out")
summary(deg_out_rep)

deg_rep_df <- data.frame(node_id = as.integer(V(rep_graph)), 
                         in_degree = deg_in_rep,
                         out_degree = deg_out_rep)



deg_rep_df_long <- pivot_longer(data = deg_rep_df, 
                                cols = in_degree:out_degree,
                                names_to = 'degree_type', 
                                names_ptypes = list(degree_type = factor()),
                                values_to = 'degree_value')


ggplot(data = deg_rep_df_long, 
       mapping = aes(x = node_id, y = degree_value, fill = degree_type)) +
  geom_col(width = 10, position = 'dodge') +
  ggtitle( "Ulazna i izlazna centranost za mrežu odgovora") +
  labs(x = "ID èvora", y = "Ulazni i izlazni stepen") +
  scale_fill_discrete(name = "Tip stepena",
                      labels = c("Ulazna centralnost", "Izlazna centralnost")) +
  scale_x_continuous(breaks = seq(0,max(deg_rep_df_long$node_id),30)) +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram of degree distribution for replied network

max_degree <- max(deg_rep_df_long$degree_value)

ggplot(deg_rep_df_long, aes(x = degree_value, fill = degree_type)) + 
  geom_histogram(binwidth = 10, position = "dodge") + 
  scale_fill_discrete(name = "Tip stepena") +
  labs(title = "Distribucija stepena za mrežu odgovora", x = "Vrednost stepena", y = "Broj") +
  scale_x_continuous(breaks = seq(0,max_degree,4)) +
  theme(plot.title = element_text(hjust = 0.5))


# Vizalizacija ulaznog stepena po boji i izlaznog po velièini èvora

in_degree_colors_rep = attr_based_color_gradient(g_attr = deg_rep_df$in_degree, 
                                                 pal_end_points = c('#99FF33', '#003300'))

ggnet2(rep_graph, directed = T,
       node.color = in_degree_colors_rep,
       size = 'outdegree', 
       max_size = 8,
       arrow.size = 5, 
       arrow.gap = 0.005,
       edge.color = '#333333',
       segmet.alpha + 4) + 
  ggtitle('Mreža odgovora za ulaznu i izlaznu centranost') + 
  theme(plot.title = element_text(hjust = 0.5))


# Centralnost intermedijalnosti 

rep_betweenness <- data.frame(node_id = as.integer(V(rep_graph)),
                              betwenness = betweenness(rep_graph))

summary(rep_betweenness$betwenness)

# Èvorovi sa najveæom intermedijalnošæu 

max(rep_betweenness$betwenness)
which(rep_betweenness$betwenness == max(rep_betweenness$betwenness))
View(rep_betweenness)

# Vizualizacija intermedijalnosti 

ggnet2(rep_graph, directed = T,
       node.color = 'orange3',
       size = rep_betweenness$betwenness,
       max_size = 15,
       arrow.size = 5, 
       arrow.gap = 0.005,
       edge.color = 'grey46') + 
  ggtitle('Intermedijalnost za mrežu odgovora') + 
  theme(plot.title = element_text(hjust = 0.5))

# Centranost ranga

page_rep <- page.rank(rep_graph, directed = T, weights = E(rep_graph)$weight)$vector
page_rep <- round(page_rep, digits = 1)

summary(page_rep)
which(page_rep == max(page_rep))

# Korelacija mera centralnosti 

rep_centr_all <- data.frame(node_id = nodes_rep$id,
                            in_degree = degree(rep_graph, mode = 'in'),
                            out_degree = degree(rep_graph, mode = 'out'),
                            betweenness = rep_betweenness$betwenness,
                            rank = page_rep)

View(rep_centr_all)


apply(rep_centr_all[ ,-1], 2, shapiro.test)

centr_corr_rep <- cor(rep_centr_all[,-1],
                      use = 'complete.obs',
                      method = 'spearman')


ggcorrplot(centr_corr_rep, 
           title = "Korelaciona matrica metrika centralnosti za mrežu odgovora",
           ggtheme = theme_grey(),
           colors = c("coral3", "white", "dodgerblue4"),
           hc.order = T,
           lab = T,
           method = "square",
           type ="lower")


# Proseèna putanja, gustina ivica i tranzitivnost

transitivity(rep_graph, type = 'global')
edge_density(rep_graph)


# Raèunanje proseka najkraæe putanje za èvorore u mreži odgovora

rep_dist_in <- distances(rep_graph, mode = 'in', 
                         weights = E(rep_graph)$weight)
rep_dist_out <- distances(rep_graph, mode = 'out', 
                          weights = E(rep_graph)$weight)
avg_dist_rep <- mean_distance(rep_graph, directed = T)
avg_dist_rep

rep_dist_in[is.infinite(rep_dist_in)] <- NA
rep_dist_in <- apply(rep_dist_in, 1, function(x) mean(x, na.rm = T))
summary(rep_dist_in)

rep_dist_out[is.infinite(rep_dist_out)] <- NA
rep_dist_out <- apply(rep_dist_out, 1, function(x) mean(x, na.rm = T))
summary(rep_dist_out)

# Najudaljeniji èvorovi 

most_apart_rep <- farthest_vertices(rep_graph, weights = rep_graph$weight)$vertices
most_apart_rep
big_dist_rep <- shortest_paths(rep_graph, 
                               from = most_apart_rep[1], 
                               to = most_apart_rep[2],
                               weights = E(rep_graph)$weight,
                               output = 'epath')
big_dist_rep

# Najveæa distanca u mreži 

diameter(rep_graph, directed = T, unconnected = T, weights = NA)

giant_com_rep <- components(rep_graph, mode = 'strong')

# Izdvajanje najvažnijih èvorova po metrikama centralnosti

most_important_nodes_rep <- list()
most_important_nodes_rep$in_degree <- rep_centr_all %>% 
  select(node_id, in_degree) %>%
  arrange(desc(in_degree))

most_important_nodes_rep$out_degree <-  rep_centr_all %>%
  select(node_id, out_degree) %>%
  arrange(desc(out_degree))

most_important_nodes_rep$betweenness <- rep_centr_all %>% 
  select(node_id, betweenness) %>%
  arrange(desc(betweenness))

most_important_nodes_rep$rank <-  rep_centr_all %>%
  select(node_id, rank) %>%
  arrange(desc(rank))

most_important_nodes_rep$in_degree[1:10, ]
most_important_nodes_rep$out_degree[1:10, ]
most_important_nodes_rep$betweenness[1:10, ]
most_important_nodes_rep$rank[1:10, ]

# Klaster analiza uz pomoæ algoritama za detekciju zajednica u mreži 

is_connected(rep_graph, mode = 'strong')
rep_und <- as.undirected(rep_graph, 
                         mode = 'collapse',
                         edge.attr.comb = 'max')
rep_giant <- components(rep_und, mode = 'weak')
rep_giant <- induced_subgraph(rep_graph, which(rep_giant$membership == which.max(rep_giant$csize)))
rep_giant

edge_rep <- cluster_edge_betweenness(rep_giant, weights = NA, directed = F)

rep_louvain <- cluster_louvain(as.undirected(rep_giant),weights = E(rep_giant)$weight)
rep_louvain

rep_walktrap <- cluster_walktrap(rep_giant, weights = E(rep_giant)$weight, steps = 5)
rep_walktrap

rep_info <- infomap.community(rep_giant, e.weights = E(rep_giant)$weight)
rep_info

modularity_scores_rep <- list()
modularity_scores_rep$rep_louvain <- modularity(rep_louvain)
modularity_scores_rep$rep_walktrap <- modularity(rep_walktrap)
modularity_scores_rep$rep_info <- modularity(rep_info)

modularity_scores_rep


V(rep_giant)$community <- rep_louvain$membership
nodes_cl_rep <- data.frame(id = V(rep_giant)$name, title = V(rep_giant)$name, group = V(rep_giant)$community)
nodes_cl_rep <- nodes_cl_rep[order(nodes_cl_rep$id, decreasing = F),]
edges_cl_rep <- get.data.frame(rep_giant, what="edges")[1:2]

visNetwork(nodes_cl_rep, edges_cl_rep, main = 'Klasteri mreže odgovora') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)



men_hist <- degree_distribution(men_graph)


ggplot(data_frame(y=degree_distribution(men_graph), x=1:length(y))) +
  geom_segment(aes(x, y, xend=x, yend=0), color="slateblue") +
  scale_y_continuous(expand=c(0,0), trans="sqrt") +
  labs(x="Stepen", y="Gustina (sqrt scale)", title="Raspodela stepena za mrezu spominjanja") +
  theme_ipsum_rc(grid="Y", axis="x")   



deg_rep_df <- data.frame(node_id = as.integer(V(rep_graph)),
                         degree_dist = degree(rep_graph))

ggplot(deg_rep_df,aes(x = log1p(degree_dist))) +
  geom_density() +
  geom_histogram(binwidth = 0.4, position = "dodge", color = 'darkred', fill = 'indianred1') +
  xlab("Stepen") + ylab("Frekvencija") + 
  ggtitle("Raspodela stepena za mrežu odgovora") + 
  theme(plot.title = element_text(hjust = 0.5))
