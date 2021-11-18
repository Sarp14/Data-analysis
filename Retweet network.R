# Mreža ritvitovanja


# Lista ritvitovanja

statues_retweets <- statues_tweets %>% 
  select(screen_name, retweet_screen_name, retweet_count) %>%
  rename( sender = screen_name , retweet =  retweet_screen_name)

statues_retweets <- unnest(statues_retweets, retweet)

retweet_edgelist <- statues_retweets %>% 
  select(sender, retweet, retweet_count) %>% 
  filter(complete.cases(.)) %>% 
  group_by(sender, retweet) %>%
  unnest(retweet) %>% 
  summarise(weight = n()) %>% 
  ungroup()

# Mreža ritvitovanja 

ret_graph <- graph_from_data_frame(retweet_edgelist)
ret_graph

ret_graph <- simplify(ret_graph)
no_edges_nodes <- which(degree(ret_graph) == 0)
ret_graph <- delete_vertices(ret_graph, no_edges_nodes)

nodes_ret <- data.frame(id = V(ret_graph)$name, stringsAsFactors = F)
edges_ret <- data.frame(as_edgelist(ret_graph), stringsAsFactors = F)
colnames(edges_ret) <- c("from", "to")


ret_visn <- toVisNetworkData(ret_graph)
ret_visn$edges$value <- ret_visn$edges$weight

ret_network <- visNetwork(nodes = ret_visn$nodes, 
                          edges = ret_visn$edges, 
                          main = 'Mreža ritvitovanja') %>% 
  visNodes(color = "#00CC33") %>% 
  visEdges(arrows = "to", color = "darkgreen", smooth = T) %>%
  visOptions(highlightNearest = T, nodesIdSelection = T)

ret_network

# Merenje centralnosti stepena 
# Ulazna i izlazna centralnost za mrežu ritvitovanja

deg_in_ret <- degree(ret_graph, mode = "in", normalized = T)
summary(deg_in_ret)

deg_out_ret <- degree(ret_graph, mode = "out", normalized = T)
summary(deg_out_ret)

# Histogram distribucija centralnosti stepena za mrežu ritvitovanja  

max_degree <- max(deg_ret_df_long$degree_value)

ggplot(deg_ret_df_long, aes(x = degree_value,  fill = degree_type)) + 
  geom_histogram(binwidth = 80, bins = 150, position = "dodge") + 
  scale_fill_discrete(name = "Tip stepena") +
  labs(title = "Raspodela stepena za mrežu ritvitovanja", x = "Vrednost stepena", y = "Frekvencija") +
  scale_x_continuous(breaks = seq(0,max_degree,100)) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Centralnost intermedijalnosti 

ret_betweenness <- data.frame(node_id = as.integer(V(ret_graph)),
                              betwenness = betweenness(ret_graph))
summary(ret_betweenness$betwenness)

# Èvorovi sa najveæom intermedijalnošæu 

max(ret_betweenness$betwenness)
which(ret_betweenness$betwenness == max(ret_betweenness$betwenness))
View(ret_betweenness)

# Centranost ranga

page_ret <- page.rank(ret_graph, directed = T, weights = E(ret_graph)$weight)$vector
page_ret <- round(page_ret, digits = 1)

summary(page_ret)
which(page_ret == max(page_ret))

# Korelacija mera centralnosti 

ret_centr_all <- data.frame(node_id = nodes_ret$id,
                            in_degree = degree(ret_graph, mode = 'in'),
                            out_degree = degree(ret_graph, mode = 'out'),
                            betweenness = ret_betweenness$betwenness,
                            rank = page_ret)

View(ret_centr_all)

apply(ret_centr_all[0:5000,-1], 2, shapiro.test)
centr_corr_ret <- cor(ret_centr_all[,-1],
                      use = 'complete.obs',
                      method = 'spearman')


ggcorrplot(centr_corr_ret, 
           title = "Korelaciona matrica metrika centralnosti za mrežu ritvitovanja",
           ggtheme = theme_grey(),
           colors = c("coral3", "white", "dodgerblue4"),
           hc.order = T,
           lab = T,
           method = "square",
           type ="lower")


# Proseèna putanja, gustina ivica i tranzitivnost

transitivity(ret_graph, type = 'global')
edge_density(ret_graph)

# Raèunanje proseka najkraæe putanje za èvorore u mreži ritvitovanja

ret_dist_in <- distances(ret_graph, mode = 'in', 
                         weights = E(ret_graph)$weight)
ret_dist_out <- distances(ret_graph, mode = 'out', 
                          weights = E(ret_graph)$weight)

avg_dist_ret <- mean_distance(ret_graph, directed = T)
avg_dist_ret

ret_dist_in[is.infinite(ret_dist_in)] <- NA
ret_dist_in <- apply(ret_dist_in, 1, function(x) mean(x, na.rm = T))
summary(ret_dist_in)

ret_dist_out[is.infinite(ret_dist_out)] <- NA
ret_dist_out <- apply(ret_dist_out, 1, function(x) mean(x, na.rm = T))
summary(ret_dist_out)

# Najudaljeniji èvorovi

most_apart_ret <- farthest_vertices(ret_graph, weights = ret_graph$weight)$vertices
most_apart_ret
big_dist_ret <- shortest_paths(ret_graph, 
                               from = most_apart_ret[1], 
                               to = most_apart_ret[2],
                               weights = E(ret_graph)$weight,
                               output = 'epath')
big_dist_ret

# Najveæa distanca u mreži 

diameter(ret_graph,directed = T, unconnected = T, weights = NA)

# Izdvajanje najvažnijih èvorova po metrikama centralnosti

most_important_nodes_ret <- list()
most_important_nodes_ret$in_degree <- ret_centr_all %>% 
  select(node_id, in_degree) %>%
  arrange(desc(in_degree))

most_important_nodes_ret$out_degree <-  ret_centr_all %>%
  select(node_id, out_degree) %>%
  arrange(desc(out_degree))

most_important_nodes_ret$betweenness <- ret_centr_all %>% 
  select(node_id, betweenness) %>%
  arrange(desc(betweenness))

most_important_nodes_ret$rank <-  ret_centr_all %>%
  select(node_id, rank) %>%
  arrange(desc(rank))


most_important_nodes_ret$in_degree[1:10, ]
most_important_nodes_ret$out_degree[1:10, ]
most_important_nodes_ret$betweenness[1:10, ]
most_important_nodes_ret$rank[1:10, ]

# Klaster analiza uz pomoæ algoritama za detekciju zajednica u mreži 

is_connected(ret_graph, mode = 'strong')
ret_und <- as.undirected(ret_graph, 
                         mode = 'collapse',
                         edge.attr.comb = 'max')
ret_giant <- components(ret_und, mode = 'weak')
ret_giant <- induced_subgraph(ret_graph, which(ret_giant$membership == which.max(ret_giant$csize)))
ret_giant

ret_louvain <- cluster_louvain(as.undirected(ret_giant), weights = E(ret_giant)$weight)
ret_louvain

ret_walktrap <- cluster_walktrap(ret_giant, weights = E(ret_giant)$weight, steps = 5)
ret_walktrap

ret_info <- infomap.community(ret_giant, e.weights = E(ret_giant)$weight)
ret_info


modularity_scores_ret <- list()
modularity_scores_ret$ret_louvain <- modularity(ret_louvain)
modularity_scores_ret$ret_walktrap <- modularity(ret_walktrap)
modularity_scores_ret$ret_info <- modularity(ret_info)

modularity_scores_ret


V(ret_giant)$community <- ret_louvain$membership
nodes_cl_ret <- data.frame(id = V(ret_giant)$name, title = V(ret_giant)$name, group = V(ret_giant)$community)
nodes_cl_ret <- nodes_cl_ret[order(nodes_cl_ret$id, decreasing = F),]
edges_cl_ret <- get.data.frame(ret_giant, what="edges")[1:2]

visNetwork(nodes_cl_ret, edges_cl_ret, main = 'Klasteri mreže ritvitovanja') %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)


deg_ret_df <- data.frame(node_id = as.integer(V(ret_graph)),
                         degree_dist = degree(ret_graph))

ggplot(deg_ret_df,aes(x = log1p(degree_dist))) +
  geom_density() +
  geom_histogram(binwidth = 0.5, position = "dodge", color = 'darkblue', fill = 'lightblue') +
  xlab("Stepen") + ylab("Frekvencija") + 
  ggtitle("Raspodela stepena za mrežu ritvitovanja") + 
  theme(plot.title = element_text(hjust = 0.5))
