
# INICIO ------------------------------------------------------------------

rm(list = ls())
graphics.off()
opar <- par()

# LIBRERIAS ---------------------------------------------------------------

library(osmdata)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(sfnetworks)
library(tidygraph)
library(tidyverse)
library(igraph)
library(dbscan)
library(TSP)

# DATA --------------------------------------------------------------------

# Buscando con parametro "lugar" [1]

lugar <- "Vitacura, Chile"

getbb(place_name = lugar)


calles <- getbb(lugar) %>%
        opq() %>%
        add_osm_feature(key = "highway", 
                        value = c("motorway", 
                                  "primary", 
                                  "secondary", 
                                  "tertiary", 
                                  "residential", "trunk")) %>%
        osmdata_sf()

# Buscando por bbox [2]

# http://bboxfinder.com/#-33.686068,-70.926361,-33.272565,-70.428543
min <- c(-70.926361,-33.686068)
max <- c(-70.428543,-33.272565)

lugar_df <- as.matrix(data.frame(min, max))
lugar_df

row.names(lugar_df) <- c("x","y")


calles <- lugar_df %>%
        opq() %>%
        add_osm_feature(key = "highway", 
                        value = c("motorway", 
                                  "primary", 
                                  "secondary", 
                                  "tertiary", 
                                  "residential", "trunk")) %>%
        osmdata_sf()

calles

rm(max, min, lugar_df)


# Siguiendo (independiente de como fue buscada la data de OSM) ==========

calles$osm_lines$highway <- as.factor(calles$osm_lines$highway)

# glimpse(calles$osm_lines)
levels(calles$osm_lines$highway)

carreteras <- calles$osm_lines

carreteras <- dplyr::select(.data = carreteras, 
                            name, 
                            highway, 
                            #lanes, 
                            #maxspeed, 
                            #oneway, 
                            #surface, 
                            geometry)

# carreteras <- dplyr::mutate(.data = carreteras, ID = row_number())




net = as_sfnetwork(carreteras, directed = FALSE) # ORIGINAL

# net = as_sfnetwork(carreteras, node_key = "name") # PRUEBA

net

# Mirando RED
plot(carreteras[2])
plot(net)
plot(st_geometry(net, "edges"))
plot(st_geometry(net, "nodes"))



# VAMOS A LIMPIAR LA RED --------------------------------------------------

# 2. Network pre-processing and cleaning ----------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/preprocess_and_clean.html

# Simplify network --------------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/preprocess_and_clean.html#simplify-network

# edge_colors = function(x) rep(sf.colors(1000, categorical = TRUE)[-2], 2)[c(1:ecount(x))]

# clr <- sf.colors(n = 2)

clr <- c("#eb4034", "#1b1ed1") # Rojo y Azul

simple = net %>%
        activate("edges") %>%
        filter(!edge_is_multiple()) %>%
        filter(!edge_is_loop())

plot(st_geometry(net, "edges"), col = clr, lwd = 2)
plot(st_geometry(net, "nodes"), pch = 20, cex = 1, add = TRUE)
plot(st_geometry(simple, "edges"), col = clr, lwd = 2)
plot(st_geometry(simple, "nodes"), pch = 20, cex = 1, add = TRUE)


# Subdivide edges ---------------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/preprocess_and_clean.html#subdivide-edges

subdivision = convert(simple, to_spatial_subdivision)
#> Warning: to_spatial_subdivision assumes attributes are constant over geometries

plot(st_geometry(simple, "edges"), col = clr, lwd = 2)
plot(st_geometry(simple, "nodes"), pch = 20, cex = 1, add = TRUE)
plot(st_geometry(subdivision, "edges"), col = clr, lwd = 2)
plot(st_geometry(subdivision, "nodes"), pch = 20, cex = 1, add = TRUE)

# Smooth pseudo nodes -----------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/preprocess_and_clean.html#smooth-pseudo-nodes

smoothed = convert(subdivision, to_spatial_smooth)

plot(st_geometry(subdivision, "edges"), col = clr, lwd = 2)
plot(st_geometry(subdivision, "nodes"), pch = 20, cex = 1, add = TRUE)
plot(st_geometry(smoothed, "edges"), col = clr, lwd = 2)
plot(st_geometry(smoothed, "nodes"), pch = 20, cex = 1, add = TRUE)


# Eliminando redes no utilizadas

rm(simple, subdivision)

# CENTRALITY MEASURES -----------------------------------------------------

library(netrankr)

# Centralidad [1]
net_central <- smoothed %>% 
        activate("nodes") %>% # Activamos nodos
        mutate(bc = centrality_betweenness()) # Medida de centralidad usada, se agrega una columna

ggplot() +
  geom_sf(data = st_as_sf(net_central, "edges"), col = "grey50") +
  geom_sf(data = st_as_sf(net_central, "nodes"), aes(col = bc, size = bc)) +
  ggtitle("Betweenness centrality en Vitacura, RM, Chile.")

# Centralidad [2]

net_central <- smoothed %>% 
  activate("nodes") %>% # Activamos nodos
  mutate(bc = centrality_hub()) # Medida de centralidad usada, se agrega una columna

ggplot() +
  geom_sf(data = st_as_sf(net_central, "edges"), col = "grey50") +
  geom_sf(data = st_as_sf(net_central, "nodes"), aes(col = bc, size = bc)) +
  ggtitle("Betweenness centrality en Vitacura, RM, Chile.")





# Routing -----------------------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/routing.html

# TRAVELING SALES PERSON PROBLEM
# https://github.com/mhahsler/TSP


# Usando smoothed net

net_path <- smoothed %>% 
        activate("edges") %>% 
        mutate(weight = edge_length())

net_path
plot(net_path)

# Calculating shortest paths

paths = st_network_paths(net_path, from = 1, to = 100)
paths



# Calculando...

paths %>%
        slice(1) %>%
        pull(node_paths) %>%
        unlist()


paths %>%
        slice(1) %>%
        pull(edge_paths) %>%
        unlist()


plot_path = function(node_path) {
        net_path %>%
                activate("nodes") %>%
                slice(node_path) %>%
                plot(cex = 1.5, lwd = 1.5, add = TRUE)
}


# Ploteo

plot(net_path, col = "grey")

paths %>%
        pull(node_paths) %>%
        walk(plot_path)

net_path %>%
        activate("nodes") %>%
        st_as_sf() %>%
        slice(c(1, 100)) %>%
        plot(col = clr, pch = 15, cex = 2, lwd = 2, add = TRUE)



# Esto es con puntos determinados:
  
p1 <- st_sfc(st_point(c(-70.578006, -33.389827))) # Casa
p2 <- st_sfc(st_point(c(-70.559331, -33.389929))) # Antartica Chilena

# SPATIAL MORPHERS --------------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html


# Morphing and unmorphing -------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html#morphing-and-unmorphing

net = as_sfnetwork(carreteras, directed = FALSE) %>%
        st_transform(4326) # 3857? # https://epsg.io/

grouped_net = net %>%
        morph(to_linegraph) %>%
        mutate(group = group_louvain()) %>%
        unmorph()

grouped_net


# The algorithm detected 820 communities. (Caso de Vitacura para las 820 comunidades) (34 en el ejemplo)
grouped_net %>%
        activate("edges") %>%
        pull(group) %>%
        unique() %>%
        length()


# In all grouping functions in tidygraph, the group index 1 belongs the largest group, 
# the index 2 to the second largest group, etcetera. 
# Lets plot only the first 10 of the 34 groups, to keep the plot clear. (34 grupos en el ejemplo)

plot(st_geometry(net, "edges"), col = "grey", lwd = 0.5)

grouped_net %>%
        activate("edges") %>%
        st_as_sf() %>%
        transmute(group = as.factor(group)) %>%
        filter(group %in% c(1:11)) %>% # Primeros 10
        plot(lwd = 4, add = TRUE)



# ANOTHER APPLICATION

# Another application of the tidygraph::to_linegraph() morpher is to find “cut edges” in the network. 
# These are edges that break the connectivity of a connected component when they are removed. 
# Hence, they have a crucial function in preserving the connectivity of a network.

new_net = net %>%
        mutate(is_cut = node_is_cut()) %>%
        morph(to_linegraph) %>%
        mutate(is_cut = node_is_cut()) %>%
        unmorph()

cut_nodes = new_net %>%
        activate("nodes") %>%
        filter(is_cut) %>%
        st_geometry()

cut_edges = new_net %>%
        activate("edges") %>%
        filter(is_cut) %>%
        st_geometry()

plot(net, col = "grey", main = "Cut nodes")
plot(cut_nodes, col = "red", pch = 20, cex = 2, add = TRUE)
plot(net, col = "grey", main = "Cut edges")
plot(cut_edges, col = "red", lwd = 4, add = TRUE)


# Internally, a morphed state of a network is a list, in which each element is a network on its own. 
# Some morphers create a list with only a single element, like the linegraph example above, 
# while others create a list with multiple elements.

morphed_net = morph(net, to_components)

morphed_net


class(morphed_net)
#> [1] "morphed_sfnetwork" "morphed_tbl_graph" "list"

length(morphed_net)
#> [1] 14 (En el ejemplo)


# Converting --------------------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html#converting


convert(net, to_complement)



# SPATIAL MORPHERS --------------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html#spatial-morphers

# https://tidygraph.data-imaginist.com/reference/morphers.html


# To Spatial Contracted ---------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html#to_spatial_contracted


new_net = net %>%
        activate("nodes") %>%
        filter(group_components() == 1) %>%
        mutate(foo = sample(c(1:10), graph_order(), replace = TRUE)) %>%
        mutate(bar = sample(c(TRUE, FALSE), graph_order(), replace = TRUE)) %>%
        mutate(louvain = as.factor(group_louvain()))

contracted_net = convert(
        new_net,
        to_spatial_contracted,
        louvain,
        simplify = TRUE,
        summarise_attributes = list(
                foo = "sum",
                bar = function(x) any(x),
                louvain = "first"
        )
)

plot(st_geometry(new_net, "edges"), main = "Grouped nodes")
plot(st_as_sf(new_net)["louvain"], key.pos = NULL, pch = 20, add = TRUE)
plot(st_geometry(contracted_net, "edges"), main = "Contracted network")
plot(
        st_as_sf(contracted_net)["louvain"],
        cex = 2, key.pos = NULL,
        pch = 20, add = TRUE
)


# To Spatial Directed -----------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html#to_spatial_directed

# The to_spatial_directed() morpher turns an undirected network 
# into a directed one based on the direction given by the linestring geometries of the edges. 
# Hence, from the node corresponding to the first point of the linestring, 
# to the node corresponding to the last point of the linestring. 
# 
# This in contradiction to tidygraph::to_directed(), 
# which bases the direction on the node indices given in the to and from columns of the edges. 
# In undirected networks the lowest node index is always used as from index, 
# no matter the order of endpoints in the edges’ linestring geometry. 
# Therefore, the from and to node indices of an edge may not always 
# correspond to the first and last endpoint of the linestring geometry, 
# and to_spatial_directed() gives different results as tidygraph::to_directed().

net %>%
        activate("nodes") %>%
        mutate(bc_undir = centrality_betweenness()) %>%
        morph(to_spatial_directed) %>%
        mutate(bc_dir = centrality_betweenness()) %>%
        unmorph() %>%
        mutate(bc_diff = bc_dir - bc_undir) %>%
        arrange(bc_diff, desc())


# To Spatial Explicit -----------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html#to_spatial_explicit

# If your original network is spatially implicit (i.e. edges do not have a geometry list column), 
# the to_spatial_explicit() morpher explicitizes the edges by creating a geometry list column for them. 
# If the edges table can be directly converted to an sf object using sf::st_as_sf(), extra arguments can be provided as ..., 
# which will be forwarded to sf::st_as_sf() internally. 
# Otherwise, straight lines will be drawn between the end nodes of each edge. 
# The morphed state contains a single sfnetwork.


implicit_net = st_set_geometry(activate(net, "edges"), NULL)
explicit_net = convert(implicit_net, to_spatial_explicit)

plot(implicit_net, draw_lines = FALSE, main = "Implicit edges")
plot(explicit_net, main = "Explicit edges")



# To Spatial Neighborhood -------------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html#to_spatial_neighborhood

# The to_spatial_neighborhood() morpher limits the original network 
# to those nodes that are part of the neighborhood of a specified origin node. 
# This origin node can be specified by a node index, but also by any geospatial point (as sf or sfc object). 
# Internally, such a point will be snapped to its nearest node before calculating the neighborhood. 
# A neighborhood contains all nodes that can be reached within a certain cost threshold from the origin node. 
# The morphed state contains a single sfnetwork.



# As an example we will calculate multiple neighborhoods with different thresholds.
# First we set the geographic lengths of the edges as the edge weights.
# These weights will automatically be used when calculating travel costs.
# Just as in the shortest paths calculation functions.
new_net = net %>%
        activate("edges") %>%
        mutate(weight = edge_length())

# Define the origin location.
p = net %>%
        st_geometry() %>%
        st_combine() %>%
        st_centroid()

# Otro punto de origen si no funciona el anterior
p <- st_point(c(-70.59761, -33.39034))

# Define the threshold values (in meters).
# Define also the colors to plot the neighborhoods in.
thresholds = rev(seq(100, 1000, 100))
palette = sf.colors(n = 10)

# Plot the results.
plot(net, col = "grey")

for (i in c(1:10)) {
        nbh = convert(net, to_spatial_neighborhood, p, thresholds[i])
        plot(nbh, col = palette[i], add = TRUE)
}

plot(p, pch = 8, cex = 2, lwd = 2, add = TRUE)



# To Spatial Shortest Paths -----------------------------------------------
# https://luukvdmeer.github.io/sfnetworks/articles/morphers.html#to_spatial_shortest_paths


net %>%
        activate("edges") %>%
        convert(
                to_spatial_shortest_paths,
                from = 1, to = 100,
                weights = edge_length()
        )


new_net = net %>%
        activate("edges") %>%
        morph(
                to_spatial_shortest_paths,
                from = 1, to = seq(10, 100, 10),
                weights = edge_length()
        ) %>%
        mutate(in_paths = TRUE) %>%
        unmorph()

new_net %>%
        st_geometry() %>%
        plot(col = "grey", lwd = 2)

new_net %>%
        filter(in_paths) %>%
        st_geometry() %>%
        plot(col = "red", lwd = 4, add = TRUE)

# PROBANDO ----------------------------------------------------------------
# Ver tutorial en: https://kateto.net/netscix2016.html
igraph::E(net)
igraph::V(net)
igraph::edge.attributes(graph = net, index = E(net))
igraph::vertex.attributes(graph = net, index = V(net))


E(net)$name

edge_attr(graph = net, name = "name", index = 2598) # 2598 = Calle Dolores para data de Vitacura
vertex_attr(graph = net)

graph_attr(net)
graph_attr_names(new_net)
