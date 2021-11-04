
# INICIO ------------------------------------------------------------------

# SF Networks TEST

rm(list = ls())
graphics.off()
opar <- par()


# DETACH PACKAGES ---------------------------------------------------------

detachAllPackages <- function() {
        
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils",
                            "package:datasets","package:methods","package:base")
        
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        
        package.list <- setdiff(package.list,basic.packages)
        
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}

detachAllPackages()

rm(detachAllPackages)

# LIBRERIAS ---------------------------------------------------------------

library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
library(igraph)

ls(name = "package:sfnetworks") # Lista de funciones

# Otras funciones se llamaran sin cargar la libreria


# DATA --------------------------------------------------------------------
# Trae "roxel" como data de ejemplo

str(roxel)
glimpse(roxel)
class(roxel)


# CONSTRUCTION ------------------------------------------------------------
# From a nodes and edges table
# https://luukvdmeer.github.io/sfnetworks/articles/structure.html



# Mas explicacion en: https://geocompr.robinlovelace.net/spatial-class.html#geometry
p1 = st_point(c(7, 51))
p2 = st_point(c(7, 52))
p3 = st_point(c(8, 52))
p4 = st_point(c(8, 51.5))

l1 = st_sfc(st_linestring(c(p1, p2)))
l2 = st_sfc(st_linestring(c(p1, p4, p3)))
l3 = st_sfc(st_linestring(c(p3, p2)))

edges = st_as_sf(c(l1, l2, l3), crs = 4326)
nodes = st_as_sf(c(st_sfc(p1), st_sfc(p2), st_sfc(p3)), crs = 4326)

edges$from = c(1, 1, 3)
edges$to = c(2, 3, 2)

net = sfnetwork(nodes, edges)
net

class(net)
#> [1] "sfnetwork" "tbl_graph" "igraph"
plot(net)

# By default, the created network is a directed network. 
# If you want to create an undirected network, set directed = FALSE.

net = sfnetwork(nodes, edges, directed = FALSE)
net

class(net)
plot(net)



# -----

# Instead of from and to columns containing integers that refer to node indices, 
# the provided edges table can also have from and to columns containing characters that refer to node keys. 
# In that case, you should tell the construction function which column in the nodes table contains these keys. 
# Internally, they will then be converted to integer indices.

nodes$name = c("city", "village", "farm")
edges$from = c("city", "city", "farm")
edges$to = c("village", "farm", "village")

edges


net = sfnetwork(nodes, edges, node_key = "name")
#> Checking if spatial network structure is valid...
#> Spatial network structure is valid
net

plot(net)

# -----

edges$from = c(1, 1, 3)
edges$to = c(2, 3, 2)

net = sfnetwork(nodes, edges, length_as_weight = TRUE)
net

# -----

st_geometry(edges) = NULL

other_net = sfnetwork(nodes, edges, edges_as_lines = TRUE)

plot(net, cex = 2, lwd = 2, main = "Original geometries")
plot(other_net, cex = 2, lwd = 2, main = "Straight lines")



# ----------------

st_geometry(edges) = st_sfc(c(l2, l3, l1), crs = 4326)

net = sfnetwork(nodes, edges)
#> Checking if spatial network structure is valid...
#> Error: Edge boundaries do not match their corresponding nodes


# From an sf object with linestring geometries ----------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#from-an-sf-object-with-linestring-geometries

roxel

plot(roxel)
plot(roxel[2])

net <- as_sfnetwork(x = roxel)
net
plot(net)


#  From a network specific file type --------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#from-a-network-specific-file-type

url = "https://raw.githubusercontent.com/ComplexNetTSP/Power_grids/v1.0.0/Countries/Netherlands/graphml/Netherlands_highvoltage.graphml"

igraph::read_graph(url, format = "graphml") %>%
        as_tbl_graph()



igraph::read_graph(url, format = "graphml") %>%
        as_sfnetwork(wkt = "wktsrid4326", crs = 4326)
#> Checking if spatial network structure is valid...
#> Spatial network structure is valid


graphml_net = igraph::read_graph(url, format = "graphml") %>%
        as_sfnetwork(wkt = "wktsrid4326", crs = 4326) %>%
        convert(to_spatial_explicit, wkt = "wktsrid4326", crs = 4326, .clean = TRUE)
#> Checking if spatial network structure is valid...
#> Spatial network structure is valid

graphml_net

plot(graphml_net)



# Activation --------------------------------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#activation

net %>%
        activate("edges") %>%
        mutate(weight = edge_length()) %>%
        activate("nodes") %>%
        mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))



# Extraction --------------------------------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#extraction

net %>%
        activate("nodes") %>%
        st_as_sf()

st_as_sf(net, "edges")



# Visualization -----------------------------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#visualization

plot(net)

# Con ggplot2
autoplot(net) + ggtitle("Road network of Münster Roxel")


# --

net = net %>%
        activate("nodes") %>%
        mutate(bc = centrality_betweenness())

ggplot() +
        geom_sf(data = st_as_sf(net, "edges"), col = "grey50") +
        geom_sf(data = st_as_sf(net, "nodes"), aes(col = bc, size = bc)) +
        ggtitle("Betweenness centrality in Münster Roxel")

#  Spatial information ----------------------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#spatial-information

net %>%
        activate("nodes") %>%
        st_geometry()

st_geometry(net, "edges")



net %>%
        activate("edges") %>%
        st_set_geometry(NULL) %>%
        plot(draw_lines = FALSE, main = "Edges without geometries")

net %>%
        activate("nodes") %>%
        st_set_geometry(NULL) %>%
        plot(vertex.color = "black", main = "Nodes without geometries")

# --
as_sfnetwork(roxel, directed = TRUE) %>%
        activate("edges") %>%
        st_reverse()
#> Warning: In directed networks st_reverse swaps columns 'to' and 'from'


# Coordinates -------------------------------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#coordinates

node_coords = net %>%
        activate("nodes") %>%
        st_coordinates()

node_coords[1:4, ]



# Currently there are neither Z nor M coordinates.
st_z_range(net)
#> NULL
st_m_range(net)
#> NULL

# Add Z coordinates with value 0 to all features.
# This will affect both nodes and edges, no matter which element is active.
st_zm(net, drop = FALSE, what = "Z")




net %>%
        st_zm(drop = FALSE, what = "Z") %>%
        mutate(X = node_X(), Y = node_Y(), Z = node_Z(), M = node_M())
#> Warning: M coordinates are not available


#  Coordinate Reference System --------------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#coordinate-reference-system

st_crs(net)

st_transform(net, 3035)


#  Bounding box -----------------------------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#bounding-box

net %>%
        activate("nodes") %>%
        st_bbox()
#>      xmin      ymin      xmax      ymax 
#>  7.522622 51.941512  7.546705 51.961203


# ---

node1 = st_point(c(8, 51))
node2 = st_point(c(7, 51.5))
node3 = st_point(c(8, 52))
node4 = st_point(c(9, 51))
edge1 = st_sfc(st_linestring(c(node1, node2, node3)))

nodes = st_as_sf(c(st_sfc(node1), st_sfc(node3), st_sfc(node4)))
edges = st_as_sf(edge1)
edges$from = 1
edges$to = 2

small_net = sfnetwork(nodes, edges)
#> Checking if spatial network structure is valid...
#> Spatial network structure is valid

node_bbox = st_as_sfc(st_bbox(activate(small_net, "nodes")))
edge_bbox = st_as_sfc(st_bbox(activate(small_net, "edges")))
net_bbox = st_as_sfc(st_network_bbox(small_net))

plot(small_net, lwd = 2, cex = 4, main = "Element bounding boxes")
plot(node_bbox, border = "red", lty = 2, lwd = 4, add = TRUE)
plot(edge_bbox, border = "blue", lty = 2, lwd = 4, add = TRUE)
plot(small_net, lwd = 2, cex = 4, main = "Network bounding box")
plot(net_bbox, border = "red", lty = 2, lwd = 4, add = TRUE)



#  Attribute-geometry relationships ---------------------------------------

# https://luukvdmeer.github.io/sfnetworks/articles/structure.html#attribute-geometry-relationships

net %>%
        activate("edges") %>%
        st_set_agr(c("name" = "constant", "type" = "constant")) %>%
        st_agr()
#>     from       to     name     type 
#>     <NA>     <NA> constant constant 
#> Levels: constant aggregate identity

plot(net)
