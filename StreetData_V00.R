# Para graficos en ggplot2 -----


# https://ggplot2tutor.com/tutorials/streetmaps
# https://rstudio-pubs-static.s3.amazonaws.com/571511_3b48facd6dd04f3da69f374ad79f19f1.html


# INICIO PRUEBA -----------------------------------------------------------

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


#  DATA 1 -----------------------------------------------------------------


lugar <- "Providencia, Santiago"

getbb(place_name = lugar)


# Con calles residenciales

calles <- getbb(lugar) %>%
        opq() %>%
        add_osm_feature(key = "highway", 
                        value = c("motorway", 
                                  "primary", 
                                  "secondary", 
                                  "tertiary", 
                                  "residential")) %>% # Trunk eliminado
        osmdata_sf()


calles$osm_lines$highway <- as.factor(calles$osm_lines$highway)

glimpse(calles$osm_lines)
levels(calles$osm_lines$highway)


plot(calles$osm_lines[34])

# pal <- colorFactor(palette = topo.colors(6), domain = calles$osm_lines$highway)
pal <- colorFactor(palette = c("red","orange","green", "yellow", "white"), domain = calles$osm_lines$highway)

leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addPolylines(data = calles$osm_lines, color = pal(x = calles$osm_lines$highway), weight = 2, 
                     label = htmltools::htmlEscape(calles$osm_lines$highway)) %>% 
        addLegend("bottomleft", pal = pal, values = calles$osm_lines$highway)

# DATA 2 ------------------------------------------------------------------

lugar <- "Region Metropolitana Santiago"

getbb(place_name = lugar)


# Sin calles residenciales

calles <- getbb(lugar) %>%
        opq() %>%
        add_osm_feature(key = "highway",
                        value = c("motorway",
                                  "primary",
                                  "secondary",
                                  "tertiary")) %>% # Trunk eliminado
        osmdata_sf()

calles$osm_lines$highway <- as.factor(calles$osm_lines$highway)

glimpse(calles$osm_lines)
levels(calles$osm_lines$highway)


plot(calles$osm_lines[34])


carreteras <- calles$osm_lines

carreteras <- dplyr::select(.data = carreteras, name, highway, lanes, maxspeed, oneway, surface, geometry)

plot(carreteras[2])

# CONSTRUCTION ------------------------------------------------------------

# TUTORIAL ACA
# https://luukvdmeer.github.io/sfnetworks/articles/structure.html


class(roxel)
class(calles$osm_lines)

carreteras <- calles$osm_lines

carreteras <- dplyr::select(.data = carreteras, name, highway, lanes, maxspeed, oneway, surface, geometry)


net = as_sfnetwork(carreteras)

plot(net)




# ACTIVATION --------------------------------------------------------------

net %>%
        activate("edges") %>%
        mutate(weight = edge_length()) %>%
        activate("nodes") %>%
        mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))


# EXTRACTION --------------------------------------------------------------

net %>%
        activate("nodes") %>%
        st_as_sf()



st_as_sf(net, "edges")


# VISUALIZATION -----------------------------------------------------------

plot(net)

autoplot(net) + ggtitle("Red de calles comuna de Providencia, Santiago de Chile")


# For advanced visualization, we encourage to extract nodes and edges as sf objects, 
# and use one of the many ways to map those in R, either statically or interactively

net = net %>%
        activate("nodes") %>%
        mutate(bc = centrality_betweenness())

ggplot() +
        geom_sf(data = st_as_sf(net, "edges"), col = "grey50") +
        geom_sf(data = st_as_sf(net, "nodes"), aes(col = bc, size = bc)) +
        ggtitle("Betweenness centrality en la Región Metropolitana, Chile")

