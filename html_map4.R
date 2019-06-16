
rm(list = ls())

#=========================================================================
#= MAPA de SP ============================================================ 
#=========================================================================
library(ggmap); library(ggplot2); library(dplyr); library(rgdal);library(gridExtra);
library(dplyr); library(lattice);library(sp);library(leaflet);library(leaflet.extras);
library(devtools);library(htmlwidgets);library(webshot)

# importando o shape do Brasil ------------------------------------------

library(raster)
brasil <- shapefile("W:/BRADESCO/mapa Html/Shp/Estados_do_Brasil/Brasil.shp")
plot(brasil)
# importando o shape de SP ----------------------------------------------

shape <- shapefile("W:/BRADESCO/mapa Html/Shp/Bairros SP/DISTRITO_MUNICIPAL_SP_SMDUPolygon.shp")
plot(shape)

# Pontos --------------------------------------------------------------

pontos <- shapefile("W:/BRADESCO/mapa Html/Shp/LAYER_AREAS_CONTAMINADAS/DEINFO_AREAS_CONTAMINADAS.shp")
plot(pontos)

# Manipulação e simulação das informações ------------------------------

LatLong <- coordinates(spTransform(pontos, CRS("+proj=longlat +datum=WGS84"))) %>%  as.data.frame()
names(LatLong) <- c("long","lat")

# Contagem 
LatLong$n    <- rpois(length(LatLong$long), lambda = 9) 
N <- 30
LatLong$n[1] <- N
LatLong$n[2] <- N

LatLong$id   <- if_else(LatLong$n > 10, 1, 0)

# Taxa
LatLong$rate <- LatLong$n/N


# -------------------------------------------------------------------------
# Criando os labels
# -------------------------------------------------------------------------

pivs <- paste0("<strong>IBEU: </strong>", shape$Nome, " <strong>Setor: </strong> ",shape$Codigo)

labels <- sprintf(
  "<strong>%s</strong><br/> </strong>",
  shape$Nome) %>% lapply(htmltools::HTML)

# Popup para os valores dos pontos

mytext <- paste("<strong>ID: </strong>", LatLong$id, "<br/>", 
                "<strong>Contagem: </strong>", LatLong$n, "<br/>", 
                "<strong>%Porcentagem: </strong>", LatLong$rate %>% round(3)*100, "<br/>",
                sep="") %>%
  lapply(htmltools::HTML)

# -------------------------------------------------------------------------------
# Criando as cores
# --------------------------------------------------------------------------------

pal1 <- colorFactor(c("navy", "red"), domain = c(0, 1))

pal2 <- colorQuantile("RdYlBu", LatLong$n, n = 5)

qpal <- colorNumeric(palette = "RdYlBu", domain = LatLong$n)

mybins <- seq(0, 1, by = 0.2)
mypalette = colorBin(palette="RdYlBu", domain = LatLong$rate, na.color="transparent", bins = mybins)

# -------------------------------------------------------------------------------
# Criando um icone
# --------------------------------------------------------------------------------
brad <- makeIcon(
  iconUrl = "https://cdn3.iconfinder.com/data/icons/one-piece-flat/48/Cartoons__Anime_One_Piece_Artboard_6-512.png",
  iconWidth = 70, iconHeight = 70,
  iconAnchorX = 20, iconAnchorY = 20)

#----------------------------------------------------------------------------------
# O Mapa --------------------------------------------------------------------------
#----------------------------------------------------------------------------------

mapa <- leaflet() %>% addTiles() %>% 
  # Basemap
  addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
  addProviderTiles(providers$Stamen.Toner, group = "StamenToner") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%

  # --------------------------------------------------------------------------------------
# Bairros de SP  
  addPolygons(data = brasil,
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = F),
              group = "Brasil") %>% 
  # --------------------------------------------------------------------------------------
  # Bairros de SP
  setView( -46.6333,-23.5505, 2) %>%
  addPolygons(data = shape, 
              fillColor = ~Nome,  weight = 1,
              opacity = 0.9, dashArray = "2", color = "black",
              highlight = highlightOptions(
                weight = 5, color = "black",
                dashArray = "",  fillOpacity = 0.9, fillColor = "black", 
                bringToFront = F), label = labels, group = "Bairros",
                labelOptions = labelOptions(noHide = F, direction = "right")) %>% 
  # --------------------------------------------------------------------------------------
  # Plotando icones
  addMarkers(data = LatLong,
                   lng = ~long, lat = ~lat,
                   icon = brad,
                   group = "Icon") %>%
  # --------------------------------------------------------------------------------------
  # Plotando os pontos e popup do n
  addCircleMarkers(data = LatLong,
                   lng = ~long, lat = ~lat, radius = 3,
                   popup = paste("n: ",LatLong$n), color = "black",
                   group = "Pontos") %>%
  # --------------------------------------------------------------------------------------
  # Plotando os pontos com informações descritivas
  addCircleMarkers(data = LatLong,
                   lng = ~long, lat = ~lat, radius = 5,
                   popup = mytext,
                   group = "Informações dos pontos") %>%
  # --------------------------------------------------------------------------------------
  # Cartograma de pontos
  addCircles(data = LatLong, radius = 300, 
                   lng = ~long, lat = ~lat,
                   fillColor = ~qpal(n),
                   color = ~qpal(n),
                   fillOpacity = 0.9,
                   group = "Densidade 1", stroke = T) %>% 
  # --------------------------------------------------------------------------------------
  # Cartograma de pontos n intensidade da cor e rate tamanho
  addCircles(data = LatLong, 
                   lng = ~long, lat = ~lat, radius = LatLong$rate*1000,
                   fillColor = ~qpal(n),
                   color =  ~qpal(n),
                   fillOpacity = 0.7,
                   group = "Densidade 2") %>% 
  # --------------------------------------------------------------------------------------
  # HeatMap 1 proximidade de pontos
  addWebGLHeatmap(data = LatLong,
                  lng = ~long, lat = ~lat, size = 30, units = 'px',
                  group = "HeatMap proximidade") %>%   
  # --------------------------------------------------------------------------------------
  # HeatMap 1 proximidade de pontos
  addHeatmap(data = LatLong,
             lng = ~long, lat = ~lat,
             intensity = ~n,
             blur = 10, min = 0, max = 30,
             group = "HeatMap frequência") %>% 
  # --------------------------------------------------------------------------------------
  # Cluster de proximidade 
  addCircleMarkers(data = LatLong,
                   lng = ~long, lat = ~lat,
                   radius = ~ ifelse(rate > 0.5, 2, 10),
                   clusterOptions = markerClusterOptions(),
                   fillOpacity = 0.9,
                   group = "Clusters de proximidade") %>% 
  # --------------------------------------------------------------------------------------
  # Legenda Densidade 1
  addLegend("bottomright", pal = qpal, values = LatLong$n,
          title = "Número de<br/>observações",
          labFormat = labelFormat(prefix = "n:"),
          opacity = 1, group = "Densidade 1") %>%
  
  # Legenda Densidade 2
  addLegend("bottomright", pal = qpal, values = LatLong$n,
            title = "Número de<br/>observações",
            labFormat = labelFormat(prefix = "n:"),
            opacity = 1, group = "Densidade 2") %>% 

  # --------------------------------------------------------------------------------------  
  addLayersControl(
    baseGroups = c("OpenStreetMap","StamenToner","WorldImagery"),
    overlayGroups = c("Brasil","Bairros","Icon","Pontos","Informações dos pontos",
                      "Densidade 1","Densidade 2","HeatMap proximidade","HeatMap frequência", 
                      "Clusters de proximidade"),
    options = layersControlOptions(collapsed = F)) %>% 
  
  # Desmarcando a caixa de seleção 
  hideGroup(c("Brasil","Bairros","Icon","Pontos","Informações dos pontos",
              "Densidade 1","Densidade 2","HeatMap proximidade","HeatMap frequência",
              "Clusters de proximidade"))

mapa

#saveWidget(mapa, "W:/BRADESCO/mapa Html/Html/SP_RISK.html")

#http://datageo.ambiente.sp.gov.br/geoserver/datageo/DISTRITO_MUNICIPAL_SP_SMDU/wfs?version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=DISTRITO_MUNICIPAL_SP_SMDU
