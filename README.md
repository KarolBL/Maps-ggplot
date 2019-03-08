# Maps with R
Creating maps with R library `ggplot`.

## Required packages and functions

### Importing data  

`rgdal::readOGR()` : shapefiles

`ggmap::getmap()` : raster from Google maps API 

### Coropleth map

`ggplot2::geom_polygon()` : primitive polygons

`ggplot()+geom_map()` : maps + data

`mxmaps::` : ad hoc function

### Labeling

`ggplot2::geom:label` : as text

`sp::coordinates()` : location in centroids

`ifelse()+ggreppel::geom_label_repel()` : partial labeling
