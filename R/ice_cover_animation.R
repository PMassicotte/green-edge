rm(list = ls())

# Load data ---------------------------------------------------------------

load("data/ice_cover/shapefiles.rda")

map <- readOGR("/home/pmassicotte/Downloads/Northern_Canada_2011.shp/shapefiles/LCC_NAD83/",
               "hydro_land_p") %>% 
  spTransform(CRSobj = "+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
+ellps=WGS84 +towgs84=0,0,0 ")

# Plot th epngs -----------------------------------------------------------

for (i in 1:length(ice_cover$shapefile)) {
  
  fn <- sprintf("/home/pmassicotte/Desktop/gif/im%03d.png", i)
  
  png(fn)
  plot(map, col = "gray", main = ice_cover$date[i], lwd = 0.5)
  plot(ice_cover$shapefile[[i]], col = "red", add = TRUE, lwd = 0.5)
  dev.off()
  
}

system("convert /home/pmassicotte/Desktop/gif/*.png -delay 3 -loop 0 /home/pmassicotte/Desktop/gif/annimation.gif")
