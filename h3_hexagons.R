h3_hexagons <- function(location,option = c("bb","shp"),input_sf,resolution = 9) {
  
  require(osmdata)
  require(sf)
  require(h3jsr)
  
  if(option == "bb"){
  
    boundingBox <- getbb(location,silent=F)
    
    boundingBox_sf <- getbb(location, format_out = "sf_polygon")
    
  }else{
    
    boundingBox_sf <- input_sf
  }
  
  h3_addresses <- h3jsr::polyfill(boundingBox_sf, res = resolution,simple = FALSE)
  
  grid <- h3_to_polygon(unlist(h3_addresses[[1]]), simple = FALSE)
  
}
