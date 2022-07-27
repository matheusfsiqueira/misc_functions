

# Script para obter DEM de elevação para um dado local --------------------


getElevationData <- function(location,source="aws",zoom = 10,dest_file="exportDEM.tif"){
  
  require(elevatr)
  require(osmdata)
  require(sf)
  require(raster)
  
  
  if (file.exists(dest_file)) {
    #Delete file if it exists
    file.remove(dest_file)
  }
  
  # Set Bounding Box
  
 
  boundingBox <- getbb(location,silent=F)
  
  boundingBox_sf <- getbb(location, format_out = "sf_polygon")
  
  
  # Get raster
  
  
  elevatr::set_opentopo_key("1132aca642400abd0d668982f2551a58")
  
  dem_AreaEstudo <- get_elev_raster(boundingBox_sf, src = source, clip = "bbox",expand = 0, z = zoom) # src = c("aws",gl3“,”gl1“,”alos“,”srtm15plus"), expand = sobra após o bbox
  
  # notas sobre zoom: https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
  
  # Output
  
  writeRaster(dem_AreaEstudo,dest_file)
  
  return(dem_AreaEstudo)
  
  
}
