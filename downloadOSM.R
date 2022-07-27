
# Download OpenStreetMaps ----------------------------------------------------

download_OSM <- function(location,option = c("sf","pbf"),osm_layer = "highway",dest_path="") {
  
  dest_path <- paste0(getwd(),"/",dest_path)
  
  require(osmdata)
  require(sf)
  
  boundingBox <- getbb(location,silent=F)
  
  boundingBox_sf <- getbb(location, format_out = "sf_polygon")
  
  boundingBox_stBbox <- st_bbox(boundingBox_sf)
  
  # Download como objeto sf
  
  if(option == "sf"){
    
    dadosOSM <- opq(bbox = boundingBox) %>%
      add_osm_feature(key = osm_layer) %>% 
      osmdata_sf ()
    
    return(dadosOSM)
    
  }else {
    
    # Download arquivos .PBF
    
    require(stringr)
    
    nomeArquivo <- paste0(str_replace_all(str_replace_all(location,",","_")," ",""),".osm")
    
    require(osmextract)
    
    require(OSMtools) # remotes::install_github("ITSleeds/OSMTools")
    
    location_OSM <- oe_match(location)
    
    # Download geofabrik
    
    httr::timeout(600)
    options(timeout = 600)
    
    dadosOSM <- oe_download(location_OSM$url, provider="geofabrik", force_download=F, file_basename = paste0(nomeArquivo,".pbf"), download_directory = dest_path)
    
    arquivoGeofabrik <- paste0(dest_path,"/","geofabrik_",nomeArquivo)
    
    # Clip do bounding box
    
    OSMtools::osmt_convert(paste0(arquivoGeofabrik,".pbf"),format_out="o5m", bbox = boundingBox_stBbox)
    
    # Filtro layer
    
    if(osm_layer != ""){
      
      filename_output <- paste0(dest_path,"/",osm_layer,"_",nomeArquivo)
      
      OSMtools::osmt_filter(file = paste0(arquivoGeofabrik,".o5m"), path_out = paste0(filename_output,".o5m"), keep = paste0(osm_layer,"="))
      
      OSMtools::osmt_convert(paste0(filename_output,".o5m"),format_out="pbf")
      
    }else{
      
      # output final
      
      filename_output <- paste0(dest_path,"/",nomeArquivo)
      
      OSMtools::osmt_convert(paste0(filename_output,".o5m"),format_out="pbf")
    }
    
    # output em Geopackage
    
    dadosOSM_gpkg <- oe_vectortranslate(paste0(filename_output,".pbf"), layer= "lines",force_vectortranslate=T)
    
    return(dadosOSM_gpkg)
    
  }
}

