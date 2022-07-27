# Verificações Java

#   rJava::.jinit()
#   rJava::.jcall("java.lang.System", "S", "getProperty", "java.version")

#   system("java -version")

# melhor forma de trocar a versão do java é ajustar a environment variable JAVA_HOME. A estratégia consiste em mudar a variavel do usuario (pq nao exige previlegio de adm), setando depois JAVA_HOME como variavel derivada do usuário.
# Variaveis do usuario sobrepoem as do sistema, mas o path do sistema vem antes (e é lido primeiro)

# Setup R5r ---------------------------------------------------------------

configura_r5r <- function(diretorio){
  
  # requer o java development kit 11
  
  system("setx JAVA_HOME \"C:\\Program Files\\Java\\jdk-11.0.15.1\"")
  
  require(r5r)
  
  options(java.parameters = "-Xmx10G")
  
  r5r_core <- setup_r5(data_path = diretorio, 
                       overwrite = FALSE, # do contrario sempre vai ficar construindo a rede (caso queira atualizar, deletar o arquivo dat)
                       verbose = F)
  
  r5r_sitrep()
  
  return(r5r_core)
  
}


# Setup Open Trip Planner -------------------------------------------------

# repositório: https://repo1.maven.org/maven2/org/opentripplanner/otp/,

# discussão sobre versões OTP: http://docs.opentripplanner.org/en/latest/Version-Comparison/

# referencia: https://docs.ropensci.org/opentripplanner/reference/


createDirectoriesOTP <- function(path,graph){
  
  dir.create(paste0(path,"/graphs"))
  
  dir.create(paste0(path,"/graphs/",graph))
  
}

configura_OTP <- function(diretorio,router){
  
  # requer o java 8
  
  system("setx JAVA_HOME \"C:\\Program Files\\Java\\jre1.8.0_333\"")
  
  N <- 8 # numero de gigas de memoria
  
  require(opentripplanner) 
  
  # otp_check_java()
  
  path_data <- diretorio
  
  # Download OTP
  
  path_otp <- otp_dl_jar(version = "1.5.0", cache = T) 
  
  
  # Create directory
  
  createDirectoriesOTP(path_data,router)
  
  # Configuration, ref: https://opentripplanner.readthedocs.io/en/latest/Configuration/
  
  otpConfig_otp <<- otp_make_config("otp") # c("otp", "build", "router")
  
  otpConfig_build <<- otp_make_config("build") 
  
  otpConfig_router <<- otp_make_config("router")
  
  # Build graph
  
  log1 <- otp_build_graph(otp = path_otp, dir = path_data, router = router, memory = 1024 * N) 
  
  # Launch OTP and load the graph
  
  log2 <- otp_setup(otp = path_otp, dir = path_data,router= router)
  
  # Connection to OTP
  
  otpcon <- otp_connect(router = router,timezone = "America/Fortaleza")
  

}


# Configura Dodgr ---------------------------------------------------------

configura_dodgr <- function(location,dest_dir="",overwrite = FALSE){
  
  
  if(!is.character(location)){
    
    require(sf)
    
    location <- as.data.frame(location %>% st_coordinates())
    
  }
  
  cache <- ifelse(file.exists(paste0(dest_dir,"/Dodgr_weightedNetwork_sc.RDS")),TRUE,FALSE)
    
  if(cache == FALSE | overwrite == TRUE){
  
      require(dodgr)
      require(osmdata)
      
      print("Baixando dados OSM...")
      
      #bb <- osmdata::getbb(location)
      
      if(is.character(location)){
    
          net <- dodgr_streetnet (bbox = location, quiet = F)
          net_sc <- dodgr_streetnet_sc (bbox = location, quiet = F)
      
      }else{
        
          net <- dodgr_streetnet (pts = location, quiet = F)
          net_sc <- dodgr_streetnet_sc (pts = location, quiet = F)
        
      }
      
      print("Configurando grafos...")
      
      wp <- weighting_profiles$weighting_profiles
      
      net_weighted <- weight_streetnet (net, wt_profile = "motorcar") # c(foot, horse, wheelchair, bicycle, moped, motorcycle, motorcar, goods, hgv,psv)
      net_weighted_sc <- weight_streetnet (net_sc, wt_profile = "motorcar") # c(foot, horse, wheelchair, bicycle, moped, motorcycle, motorcar, goods, hgv,psv)
      
      saveRDS(net,file=paste0(dest_dir,"/Dodgr_network.RDS"))
      saveRDS(net_sc,file=paste0(dest_dir,"/Dodgr_network_sc.RDS"))
      saveRDS(net_weighted,file=paste0(dest_dir,"/Dodgr_weightedNetwork.RDS"))
      saveRDS(net_weighted_sc,file=paste0(dest_dir,"/Dodgr_weightedNetwork_sc.RDS"))
      
  }else{
    
      net_weighted <- readRDS(paste0(dest_dir,"/Dodgr_weightedNetwork_sc.RDS"))
  }
  
  return(net_weighted)
  
}


# Limpeza R5r e OTP-------------------------------------------------------------

finaliza_r5r <- function(r5r_core){
  
  stop_r5(r5r_core)
  rJava::.jgc(R.gc = TRUE)
  
}

finaliza_OTP <- function(){
  
  otp_stop()
  
}

