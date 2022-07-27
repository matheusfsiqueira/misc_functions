require(dplyr)
require(RSelenium)
require(binman)
require(wdman)
require(R.utils)
require(stringr)

# Desligar selenium -------------------------------------------------------

desabilitarSelenium <- function(rD){
  
  remDr <<- rD[["client"]]
  
  remDr$close()
  
  rD[["server"]]$stop()
  
  rm(rD, envir = .GlobalEnv)
  rm(remDr, envir = .GlobalEnv);gc()
  
}

desabilitarJava <- function(){
  
  if(.Platform$OS.type == "unix"){
    
    system("pkill java",
           intern = FALSE,
           ignore.stdout = FALSE)
  }else{
    
    system("taskkill /im java.exe /f",
           intern = FALSE,
           ignore.stdout = FALSE)
  }
  
}


# Setup Selenium ----------------------------------------------------------

habilitarSelenium <- function(tipo = "chrome"){
  
  if(exists("rD")){
    
    print("Selenium configurado!")
    
    try(desabilitarSelenium(rD))
    try(desabilitarJava())
    
  }
    
  print("Configurando Selenium...")
  
  try(desabilitarJava())
  
  # Identifica Versão atual chrome
  
  if(.Platform$OS.type == "unix") {
    
    chrome_driver_version <- system('/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome --version')
    
  }else{
    
    
    chrome_driver_version <- system2(command = "wmic",
                                     args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                     stdout = TRUE,
                                     stderr = TRUE)[3] %>% stringr::str_extract(pattern = "(?<=Version=)(\\d+\\.){3}")
    
    versaoChrome <- substr(chrome_driver_version,1,str_locate(chrome_driver_version,"\\.")-1)
    
  }
  
  # Identifica e atualiza versões chromedriver
  
  chrome <- wdman::chrome(verbose = TRUE, retcommand = TRUE, check = TRUE)
  
  available.versions <- binman::list_versions("chromedriver") 
  
  # download chromedriver específico em https://chromedriver.chromium.org/downloads, 
  
  # obter diretorio destino através do comando rappdirs::user_data_dir()
  
  if(.Platform$OS.type == "unix") {
    
    available.versions <- available.versions$mac64 %>% str_sort(numeric=T)
    
  } else {
    
    available.versions <- available.versions$win32 %>% str_sort(numeric=T)
    
  }
  
  versoesChromedriver <- data.frame(versao_detalhada = available.versions)
  
  versoesChromedriver <- versoesChromedriver %>% mutate(versao_agreg = substr(versao_detalhada,1,str_locate(versao_detalhada,"\\.")-1))
  
  versaoUtilizada <- versoesChromedriver %>% filter(versao_agreg == versaoChrome) %>% select(versao_detalhada)
  
  versaoUtilizada <- versaoUtilizada[nrow(versaoUtilizada),1]
  
  
  if(tipo == "chrome"){ # Configura driver chrome
  
    rD <- rsDriver(browser=c("chrome"),port=4444L,chromever = versaoUtilizada, verbose = F)
    
  }else{ #Configura driver firefox
  
    rD <- rsDriver(browser=c("firefox"),port=4444L,verbose = F)
    
  }
  
  output <- rD
  
}



