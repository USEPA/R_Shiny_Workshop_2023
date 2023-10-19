
# Write a function to retrieve echo data.

library(httr)
library(jsonlite)
library(dplyr)
library(sf)

# Create a function to pull echo data
echo_get_facilities <- function(x, drop_unknown_program=NULL){

  # bounding box of sf object
  bb <- round(as.numeric(st_bbox(st_transform(x,4326))),3)
  
  # Build url to get Query ID
  print("Getting Query ID for selected Area...")
  url <- paste0("https://echodata.epa.gov/echo/echo_rest_services.get_facilities?output=JSON&p_c1lat=",
                bb[4],"&p_c1lon=",bb[1],"&p_c2lat=",bb[2],"&p_c2lon=",bb[3])
  
  # Get Query ID
  res <- GET(url)
  data <- fromJSON(rawToChar(res$content))
  qid <- data$Results$QueryID
  
  # Use QID to pull facility data
  print("Downloading Facility Data...")
  gres <- GET(paste0("https://echodata.epa.gov/echo/echo_rest_services.get_geojson?qid=",qid))
  gdata <- fromJSON(rawToChar(gres$content))
  
  #gdata <- fromJSON(readLines(rawToChar(gres$content)), warn = F)
  
  print("Tidying Up...")
  
  # Extract facility data from API response
  features <- gdata$features
  
  # get facility coordinates
  geo <- as.data.frame(matrix(unlist(features$geometry$coordinates),ncol = 2, byrow = TRUE))%>%
    setNames(c("x","y"))
  
  # get facility info
  prop <- select(features$properties,
                 FacName,FacStreet,FacCity,FacState,FacZip,RegistryID,FacCounty,
                 FacFederalFlg,FacNAICSCodes,TscaIDs,FacSNCFlg,FacQtrsWithNC,
                 FacComplianceStatus,CAAComplianceStatus,CWAComplianceStatus,
                 RCRAComplianceStatus,SDWAComplianceStatus,FacInspectionCount,
                 FacDaysLastInspection,FacDateLastInspection,CAAEvaluationCount
                 ,CWAInspectionCount,RCRAInspectionCount)%>%
    setNames(str_replace(colnames(.),"Fac",""))
  
  # Combine and subset facility info and coordinates, then crop to spatial extent
  # get crs
  crs <- substr(st_crs(x)$input,nchar(st_crs(x)$input)-3,nchar(st_crs(x)$input))
  
  features.sf <- cbind(prop,geo)%>%
    mutate(Program = if_else(!is.na(CAAComplianceStatus),"CAA",
                             if_else(!is.na(CWAComplianceStatus),"CWA",
                                     if_else(!is.na(RCRAComplianceStatus),"RCRA",
                                             if_else(!is.na(SDWAComplianceStatus),"SDWA","Other")))),
           DateLastInspection = lubridate::ymd(substr(DateLastInspection,1,10)))%>%
    st_as_sf(coords = c("x","y"), crs = 4326)%>%
    st_transform(crs = st_crs(x))%>%
    st_crop(x)
  
  if(drop_unknown_program == TRUE){
    features.sf <- features.sf%>%
      filter(!is.na(Program))
  }
  
  return(features.sf)
  
}



# Print console output
withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}
