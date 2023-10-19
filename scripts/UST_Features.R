# Download USTFinder Data
# Facilities
url0 <- parse_url("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services")
url0$path <- paste(url0$path, "UST_Finder_Feature_Layer_2/FeatureServer/0/query", sep = "/")
url0$query <- list(geometryType="esriGeometryEnvelope",
                   geometry=bbStr,
                   outFields = "*",
                   returnGeometry = "true",
                   f = "geojson")
request0 <- build_url(url0)

fac <- st_read(request0)

# Fix County names in facilities data
cntys <- st_read(here("boundaries/counties.shp"))%>%
  filter(STATE_NAME %in% riskStates$NAME)%>%
  select(NAME)%>%
  st_transform(st_crs(facilities.active))

cnty.intersect <- st_intersection(facilities.active,cntys)


# Tanks
# USTs
url4 <- parse_url("https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services")
url4$path <- paste(url4$path, "UST_Finder_Feature_Layer_2/FeatureServer/4/query", sep = "/")
url4$query <- list(where = wc,
                   outFields = "*",
                   f = "json")
request4 <- build_url(url4)

ust <- st_read(request4)

ust.active <- ust%>%
  filter(Facility_ID %in% out$Facility_ID & Tank_Status == "Open")



# Fix County names in facilities data
cntys <- st_read(here("boundaries/counties.shp"))%>%
  filter(STATE_NAME %in% riskStates$NAME)%>%
  select(NAME)%>%
  st_transform(st_crs(facilities.active))

cnty.intersect <- st_intersection(facilities.active,cntys)