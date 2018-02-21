################################################################
# Function Definition
################################################################

# Transfer the coordinates into state name
which_state <- function(mapData, long, lat) {
  mapData$long_diff <- mapData$long - long
  mapData$lat_diff <- mapData$lat - lat
  
  # only compare borders near the clicked point to save computing time
  mapData <- mapData[abs(mapData$long_diff) < 20 & abs(mapData$lat_diff) < 15, ]
  
  # calculate the angle between the vector from this clicked point to border and c(1, 0)
  vLong <- mapData$long_diff
  vLat <- mapData$lat_diff
  mapData$angle <- acos(vLong / sqrt(vLong^2 + vLat^2))
  
  # calculate range of the angle and select the state with largest range
  rangeAngle <- tapply(mapData$angle, mapData$region, function(x) max(x) - min(x))
  return(names(sort(rangeAngle, decreasing = TRUE))[1])
}

# Proper function for turning the first letter into upper case 
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
dt.data.scale$STATE <- proper(dt.data.scale$STATE)
dt.data.scale[,c(5,6,7)] <- (dt.data.scale[,c(5,6,7)])*(-1)