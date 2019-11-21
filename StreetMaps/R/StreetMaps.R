#library(httr)
#library(leaflet)
#library(rjson)
#library(jsonlite)

#' Draw the municipality boundaries
#' @param dataset one of the following (fi-8, ch-8, no-4, no-7, dk-4, se-4, se-7, us-4, gl-7)
#'
#' @return The object of class Maps_api.
#' @examples
#' \dontrun{
#' Maps_api("se-4")
#' Maps_api("se-7")}

Maps_api <- function(dataset) {
  path<-sprintf("/v2/%s/geo/2000", dataset)
  url <- modify_url("http://api.thenmap.net", path = path)
  ua <- user_agent("http://github.com/KarDeMumman/StreetMaps")
  resp<-GET(url,ua)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, "text"))
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "Thenmap API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  bin <- content(resp, "raw")
  writeBin(bin, "data.geojson")
  data <- geojsonio::geojson_read("data.geojson", what = "sp")
  
  map <- leaflet(data)
  map <- addTiles(map)
  map <- setView(map, lng = 18.961619, lat = 58.298584, zoom = 3)
  map <- addPolygons(map)
  #map
  #map <- addGeoJSON(map, parsed)
  
  structure(
    list(
      content = parsed,
      spdata = data,
      path = path,
      response = resp,
      map = map
    ),
    class = "Maps_api"
  )
}

