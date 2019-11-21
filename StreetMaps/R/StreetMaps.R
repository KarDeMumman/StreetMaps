#library(httr)
#library(leaflet)
#library(rjson)
#library(jsonlite)

#' Retrieve the municipality/country boundaries from the year specified (default 2010)
#' @param dataset one of the following (fi-8, ch-8, dk-7, se-7, us-4, gl-7, world-2)
#' @param year the year for the data to be retrieved (2010-present)
#'
#' @return The object of class Maps_api.
#' @examples
#' \dontrun{
#' Maps_api("us-4")
#' Maps_api("se-7, 2015")}

Maps_api <- function(dataset, year=2010) {
  path<-sprintf("/v2/%s/geo/%s", dataset, year)
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
  if(file.exists("data.geojson")) invisible(file.remove("data.geojson"))
  
  #map <- leaflet(data)
  #map <- addTiles(map)
  #map <- setView(map, lng = 18.961619, lat = 58.298584, zoom = 3)
  #map <- addPolygons(map)
  #map
  #map <- addGeoJSON(map, parsed)
  
  structure(
    list(
      content = parsed,
      spdata = data,
      path = path,
      response = resp
    ),
    class = "Maps_api"
  )
}

