library(httr)



Maps_api <- function(path) {
  url <- modify_url("http://api.thenmap.net", path = path)
  ua <- user_agent("http://github.com/KarDeMumman/StreetMaps")
  
 
  resp<-GET(url,ua)
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
    
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "Maps_api"
  )
}

print.Maps_api <- function(x, ...) {
  cat("<GitHub ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
} 
  

#resp <- Maps_api("/v2/se-7/data/1974")


