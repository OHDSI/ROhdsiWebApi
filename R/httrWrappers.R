#' An internal wrapper for GET that accepts an authentication header
#' 
#' The primary reason for this wrapper is to silently ignore the authHeader if it is empty
GET <- function(url, config = list(), ..., handle = NULL){
  
  authHeader <- Sys.getenv("WEBAPI_AUTHHEADER")
 
  if (authHeader == "") {
    response <- httr::GET(url, config, handle, ...)
  } else {
    response <- httr::GET(url, config, handle, httr::add_headers(Authorization = authHeader), ...)
  }
  response
}
  
#' An internal wrapper for POST that accepts an authentication header
#' 
#' The primary reason for this wrapper is to silently ignore the authHeader if it is empty
POST <- function(url, config = list(), ..., body = NULL, 
                 encode = c("multipart", "form", "json", "raw"), handle = NULL){
  
  authHeader <- Sys.getenv("WEBAPI_AUTHHEADER")
  
  if (authHeader == "") {
    response <- httr::POST(url, config, body, encode, handle, ...)
  } else {
    response <- httr::POST(url, config, body, encode, handle, 
                           httr::add_headers(Authorization = authHeader), ...)
  }
  response
}

#' An internal wrapper for DELETE that accepts an authentication header
#' 
#' The primary reason for this wrapper is to silently ignore the authHeader if it is empty
DELETE <- function(url, authHeader, config = list(), ..., body = NULL, 
                   encode = c("multipart", "form", "json", "raw"), handle = NULL){
  
  authHeader <- Sys.getenv("WEBAPI_AUTHHEADER")
  
  if (authHeader == "") {
    response <- httr::DELETE(url, config, body, encode, handle, ...)
  } else {
    response <- httr::DELETE(url, config, body, encode, handle, 
                             httr::add_headers(Authorization = authHeader), ...)
  }
  response
}

