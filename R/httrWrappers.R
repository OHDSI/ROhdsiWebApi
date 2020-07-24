#' Internal wrappers for httr request functions 
#' 
#' The primary reason for this wrapper is to add the appropriate authorization header to requests.
#' This file also centralizes http error handling.
GET <- function(url, config = list(), ..., handle = NULL) {
  request(url, method = "GET", config = config, handle = handle, ...)
}
  
POST <- function(url, config = list(), ..., body = NULL, encode = "json", handle = NULL) {
  request(url, method = "POST", config = config, handle = handle, body = body, encode = encode, ...)
}

PUT <- function(url, config = list(), ..., body = NULL, encode = "json", handle = NULL) {
  request(url, method = "PUT", config = config, handle = handle, body = body, encode = encode, ...)
}

DELETE <- function(url, config = list(), ..., body = NULL, encode = "json", handle = NULL){
  request(url, method = "DELETE", config = config, handle = handle, body = body, encode = encode, ...)
}

request <- function(url, method, config, handle, body = NULL, encode = "json", ...) {
  method <- switch(
    method,
    GET    = httr::GET,
    POST   = httr::POST,
    PUT    = httr::PUT,
    DELETE = httr::DELETE,
    stop(paste("Not a recognized HTTP method:", method))
  )
  
  # find baseUrl from cached baseUrls
  baseUrls <- names(ROWebApiEnv)
  baseUrl <- baseUrls[stringr::str_detect(url, baseUrls)]
  
  if (length(baseUrl) == 1 && !is.null(ROWebApiEnv[[baseUrl]]$authHeader)) {
    authHeader <- ROWebApiEnv[[baseUrl]]$authHeader
    response <- method(url = url, 
                       config = config,
                       handle = handle,
                       body = body,
                       encode = encode,
                       httr::add_headers(Authorization = authHeader),
                       ...)
  } else {
    response <- method(url = url, 
                       config = config,
                       handle = handle,
                       body = body,
                       encode = encode,
                       ...)
  }
  
  # centralized http error handling for all requests
  if(httr::status_code(response) == 401) {
    rlang::abort("http error 401: Unauthorized request.
       Try running `authorizeWebAPI()`", 
       class = "http-unauthorized")
  }
  
  if(httr::status_code(response) == 403) {
    rlang::abort("http error 403: Forbidden request.
       You do not have permission to perform this action.",
       class = "http-forbidden")
  }
  
  if(httr::status_code(response) == 404) {
    rlang::abort("http error 404: Resource not found.
       The resource or action you requested was not found by WebAPI",
       class = "http-notfound")
  }
  
  if(httr::status_code(response) == 500) {
    rlang::abort("http error 500: Internal server error.
       The server encountered a problem when trying to fulfill this request.",
       class = "http-server")
  }
  response
}
