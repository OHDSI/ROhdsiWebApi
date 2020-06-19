
#' Get generation information for %categoryFirstUpper% id.
#'
#' @details
#' Get generation (execution) information about %categoryFirstUpper% for a %category%Id.
#'  
#' @template WebApiConnection
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @return
#' An R object representing the %categoryFirstUpper% definition
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' get%categoryFirstUpper%GenerationInformation(wc, %category%Id = 13242)
#' }
#' @export
get%categoryFirstUpper%GenerationInformation <- function(wc, %category%Id){
  .checkBaseUrl(wc$baseUrl)
  response <- getGenerationInformation(wc = wc, id = %category%Id, category = '%category%')
 return(response)
}


#' Invoke generation of %categoryFirstUpper% id.
#'
#' @details
#' Invoke the generation of %categoryFirstUpper% id in the WebApi.
#'  
#' @template WebApiConnection
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @template SourceKey
#' @return   A tibble with job status information.
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' invoke%categoryFirstUpper%Generation(wc, 
#' %category%Id = 13242,
#' sourceKey = 'HCUP')
#' }
#' @export
invoke%categoryFirstUpper%Generation <- function(wc, %category%Id, sourceKey){
  .checkBaseUrl(wc$baseUrl)
  response <-
    invokeGeneration(
      wc,
      id = %category%Id,
      category = '%category%',
      sourceKey = sourceKey
    )
  return(response)
}

#' Cancel generation of %categoryFirstUpper% id.
#'
#' @details
#' Cancel the generation of %categoryFirstUpper% id in the WebApi.
#'  
#' @template WebApiConnection
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @template SourceKey
#' @return   A tibble with job status information.
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' cancel%categoryFirstUpper%Generation(wc, %category%Id = 13242, 
#' sourceKey = 'HCUP')
#' }
#' @export
cancel%categoryFirstUpper%Generation <- function(wc, %category%Id, sourceKey){
  .checkBaseUrl(wc$baseUrl)
  response <-
    cancelGeneration(
      wc,
      id = %category%Id,
      category = '%category%',
      sourceKey = sourceKey
    )
  return(response)
}


#' Get results for a %categoryFirstUpper% Id.
#'
#' @details
#' Get the results for %categoryFirstUpper% id.
#'  
#' @template WebApiConnection
#' @template %categoryFirstUpper%Id
#' @return            An R object with results.
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' get%categoryFirstUpper%Results(wc, %category%Id = 342)
#' }
#' @export
# Check name
get%categoryFirstUpper%Results <- function(wc, %category%Id) {
  result <- getResults(wc = wc, id = %category%Id, category = '%category%')
  return(result)
}
