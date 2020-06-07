
#' Get generation information for %categoryFirstUpper% id.
#'
#' @details
#' Get generation (execution) information about %categoryFirstUpper% for a %category%Id.
#'  
#' @template BaseUrl
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @return
#' An R object representing the %categoryFirstUpper% definition
#' 
#' @examples 
#' \dontrun{
#' get%categoryFirstUpper%GenerationInformation(%category%Id = 13242, 
#'                                              baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
get%categoryFirstUpper%GenerationInformation <- function(%category%Id, baseUrl){
  .checkBaseUrl(baseUrl)
  response <- getGenerationInformation(id = %category%Id, baseUrl = baseUrl, category = '%category%')
 return(response)
}


#' Invoke generation of %categoryFirstUpper% id.
#'
#' @details
#' Invoke the generation of %categoryFirstUpper% id in the WebApi.
#'  
#' @template BaseUrl
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @template SourceKey
#' @return   A tibble with job status information.
#' 
#' @examples 
#' \dontrun{
#' invoke%categoryFirstUpper%Generation(%categoryFirstUpper%Id = 13242, 
#' baseUrl = "http://server.org:80/WebAPI",
#' sourceKey = 'HCUP')
#' }
#' @export
invoke%categoryFirstUpper%Generation <- function(%category%Id, baseUrl, sourceKey){
  .checkBaseUrl(baseUrl)
  response <-
    invokeGeneration(
      id = %category%Id,
      baseUrl = baseUrl,
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
#' @template BaseUrl
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @template SourceKey
#' @return   A tibble with job status information.
#' 
#' @examples 
#' \dontrun{
#' cancel%categoryFirstUpper%Generation(%categoryFirstUpper%Id = 13242, 
#' baseUrl = "http://server.org:80/WebAPI",
#' sourceKey = 'HCUP')
#' }
#' @export
cancel%categoryFirstUpper%Generation <- function(%category%Id, baseUrl, sourceKey){
  .checkBaseUrl(baseUrl)
  response <-
    cancelGeneration(
      id = %category%Id,
      baseUrl = baseUrl,
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
#' @template BaseUrl
#' @template %categoryFirstUpper%Id
#' @return            An R object with results.
#' 
#' @examples 
#' \dontrun{
#' get%categoryFirstUpper%Results(%category%Id = 342, 
#' baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
# Check name
get%categoryFirstUpper%Results <- function(%category%Id, baseUrl) {
  result <- getResults(baseUrl = baseUrl, id = %category%Id, category = '%category%')
  return(result)
}
