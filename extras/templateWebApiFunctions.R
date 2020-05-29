

#' Retrieve the meta data for %categoryFirstUpper% definitions.
#'
#' @details
#' Obtains the meta data of WebApi specifications such as id, name, created/modified 
#' details, hash object, etc. from WebApi for %categoryFirstUpper%. This function is useful 
#' to retrieve the current %categoryFirstUpper% specifications.  
#'  
#' @template BaseUrl
#' @return
#' A tibble of specification metadata for %categoryFirstUpper%. Note: modifiedDate and createdDate are
#' returned as text/character.
#' 
#' @examples 
#' \dontrun{
#' get%categoryFirstUpper%DefinitionsMetaData(baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
get%categoryFirstUpper%DefinitionsMetaData <- function(baseUrl){
  .checkBaseUrl(baseUrl)
  return(getDefinitionsMetadata(baseUrl = baseUrl,
                                      categories = c("%category%")))
}


#' Check if %categoryFirstUpper% id is valid.
#'
#' @details
#' Checks if a set of id for a %categoryFirstUpper% is valid, i.e. checks if all the ids
#' exists in the WebApi i.e. valid. 
#'  
#' @template BaseUrl
#' @param %category%Ids        A list of integer id(s) of the %categoryFirstUpper% to be tested for validity.
#' @return
#' A logical vector indicating which of the ids are valid.
#' 
#' @examples 
#' \dontrun{
#' isValid%categoryFirstUpper%Id(%category%Ids = c(13242, 3423, 34), baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
isValid%categoryFirstUpper%Id <- function(%category%Ids, baseUrl){
  .checkBaseUrl(baseUrl)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(%category%Ids, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  validIds <- getDefinitionsMetadata(baseUrl = baseUrl, categories = "%category%")
  return(%category%Ids %in% validIds$id)
}




#' Get %categoryFirstUpper% id definition.
#'
#' @details
#' Obtain the %categoryFirstUpper% definition from WebAPI for a given %categoryFirstUpper% id
#'  
#' @template BaseUrl
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @return
#' An R object representing the %categoryFirstUpper% definition
#' 
#' @examples 
#' \dontrun{
#' get%categoryFirstUpper%Definition(%category%Id = 13242, baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
get%categoryFirstUpper%Definition <- function(%category%Id, baseUrl){
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(%category%Id, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  if (isTRUE(isValid%categoryFirstUpper%Id(%category%Ids = %category%Id, baseUrl = baseUrl))) {
    url <- paste0(baseUrl, "/", "%categoryWebApi%", "/", %category%Id)
    if ('characterization' == "%category%") {
      url <- paste0(url, "/export")
    }
    metaData <- httr::GET(url)
    metaData <- httr::content(metaData)
    if (!is.null(metaData$payload$message)) {
      stop(metaData$payload$message)
    }
    
    if (is.null(metaData$expression)) {
      if (!is.null(metaData$specification)) {
        metaData$expression <- metaData$specification
        metaData$specification <- NULL
      } else {
        url <- paste0(url, "/expression")
        data <- httr::GET(url)
        data <- httr::content(data)
        metaData$expression <- data
      }
    }
    if (is.character(metaData$expression)) {
      metaData$expression <- RJSONIO::fromJSON(metaData$expression)
    }
    return(metaData)
  } else {
    stop("%categoryFirstUpper%Id : %category%Id is not present in the WebApi.")
  }
}




#' Delete %categoryFirstUpper% id definition.
#'
#' @details
#' Delete the %categoryFirstUpper% definition from WebAPI for a given %categoryFirstUpper% id
#'  
#' @template BaseUrl
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @return
#' An R object representing the %categoryFirstUpper% definition
#' 
#' @examples 
#' \dontrun{
#' delete%categoryFirstUpper%Definition(%category%Id = 13242, baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
delete%categoryFirstUpper%Definition <- function(%category%Id, baseUrl){
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(%category%Id, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  if (isTRUE(isValid%categoryFirstUpper%Id(%category%Ids = %category%Id, baseUrl = baseUrl))) {
    url <- paste0(baseUrl, "/", "%categoryWebApi%", "/", %category%Id)
    response <- httr::DELETE(url)
    response <- httr::http_status(response)
  } else {
    stop("%categoryFirstUpper%Id : %category%Id is not present in the WebApi.")
  }
}
