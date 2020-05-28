

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
#' @param    ids        A list of integer id(s) of the %categoryFirstUpper% to be tested for validity.
#' @return
#' A logical vector indicating which of the ids are valid.
#' 
#' @examples 
#' \dontrun{
#' isValid%categoryFirstUpper%Id(ids = c(13242, 3423, 34), baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
isValid%categoryFirstUpper%Id <- function(ids, baseUrl){
  .checkBaseUrl(baseUrl)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(ids, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  validIds <- getDefinitionsMetadata(baseUrl = baseUrl, categories = "%category%")
  return(ids %in% validIds)
}




#' Get %categoryFirstUpper% id definition.
#'
#' @details
#' Obtain the %categoryFirstUpper% definition from WebAPI for a given %categoryFirstUpper% id
#'  
#' @template BaseUrl
#' @template %categoryFirstUpper%Id
#' @return
#' An R object representing the %categoryFirstUpper% definition
#' 
#' @examples 
#' \dontrun{
#' get%categoryFirstUpper%Definition(%categoryFirstUpper%Id = 13242, baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
get%categoryFirstUpper%Definition <- function(%categoryFirstUpper%Id, baseUrl){
  .checkBaseUrl(baseUrl)
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(%categoryFirstUpper%Id, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  if (isTRUE(isValid%categoryFirstUpper%Id(ids = %categoryFirstUpper%Id, baseUrl = baseUrl))) {
    url <- paste0(baseUrl, "/", "%categoryWebApi%", "/", %categoryFirstUpper%Id)
    metaData <- httr::GET(url)
    metaData <- httr::content(metaData)
    if (!is.null(metaData$payload$message)) {
      stop(metaData$payload$message)
    }
    return(metaData)
  } else {
    stop(paste0(%category%Id, ":", %categoryFirstUpper%Id, " is not present in the WebApi."))
  }
}
