

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
get%categoryFirstUpper%DefinitionsMetaData <- function(baseUrl) {
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
isValid%categoryFirstUpper%Id <- function(ids, baseUrl) {
  .checkBaseUrl(baseUrl)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(ids, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  validIds <- getDefinitionsMetadata(baseUrl = baseUrl, categories = "%category%")
  return(ids %in% validIds)
}
