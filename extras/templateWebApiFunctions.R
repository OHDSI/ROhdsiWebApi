#' Get the meta data for %categoryFirstUpper% definitions.
#' \lifecycle{stable}
#' @details
#' Get the meta data of WebApi specifications such as id, name, created/modified 
#' details, hash object, etc. from WebApi for %categoryFirstUpper%. This function is useful 
#' to retrieve the current %categoryFirstUpper% specifications.  
#'  
#' @template WebApiConnection                        
#' @return
#' A tibble of specification metadata for %categoryFirstUpper%. Note: modifiedDate and createdDate are
#' returned as text/character.
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' get%categoryFirstUpper%DefinitionsMetaData(wc)
#' }
#' @export
get%categoryFirstUpper%DefinitionsMetaData <- function(wc){
  metaData <- getDefinitionsMetadata(wc = wc, category = c("%category%"))
  return(metaData)
}


#' is %categoryFirstUpper% id a valid definition in the WebApi.
#' \lifecycle{stable}
#' @details
#' Checks if a set of id for a %categoryFirstUpper% is valid. The following checks are 
#' performed. 1) checks if all the ids exists in the WebApi i.e. valid. 
#'  
#' @template WebApiConnection
#' @param %category%Ids        A list of integer id(s) of the %categoryFirstUpper% to be tested for validity.
#' @return                     A logical vector indicating if an ID is valid.
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' isValid%categoryFirstUpper%Id(wc, %category%Ids = c(13242, 3423, 34))
#' }
#' @export
isValid%categoryFirstUpper%Id <- function(wc, %category%Ids) {
  result <- isValidId(wc = wc, category = '%category%', ids = %category%Ids)
  return(result)
}


#' Get %categoryFirstUpper% id definition.
#' \lifecycle{stable}
#' @details
#' Obtain the %categoryFirstUpper% definition from WebAPI for a given %categoryFirstUpper% id
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
#' get%categoryFirstUpper%Definition(wc, %category%Id = 13242)
#' }
#' @export
get%categoryFirstUpper%Definition <- function(wc, %category%Id){
  result <- getDefinition(wc = wc, id = %category%Id, category = '%category%')
  return(result)
}


#' Delete %categoryFirstUpper% id definition.
#' \lifecycle{stable}
#' @details
#' Delete the %categoryFirstUpper% definition from WebAPI for a given %categoryFirstUpper% id
#'  
#' @template WebApiConnection
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @return               None, unless error.
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' delete%categoryFirstUpper%Definition(wc, %category%Id = 13242)
#' }
#' @export
delete%categoryFirstUpper%Definition <- function(wc, %category%Id){
  result <- deleteDefinition(wc = wc, id = %category%Id, category = '%category%')
  return(result)
}


#' Check if %categoryFirstUpper% definition name exists.
#' \lifecycle{stable}
#' @details
#' Check if a string name already exists in the WebApi as a %categoryFirstUpper% definition name.
#'  
#' @template WebApiConnection
#' @param %category%Name    A string name for the %categoryFirstUpper% to be checked.
#' @return                  If found, the function will return a tibble with details of the specification.
#'                          If not found, FALSE will be returned.
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' exists%categoryFirstUpper%Name(wc, %category%Name = 'this text string needs to be checked') 
#' }
#' @export
# Check name
exists%categoryFirstUpper%Name <- function(wc, %category%Name) {
  definitionsMetaData <- get%categoryFirstUpper%DefinitionsMetaData(wc = wc)
  matched <- definitionsMetaData %>% 
    dplyr::filter(.data$name == %category%Name)
  if (nrow(matched) > 0) {
    return(matched)
  } else {
    FALSE
  }
}


#' Detect the presence of string matched %categoryFirstUpper% definitions.
#' \lifecycle{stable}
#' @details
#' Detect string matched %categoryFirstUpper% definition names from the WebApi, and retrieve
#' metadata definitions.
#'  
#' @template WebApiConnection
#' @param pattern   A pattern to look for. See \link[stringr]{str_detect} for details.
#' @param negate    If TRUE, return non-matching elements. See \link[stringr]{str_detect} for details.
#' @return          FALSE if no matches. If matched - output from \link[ROhdsiWebApi]{get%categoryFirstUpper%DefinitionsMetaData}
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' detect%categoryFirstUpper%s(wc, pattern = 'this text string to search in pattern') 
#' }
#' @export
# Check name
detect%categoryFirstUpper%sByName <- function(wc, pattern, negate = FALSE) {
  definitionsMetaData <- get%categoryFirstUpper%DefinitionsMetaData(wc = wc)
  matched <- definitionsMetaData %>% 
    dplyr::filter(stringr::str_detect(string = .data$name, pattern = pattern, negate = negate))
  if (nrow(matched) > 0) {
    return(matched)
  } else {
    return(FALSE)
  }
}




#' Post %categoryFirstUpper% definition.
#' \lifecycle{maturing}
#' @details
#' Post %categoryFirstUpper% definition to WebAPI
#'  
#' @template WebApiConnection
#' @param name           A valid name for the definition. WebApi will use this name (if valid) as
#'                       the name of the definition. WebApi checks for validity,
#'                       such as uniqueness, absence of unacceptable character etc. An error might be thrown.
#' @param %category%Definition    An R list object containing the expression for the specification. 
#'                                          This will be converted to JSON expression by function and posted into the WebApi.
#'                                          Note: only limited checks are performed in R to check the validity of this
#'                                          expression.
#' @return            This function will return a dataframe object with one row
#'                    describing the posted WebApi expression and its details.
#'                    If unsuccessful a STOP message will be shown.
#' 
#' @examples 
#' \dontrun{
#' wc <- connectWebApi(baseUrl = "http://server.org:80/WebAPI")
#' post%categoryFirstUpper%Definition(wc, name = "new valid name", 
#' %category%Definition = definition)
#' }
#' @export
post%categoryFirstUpper%Definition <- function(wc, name, %category%Definition){
  result <- postDefinition(wc, name = name, category = '%category%', definition = %category%Definition)
  return(result)
}
