#' Get the meta data for %categoryFirstUpper% definitions.
#' 
#' @details
#' Get the meta data of WebApi specifications such as id, name, created/modified 
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
  baseUrl <- gsub("/$", "", baseUrl)
  metaData <- getDefinitionsMetadata(baseUrl = baseUrl, category = c("%category%"))
  return(metaData)
}


#' is %categoryFirstUpper% id a valid definition in the WebApi.
#' 
#' @details
#' Checks if a set of id for a %categoryFirstUpper% is valid. The following checks are 
#' performed. 1) checks if all the ids exists in the WebApi i.e. valid. 
#'  
#' @template BaseUrl
#' @param %category%Ids        A list of integer id(s) of the %categoryFirstUpper% to be tested for validity.
#' @return                     A logical vector indicating if an ID is valid.
#' 
#' @examples 
#' \dontrun{
#' isValid%categoryFirstUpper%Id(%category%Ids = c(13242, 3423, 34), baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
isValid%categoryFirstUpper%Id <- function(%category%Ids, baseUrl) {
  baseUrl <- gsub("/$", "", baseUrl)
  result <- isValidId(baseUrl = baseUrl, category = '%category%', ids = %category%Ids)
  return(result)
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
  baseUrl <- gsub("/$", "", baseUrl)
  result <- getDefinition(id = %category%Id, baseUrl = baseUrl, category = '%category%')
  return(result)
}


#' Delete %categoryFirstUpper% id definition.
#' 
#' @details
#' Delete the %categoryFirstUpper% definition from WebAPI for a given %categoryFirstUpper% id
#'  
#' @template BaseUrl
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @return               None, unless error.
#' 
#' @examples 
#' \dontrun{
#' delete%categoryFirstUpper%Definition(%category%Id = 13242, baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
delete%categoryFirstUpper%Definition <- function(%category%Id, baseUrl){
  baseUrl <- gsub("/$", "", baseUrl)
  result <- deleteDefinition(baseUrl = baseUrl, id = %category%Id, category = '%category%')
  return(result)
}


#' Check if %categoryFirstUpper% definition name exists.
#' 
#' @details
#' Check if a string name already exists in the WebApi as a %categoryFirstUpper% definition name.
#'  
#' @template BaseUrl
#' @param %category%Name    A string name for the %categoryFirstUpper% to be checked.
#' @return                  If found, the function will return a tibble with details of the specification.
#'                          If not found, FALSE will be returned.
#' 
#' @examples 
#' \dontrun{
#' exists%categoryFirstUpper%Name(%category%Name = 'this text string needs to be checked', 
#' baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
# Check name
exists%categoryFirstUpper%Name <- function(%category%Name, baseUrl) {
  baseUrl <- gsub("/$", "", baseUrl)
  definitionsMetaData <- get%categoryFirstUpper%DefinitionsMetaData(baseUrl = baseUrl)
  matched <- definitionsMetaData %>% 
    dplyr::filter(.data$name == %category%Name)
  if (nrow(matched) > 0) {
    return(matched)
  } else {
    FALSE
  }
}


#' Detect the presence of string matched %categoryFirstUpper% definitions.
#' 
#' @details
#' Detect string matched %categoryFirstUpper% definition names from the WebApi, and retrieve
#' metadata definitions.
#'  
#' @template BaseUrl
#' @param pattern   A pattern to look for. See \link[stringr]{str_detect} for details.
#' @param negate    If TRUE, return non-matching elements. See \link[stringr]{str_detect} for details.
#' @return          FALSE if no matches. If matched - output from \link[ROhdsiWebApi]{get%categoryFirstUpper%DefinitionsMetaData}
#' 
#' @examples 
#' \dontrun{
#' detect%categoryFirstUpper%s(pattern = 'this text string to search in pattern', 
#' baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
# Check name
detect%categoryFirstUpper%sByName <- function(pattern, negate = FALSE, baseUrl) {
  baseUrl <- gsub("/$", "", baseUrl)
  definitionsMetaData <- get%categoryFirstUpper%DefinitionsMetaData(baseUrl = baseUrl)
  matched <- definitionsMetaData %>% 
    dplyr::filter(stringr::str_detect(string = .data$name, pattern = pattern, negate = negate))
  if (nrow(matched) > 0) {
    return(matched)
  } else {
    return(FALSE)
  }
}




#' Post %categoryFirstUpper% definition.
#' 
#' @details
#' Post %categoryFirstUpper% definition to WebAPI
#'  
#' @template BaseUrl
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
#' post%categoryFirstUpper%Definition(name = "new valid name", 
#' %category%Definition = definition, 
#' baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
post%categoryFirstUpper%Definition <- function(name, %category%Definition, baseUrl){
  baseUrl <- gsub("/$", "", baseUrl)
  result <- postDefinition(name = name, baseUrl = baseUrl, category = '%category%', definition = %category%Definition)
  return(result)
}


#' Update a %categoryFirstUpper% definition.
#' 
#' @details
#' Update a %categoryFirstUpper% definition.
#'
#' @template BaseUrl
#' @param %category%Definition    An R list object containing the expression for the specification.
#'                                Must include id, name and expression.
#'                                This will be converted to JSON expression by function and posted into the WebApi.
#'                                The definition will be checked against the WebApi instance for errors
#'
#' @examples
#' \dontrun{
#' definition <- get%categoryFirstUpper%Definition(id = 13242, baseUrl = "http://server.org:80/WebAPI", category = %category%)
#' definition$name <- "My new name for this"
#' update%categoryFirstUpper%(%category%Definition, baseUrl, category = "cohort")
#' }
#' @export
# Check name
update%categoryFirstUpper%Definition <- function(%category%Definition, baseUrl) {
  baseUrl <- gsub("/$", "", baseUrl)
  result <- updateDefinition(baseUrl = baseUrl, definition = %category%Definition, category = '%category%')
  return(result)
}

