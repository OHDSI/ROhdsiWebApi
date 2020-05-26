
  
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
#' %functionNameDefinitionsMetaData%(baseUrl = "http://server.org:80/WebAPI")
#' 
#' @export
%functionNameDefinitionsMetaData% <- function(baseUrl) {
  .checkBaseUrl(baseUrl)
  return(getWebApiMetadata(baseUrl = baseUrl,
                           categories = c("%category%")
                           )
         )
}


#' Check if %categoryFirstUpper% id is valid.
#'
#' @details
#' Checks if a set of id for a %categoryFirstUpper% is valid, i.e. checks if all the ids
#' exists in the WebApi i.e. valid. 
#'  
#' @template BaseUrl
#' @param    ids        A list of integer id(s) of the %categoryFirstUpper% to be tested for validity.
#' @param    verbose    Do you want to know what ids are invalid? This parameter will return
#'                      list of integers representing invalid ids. Default = FALSE
#' @return
#' Function will return TRUE if all ids in the set are 
#' valid. Function will return FALSE if any id is not valid. Optionally, specify = TRUE
#' will return a list of ids that are no valid.
#' 
#' @examples 
#' %functionNameIsValid%(baseUrl = "http://server.org:80/WebAPI", ids = c(13242, 3423, 34))
#' 
#' @export
%functionNameIsValid% <- function(baseUrl, ids, verbose = FALSE) {
  .checkBaseUrl(baseUrl)
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(ids, add = errorMessage)
  checkmate::assertLogical(verbose, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  validIds <- getWebApiMetadata(baseUrl = baseUrl,
                                categories = c("%category%")
  )
  
  invalidIds <- tidyr::tibble(id = ids) %>%
    dplyr::anti_join(y = validIds,
                     by = c('id' = 'id')
    )
  
  
    if (nrow(invalidIds) > 0) {
      if (verbose == TRUE) {
        return(invalidIds %>% dplyr::select(id) %>% dplyr::distinct() %>% dplyr::pull())
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
}
