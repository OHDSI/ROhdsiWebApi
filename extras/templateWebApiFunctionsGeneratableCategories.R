
#' Get generation information for %categoryFirstUpper% id.
#'
#' @details
#' Get generation (execution) information about %categoryFirstUpper% for a given combination of 
#' sourceKey and %category%Id.
#'  
#' @template BaseUrl
#' @template SourceKey
#' @param %category%Id   An integer id representing the id that uniquely identifies a 
#'                       %categoryFirstUpper% definition in a WebApi instance.
#' @return
#' An R object representing the %categoryFirstUpper% definition
#' 
#' @examples 
#' \dontrun{
#' get%categoryFirstUpper%GenerationInformation(%category%Id = 13242, 
#'                                              baseUrl = "http://server.org:80/WebAPI",
#'                                              sourceKey = 'HCUP')
#' }
#' @export
get%categoryFirstUpper%GenerationInformation <- function(%category%Id, sourceKey, baseUrl){
  .checkBaseUrl(baseUrl)
  
  argument <- ROhdsiWebApi:::.getStandardCategories() %>% 
    dplyr::filter(categoryStandard == '%category%')
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(%category%Id, add = errorMessage)
  checkmate::assertScalar(sourceKey, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  url <- paste0(baseUrl, "/", argument$categoryUrl, "/", %category%Id, "/", argument$categoryUrlGenerationInformation)
  response <- httr::GET(url)
  if (!response$status_code %in% c(100,200)) {
    stop('No %categoryFirstUpper% generation information found.')
  }
  response <- httr::content(response)
  
  responseAll <- list()
  for (i in (1:length(response))) {
    responseAll[[i]] <- response[[i]] %>%
      unlist(recursive = TRUE, use.names = TRUE) %>%
      as.matrix() %>%
      t() %>%
      tidyr::as_tibble() %>%
      dplyr::mutate_if(.integerCharacters, as.integer) %>%
      dplyr::mutate_if(.numericCharacters, as.numeric) %>%
      dplyr::mutate_if(.logicalCharacters, as.logical) %>% 
      dplyr::mutate(%category%Id = !!%category%Id) %>% 
      dplyr::mutate(listId = !!i)
  }
  response <- dplyr::bind_rows(responseAll)
  
  cdmDataSources <- getCdmSources(baseUrl) %>% dplyr::select(.data$sourceId, .data$sourceKey)
  
  if ("executionInfo.id.sourceId" %in% colnames(response)) {
    names(response) <- stringr::str_replace(string = names(response), pattern = 'executionInfo.', replacement = "")
  }
  if ("id.sourceId" %in% colnames(response)) {
    names(response) <- stringr::str_replace(string = names(response), pattern = 'id.', replacement = "")
  }
  if ("sourceId" %in% colnames(response)) {
    response <- response %>% 
      dplyr::left_join(y = cdmDataSources, by = "sourceId")
  }
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
#' @return
#' An R object representing the %categoryFirstUpper% definition
#' 
#' @examples 
#' \dontrun{
#' invoke%categoryFirstUpper%Definition(%categoryFirstUpper%Id = 13242, 
#' baseUrl = "http://server.org:80/WebAPI",
#' sourceKey = 'HCUP')
#' }
#' @export
invoke%categoryFirstUpper%Definition <- function(%category%Id, baseUrl, sourceKey){
  .checkBaseUrl(baseUrl)
  
  argument <- ROhdsiWebApi:::.getStandardCategories() %>% 
    dplyr::filter(categoryStandard == '%category%')
  
  #get valid source keys from webapi
  validSourceKeys <- getCdmSources(baseUrl = baseUrl) %>% dplyr::select(sourceKey) %>% dplyr::distinct() %>% dplyr::pull()
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(%category%Id, add = errorMessage)
  checkmate::assertScalar(sourceKey, add = errorMessage)
  checkmate::assertNames(sourceKey, 
                         subset.of = validSourceKeys %>% 
                           dplyr::select(sourceKey) %>% 
                           dplyr::distinct() %>% 
                           dplyr::pull(),
                         add = errorMessage
  )
  checkmate::reportAssertions(errorMessage)
  
  if (isTRUE(isValid%categoryFirstUpper%Id(%category%Ids = %category%Id, baseUrl = baseUrl))) {
    url <- paste0(baseUrl, "/", argument$categoryUrl, "/", %category%Id)
    response <- httr::DELETE(url)
    response <- httr::http_status(response)
  } else {
    stop("%categoryFirstUpper%Id : %category%Id is not present in the WebApi.")
  }
}
