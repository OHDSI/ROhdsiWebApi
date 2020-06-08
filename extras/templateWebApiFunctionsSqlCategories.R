
#' Get SQL query for %categoryFirstUpper% definition.
#'
#' @details
#' Given a valid %categoryFirstUpper% definition R-object (not JSON) 
#' this function will return the parameterized SQL in OHDSI SQL dialect. This
#' SQL then may be used along with OHDSI R-package 'SQLRender' to render/translate to target
#' SQL dialect and parameters rendered.
#'  
#' @template BaseUrl
#' @param %category%Definition   An R list object (not JSON) representing the %categoryFirstUpper% 
#'                               definition. It is the output R expression object of
#'                               list object from \code{%categoryFirstUpper%Definition}
#' @return
#' An R object containing the SQL for %categoryFirstUpper% definition.
#' 
#' @examples 
#' \dontrun{
#' get%categoryFirstUpper%Sql(%categoryFirstUpper%Definition = 
#' (get%categoryFirstUpper%Definition(cohortId = 13242, baseUrl = baseUrl)), 
#' baseUrl = "http://server.org:80/WebAPI")
#' }
#' @export
get%categoryFirstUpper%Sql <- function(%category%Definition, baseUrl){
  .checkBaseUrl(baseUrl)
  
  arguments <- .getStandardCategories()
  argument <- arguments %>% 
    dplyr::filter(.data$categoryStandard == '%category%')
  
  if (!'%category%' %in% c('cohort')) {
    ParallelLogger::logError("Retrieving SQL for ", argument$categoryFirstUpper, " is not supported")
    stop()
  }
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertList(x = %category%Definition, min.len = 1, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  url <- paste0(baseUrl, "/", argument$categoryUrl, "/sql/")
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  
  if ('cohort' %in% "%category%") {
    if ('expression' %in% names(%category%Definition)) {
      expression <- %category%Definition$expression
    } else {
      expression <- %category%Definition
    }
  }
  
  # this code does not work for incidence rate
  # if ('incidenceRate' %in% "%category%") {
  #   expression <- list()
  #   if ("expression" %in% names(Definition)) {
  #     expression$expression <- Definition$expression
  #     expression$option <- list(cohortTable = "@cohortTable",
  #                               tempSchema = "@tempSchema",
  #                               vocabularySchema = "@vocabularyDatabaseSchema",
  #                               resultsSchema = "@resultsDatabaseSchema",
  #                               cdmSchema = "@cdmDatabaseSchema")
  #     
  #   } else {
  #     expression$expression <- Definition
  #     expression$option <- list(cohortTable = "@cohortTable",
  #                               tempSchema = "@tempSchema",
  #                               vocabularySchema = "@vocabularyDatabaseSchema",
  #                               resultsSchema = "@resultsDatabaseSchema",
  #                               cdmSchema = "@cdmDatabaseSchema")
  #   }
  # }
  validJsonExpression <- RJSONIO::toJSON(expression)
  body <- RJSONIO::toJSON(list(expression = RJSONIO::fromJSON(validJsonExpression)), digits = 23)
  req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
  sql <- (httr::content(req))$templateSql
  return(sql)
}
