
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
  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertList(x = %category%DefinitionExpression, min.len = 1, add = errorMessage)
  checkmate::reportAssertions(errorMessage)
  
  url <- paste0(baseUrl, "/", "%categoryWebApi%", "/sql/")
  httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
  
  if ('expression' %in% names(%category%Definition)) {
    expression <- %category%Definition$expression
  } else {
    expression <- %category%Definition
  }
  validJsonExpression <- RJSONIO::toJSON(expression)
  body <- RJSONIO::toJSON(list(expression = RJSONIO::fromJSON(validJsonExpression)), digits = 23)
  req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
  sql <- (httr::content(req))$templateSql
  return(sql)
}
