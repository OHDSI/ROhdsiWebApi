#' Export cohort definition set from WebAPI
#'
#' @param baseUrl                 The base URL for the WebApi instance, for example:
#'                                "http://server.org:80/WebAPI".
#' @param cohortIds               A set of cohortIds to fetch from WebApi.
#' @param generateStats           Should cohort inclusion rule statistics be generated?
#'
#' @details
#' Constructs a CohortDefinition set containing the following fields: \describe{
#' \item{atlasId}{The cohort ID in
#' ATLAS.} \item{cohortId}{a copy of the value in atlasId.} \item{cohortName}{The name of the cohort.} 
#' \item{sql}{The cohort generation sql.} \item{json}{The cohort definition JSON.}
#' \item{logicDescription}{The cohort description.}}
#'
#' @export
exportCohortDefinitionSet <- function(baseUrl, cohortIds = c(), generateStats) {
  
  if (length(cohortIds) == 0) {
    stop("Must provide a non-zero length cohortIds vector.")
  }
  cohortDefinitionSet <- data.frame(atlasId = integer(), 
                                    cohortId = integer(), 
                                    cohortName = character(), 
                                    sql=character(), 
                                    json=character(), 
                                    logicDescription=character(), stringsAsFactors = FALSE)
  
  for (i in 1:length(cohortIds)) {
    cohortId = cohortIds[i];
    writeLines(paste("Fetching cohortId:", cohortId));
    object <- getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)
    json <- .toJSON(object$expression, pretty = TRUE)
    sql <- getCohortSql(baseUrl = baseUrl, cohortDefinition = object, generateStats = generateStats)
    cohortDefinitionSet <- rbind(cohortDefinitionSet, 
                                 data.frame(atlasId = cohortId, 
                                            cohortId = cohortId, 
                                            cohortName = object$name, 
                                            sql = sql, 
                                            json = json,
                                            logicDescription = ifelse(is.null(object$description), NA, object$description),
                                            stringsAsFactors = FALSE)
                                 );
  }  
  
  return(cohortDefinitionSet);
}
