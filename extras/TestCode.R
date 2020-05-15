library(ROhdsiWebApi)
baseUrl <- Sys.getenv("baseUrl")

estimation <- getEstimationDefinition(estimationId = 124, baseUrl = baseUrl)

cohortId <- estimation$specification$cohortDefinitions[[1]]$id

cohortDefinition <- getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)

conceptSetDefinition <- cohortDefinition$expression$ConceptSets[[1]]

conceptSetDefinition <- getConceptSetDefinition(conceptSetId = estimation$specification$conceptSets[[1]]$id, baseUrl = baseUrl)

conceptSetDefinition <- getConceptSetDefinition(conceptSetId = 10677, baseUrl = baseUrl)

convertConceptSetDefinitionToTable(conceptSetDefinition)


conceptIds <- resolveConceptSet(conceptSetDefinition = conceptSetDefinition, baseUrl = baseUrl)

getConcepts(conceptIds = conceptIds, baseUrl = baseUrl)

getSourceConcepts(conceptIds = conceptIds, baseUrl = baseUrl)





createConceptSetWorkbook(conceptSetIds = c(124, 125),
                         fileName = "c:/temp/workbook.xlsx",
                         baseUrl = baseUrl,
                         included = TRUE,
                         mapped = TRUE)

getCohortGenerationInformation(5665, baseUrl)

getCdmSources(baseUrl)

cohortResults <- getCohortResults(cohortId = 5665, baseUrl = baseUrl, sourceKey = "CDM_JMDC_V1106")
cohortResults$inclusionRuleStats
cohortResults$summary
