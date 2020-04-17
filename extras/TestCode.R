library(ROhdsiWebApi)
baseUrl <- Sys.getenv("baseUrl")

estimation <- getEstimation(estimationId = 124, baseUrl = baseUrl)

cohortId <- estimation$cohortDefinitions[[1]]$id

cohortDefinition <- getCohortDefinition(cohortId = cohortId, baseUrl = baseUrl)

conceptSet <- cohortDefinition$expression$ConceptSets[[1]]

conceptSet <- getConceptSet(conceptSetId = estimation$conceptSets[[1]]$id, baseUrl = baseUrl)

conceptSet <- getConceptSet(conceptSetId = 10677, baseUrl = baseUrl)

convertConceptSetToTable(conceptSet)


conceptIds <- resolveConceptSet(conceptSet = conceptSet, baseUrl = baseUrl)

concepts <- getConcepts(conceptIds = conceptIds, baseUrl = baseUrl)

sourceConcepts <- getSourceConcepts(conceptIds = conceptIds, baseUrl = baseUrl)
