# plumber.R

# cdm data
cdmSourceData <- list(
  # source 1
  list(
    sourceId = 1L, 
    sourceName = "synthea", 
    sourceDialect = "postgresql", 
    sourceKey = "synthea", 
    daimons = list(
      list(sourceDaimonId = 5L, daimonType = "CDM", tableQualifier = "synthea", priority = 0L), 
      list(sourceDaimonId = 6L, daimonType = "Vocabulary", tableQualifier = "synthea", priority = 0L), 
      list(sourceDaimonId = 7L, daimonType = "Results", tableQualifier = "synthea_results", priority = 0L)
    )
  ), 
  # source 2                   
  list(
    sourceId = 2L, 
    sourceName = "synpuf", 
    sourceDialect = "postgresql", 
    sourceKey = "synpuf", 
    daimons = list(
      list(sourceDaimonId = 2L, daimonType = "CDM", tableQualifier = "synpuf", priority = 1L), 
      list(sourceDaimonId = 3L, daimonType = "Vocabulary", tableQualifier = "synpuf", priority = 1L), 
      list(sourceDaimonId = 4L, daimonType = "Results", tableQualifier = "synpuf_results", priority = 1L)
    ) 
  )
)

#* Get Webapi version info
#* @get /
#* @serializer unboxedJSON
function() {
  list(status = 'alive')
}

#* Get Webapi version info
#* @get /info
#* @serializer unboxedJSON
function() {
  list(version = '2.7.6')
}

#* Generate a Bearer token to use with WebAPI using db security
#* @param login 
#* @param password 
#* @post /user/login/db
#* @serializer unboxedJSON
function(login, password, res) {
  if(login == "testUser" && password == "testPassword"){
    res$headers <- list(bearer = "0000")
    return("success")
  } else {
    res$status <- 401 # unauthorized
    return("fail")
  }
}

#* Get info about CDM sources
#* @get /source/sources
#* @serializer unboxedJSON
function() {
  cdmSourceData
}

#* Get priority vocabulary key
#* @get /source/priorityVocabulary
#* @serializer unboxedJSON
function() {
  cdmSourceData[[1]]
}

#* Echo an http status code
#* @get /echoStatus
#* @param status 
#* @serializer unboxedJSON
function(status, res) {
  res$status <- as.integer(status)
  return(paste("status is", status))
}
