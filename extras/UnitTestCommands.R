detach("package:ROhdsiWebApi", unload=TRUE)
covResults <- covr::package_coverage()
covr::report(covResults)
