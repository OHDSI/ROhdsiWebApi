ROhdsiWebApi
================

ROhdsiWebApi

[![Build
Status](https://travis-ci.org/OHDSI/ROhdsiWebApi.svg?branch=master)](https://travis-ci.org/OHDSI/ROhdsiWebApi)
[![codecov.io](https://codecov.io/github/OHDSI/ROhdsiWebApi/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/ROhdsiWebApi?branch=master)

## Introduction

An R package with functions to interact with OHDSI WebAPI web services,
identified by baseUrl. This package is useful to use R to make GET,
POST, PUT, DELETE API calls using R-script. This is very useful for
reproducible research, and also to run long running studies in batch
mode. An example of a WebApi endpoint is
“<http://server.org:80/WebAPI>”.

ROhdsiWebApi is part of
[HADES](https://ohdsi.github.io/Hades/index.html)

The package supports the following functional categories in WebApi.

    ## # A tibble: 7 x 2
    ##   ` Category`      Features                                                 
    ##   <chr>            <chr>                                                    
    ## 1 ConceptSet       Functions for interfacing with ConceptSet in WebApi      
    ## 2 Cohort           Functions for interfacing with Cohort in WebApi          
    ## 3 IncidenceRate    Functions for interfacing with IncidenceRate in WebApi   
    ## 4 Estimation       Functions for interfacing with Estimation in WebApi      
    ## 5 Prediction       Functions for interfacing with Prediction in WebApi      
    ## 6 Characterization Functions for interfacing with Characterization in WebApi
    ## 7 Pathway          Functions for interfacing with Pathway in WebApi

The package follows a generic design pattern: \* Functions that go from
WebApi to R: will return a R-object (e.g. list, data frame). The
function will handle the conversion of non R-objects like JSON objects
into R-objects in the background. The returned objects may be inspected
in R. We have ensured that all returned objects are represented in the
proper data types e.g. dates in are type date, instead of string, use of
integer where appropriate etc. \* Functions that go from R into WebApi:
will start with a R-object (e.g. list, data frame). If the WebApi
requires a JSON object, the function will handle the conversion of
R-object into JSON in the background.

The package follows a consistent naming conventions - with the
identifier for the functional category clearly in the name. There are no
direct reference to ‘Atlas’, and instead ‘WebApi’ is used = because the
package is designed to interface directly with WebApi.

## Technology

  - ROhdsiWebApi is an R package.

## System Requirements

  - Requires R.

## Installation

  - In R, use the following commands to download and install
    ROhdsiWebApi:

<!-- end list -->

``` r
remotes::install_github("OHDSI/ROhdsiWebApi")
```

## User Documentation

  - Documentation can be found on the [package
    website](https://ohdsi.github.io/ROhdsiWebApi).

PDF versions of the documentation are also available:

  - Vignette:
      - [Working With
        Cohorts](https://raw.githubusercontent.com/OHDSI/ROhdsiWebApi/master/inst/doc/WorkingWithCohorts.pdf)
  - Package manual:
      - [ROhdsiWebApi](https://raw.githubusercontent.com/OHDSI/ROhdsiWebApi/master/extras/ROhdsiWebApi.pdf)

## Support

  - Developer questions/comments/feedback:
    <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
  - We use the
    <a href="https://github.com/OHDSI/ROhdsiWebApi/issues">GitHub issue
    tracker</a> for all bugs/issues/enhancements

## Contributing

  - Read [here](https://ohdsi.github.io/Hades/contribute.html) how you
    can contribute to this package.

## License

  - ROhdsiWebApi is licensed under Apache License 2.0

## Development

  - ROhdsiWebApi is being developed in R Studio.
