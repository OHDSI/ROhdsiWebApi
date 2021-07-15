ROhdsiWebApi
================

[![Build Status](https://github.com/OHDSI/ROhdsiWebApi/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/ROhdsiWebApi/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/ROhdsiWebApi/coverage.svg?branch=testathon)](https://codecov.io/github/OHDSI/ROhdsiWebApi?branch=testathon)

ROhdsiWebApi is part of [HADES](https://ohdsi.github.io/Hades).

## Introduction

ROhdsiWebApi is a R based interface to
[‘WebApi’](https://github.com/ohdsi/webapi) (OHDSI RESTful
services), and performs GET/PULL/POST/DELETE calls via the WebApi. All
objects starting from R or output to R - are analysis ready R-objects
like list and data.frame. The package handles the intermediary steps by
converting R-objects to JSON and vice versa. To ensure r-objects are
analysis ready, the objects are type converted where possible,
e.g. date/date time are converted from string to POSIXct.

This package makes reproducible research easier, by offering ability to
retrieve detailed study specifications, transport study specifications
from one instance to another, programmatically invoke the generation of
a sequence of steps that are part of a study, manage running studies in
batch mode.

An example of a WebApi endpoint is “<http://server.org:80/WebAPI>”.

## Generic design pattern:

  - Functions that go from WebApi to R: will return a R-object
    (e.g. list, data frame). The function will handle the conversion of
    non R-objects like JSON objects into R-objects in the background.
    The returned objects may be inspected in R. We have ensured that all
    returned objects are represented in the proper data types e.g. dates
    in are type date, instead of string, use of integer where
    appropriate etc.

  - Functions that go from R into WebApi: will start with a R-object
    (e.g. list, data frame). If the WebApi requires a JSON object, the
    function will handle the conversion of R-object into JSON in the
    background.

The package follows a consistent naming conventions - with the
identifier for the functional category clearly in the name. There are no
direct reference to ‘Atlas’, and instead ‘WebApi’ is used = because the
package is designed to interface directly with WebApi.

## Supported WebApi functional categories.

| Category         | Features                                                  |
| :--------------- | :-------------------------------------------------------- |
| ConceptSet       | Functions for interfacing with ConceptSet in WebApi       |
| Cohort           | Functions for interfacing with Cohort in WebApi           |
| IncidenceRate    | Functions for interfacing with IncidenceRate in WebApi    |
| Estimation       | Functions for interfacing with Estimation in WebApi       |
| Prediction       | Functions for interfacing with Prediction in WebApi       |
| Characterization | Functions for interfacing with Characterization in WebApi |
| Pathway          | Functions for interfacing with Pathway in WebApi          |

## Technology

ROhdsiWebApi is an R package. Note: ROhdsiWebApi relies on the
availability of a [WebApi](https://github.com/ohdsi/webapi). The
[documentation of the WebApi](http://webapidoc.ohdsi.org/index.html)
(OHDSI RESTful services).

Note: As of V1.0.0, we do not support WebApi that has security enabled.
This is considered a road map item.

## System Requirements

Requires R. WebApi.

## Installation

  - In R, use the following commands to download and install
    ROhdsiWebApi:

<!-- end list -->

``` r
remotes::install_github("OHDSI/ROhdsiWebApi")
```

## User Documentation

Documentation can be found on the [package
website](https://ohdsi.github.io/ROhdsiWebApi).

PDF versions of the documentation are also available:

  - [Vignette](https://raw.githubusercontent.com/OHDSI/ROhdsiWebApi/master/inst/doc/UsingROhdsiWebApi.pdf)

  - [Package
    manual](https://raw.githubusercontent.com/OHDSI/ROhdsiWebApi/master/extras/ROhdsiWebApi.pdf)

## Support

  - Developer questions/comments/feedback:
    <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
  - We use the
    <a href="https://github.com/OHDSI/ROhdsiWebApi/issues">GitHub issue
    tracker</a> for all bugs/issues/enhancements

## Contributing

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can
contribute to this package.

## License

ROhdsiWebApi is licensed under Apache License 2.0

## Development

ROhdsiWebApi is being developed in R Studio.
