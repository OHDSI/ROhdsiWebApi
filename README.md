ROhdsiWebApi
================

# ROhdsiWebApi

[![Build
Status](https://travis-ci.org/OHDSI/ROhdsiWebApi.svg?branch=master)](https://travis-ci.org/OHDSI/ROhdsiWebApi)
[![codecov.io](https://codecov.io/github/OHDSI/ROhdsiWebApi/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/ROhdsiWebApi?branch=master)

# Introduction

An R package with functions to call WebAPI web services that support
concept sets, cohort definitions, and cohort characterizations. For most
functions, you will need to provide the baseUrl, which is the URL
endpoint for your WebAPI instance. An example is
“<http://server.org:80/WebAPI>”.

# Features

  - Functions for interfacing with WebAPI
  - Functions for interfacing with Concept Sets in Atlas
  - Functions for interfacing with Cohort Definitions in Atlas
  - Functions for interfacing with Cohort Characterizations in Atlas

# Technology

ROhdsiWebApi is an R package.

# System Requirements

Requires R.

# Installation

1.. In R, use the following commands to download and install
ROhdsiWebApi:

``` r
install.packages("drat")
drat::addRepo("OHDSI")
install.packages("ROhdsiWebApi")
```

## User Documentation

Documentation can be found on the [package
website](https://ohdsi.github.io/ROhdsiWebApi).

PDF versions of the documentation are also available: - Vignette:
[Working With
Cohorts](https://raw.githubusercontent.com/OHDSI/ROhdsiWebApi/master/inst/doc/WorkingWithCohorts.pdf)
- Package manual:
[ROhdsiWebApi.pdf](https://raw.githubusercontent.com/OHDSI/ROhdsiWebApi/master/extras/ROhdsiWebApi.pdf)

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

### Development status

Beta
