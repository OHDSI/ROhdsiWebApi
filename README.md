ROhdsiWebApi
=============

[![Build Status](https://travis-ci.org/OHDSI/ROhdsiWebApi.svg?branch=master)](https://travis-ci.org/OHDSI/ROhdsiWebApi)
[![codecov.io](https://codecov.io/github/OHDSI/ROhdsiWebApi/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/ROhdsiWebApi?branch=master)

Introduction
=============
An R package with functions to call WebAPI web services that support concept sets, cohort definitions, and cohort characterizations. For most functions, you will need to provide the baseUrl, which is the URL endpoint for your WebAPI instance. An example is "http://server.org:80/WebAPI".

Concept Set Functions
======================

* **Get Concept Set Name**
  + Obtains the name of a concept set. A formatted name option is available, where any text in brackets are removed and underscores are converted to spaces.

* **Get Concept Set Concept Ids**
  + Obtains the full list of concept Ids in a concept set, given a Vocabulary Source Key (as defined in your WebAPI)
  + Vocabulary source keys can be either explicitly defined, or you can have the function identify the Default Vocabulary in your WebAPI

* **Get Concept Set Expression**
  + Obtains the JSON expression from WebAPI for a given concept set. Useful for sharing or archiving a concept set definition.

* **Get Set Expression Concept Ids**
  + Based on a concept set expression (perhaps obtained using getConceptSetExpression), get all the concept Ids that the expression would produce, given a Vocabulary Source Key (as defined in your WebAPI). 
  + Vocabulary source keys can be either explicitly defined, or you can have the function identify the Default Vocabulary in your WebAPI
  
* **Insert Concept Set Concept Ids in Package**
  + Inserts a set of concept sets' concept ids into a study package. Useful for archival of a study package.
  
* **Create Concept Set Workbook**
  + Saves a set of concept sets expressions, included concepts, and mapped concepts into a workbook
  


Cohort Definition Functions
============================

* **Get Cohort Definition Expression**
  + Obtain the JSON expression from your WebAPI for a given cohort id
  
* **Insert Cohort Definition in Package**
  + Inserts the JSON expression and SQL scripts into a study package
  
* **Insert Cohort Definition Set in Package**
  + Inserts a set of cohort definition JSON expressions and SQL scripts into a study package

* **Get Concept Sets and Concepts From a Cohort Definition**
  + Gets concept set expressions and concept ids from a cohort definition
  
* **Get Cohort Generation Statuses**
  + Obtains cohort generation statuses for a collection of cohort definition Ids and CDM sources. 
  + Useful if running multiple cohort generation jobs that are long-running.

* **Invoke Cohort Set Generation**
  + Invokes the generation of a set of cohort definitions across a set of CDMs set up in WebAPI.
  
* **Get Cohort Inclusion Rules and Person Counts**
  + Obtains a summary of the inclusion rules and person counts for a cohort definition

Cohort Characterization Functions
==================================

**IN PROGRESS**


WebAPI Functions
================

* **Get WebAPI Version**
  + Obtains the version number of the WebAPI instance

* **Get CDM sources **
  + Obtains all sources configured in the WebAPI instance
