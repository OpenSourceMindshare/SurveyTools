## ----setup, include = FALSE----------------------------------------------
library(surveytools)
library(tidyverse)
knitr::opts_chunk$set(
	eval = FALSE,
	collapse = TRUE,
	comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  demoRaw <- read_csv('demo.csv')

## ----eval=FALSE----------------------------------------------------------
#  get_attributes(demoRaw,export=TRUE,filename='demoExportedParameters.csv')

## ----eval=FALSE----------------------------------------------------------
#  demo <- import_attributes('demoExportedParameters.csv',demo)

## ----echo=FALSE, message=FALSE, warning=FALSE,eval=TRUE------------------
knitr::kable(get_attributes(demo)[1:10,])

## ------------------------------------------------------------------------
#  test <- tabulate_data(demo)

## ------------------------------------------------------------------------
#  weighted <- tabulate_data(demo,weights=demo['Weight_global'])

## ------------------------------------------------------------------------
#  segmentedCountry <- tabulate_data(demo,weights=demo['Weight_global'],segments=demo['Country'])

## ------------------------------------------------------------------------
#  segmentedLifestage <- tabulate_data(demo,weights=demo['Weight_global'],segments=extract_segments(demo))

## ------------------------------------------------------------------------
#  segmentedBasedLifestage <- tabulate_data(demo,weights=demo['Weight_global'],segments=extract_segments(demo),addBase = TRUE)

## ------------------------------------------------------------------------
#  indexedBasedLifestage <- index_table(segmentedBasedLifestage,'Base')

