###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# Documentation of the 'RTest' package.                                                           #
#                                                                                                 #
# Date:           25 - Jan - 2016                                                                 #
# Author:         Matthias Pfeifer (matthias.pfeifer@roche.com)                                   #
#                                                                                                 #
###################################################################################################



#' RTest: A XML-based Testing System For R Packages
#'
#' The R package RTest is a software framework for standardized unit testing of R packages
#' developed and maintained by Roche-Diagnostics.
#' It implements a general methods for loading and executing
#' XML-based test cases as well as for reporting. RTest is not exetuable by its own as it provides
#' only the basic and general methods for standardized testing. Therefore, extensions of the RTest
#' package are required, which will implement the detailed and specific requirements of individual
#' projects or packages. These extensions are the test adapters that understand the scheme of a
#' project's test case definitions (i.e. the XML definitions) and impelement the test logic and
#' test execution procedures. This concept allows a flexible usage of unit test framework, however,
#' a common test system and strategy as well as report design will be maintained for all
#' Roche-Diagnostics R-packages and for everybody using RTest.
#'
#' RTest uses the open source R package 'testthat' implemented by Hadley Wickham. It is a unit
#' testing system for R and provides a set of methods for executing unit tests for checking
#' different types of exceptions. However, it requires that the tests and exceptions are defined
#' in the source code and does not allow a flexibile definition of input and reference values in
#' XML files or any other file format. Therefore, it is used as unit testing system in the RTest
#' package, which itself implements the functionalities to use XML-based test case definitions.
#'
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
#'
#' @docType package
#'
#' @name RTest
NULL

