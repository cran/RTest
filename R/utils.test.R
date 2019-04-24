###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# RTest Testing Uitility Functions                                                                #
#                                                                                                 #
# This file defines a set of utility functions to executes general testthat test, which can be    #
# defined for several different packages.                                                         #
#                                                                                                 #
# Date:           25 - Jan - 2016                                                                 #
# Author:         Matthias Pfeifer (matthias.pfeifer@roche.com)                                   #
#                                                                                                 #
###################################################################################################





# test_execution_silent  ###########################################################################

#' Tests Silent Execution of an Function
#'
#' @param    what,args          Parameters for execution of the test function
#'                              (see \code{\link{do.call}}).
#' @param    xmlTestSpec        (\code{XMLNode}) The XML definition of type 'RTestTest_variable'.
#' @param    ...                Additional parameters passed to \code{\link{do.call}}.
#'
#' @return \code{ANY} result of test functin
#'
#' @seealso  \code{\link{do.call}}
#'
#' @examples
#' value <- test_execution(
#'       "sum",
#'       list(x=2,y=3),
#'       xmlTestSpec=XML::xmlNode(
#'           name="execution",
#'           attrs=list('execution-type'="silent"))
#' )
#' stopifnot(value==5)
#'
#' # Create a function that always produces warnings
#'
#' sum_test <- function(...){
#'   warning("test")
#'   sum(...)
#' }
#'
#' # Let this function run and crash, if it crashes check if the error contains "produced warnings"
#'
#' tryCatch(
#' test_execution(
#'    "sum_test",
#'     list(x=2,y=3),
#'    xmlTestSpec=XML::xmlNode(name="execution",attrs=list("execution-type"="silent"))
#'  ),error=function(e){
#'     stopifnot(grepl("produced warnings",e))
#'  })
#' @importFrom utils packageVersion
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
test_execution <- function(what, args, xmlTestSpec=NULL, ...) {

  if(is.null(xmlTestSpec)){
    xmlTestSpec <- XML::xmlNode("execution",attrs=c("execution-type"="silent"))
  }
  test.attrs <- xmlAttrs(xmlTestSpec)

  # Global settings of the test -------------------------------------------------------------------

  test.type      <-
      ifelse(!is.null(test.attrs[["execution-type"]]),
          test.attrs[["execution-type"]], "silent")

  test.name      <-
      ifelse("desc" %in% names(test.attrs),
          test.attrs[["desc"]],
          switch(test.type,
              "silent"  = paste("Execute function silently.",paste0("(",what,")")),
              "output"  = paste("Execute function with output.",paste0("(",what,")")),
              "message" = paste("Execute function with message(s).",paste0("(",what,")")),
              "warning" = paste("Execute function with warning(s).",paste0("(",what,")")),
              "error"   = paste("Execute function with error(s).",paste0("(",what,")")),
        stop("Test type '",test.type,"' not implemented.")
          ))

  #  message(test.name)


  # Perform test ----------------------------------------------------------------------------------

  # Initialize variable to store result of computation
  result <- NULL

  force_implementation <- if(!is.null(options("force_implementation")[[1]])){
        as.logical(options("force_implementation"))
      }else{
        FALSE
      }

  test_that(test.name, {
        # Check different execution types....
        if(test.type == "silent") {
          # ... without any message / warning / error
      if(as.numeric(
        stringr::str_extract(
            as.character(packageVersion("testthat")),"[0-9]{1,2}\\.[0-9]{1,2}")) >=
          2 && !force_implementation){
        expect_silent_RTest(
                result <<- do.call(what = what, args = args, ...)
            )
      }else{
        expect_silent(
                result <<- do.call(what = what, args = args, ...)
            )
      }

        } else if(test.type == "output") {
          # ... with message(s)
          expect_output(
              result <<- do.call(what = what, args = args, ...)
          )

        } else if(test.type == "message") {
          # ... with message(s)
          expect_message(
              result <<- do.call(what = what, args = args, ...)
          )

        } else if(test.type == "warning") {
          # ... with warning(s)
          expect_warning(
              result <<- do.call(what = what, args = args, ...)
          )

        } else if(test.type == "error") {
          # ... with error
          expect_error(
              result <<- do.call(what = what, args = args, ...)
          )

        }
      })


  # Return result of function ---------------------------------------------------------------------

  return(result)
}



# test_returnValue_variable #######################################################################

#' Tests a Standard R 'variable' ('RTestTest_vector_variable')
#'
#' @param    result             (\code{object}) The result object to be tested.
#' @param    reference          (\code{object}) The reference object.
#' @param    xmlTestSpec        (\code{XMLNode}) The XML definition of type 'RTestTest_variable'.
#' @param    add.desc          (\code{character}) Additional description added to the XML
#'                             definition.
#'
#' @seealso \code{\link[XML]{XMLNode-class}}
#'
#' @examples
#'
#' # Cleaning up
#'
#' tryCatch(unloadNamespace("RTest"))
#' tryCatch(unloadNamespace("testthat"))
#' library(RTest)
#'
#' data <- '<test_variable desc="Compare a value"
#'           diff-type="absolute" compare-type="equal" tolerance="1E-3"/>'
#' xmlTestSpec <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#'
#' test_returnValue_variable(5,5,xmlTestSpec)
#'
#' test_returnValue_variable(5.0001,5,xmlTestSpec)
#'
#' # Compare variable with a stricter tolerance
#'
#' data <- '<test_variable desc="Compare a value"
#'           diff-type="relative" compare-type="equal" tolerance="1E-6"/>'
#' xmlTestSpec <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#'
#' tryCatch(unloadNamespace("RTest"))
#' tryCatch(unloadNamespace("testthat"))
#' library(RTest)
#'
#' tryCatch(
#'     test_returnValue_variable(5.0001,5,xmlTestSpec),error=function(e){
#'       stopifnot(grepl("5.0001 not equal to 5.",e))
#'     })
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
test_returnValue_variable <- function(result, reference, xmlTestSpec, add.desc = NULL) {
  if(is.null(xmlTestSpec)){
    xmlTestSpec <- xmlNode("return-value",attrs=list("compare-type"="equal"))
  }
  test.attrs <- xmlAttrs(xmlTestSpec)


  # Global settings of the test -------------------------------------------------------------------

  # Get the global settings of the test from the current XML definition.

  test.name      <-
      ifelse("desc" %in% names(test.attrs),
          test.attrs[["desc"]], "Check return value (variable).")

  if(!is.null(add.desc))
    test.name <- paste0(add.desc," ",test.name)

  test.diffType  <-
      ifelse("diff-type" %in% names(test.attrs),
          test.attrs[["diff-type"]], "absolute")

  test.compareType  <-
      ifelse("compare-type" %in% names(test.attrs),
          test.attrs[["compare-type"]], "equal")

  test.tolerance <-
      ifelse("tolerance" %in% names(test.attrs),
          as.numeric(test.attrs[["tolerance"]]), 1.5e-8)



  # Perform test ----------------------------------------------------------------------------------

  test_that(test.name, {

        # Level 1: Test value - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        test.info <- paste0(
            "{",
            "\"Test\":\"Equal Value\", ",
            "\"Name\":\"%s\", ",
            "\"Received\":\"%s\", \"Data Type\":\"%s\", ",
            "\"Expected\":\"%s\", \"Data Type\":\"%s\", ",
            "\"Diff Type\":\"%s\", \"Compare Type\":\"%s\", ",
            "\"Tolerance\":\"%s\" ",
            "}")

        # Get data
        rec <- unname(result)
        exp <- unname(reference)

        # Get data types
        rec.type <- typeof(rec)
        exp.type <- typeof(exp)

        # Handle factors as strings for comparison
        if(is.factor(rec)) rec <- levels(rec)[rec]
        if(is.factor(exp)) exp <- levels(exp)[exp]

        # Tolerance set to very small number, like in all.equal (which is used by testthat)
        #     https://stat.ethz.ch/R-manual/R-devel/library/base/html/all.equal.html
        if(test.tolerance == 0)
          test.tolerance <- 1.5e-8


        switch(test.compareType,
            "equal" = {
              do.call(
                  "expect_equal",
                  list(
                      object    = rec,
                      expected  = exp,
                      tolerance = test.tolerance,
                      scale     = if(test.diffType == "absolute") 1 else NULL,
                      info      = sprintf(
                          test.info,
                          test.name,
                          htmlify_string(rec), rec.type,
                          htmlify_string(exp), exp.type,
                          test.diffType,
                          test.compareType,
                          test.tolerance)
                  )
              )
            },
            "less_than" = {
              do.call(
                  "expect_lt",
                  list(
                      object    = rec,
                      expected  = exp,
                      info      = sprintf(
                          test.info,
                          test.name,
                          rec, rec.type,
                          exp, exp.type,
                          "absolute",
                          test.compareType,
                          0)
                  )
              )
            },
            "more_than" = {
              do.call(
                  "expect_gt",
                  list(
                      object    = rec,
                      expected  = exp,
                      info      = sprintf(
                          test.info,
                          test.name,
                          rec, rec.type,
                          exp, exp.type,
                          "absolute",
                          test.compareType,
                          0)
                  )
              )
            },
            "regex"={
              test.info[["Diff Type"]] <- "regex"
              test.info[["Tolerance"]] <- 0
              do.call(
                  "expect_match",
                  list(
                      object    = rec,
                      regexp    = htmlify_string(exp),
                      info      = test.info
                  )
              )
            },
            stop("Compare type '", test.compareType,"' currently not implemented.")
        )
      })
}






# test_returnValue_vector_elementbyelement ########################################################

#' Tests a Standard R 'vector' Element-By-Element ('RTestTest_vector_elementbyelement')
#'
#' @param    result             (\code{vector}) The result vector to be tested
#' @param    reference          (\code{vector}) The reference vector
#' @param    xmlTestSpec        (\code{XMLNode}) The XML definition of type
#'                              'RTestTest_vector_elementbyelement'
#' @param    add.desc          (\code{character}) Additional description added to the XML
#'                             definition.
#' @examples
#'
#' # Cleaning up
#'
#' tryCatch(unloadNamespace("RTest"))
#' tryCatch(unloadNamespace("testthat"))
#' library(RTest)
#'
#' data <- '<test_variable desc="Compare a value" diff-type="absolute" compare-type="equal"
#'         tolerance="1E-3"/>'
#' xmlTestSpec <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#'
#' test_returnValue_vector_elementbyelement(c(5,5),c(5,5),xmlTestSpec)
#' test_returnValue_vector_elementbyelement(c(5,5),c(5,5.000001),xmlTestSpec)
#'
#' data <- '<test_variable desc="Compare a value" diff-type="relative" compare-type="equal"
#'         tolerance="1E-6"/>'
#' xmlTestSpec <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#'
#' tryCatch(unloadNamespace("RTest"))
#' tryCatch(unloadNamespace("testthat"))
#' library(RTest)
#'
#' tryCatch(
#'     test_returnValue_vector_elementbyelement(c(5,5),c(5,5.0001),xmlTestSpec),
#'     error=function(e){
#'       stopifnot(grepl("5 not equal to 5.0001.",e))
#'      })
#'
#' @seealso \code{\link[XML]{XMLNode-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
test_returnValue_vector_elementbyelement <- function(result, reference, xmlTestSpec,
    add.desc = NULL) {
  if(is.null(xmlTestSpec)){
    xmlTestSpec <- xmlNode("return-value",attrs=list("compare-type"="equal"))
  }
  test.attrs <- xmlAttrs(xmlTestSpec)

  # Global settings of the test -------------------------------------------------------------------

  # Get the global settings of the test from the current XML definition. These are mendatory and
  # will be used for the subsequent element-by-element tests if not overwritten by other
  # specifications on the element level

  test.name      <-
      ifelse("desc" %in% names(test.attrs),
          test.attrs[["desc"]], "Check return value (variable).")

  if(!is.null(add.desc)) test.name <- paste0(add.desc," ",test.name)

  test.diffType  <-
      ifelse("diff-type" %in% names(test.attrs),
          test.attrs[["diff-type"]], "absolute")

  test.compareType  <-
      ifelse("compare-type" %in% names(test.attrs),
          test.attrs[["compare-type"]], "equal")

  test.tolerance <-
      ifelse("tolerance" %in% names(test.attrs),
          as.numeric(test.attrs[["tolerance"]]), 1.5e-8)

  # Get the entries and settings from the reference vector ----------------------------------------

  elems <- lapply(1:length(reference),
      function(i) {

        elem <- list()

        elem$name        <- if(is.null(names(reference))) i else names(reference)[i]

        elem$diffType    <- test.diffType
        elem$compareType <- test.compareType
        elem$tolerance   <- test.tolerance

        return(elem)
      })

  names(elems) <- sapply(elems, function(e) e$name)


  # Check if specific specifications for single elements are done ---------------------------------

  if(length(xmlChildren(xmlTestSpec)) > 0) {
    # If any children (i.e. single elements) are defined, use the settings of them for the test

    xmlApply(xmlTestSpec,
        function(xmlElemItem) {

          attrs     <- xmlAttrs(xmlElemItem)

          name      <- attrs[["name"]]

          if("diff-type" %in% names(attrs)) elems[[name]][["diffType"]] <<- attrs[["diff-type"]]

          if("compare-type" %in% names(attrs)) elems[[name]][["compareType"]] <<- attrs[["compare-type"]]

          if("tolerance" %in% names(attrs)) elems[[name]][["tolerance"]] <<- as.numeric(attrs[["tolerance"]])
        })
  }



  # Perform test ----------------------------------------------------------------------------------


  test_that(test.name, {

        # Level 1: Test vector length   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        rec.n <- length(result)
        exp.n <- length(reference)

        test.info <- paste0(
            "{",
            "\"Test\":\"%s\", ",
            "\"Received\":\"%s\", \"Expected\":\"%s\"",
            "}")

        expect_equal(
            object    = rec.n,
            expected  = exp.n,
            info      = sprintf(test.info, "Equal Vector Length", rec.n, exp.n))


        if(rec.n == exp.n) {

          # Level 2: Test vector names (if specified) - - - - - - - - - - - - - - - - - - - - - - - -

          if(!is.null(names(reference))) {

            for(i in 1:length(reference)) {
              rec <- names(result)[i]
              exp <- names(reference)[i]

              test.info <- jsonlite::toJSON(
                  list(
                      Test     = "Equal Vector Names",
                      Received = rec,
                      Expected = exp),
                  digits = NA
              )

              expect_equal(
                  object    = rec,
                  expected  = exp,
                  info      = test.info)
            }
          }


          # Level 3: Test element by element - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          if(length(reference) > 0) {
            sapply(1:length(reference), function(i) {
                  elem <- elems[[i]]

          if(!is.na(unname(result[elem$name]))){
            # Get data
            rec <- unname(result[elem$name])
            exp <- unname(reference[elem$name])
          }else{
            # Get data
            rec <- unname(result[i])
            exp <- unname(reference[i])
          }

          # Get data types
                  rec.type <- typeof(rec)
                  exp.type <- typeof(exp)

                  # Handle factors as strings for comparison
                  if(is.factor(rec)) rec <- levels(rec)[rec]
                  if(is.factor(exp)) exp <- levels(exp)[exp]

                  # Tolerance set to very small number, like in all.equal (which is used by testthat)
                  #     https://stat.ethz.ch/R-manual/R-devel/library/base/html/all.equal.html
                  if(elem$tolerance == 0) elem$tolerance <- 1.5e-8

                  test.info <- list(
                      Test               = "Equal Value",
                      i                  = i,
                      Name               = elem$name,
                      Received           = rec,
                      "Data Type (Rec.)" = rec.type,
                      Expected           = exp,
                      "Data Type (Exp.)" = exp.type,
                      "Diff Type"        = NA,
                      "Compare Type"     = elem$compareType,
                      Tolerance          = NA
                  )

                  switch(test.compareType,
                      "equal" = {

                        test.info[["Diff Type"]] <- elem$diffType
                        test.info[["Tolerance"]] <- elem$tolerance

                        do.call(
                            "expect_equal",
                            list(
                                object    = rec,
                                expected  = exp,
                                tolerance = elem$tolerance,
                                scale     = if(elem$diffType == "absolute") 1 else exp,
                                info      = jsonlite::toJSON(test.info,
                                    digits = NA)
                            )
                        )
                      },

                      "less_than" = {

                        test.info[["Diff Type"]] <- "absolute"
                        test.info[["Tolerance"]] <- 0

                        do.call(
                            "expect_less_than",
                            list(
                                object    = rec,
                                expected  = exp,
                                info      = jsonlite::toJSON(test.info,
                                    digits = NA)
                            )
                        )
                      },

                      "more_than" = {

                        test.info[["Diff Type"]] <- "absolute"
                        test.info[["Tolerance"]] <- 0

                        do.call(
                            "expect_more_than",
                            list(
                                object    = rec,
                                expected  = exp,
                                info      = jsonlite::toJSON(test.info,
                                    digits = NA)
                            )
                        )

                      },

                      "regex"={
                        test.info[["Diff Type"]] <- "regex"
                        test.info[["Tolerance"]] <- 0

                        do.call(
                            "expect_match",
                            list(
                                object    = rec,
                                regexp    = exp,
                                info      = jsonlite::toJSON(test.info,
                                    digits = NA)
                            )
                        )
                      },

                      stop("Compare type '",test.compareType,"' currently not implemented.")
                  )
                })
          }

        }
      })
}



# test_returnValue_data.frame_cellbycell ##########################################################

#' Tests a Standard R 'data.frame' Cell-By-Cell ('RTestTest_data.frame_cellbycell')
#'
#' @param    result             (\code{data.frame}) The result data.frame to be tested
#' @param    reference          (\code{data.frame}) The reference data.frame
#' @param    xmlTestSpec         (\code{XMLNode}) The XML definition of type
#'                              'RTestTest_data.frame_cellbycell'
#' @param    add.desc          (\code{character}) Additional description added to the XML
#'                             definition.
#'
#' @seealso \code{\link[XML]{XMLNode-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
test_returnValue_data.frame_cellbycell <- function(result, reference, xmlTestSpec, add.desc = NULL) {
  if(is.null(xmlTestSpec)){
    xmlTestSpec <- xmlNode("return-value",attrs=list("compare-type"="equal"))
  }
  test.attrs <- xmlAttrs(xmlTestSpec)

  RTest.cat(" data.frame: ",dim(result)[1]," x ",dim(result)[2]," ... ")

  # Global settings of the test -------------------------------------------------------------------

  # Get the global settings of the test from the current XML definition. These are mendatory and
  # will be used for the subsequent element-by-element (i.e. each cell between all column and rows)
  # tests if not overwritten by other specifications on the element level

  test.name      <-
      ifelse("desc" %in% names(test.attrs),
          test.attrs[["desc"]], "Check return value (data.frame).")

  if(!is.null(add.desc))
    test.name <- paste0(add.desc," ",test.name)

  test.diffType  <-
      ifelse("diff-type" %in% names(test.attrs),
          test.attrs[["diff-type"]], "absolute")

  test.compareType  <-
      ifelse("compare-type" %in% names(test.attrs),
          test.attrs[["compare-type"]], "equal")

  test.tolerance <-
      ifelse("tolerance" %in% names(test.attrs),
          as.numeric(test.attrs[["tolerance"]]), 1.5e-8)



  # Get the entries and settings from the reference vector ----------------------------------------

  elems <- lapply(1:dim(reference)[2],
      function(i) {

        elem <- list()

        elem$name        <- if(is.null(colnames(reference))) i else colnames(reference)[i]
        elem$diffType    <- test.diffType
        elem$compareType <- test.compareType
        elem$tolerance   <- test.tolerance

        return(elem)
      })

  names(elems) <- sapply(elems, function(e) e$name)


  # Check if specific specifications for single columns are done ----------------------------------

  if(length(xmlChildren(xmlTestSpec)) > 0) {
    # If any children (i.e. single elements) are defined, use the settings of them for the test

    xmlApply(xmlTestSpec,
        function(xmlElemItem) {

          attrs     <- xmlAttrs(xmlElemItem)

          name      <- attrs[["name"]]

          if("diff-type" %in% names(attrs))
            elems[[name]][["diffType"]] <<- attrs[["diff-type"]]

          if("compare-type" %in% names(attrs))
            elems[[name]][["compareType"]] <<- attrs[["compare-type"]]

          if("tolerance" %in% names(attrs))
            elems[[name]][["tolerance"]] <<- as.numeric(attrs[["tolerance"]])

          # Tolerance set to very small number, like in all.equal (which is used by testthat)
          #     https://stat.ethz.ch/R-manual/R-devel/library/base/html/all.equal.html
          if(elems[[name]][["tolerance"]] == 0)
            elems[[name]][["tolerance"]] <<- 1.5e-8
        })
  }


  # Perform test ----------------------------------------------------------------------------------

  test_that(test.name, {

        # Level 1: Test number of rows and columns - - - - - - - - - - - - - - - - - - - - - - - - -

        # Get dimensions
        exp.nrows <- dim(reference)[1]
        rec.nrows <- dim(result)[1]

        exp.ncols <- dim(reference)[2]
        rec.ncols <- dim(result)[2]

        # Get column/row names
        exp.colnames <- colnames(reference)
        rec.colnames <- colnames(result)

        exp.rownames <- rownames(reference)
        rec.rownames <- rownames(result)

        # Get data types
        exp.colTypes <- sapply(1:exp.ncols, function(i) {
              type <- typeof(reference[[i]])
              if(type=="integer"){
                if(grepl("Factor",capture.output(str(reference[[i]])))){
                  "factor"
                }else{
                  type
                }
              }else{
                type
              }
            }
        )


        rec.colTypes <- sapply(1:rec.ncols, function(i) {
              type <- typeof(result[[i]])
              if(type=="integer"){
                if(grepl("Factor",capture.output(str(result[[i]])))){
                  "factor"
                }else{
                  type
                }
              }else{
                type
              }
            }
        )


        test.info.dims <- paste0(
            "{",
            "\"Test\":\"%s\", ",
            "\"Received\":\"%s\", \"Expected\":\"%s\"",
            "}")

        expect_equal(
            object    = rec.nrows,
            expected  = exp.nrows,
            info      = sprintf(test.info.dims, "Equal Row Number", rec.nrows, exp.nrows))

        expect_equal(
            object    = rec.ncols,
            expected  = exp.ncols,
            info      = sprintf(test.info.dims, "Equal Column Number", rec.ncols, exp.ncols))


        if(exp.nrows == rec.nrows && exp.ncols == rec.ncols && rec.nrows > 0) {

          # Level 2: Test column and row names - - - - - - - - - - - - - - - - - - - - - - - - - - -

          # Test column names
          if(!is.null(colnames(result))) {
            test.info.names <- paste0(
                "{",
                "\"Test\":\"Equal Column Name\",",
                "\"Column\":\"%s\", ",
                "\"Received\":\"%s\", \"Expected\":\"%s\"",
                "}")

            #for(i in 1:dim(result)[2]) {
            tmp <- lapply(1:dim(result)[2], function(i) {
                  rec.name <- rec.colnames[i]
                  exp.name <- exp.colnames[i]

                  expect_equal(
                      object    = rec.name,
                      expected  = exp.name,
                      info      = sprintf(test.info.names, i, rec.name, exp.name))
                })
          }

          # Test row names
          if(!is.null(rownames(result))) {
            test.info.names <- paste0(
                "{",
                "\"Test\":\"Equal Row Name\",",
                "\"Row\":\"%s\", ",
                "\"Received\":\"%s\", \"Expected\":\"%s\"",
                "}")

            #for(i in 1:dim(result)[1]) {
            tmp <- lapply(1:dim(result)[1], function(i) {
                  rec.name <- rec.rownames[i]
                  exp.name <- exp.rownames[i]

                  expect_equal(
                      object    = rec.name,
                      expected  = exp.name,
                      tolerance = 0,
                      info      = sprintf(test.info.names, i, rec.name, exp.name))
                })
          }


          # Level 3: Test cell by cell - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          test.info.data <- paste0(
              "{",
              "\"Test\":\"Equal Value\", ",
              "\"Row\":\"%s\", \"Column\":\"%s\", ",
              "\"Received\":\"%s\", \"Data Type\":\"%s\", ",
              "\"Expected\":\"%s\", \"Data Type\":\"%s\", ",
              "\"Diff Type\":\"%s\", \"Compare Type\":\"%s\", ",
              "\"Tolerance\":\"%s\"",
              "}")

          #for(elem.r in 1:dim(reference)[1]) {
          #lapply(1:exp.nrows, function(elem.r) {

          #for(elem.c in 1:dim(reference)[2]) {
          #lapply(1:exp.ncols, function(elem.c) {


          lapply(1:exp.nrows, function(elem.r) {

                # Show rowname in Output
                elem.r.name <-
                    if(!is.null(exp.rownames)) paste0(elem.r," (",exp.rownames[elem.r],")")
                    else elem.r

                lapply(1:exp.ncols, function(elem.c) {
                      col <- elems[[elem.c]]

                      # Get test specifications
                      col.name        <- col$name
                      col.diffType    <- col$diffType
                      col.compareType <- col$compareType
                      col.tolerance   <- col$tolerance

                      # Show colname in Output
                      elem.c.name <-
                          if(!is.null(exp.colnames)) paste0(elem.c," (",exp.colnames[elem.c],")")
                          else elem.c

                      # Get received and expected value
                      #rec <- result[elem.r, elem.c]
                      #exp <- reference[elem.r, elem.c]
                      rec  <- result[[elem.c]][elem.r]
                      exp  <- reference[[elem.c]][elem.r]
#                rec  <- rec.c[elem.r]
#                exp  <- exp.c[elem.r]

                      # Datatypes
                      rec.type <- rec.colTypes[elem.c]
                      exp.type <- exp.colTypes[elem.c]

                      # Handle factors as strings for comparison
                      if(is.factor(exp)) {
                        rec <- levels(rec)[rec]
                        exp <- levels(exp)[exp]
                      }
                      if(col.diffType == "absolute_as_numeric"){
                        if(!is.na(suppressWarnings(as.numeric(rec))) && !is.na(suppressWarnings(as.numeric(exp)))){

                          rec.type <- "numeric"
                          exp.type <- "numeric"
                          rec <- as.numeric(rec)
                          exp <- as.numeric(exp)
                        }
                        col.diffType <- "absolute"
                      }
                      # Get testthat function for testing the expection
                      switch(test.compareType,
                          "equal" = {
                            do.call(
                                "expect_equal",
                                list(
                                    object    = rec,
                                    expected  = exp,
                                    tolerance = col.tolerance,
                                    scale     = if(col.diffType == "absolute") 1 else NULL,
                                    info      = sprintf(
                                        test.info.data,
                                        elem.r.name, elem.c.name,
                                        htmlify_string(rec), rec.type,
                                        htmlify_string(exp), exp.type,
                                        col.diffType,
                                        col.compareType,
                                        col.tolerance)
                                )
                            )
                          },

                          "less_than" = {
                            do.call(
                                "expect_less_than",
                                list(
                                    object    = rec,
                                    expected  = exp,
                                    info      = sprintf(
                                        test.info.data,
                                        elem.r.name, elem.c.name,
                                        rec, rec.type,
                                        exp, exp.type,
                                        "absolute",
                                        col.compareType,
                                        0)
                                )
                            )
                          },

                          "more_than" = {
                            do.call(
                                "expect_more_than",
                                list(
                                    object    = rec,
                                    expected  = exp,
                                    info      = sprintf(
                                        test.info.data,
                                        elem.r.name, elem.c.name,
                                        rec, rec.type,
                                        exp, exp.type,
                                        "absolute",
                                        col.compareType,
                                        0)
                                )
                            )
                          },
                          "regex"={
                            do.call(
                                "expect_match",
                                list(
                                    object    = rec,
                                    regexp    = exp,
                                    info      = sprintf(
                                        test.info.data,
                                        elem.r.name,
                                        elem.c.name,
                                        rec, rec.type,
                                        htmlify_string(exp), exp.type,
                                        "absolute",
                                        col.compareType,
                                        0)
                                )
                            )
                          },
                          stop("Compare type '", test.compareType,"' currently not implemented.")
                      )
                    })
              })

        }
      })
}

#' Tests a Standard R 'data.frame' by shape, rownames and colnames
#'     ('RTestTest_data.frame_shape')
#'
#' @param    result             (\code{data.frame}) The result data.frame to be tested
#' @param    reference          (\code{data.frame}) The reference data.frame
#' @param    xmlTestSpec         (\code{XMLNode}) The XML definition of type
#'                              'RTestTest_data.frame_cellbycell'
#' @param    add.desc          (\code{character}) Additional description added to the XML
#'                             definition.
#' @examples
#' # Cleaning up
#'
#' tryCatch(unloadNamespace("RTest"))
#' tryCatch(unloadNamespace("testthat"))
#' library(RTest)
#'
#' # create some definition of tests
#'
#' data <- '<test_df desc="Compare a value" diff-type="relative"
#'          compare-type="equal" tolerance="1E-6"/>'
#' xmlTestSpec <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#'
#' # Create data frames
#'
#' x <- data.frame(x=c(1,2,3,4),y=c(1,2,3,4))
#' y <- data.frame(x=c(1,2,3,4),y=c(1,2,3,4))
#' y_wrong_shape <- data.frame(x=c(1,2,3,4,5),y=c(1,2,3,4,5))
#' y_wrong_names <- data.frame(x=c(1,2,3,4),y1=c(1,2,3,4))
#'
#' test_returnValue_data.frame_shape(x,y,xmlTestSpec)
#'
#' # Test for shape
#'
#' tryCatch(
#'     {test_returnValue_data.frame_shape(x,y_wrong_shape,xmlTestSpec)
#'     stop("test did not find difference")},
#'     error=function(e){
#'       stopifnot(grepl("rec.nrows",e))
#'       stopifnot(grepl("exp.nrows",e))
#'       stopifnot(grepl("not equal",e))
#'     })
#'
#' # Test for column names
#'
#' data <- '<test_df check_colnames="TRUE"
#'           desc="Compare a value" diff-type="relative"
#'           compare-type="equal" tolerance="1E-6"/>'
#' xmlTestSpec <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#'
#' tryCatch({
#'     test_returnValue_data.frame_shape(x,y_wrong_names,xmlTestSpec)
#'     stop("test did not find difference")},
#'      error=function(e){
#'       stopifnot(grepl("rec.name",e))
#'       stopifnot(grepl("exp.name",e))
#'       stopifnot(grepl("not equal",e))
#'     })
#'
#' @seealso \code{\link[XML]{XMLNode-class}}
#'
#' @author   Sebastian Wolf \email{sebastian.wolf.sw1@@roche.com}
test_returnValue_data.frame_shape <- function(result, reference, xmlTestSpec, add.desc = NULL) {
  if(is.null(xmlTestSpec)){
    xmlTestSpec <- xmlNode("return-value",attrs=list("compare-type"="equal"))
  }
  test.attrs <- xmlAttrs(xmlTestSpec)

  RTest.cat(" data.frame: ",dim(result)[1]," x ",dim(result)[2]," ... ")

  # Global settings of the test -------------------------------------------------------------------

  # Get the global settings of the test from the current XML definition. These are mendatory and
  # will be used for the subsequent element-by-element (i.e. each cell between all column and rows)
  # tests if not overwritten by other specifications on the element level

  test.name      <-
      ifelse("desc" %in% names(test.attrs),
          test.attrs[["desc"]], "Check return value (variable).")

  if(!is.null(add.desc))
    test.name <- paste0(add.desc," ",test.name)

  test.diffType  <-
      ifelse("diff-type" %in% names(test.attrs),
          test.attrs[["diff-type"]], "absolute")

  test.compareType  <-
      ifelse("compare-type" %in% names(test.attrs),
          test.attrs[["compare-type"]], "equal")

  test.tolerance <-
      ifelse("tolerance" %in% names(test.attrs),
          as.numeric(test.attrs[["tolerance"]]), 1.5e-8)

  test.colnames <- ifelse("check_colnames" %in% names(test.attrs),
      as.logical(test.attrs[["check_colnames"]]), FALSE)

  test.rownames <- ifelse("check_rownames" %in% names(test.attrs),
      as.logical(test.attrs[["check_rownames"]]), FALSE)

  # Get the entries and settings from the reference vector ----------------------------------------

  elems <- lapply(1:dim(reference)[2],
      function(i) {

        elem <- list()

        elem$name        <- if(is.null(colnames(reference))) i else colnames(reference)[i]
        elem$diffType    <- test.diffType
        elem$compareType <- test.compareType
        elem$tolerance   <- test.tolerance

        return(elem)
      })

  names(elems) <- sapply(elems, function(e) e$name)



  # Perform test ----------------------------------------------------------------------------------

  test_that(test.name, {

        # Level 1: Test number of rows and columns - - - - - - - - - - - - - - - - - - - - - - - - -

        # Get dimensions
        exp.nrows <- dim(reference)[1]
        rec.nrows <- dim(result)[1]

        exp.ncols <- dim(reference)[2]
        rec.ncols <- dim(result)[2]

        # Get column/row names
        exp.colnames <- colnames(reference)
        rec.colnames <- colnames(result)

        exp.rownames <- rownames(reference)
        rec.rownames <- rownames(result)

        test.info.dims <- paste0(
            "{",
            "\"Test\":\"%s\", ",
            "\"Received\":\"%s\", \"Expected\":\"%s\"",
            "}")
        expect_equal(
            object    = rec.nrows,
            expected  = exp.nrows,
            info      = sprintf(test.info.dims, "Equal Row Number", rec.nrows, exp.nrows))

        expect_equal(
            object    = rec.ncols,
            expected  = exp.ncols,
            info      = sprintf(test.info.dims, "Equal Column Number", rec.ncols, exp.ncols))


        if(exp.nrows == rec.nrows && exp.ncols == rec.ncols && rec.nrows > 0) {

          # Level 2: Test column and row names - - - - - - - - - - - - - - - - - - - - - - - - - - -

          # Test column names
          if(!is.null(colnames(result))) {

            if(test.colnames){
              test.info.names <- paste0(
                  "{",
                  "\"Test\":\"Equal Column Name\",",
                  "\"Column\":\"%s\", ",
                  "\"Received\":\"%s\", \"Expected\":\"%s\"",
                  "}")

              #for(i in 1:dim(result)[2]) {
              tmp <- lapply(1:dim(result)[2], function(i) {
                    rec.name <- rec.colnames[i]
                    exp.name <- exp.colnames[i]

                    expect_equal(
                        object    = rec.name,
                        expected  = exp.name,
                        info      = sprintf(test.info.names, i, rec.name, exp.name))
                  })
            }

          }

          # Test row names
          if(!is.null(rownames(result))) {
            if(test.rownames){
              test.info.names <- paste0(
                  "{",
                  "\"Test\":\"Equal Row Name\",",
                  "\"Row\":\"%s\", ",
                  "\"Received\":\"%s\", \"Expected\":\"%s\"",
                  "}")

              #for(i in 1:dim(result)[1]) {
              tmp <- lapply(1:dim(result)[1], function(i) {
                    rec.name <- rec.rownames[i]
                    exp.name <- exp.rownames[i]

                    expect_equal(
                        object    = rec.name,
                        expected  = exp.name,
                        tolerance = 0,
                        info      = sprintf(test.info.names, i, rec.name, exp.name))
                  })
            }

          }


        }
      })
}


# test_returnValue_list_nodebynode ##########################################################

#' Tests a Standard R 'list' Node-By-Node ('RTestTest_list_nodebynode')
#'
#' @param    result             (\code{list}) The result list to be tested
#' @param    reference          (\code{list}) The reference list
#' @param    xmlTestSpec         (\code{XMLNode}) The XML definition of type
#'                              'RTestTest_list_nodebynode'
#' @param    add.desc          (\code{character}) Additional description added to the XML
#'                             definition.
#'
#' @seealso \code{\link[XML]{XMLNode-class}}
#'
#' @author   Sergej Potapov \email{sergej.potapov@@roche.com}
test_returnValue_list_nodebynode <- function(result, reference, xmlTestSpec, add.desc = NULL) {
  if(is.null(xmlTestSpec)){
    xmlTestSpec <- xmlNode("return-value",attrs=list("compare-type"="equal"))
  }
  test.attrs <- xmlAttrs(xmlTestSpec)

  # Global settings of the test -------------------------------------------------------------------

  # Get the global settings of the test from the current XML definition. These are mendatory and
  # will be used for the subsequent element-by-element (i.e. each cell between all column and rows)
  # tests if not overwritten by other specifications on the element level

  test.name      <-
      ifelse("desc" %in% names(test.attrs),
          test.attrs[["desc"]], "Check return value (list).")

  if(!is.null(add.desc))
    test.name <- paste0(add.desc," ",test.name)

  test.diffType  <-
      ifelse("diff-type" %in% names(test.attrs),
          test.attrs[["diff-type"]], "absolute")

  test.compareType  <-
      ifelse("compare-type" %in% names(test.attrs),
          test.attrs[["compare-type"]], "equal")

  test.tolerance <-
      ifelse("tolerance" %in% names(test.attrs),
          as.numeric(test.attrs[["tolerance"]]), 1.5e-8)

  # Get the entries and settings from the reference vector ----------------------------------------
  elems <- lapply(1:length(reference),
      function(i) {

        elem <- list()

        elem$name        <- if(is.null(names(reference))) i else names(reference)[i]
        elem$diffType    <- test.diffType
        elem$compareType <- test.compareType
        elem$tolerance   <- test.tolerance

        return(elem)
      })

  names(elems) <- sapply(elems, function(e) e$name)


  # Check if specific specifications for single columns are done ----------------------------------

  if(length(xmlChildren(xmlTestSpec)) > 0) {
    # If any children (i.e. single elements) are defined, use the settings of them for the test

    xmlApply(xmlTestSpec,
        function(xmlElemItem) {

          attrs     <- xmlAttrs(xmlElemItem)

          name      <- attrs[["name"]]

          if("diff-type" %in% names(attrs))
            elems[[name]][["diff-type"]] <<- attrs[["diff-type"]]

          if("compare-type" %in% names(attrs))
            elems[[name]][["compare-type"]] <<- attrs[["compare-type"]]

          if("tolerance" %in% names(attrs))
            elems[[name]][["tolerance"]] <<- as.numeric(attrs[["tolerance"]])
        })
  }


  # Perform test ----------------------------------------------------------------------------------

  test_that(test.name, {

        # Level 1: Test number of nodes - - - - - - - - - - - - - - - - - - - - - - - - -
        exp.nnodes <- length(reference)
        rec.nnodes <- length(result)


        test.info.dims <- paste0(
            "{",
            "\"Test\":\"%s\", ",
            "\"Received\":\"%s\", \"Expected\":\"%s\"",
            "}")

        expect_equal(
            object    = rec.nnodes,
            expected  = exp.nnodes,
            info      = sprintf(test.info.dims, "Equal Node Number", rec.nnodes, exp.nnodes))


        if(exp.nnodes == rec.nnodes) {

          # Level 2: Test node names - - - - - - - - - - - - - - - - - - - - - - - - - - -

          # Test node names
          if(!is.null(names(result))) {
            test.info.names <- paste0(
                "{",
                "\"Test\":\"Equal Node Name\",",
                "\"Column\":\"%s\", ",
                "\"Received\":\"%s\", \"Expected\":\"%s\"",
                "}")

            for(i in 1:length(result)) {
              rec.name <- names(result)[i]
              exp.name <- names(reference)[i]

              expect_equal(
                  object    = rec.name,
                  expected  = exp.name,
                  info      = sprintf(test.info.names, i, rec.name, exp.name))
            }
          }
        }
      })

  # Level 3: Test node by node - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if(length(reference) > 0) {
    for(elem in 1:length(reference)){
      if(!is.null(tryCatch(result[[elem]],error=function(e)NULL))){
        if(class(reference[[elem]]) %in% c("data.frame","matrix"))
        {
          test_returnValue_data.frame_cellbycell(
              result[[elem]], reference[[elem]], xmlTestSpec,
              add.desc = paste0(" -- List entry '",elems[[elem]]$name,"' (data.frame):")
          )

        } else if(class(reference[[elem]]) %in%
            c("numeric", "character", "logical", "integer", "factor"))
        {

          if(length(reference[[elem]])==1){



            names(result[[elem]]) <- names(reference[[elem]])
            test_returnValue_variable(
                result[[elem]],
                reference[[elem]],
                xmlTestSpec,
                add.desc = paste0(" -- List entry '",elems[[elem]]$name,"' (variable):"))

          }else{
            test_returnValue_vector_elementbyelement(
                result[[elem]], reference[[elem]], xmlTestSpec,
                add.desc = paste0(" -- List entry '",elems[[elem]]$name,"' (vector):")
            )
          }

        } else if(class(reference[[elem]]) %in% c("list","XMLNodeList","XMLNode"))
        {
          # Check if the list is an image
          if(!is.null(reference[[elem]]$image) && reference[[elem]]$image){
            test_returnValue_image(
                result[[elem]]$address,
                reference[[elem]]$address,
                xmlTestSpec,
                add.desc = paste0(" -- List entry '",elems[[elem]]$name,"' (image):"))

          }else{

            test_returnValue_list_nodebynode(
                result[[elem]], reference[[elem]], xmlTestSpec,
                add.desc = paste0(" -- List entry '",elems[[elem]]$name,"' (list):")
            )
          }
        } else {
          stop(paste0("Mode with class: '",class(reference[[elem]]),"' not supported"))
        }
      }else{
        # In case the list element was not found, return that result
        test.info.names <- paste0(
            "{",
            "\"Test\":\"Equal Node Name\",",
            "\"Received\":\"%s\", \"Expected\":\"%s\"",
            "}")
        test_returnValue_variable(
            "no list element",
            elems[[elem]]$name,
            xmlTestSpec,
            add.desc = paste0(" -- List entry '",elems[[elem]]$name,"' (ANY):"))

      }

    }
  }
}


# test_manualCheck_file ###########################################################################

#' Creates a manual check via tcl/tk interface of created output file
#'
#' @param    result             (\code{object}) The result object to be tested.
#' @param    reference          (\code{object}) The reference object.
#' @param    xmlTestSpec        (\code{XMLNode}) The XML definition of type 'RTestTest_variable'.
#' @param    add.desc           (\code{character}) Additional description added to the XML
#'                              definition.
#' @param    openrecexp         (\code{code}) Function to open/show reference and expected result.
#' @param    openrecexp.exec    (\code{logical}) Specify, if the openrecexp-function should be
#'                              executed by default or only on user decision (by click).
#'
#' @seealso \code{\link[XML]{XMLNode-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
test_manualCheck_file <- function(result, reference, xmlTestSpec=NULL, add.desc = NULL,
    openrecexp = NULL, openrecexp.exec = FALSE)
{
  testmode <- if(!is.null(options("testmode")[[1]])){
        as.logical(options("testmode"))
      }else{
        FALSE
      }

  if(is.null(xmlTestSpec)){
    xmlTestSpec <- xmlNode("return-value",attrs=list("compare-type"="equal"))
  }
  test.attrs <- xmlAttrs(xmlTestSpec)

  # Check if provided reference is a file or just a description
  refisfile <- ifelse(file.exists(reference), TRUE, FALSE)

  if(length(reference) == 1 && file.exists(reference)) {
    refisfile <- TRUE
    reference.html <- reference
    reference.txt  <- reference
  } else {
    refisfile <- FALSE
    reference.html <- paste(paste0("* ",reference), collapse = "<br>")
    reference.txt  <- paste(paste0("* ",reference), collapse = "\n")
  }



  # Global settings of the test -------------------------------------------------------------------

  # Open confirmation window to catch user decision about the output

  test.name      <-
      ifelse("desc" %in% names(test.attrs),
          test.attrs[["desc"]], "Manual check of created output")

  if(!is.null(add.desc))
    test.name <- paste0(add.desc," ",test.name)


  if(is.null(openrecexp)) {
    openrecexp <- function() {
      cat("\n\n-----------------------------------------------------\n\n")

   if( file.exists(result) ){

        cat("RESULT:\n")
        cat(result,"\n")

        if(file.info(result)$isdir) {
          tmp <- getwd()
          setwd(result)

      if(Sys.info()["sysname"]=="Windows")shell.exec(".")
          setwd(tmp)
        } else {
        if(Sys.info()["sysname"]=="Windows")shell.exec(result)
        }
    }else{
      cat("RESULT:\n")
      cat("not existing")
    }

      cat("REFERENCE:\n")
      cat(reference.txt,"\n")

      if(refisfile)
      if(Sys.info()["sysname"]=="Windows")shell.exec(reference)

      cat("\n\n-----------------------------------------------------\n\n")
    }
  }

  if(openrecexp.exec)
    do.call(openrecexp, list())

  userres <- test_manualCheck_confirmWindow(openrecexp = openrecexp, expectedTxt = reference.txt)


  # Perform test ----------------------------------------------------------------------------------

  test_that(test.name, {

        test.info <- paste0(
            "{",
            "\"Test\":\"Manual file check\", ",
            "\"Name\":\"%s\", ",
            "\"Received\":\"%s\", ",
            "\"Expected\":\"%s\", ",
            "\"User decision\":\"%s\", ",
            "\"User comment\":\"%s\" ",
            "}")

        do.call(
            "expect_equal",
            list(
                object    = userres$result,
                expected  = TRUE,
                tolerance = 1.5e-8,
                scale     = 1,
                info      = sprintf(
                    test.info,
                    "Created output file as expected - manually checked by tester.",
                    basename(result),
                    reference.html,
                    ifelse(userres$result, "passed", "failed"),
                    userres$comment
                )
            )
        )
      })
}




# test_manualCheck_confirmWindow ##################################################################

#' Creates a Tcl/Tk Window for confirmation of the manual check
#'
#' @param    openrecexp (\code{function}) Function to open a window with the received and expected
#'           values or NULL to hide all.
#' @param    expectedTxt (\code{character}) Shown as text for the expected vidow.
#'
#' @return  (\code{list}) List with elements 'result': TRUE or FALSE depending on the user's
#'           choice and 'comment': inserted user comment.
#'
#'
#' @section Package dependencies:
#' The \code{tcltk} package is required. Please install it from CRAN if not in your current library
#' path.
#'
#' @seealso \code{\link[XML]{XMLNode-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
test_manualCheck_confirmWindow <- function(openrecexp = NULL, expectedTxt = NULL) {

  if(!requireNamespace("tcltk", quietly = TRUE)) {
    stop("Package 'tcltk' is needed for this function to work. Please install it.", call. = FALSE)
  }

  testmode <- if(!is.null(options("testmode")[[1]])){
    as.logical(options("testmode"))
  }else{
    FALSE
  }

  # Initalize variable to store user decisionresult
  result <- NA


  # Create a new Tk toplevel window assigned to win1
  win <- tcltk::tktoplevel()

  tcltk::tkwm.title(win, "RTest")


  tcltk::tkgrid(tcltk::tklabel(win, text="   "))


  # Headline
  txt <- tcltk::tklabel(win, text = "MANUAL CHECK", font = tcltk::tkfont.create(weight="bold"))
  tcltk::tkgrid(txt, columnspan = 4)
  tcltk::tkgrid(tcltk::tklabel(win, text="   "))

  if(!is.null(expectedTxt)) {
    tcltk::tkgrid(tcltk::tklabel(win, text="EXPECTED RESULTS:"), columnspan = 4)
    tcltk::tkgrid(tcltk::tklabel(win, text=expectedTxt), columnspan = 4)
    tcltk::tkgrid(tcltk::tklabel(win, text="   "))
  }

  # Button to open expected/received results via passed function
  if(!is.null(openrecexp)) {
    # Create a Tk button whose function (command) is to destroy the window win1

    butRECEXP <- tcltk::tkbutton(win, text = "Open received results (file)", width = 30,
        command = function() { do.call(openrecexp, list()) })

    tcltk::tkgrid(butRECEXP, columnspan = 4)
    tcltk::tkgrid(tcltk::tklabel(win, text="   "))
  }

  # Label
  txt <- tcltk::tklabel(win, text = "Are the acceptance criteria fulfilled (\"as expected\")?")
  tcltk::tkgrid(txt, columnspan = 4)
  tcltk::tkgrid(tcltk::tklabel(win, text="   "))

  # Comment
  lbl <- tcltk::tklabel(win, text = "Comment")
  tcltk::tkgrid(lbl, columnspan = 4)

  comment <- tcltk::tclVar("")
  txtin <- tcltk::tkentry(win, width = 50, textvariable = comment)
  tcltk::tkgrid(txtin, columnspan = 4)
  tcltk::tkgrid(tcltk::tklabel(win, text="   "))

  # Buttons
  # Create a Tk button whose function (command) is to destroy the window win1
  butYES <- tcltk::tkbutton(win, text = "PASSED", width = 10,
      command = function() { result <<- TRUE; tcltk::tkdestroy(win) })

  # Create a Tk button whose function (command) is to destroy the window win1
  butNO <- tcltk::tkbutton(win, text = "FAILED", width = 10,
      command = function() { result <<- FALSE; tcltk::tkdestroy(win) })

  tcltk::tkgrid(tcltk::tklabel(win, text="   "), butYES, butNO, tcltk::tklabel(win, text="   "))
  tcltk::tkgrid(tcltk::tklabel(win, text="   "))

  # Destroy action
  tcltk::tkbind(win, "<Destroy>", function() { if(is.na(result)) result <<- FALSE; tcltk::tkdestroy(win) })

  # Focus on current window
  tcltk::tkfocus(win)

  if(testmode){
    tcltk::tkdestroy(win)
    return(list(result = TRUE, comment = "TRUE by testmode"))
  }else{

    # Wait on user interaction (i.e. until variable 'result' is set)
    while(is.na(result)){ Sys.sleep(5) }

    return(list(result = result, comment = tcltk::tclvalue(comment)))
  }
}

# test_returnValue_variable #######################################################################

#' Tests an image file with ImageMagick ('RTestTest_image')
#'
#' @param    result             (\code{object}) The result object to be tested.
#' @param    reference          (\code{object}) The reference object.
#' @param    xmlTestSpec        (\code{XMLNode}) The XML definition of type 'RTestTest_variable'.
#' @param    add.desc          (\code{character}) Additional description added to the XML
#'                             definition.
#'
#' @seealso \code{\link[XML]{XMLNode-class}}
#' @examples
#'
#' # Cleaning up
#'
#' tryCatch(unloadNamespace("RTest"))
#' tryCatch(unloadNamespace("testthat"))
#' library(RTest)
#'
#' # create some definition of tests
#'
#' data <- '<test_image desc="Compare a value" diff-type="relative"
#'          compare-type="equal" tolerance="0"/>'
#' xmlTestSpec <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' location <- find.package("RTest")
#'
#' # Create a test with equal images
#'
#' result <- paste0(location,"/images/Roche_Logo.png")
#' reference <- paste0(location,"/images/Roche_Logo.png")
#'
#' test_returnValue_image(result,reference,xmlTestSpec)
#'
#'  # Create a test with images that are not equal
#'
#' reference <- paste0(location,"/images/Roche_Logo_defect.png")
#' tryCatch(
#'    test_returnValue_image(result,reference,xmlTestSpec),
#'    error=function(e){
#'      if(!grepl("not equal to",e)){
#'        stop("image omparison defect, please check code")
#'      }
#'    })
#'
#' @importFrom magick image_compare image_read image_write
#' @author   Sebastian Wolf \email{sebastian.wolf.sw1@@roche.com}
test_returnValue_image <- function(result, reference, xmlTestSpec, add.desc = NULL) {
  if(is.null(xmlTestSpec)){
    xmlTestSpec <- xmlNode("return-value",attrs=list("compare-type"="equal"))
  }
  test.attrs <- xmlAttrs(xmlTestSpec)

  # Global settings of the test -------------------------------------------------------------------

  # Get the global settings of the test from the current XML definition.

  test.name      <-
      ifelse("desc" %in% names(test.attrs),
          test.attrs[["desc"]], "Check return image.")

  if(!is.null(add.desc))
    test.name <- paste0(add.desc," ",test.name)

  test.diffType  <-
      ifelse("diff-type" %in% names(test.attrs),
          test.attrs[["diff-type"]], "absolute")

  test.compareType  <-
      ifelse("compare-type" %in% names(test.attrs),
          test.attrs[["compare-type"]], "equal")

  test.tolerance <-
      ifelse("tolerance" %in% names(test.attrs),
          as.numeric(test.attrs[["tolerance"]]), 1.5e-8)



  # Perform test ----------------------------------------------------------------------------------

  test_that(test.name, {

        # Level 1: Test value - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        test.info <- paste0(
            "{",
            "\"Test\":\"Equal Value\", ",
            "\"Name\":\"%s\", ",
            "\"Received\":\"%s\", \"Data Type\":\"%s\", ",
            "\"Expected\":\"%s\", \"Data Type\":\"%s\", ",
            "\"Diff in percent\":\"%s\", ",
            "\"Diff Image\":\"%s\", ",
            "\"Tolerance\":\"%s\" ",
            "}")

        # Get data
        rec <- unname(result)
        exp <- unname(reference)

        # Get data types
        rec.type <- typeof(rec)
        exp.type <- typeof(exp)

        # Handle factors as strings for comparison
        if(is.factor(rec)) rec <- levels(rec)[rec]
        if(is.factor(exp)) exp <- levels(exp)[exp]

        # Tolerance set to very small number, like in all.equal (which is used by testthat)
        #     https://stat.ethz.ch/R-manual/R-devel/library/base/html/all.equal.html
        if(test.tolerance == 0)
          test.tolerance <- 1.5e-8

        difference_png_name <- tempfile( fileext = ".png")


#        ImageMagick <- if(Sys.which("magick")!=""){
#          "magick "
#        }else{
#          if(Sys.which("compare")==""){
#            stop("No ImageMagick installed. Please use \n
#                    sudo apt-get install imagemagick libmagickcore-dev libmagickwand-dev libmagic-dev \n
#                    on Linux or download ImageMagick for Windows.
#                    ")
#          }else{
#            ""
#          }
#        }
#
#    if(Sys.info()["sysname"]=="Windows"){
#      compare_result <- suppressWarnings(shell(
#              paste(ImageMagick,"compare -metric RMSE \"",
#                  gsub("\\\\", "/", result),"\" \"",
#                  gsub("\\\\", "/",reference),"\" ",
#                  paste0("\"",difference_png_name,"\""," 2>&1"),sep=""),
#              intern=T))
#    }else{
#      compare_result <- suppressWarnings(system(
#              paste(ImageMagick,"compare -metric RMSE \"",
#                  gsub("\\\\", "/", result),"\" \"",
#                  gsub("\\\\", "/",reference),"\" ",
#                  paste0("\"",difference_png_name,"\""," 2>&1"),sep=""),
#              intern=T))
#    }

#        difference_in_percent <- as.numeric(
#            sub("\\(","",
#                stringr::str_extract(compare_result[1],"\\([^\\)]*")
#            )
#        )

    image_compared <- magick::image_compare(
        image=magick::image_read(rec),
        reference_image = magick::image_read(exp),
        metric = "RMSE")

    difference_in_percent <- attributes(image_compared)$distortion

    magick::image_write(
        image_compared,
        path = difference_png_name

    )

    difference_png_name_text <- tempfile()

    base64::encode(difference_png_name, difference_png_name_text)

        src <- sprintf("data:image/png;base64,%s",
            paste(readLines(difference_png_name_text), collapse = ""))

        image_for_info <-
            sprintf("<img width=200 src='%s' alt='%s' />",
                src,
                paste0(gsub(":","_",gsub(c(" "),"_",date()))))


    switch(test.compareType,
            "equal" = {
              do.call(
                  "expect_equal",
                  list(
                      object    = difference_in_percent,
                      expected  = 0,
                      tolerance = test.tolerance,
                      scale     = if(test.diffType == "absolute") 1 else NULL,
                      info      = sprintf(
                          test.info,
                          test.name,
                          htmlify_string(rec), "Image",
                          htmlify_string(exp), "Image",
                          difference_in_percent,
                          image_for_info,
                          test.tolerance
                      )
                  )
              )
            },
            stop("Compare type '", test.compareType,"' currently not implemented.")
        )
      })
}


#' Generically compare two values with RTest
#'
#' This function compares two value by a \code{test_returnValue_...} function
#' that fits the class of the \code{reference} input parameter.
#'
#' @param result (\code{any}) Any value of type character, numeric, data.frame or list
#'   (image links do not work!)
#' @param reference (\code{any}) Any value of type character, numeric, data.frame or list
#'   (image links do not work!)
#' @param xmlTestSpec (\code{XMLNode}) An XMLNode of type \code{RTest_test_returnValue_...}
#'
#' @return The function will not return anything but call \code{testthat} functions
#'    creating outputs in the reporter
#'
#' @export
#'
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
test_returnValue_any <- function(result,reference,xmlTestSpec){
  ### ------ Check class of values (result, reference) ------ ######

  test_returnValue_variable(
      class(result),
      class(reference),
      NULL,
      add.desc="Checking output class and reference class.")

  ### ------ Compare values ------ ######

  if(class(reference)=="data.frame"){
    test_returnValue_data.frame_cellbycell(
        result,
        reference,
        xmlTestSpec = xmlTestSpec)
  }else if(class(reference)=="list"){
    test_returnValue_list_nodebynode(
        result,
        reference,
        xmlTestSpec = xmlTestSpec)
  }else if(length(reference)>1){
    test_returnValue_vector_elementbyelement(
        result = result,
        reference = reference,
        xmlTestSpec = xmlTestSpec)
  }else{
    test_returnValue_variable(
        result,
        reference,
        xmlTestSpec = xmlTestSpec)
  }
}
