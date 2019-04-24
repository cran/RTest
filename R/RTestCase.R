###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# Class Definition of 'RTestCase'                                                                 #
#                                                                                                 #
# Date:           25 - Jan - 2016                                                                 #
# Author:         Matthias Pfeifer (matthias.pfeifer@roche.com)                                   #
#                                                                                                 #
###################################################################################################



#' The 'RTestCase' Class
#'
#' Objects of the \code{RTestCase} class specify and implement the general behavoir of the
#' XML-based test cases (TC).
#'
#' On basis of Based on a XML definition file (see \code{'RTestCase.xsd'}), test cases are defined
#' for the individual packages / projects to be tested. Thereby, this class definition provides
#' the general outline of the individual test case classes (i.e. test case adapters), which have
#' to implement the project- and package-specific requirements and test logic.
#'
#' The slot '\code{tests}' represents the execution environment for a test case and stores all
#' information about the execution including test case metainformation, test result (success or
#' failed), the testthat reporter and the test cache. It has the following format:\cr
#' \code{XML:                     tests:}\cr
#' \code{tests}\cr
#' \code{ |- Pkg_1               [Pkg_1][1]}\cr
#' \code{ |    - funct_01        [Pkg_1][1][funct_01][1] = list( ... )}\cr
#' \code{ |- Pkg_1               [Pkg_1][2]}\cr
#' \code{ |   |- funct_01        [Pkg_1][2][funct_01][1] = list( ... )}\cr
#' \code{ |   |- funct_01        [Pkg_1][2][funct_01][2] = list( ... )}\cr
#' \code{ |    - funct_02        [Pkg_1][2][funct_02][1] = list( ... )}\cr
#' \code{  - Pkg_2               [Pkg_2][1]}\cr
#' \code{      - funct_01        [Pkg_2][1][funct_01][1] = list( ... )}\cr
#'
#' @slot     ID            (\code{character}) ID of the TC.
#' @slot     tc.type       (\code{character}) Type of the TC (i.e. the class name).
#' @slot     synopsis      (\code{list}) Synopsis information of the TC (as defined in the XSD
#'                         ComplexType 'RTestSynopis').
#' @slot     xml.fPath     (\code{character}) Path to XML definition file of the TC.
#' @slot     xml.root      (\code{XMLNode}) The imported TC definition as \code{XMLNode}-class
#'                         object.
#' @slot     input.data    (\code{list}) The input data of the test case, which will be filled
#'                         using the method \code{\link{readXMLInputData}}.
#' @slot     tests         (\code{list}) A list with the test results or NULL, if the test case
#'                         has not been executed (see 'Details').
#' @slot     test.for      (\code{character}) Names of the packages, which were tested in the last
#'                         execution of the test case. NULL, if the test case has not been
#'                         executed.
#' @slot     test.result   (\code{character}) Result of the last test case execution ('SUCCESS' or
#'                         'FAILURE'). NULL, if the test case has not been executed.
#'
#' @author   Matthias Pfeifer \email{matthiaspfeifer@@gmx.net}
setClass(
  Class          = "RTestCase",

  representation = representation(
    ID                = "character",
    tc.type           = "character",
    synopsis          = "list",
    xml.fPath         = "character",
    xml.root          = "XMLNode",
    input.data        = "ANY",
    tests             = "environment",
    test.for          = "ANY",
    test.result       = "ANY"
  ),

  prototype      = list(
    test.for          = NULL,
    test.result       = NULL)
)

#' Constructor for RTestCase
#' @param     ID            (\code{character}) ID of the TC.
#' @param     tc.type       (\code{character}) Type of the TC (i.e. the class name).
#' @param     synopsis      (\code{list}) Synopsis information of the TC (as defined in the XSD
#'                         ComplexType 'RTestSynopis').
#' @param     xml.fPath     (\code{character}) Path to XML definition file of the TC.
#' @param     xml.root      (\code{XMLNode}) The imported TC definition as \code{XMLNode}-class
#'                         object.
#' @param     input.data    (\code{list}) The input data of the test case, which will be filled
#'                         using the method \code{\link{readXMLInputData}}.
#' @param     tests         (\code{list}) A list with the test results or NULL, if the test case
#'                         has not been executed (see 'Details').
#' @param     test.for      (\code{character}) Names of the packages, which were tested in the last
#'                         execution of the test case. NULL, if the test case has not been
#'                         executed.
#' @param     test.result   (\code{character}) Result of the last test case execution ('SUCCESS' or
#'                         'FAILURE'). NULL, if the test case has not been executed.
#' @return (.Object) \link{RTestCase-class} Object
#'
#' @export
#'
#' @examples
#' library(RTest)
#'
#' xml.root <- XML::newXMLNode("func01")
#' RTest::xmlFromList(xml.root,
#'     list(
#'         params=list(mult=list(attributes=c(value="1",type="numeric"))),
#'         testspec=list(
#' execution=list(attributes=c("execution-type"="silent")),
#' "return-value"=list(attributes=c(
#' "compare-type"="equal",
#' "diff-type"="absolute",
#' "tolerance"=0.001
#' ))
#'         )
#'     )
#' )
#' # Add the reference result to the params and testspec and read it in again
#' xml.root <- XML::xmlRoot(XML::xmlTreeParse(
#'         paste0("<root>",
#'             capture_output(print(xml.root[[1]])),
#'             capture_output(print(xml.root[[2]])),
#'             paste(xmlWriteData_data.frame(
#'                     "reference",
#'                     data=data.frame(x=c(1,1),y=c(2,2),sum=c(3,3)),
#'                     printXML=FALSE)
#'                 ,collapse="\n"),"</root>")
#'     ))
#'
#' # Define what to test in the first test
#' # Please check the function test.RTest.funct_01 to see
#' # how it tests the code of the function ("test_fun")
#' tests <- new.env()
#' testEntry <- list(
#'     "pkg"            = "RTest",                  # test description
#'     "pgk-iter"       = "1",
#'     "func"           = "funct_01",
#'     "func-iter"      = "1",
#'     "test-code"      = "RTest::test_fun",
#'     "test-adapter"   = "RTestCase",
#'     "test-func"      = "test.RTest.funct_01",
#'     "pkg-desc"       = "no package desc",
#'     "func-desc"      = "Simple add func",
#'     "xpath"          = "/root",
#'     "reporter"       = NA,   # field for testthat reporter
#'     "result"         = NA,   # field for test result (failed/success)
#'     "cache"          = NA,   # field for caching test results
#'     "execresid"      = NA,   # field for test execution result id
#'     "specid"         = "0",   # field for test function id
#'     "riskid"         = "0"   # field for test function risk id
#' )
#'
#' # Assign test entry to test environment
#' #      pkg-name  pkg-iter    func-name  func-iter
#' tests[["RTest"]][["1"]][["funct_01"]][["1"]] <- testEntry
#'
#' # Create a TestCase Object
#' object <- RTestCase(
#'     ID="1",
#'     tc.type="RTestCase",
#'     synopsis= list(version="v1",author="Sebastian Wolf"),
#'     xml.fPath="",
#'     xml.root=xml.root,
#'     input.data=list("one"=
#'             data.frame(x=c(1,1),y=c(2,2))
#'     ),
#'     tests=tests,
#'     test.for="RTest",
#'     test.result=NA
#' )
#'
#' # Run the test
#' object <- test(object,test.for="RTest")
#' stopifnot(object@@test.result=="success")
#'
#' # Run a failing test
#'
#' object@@input.data <- list("one"=
#'         data.frame(x=c(1,2),y=c(2,1))
#' )
#' object <- test(object,test.for="RTest")
#' stopifnot(object@@test.result=="failed")
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
RTestCase <- function(
    ID                = NULL,
    tc.type           = NULL,
    synopsis          = NULL,
    xml.fPath         = NULL,
    xml.root          = NULL,
    input.data        = NULL,
    tests             = NULL,
    test.for          = NULL,
    test.result       = NULL
){
  if(missing(xml.fPath) || xml.fPath==""){
    #stop("Argument 'xml.fPath' missing.")
    new("RTestCase",
        ID                = ID,
        tc.type           = tc.type ,
        synopsis          = synopsis,
        xml.fPath         = xml.fPath,
        xml.root          = xml.root,
        input.data        = input.data,
        tests             = tests,
        test.for          = test.for,
        test.result       = test.result
    )
  }else{

    if(!file.exists(xml.fPath))
      stop("Input file '",xml.fPath,"' not exists.")


    # Open and read xml ---------------------------------------------------------------------------

    #xml.doc  <- xmlTreeParse(xml.fPath, getDTD = F)
    xml.doc  <- xmlTreeParse(xml.fPath, getDTD = F,
               handlers = list("comment"=function(x,...){NULL}), asTree = TRUE)
    xml.root <- xmlRoot(xml.doc)

    # Initialize slots ----------------------------------------------------------------------------

    # XML information - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    xml.fPath <- xml.fPath
    xml.root  <- xml.root


    # TC information (synopsis) - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ID        <- xmlValue(xml.root[["ID"]])
    tc.type   <- xmlName(xml.root)

    synopsis  <- list(
        "version"           = xmlValue(xml.root[["synopsis"]][["version"]]),

        "author"            =
            paste(
                sapply(
                    xmlElementsByTagName(xml.root[["synopsis"]], "author"),
                    function(e) xmlValue(e)
                ), collapse = ", "),

        "short-description"  =
            if(!is.null(xml.root[["synopsis"]][["short-description"]])){
              xmlValue(xml.root[["synopsis"]][["short-description"]])

            }else{
              ""
            },

        "description"       =
            if(!is.null(xml.root[["synopsis"]][["description"]])){
              xmlValue(xml.root[["synopsis"]][["description"]])
            }
            else{
              ""
            } ,

        "label"       =
            if(!is.null(xml.root[["synopsis"]][["label"]])) {
              xmlValue(xml.root[["synopsis"]][["label"]])

            }
            else{""},

        "creation-date"     =
            if(!is.null(xml.root[["synopsis"]][["creation-date"]])) {

              xmlValue(xml.root[["synopsis"]][["creation-date"]])
            }else{""},

        "change-history"    =
            if(!is.null(xml.root[["synopsis"]][["change-history"]])){

              lapply(
                  xmlElementsByTagName(xml.root[["synopsis"]][["change-history"]], "change"),
                  function(e)
                    c(
                        author = xmlAttrs(e)[["author"]],
                        date   = xmlAttrs(e)[["date"]],
                        change =  xmlValue(e)
                    )
              )
            }else{
              list()
            }
    )

    # Set Input data


    .Object <- new("RTestCase",
        ID                = ID,
        tc.type           = tc.type ,
        synopsis          = synopsis,
        xml.fPath         = xml.fPath,
        xml.root          = xml.root,
        input.data        = data.frame(),
        tests             = new.env(),
        test.for          = NA,
        test.result       = NA
    )

    # TC execution details and test results - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    .Object@input.data <- readXMLInputData(.Object)
    .Object <- initializeTests(.Object)


    # Return initialized object -------------------------------------------------------------------

    return(.Object)
  }
}


# initializeTests #################################################################################

setGeneric("initializeTests",
  function(object) standardGeneric("initializeTests"))

#' Initialize the Test Slot for a Test Case.
#'
#' This method initializes the slots '\code{tests}', '\code{test.for}' and '\code{test.result}' of
#' a object of class '\code{RTestCase}'. See description of \code{\link{RTestCase-class}} for
#' further information.
#'@name initializeTests
#' @aliases initializeTests,RTestCase-method
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("initializeTests",
  signature  = "RTestCase",

  definition = function (object) {

    # Create a new empty environment for the results ----------------------------------------------
    tests <- new.env()

    # Create and initialize test structure --------------------------------------------------------

    # Run through all tests and create the execution structure (see class documentation for
    # detailed structure)
    xmlApply(
      object@xml.root[["tests"]],
      function(xmlPkgItem) {

        # Get infos about current package test definition - - - - - - - - - - - - - - - - - - - - -

        # Name of test package
        tf.pkg <- xmlName(xmlPkgItem)

        # First iteration: Initialize empty list for package test
        if(is.null(tests[[tf.pkg]]))
          tests[[tf.pkg]] <<- list()

        # Increase counter (no. of package tests defined in XML)
        tf.pkg.i <- length(tests[[tf.pkg]]) + 1

        # Get description (if specified in XML)
        tf.pkg.testDesc <-
          if(!is.null(xmlAttrs(xmlPkgItem)[["test-desc"]])) xmlAttrs(xmlPkgItem)[["test-desc"]]
          else ""


        # Get test functions of this package test - - - - - - - - - - - - - - - - - - - - - - - - -

        # Initialize list for function tests
        tests[[tf.pkg]][[tf.pkg.i]] <<- list()

        # Get test functions of this package test
        xmlApply(
          xmlPkgItem,
          function(xmlFuncItem) {

            # Name of test function
            tf.func <- xmlName(xmlFuncItem)

            # First iteration: Initialize empty list for function test
            if(is.null(tests[[tf.pkg]][[tf.pkg.i]][[tf.func]]))
              tests[[tf.pkg]][[tf.pkg.i]][[tf.func]] <<- list()

            # Increase counter (no. of function tests defined in XML)
            tf.func.i <- length(tests[[tf.pkg]][[tf.pkg.i]][[tf.func]]) + 1

            # Get description (if specified)
            tf.func.testDesc <-
              if("test-desc" %in% names(xmlAttrs(xmlFuncItem)))
                xmlAttrs(xmlFuncItem)[["test-desc"]]
              else
                ""

            # Get specid (if specified)
            tf.func.specid <-
              if("spec-id" %in% names(xmlAttrs(xmlFuncItem)))
                xmlAttrs(xmlFuncItem)[["spec-id"]]
              else
                ""

            # Get riskid (if specified)
            tf.func.riskid <-
              if("risk-id" %in% names(xmlAttrs(xmlFuncItem)))
                xmlAttrs(xmlFuncItem)[["risk-id"]]
              else
                ""

            # Initialize test entry
            testEntry <- list(
              "pkg"            = tf.pkg,                  # test description
              "pgk-iter"       = tf.pkg.i,
              "func"           = tf.func,
              "func-iter"      = tf.func.i,
              "test-code"      = paste0(tf.pkg,"::",tf.func),
              "test-adapter"   = xmlName(object@xml.root),
              "test-func"      = paste0("test.",tf.pkg,".",tf.func),
              "pkg-desc"       = tf.pkg.testDesc,
              "func-desc"      = tf.func.testDesc,
              "xpath"          =
                paste("/",
                  xmlName(object@xml.root),
                  "tests",
                  paste0(tf.pkg,"[",tf.pkg.i,"]"),
                  paste0(tf.func,"[",tf.func.i,"]"), sep = "/"),
              "reporter"       = NA,                    # field for testthat reporter
              "result"         = NA,                    # field for test result (failed/success)
              "cache"          = NA,                    # field for caching test results
              "execresid"      = NA,                                        # field for test execution result id
              "specid"         = tf.func.specid,                # field for test function id
              "riskid"         = tf.func.riskid                  # field for test function risk id

            )

            # Assign test entry to test environment
            #      pkg-name  pkg-iter    func-name  func-iter
            tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]] <<- testEntry
          })
      })


    # Set slots -----------------------------------------------------------------------------------

    # Slot 'test'
    object@tests        <- tests

    # Slot reset 'test.for' and 'test.status' (status: not executed)
    object@test.for     <- NA
    object@test.result  <- NA


    # Return object -------------------------------------------------------------------------------
    return(object)
  }
)




# show ############################################################################################

#' Print Summary of the Test Case to Console
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @aliases show,RTestCase-method
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("show",
  signature  = "RTestCase",

  definition = function (object) {
    cat("Test Case\n")
    cat("  Object of class '",class(object),"'\n")

    cat("  @ID : ",object@ID,"\n")
    cat("  @tc.type : ",object@tc.type,"\n")

    cat("  @synopsis :\n")
    for(i in names(object@synopsis)) {
      if(i != "change-history") {
        cat("    ",paste0("$",i),": ",object@synopsis[[i]],"\n")

      } else {
        cat("     $change-history :\n")
        for(c in object@synopsis[[i]]) {
          cat("       ",c["date"],", ",c["author"],":\n")
          cat("         ",c["change"],"\n")
        }
      }
    }
    cat("\n")

    cat("  @xml.fPath : ",object@xml.fPath,"\n")
    cat("  @xml.root : ",class(object@xml.root)[1],"\n")
    cat("\n")

    cat("  @input.data : \n")
    print(ls.str(object@input.data), max.level = 1)
    cat("\n")


    cat("  @test.for : ",
      (if(!is.null(object@test.for)) object@test.for else "NULL (test not executed)"),"\n")
    cat("  @test.result : ",
      (if(!is.null(object@test.result)) object@test.result else "NULL (test not executed)"),"\n")
    cat("\n")

    cat("  @tests :\n")
    print(ls.str(object@tests), max.level=4)
    cat("\n")
  }
)



# getID ###########################################################################################

setGeneric("getID",
  function(object) standardGeneric("getID"))

#' Get ID of the Test Case
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{character})
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @name getID
#' @aliases getID,RTestCase-method
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getID",
  signature  = "RTestCase",

  definition = function (object) {
    object@ID
  }
)



# getType #########################################################################################


setGeneric("getType",
  function(object) standardGeneric("getType"))
#' Get Type of the Test Case
#'
#' @name getType
#' @aliases getType,RTestCase-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{character})
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getType",
  signature  = "RTestCase",

  definition = function (object) {
    object@tc.type
  }
)



# getSynopsis #####################################################################################


setGeneric("getSynopsis",
  function(object) standardGeneric("getSynopsis"))

#' Get Synopsis of the Test Case
#'
#' @name getSynopsis
#' @aliases getSynopsis,RTestCase-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{character})
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getSynopsis",
  signature  = "RTestCase",

  definition = function (object) {
    object@synopsis
  }
)



# getXMLSourcePath ################################################################################


setGeneric("getXMLSourcePath",
  function(object) standardGeneric("getXMLSourcePath"))
#' Get Path to the XML Input File of the Test Case
#' @name getXMLSourcePath
#' @aliases getXMLSourcePath,RTestCase-method
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{character})
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getXMLSourcePath",
  signature  = "RTestCase",

  definition = function (object) {
    object@xml.fPath
  }
)



# getXMLSourceFileName ############################################################################


setGeneric("getXMLSourceFileName",
  function(object) standardGeneric("getXMLSourceFileName"))
#' Get File Name of the XML Input File of the Test Case
#'
#' @name getXMLSourceFileName
#' @aliases getXMLSourceFileName,RTestCase-method
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{character})
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getXMLSourceFileName",
  signature  = "RTestCase",

  definition = function (object) {
    s <- strsplit(object@xml.fPath, "/")[[1]]
    return(s[length(s)])
  }
)


# getXMLRoot ######################################################################################


setGeneric("getXMLRoot",
  function(object) standardGeneric("getXMLRoot"))

#' Get the XML Root of the Test Case
#'
#' @name getXMLRoot
#' @aliases getXMLRoot,RTestCase-method
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{XMLNode})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getXMLRoot",
  signature  = "RTestCase",

  definition = function (object) {
    object@xml.root
  }
)



# getTestFor ######################################################################################


setGeneric("getTestFor",
  function(object) standardGeneric("getTestFor"))
#' Get For of Last Execution of the Test Case
#'
#' @name getTestFor
#' @aliases getTestFor,RTestCase-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{ANY})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getTestFor",
  signature  = "RTestCase",

  definition = function (object) {
    object@test.for
  }
)


# getTestResult ###################################################################################


setGeneric("getTestResult",
  function(object) standardGeneric("getTestResult"))
#' Get Result of Last Execution of the Test Case
#'
#' @name getTestResult
#' @aliases getTestResult,RTestCase-method
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{ANY})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getTestResult",
  signature  = "RTestCase",

  definition = function (object) {
    object@test.result
  }
)



# clearTest #######################################################################################


setGeneric("clearTest",
  function(object) standardGeneric("clearTest"))

#' Clear Test Reporter and Test Cache of Last Test Case Execution
#'
#' This method resets the test information of the current test case including slots
#' \code{'test.for'}, \code{'test.result'} and the cache in the \code{'tests'} test case
#' environment.
#' @name clearTest
#' @aliases clearTest,RTestCase-method
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{\link{RTestCase-class}})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("clearTest",
  signature  = "RTestCase",

  definition = function (object) {

    # Clear global test execution information -----------------------------------------------------

    object@test.for    <- NA
    object@test.result <- NA


    # Clear test environment ----------------------------------------------------------------------
    tests <- object@tests

    for(tf.pkg in ls(tests)) {
      # Level 1: packages

      for(tf.pkg.i in 1:length(tests[[tf.pkg]])) {
        # Level 2: package-iterations

        for(tf.func in names(tests[[tf.pkg]][[tf.pkg.i]])) {
          # Level 3: functions of package

          for(tf.func.i in 1:length(tests[[tf.pkg]][[tf.pkg.i]][[tf.func]])) {
            # Level 3: iteration of a function

            # Clear test reporter
            tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]][["reporter"]] <- NA

            # Clear test execution status
            tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]][["result"]]   <- NA

            # Clear test cache
            tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]][["cache"]]    <- NA

            # Clear test exec result id
            tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]][["execresid"]]<- NA
          }
        }
      }
    }

    object@tests <- tests

    # Return
    return(object)
  }
)



# readXMLInputData ################################################################################

setGeneric("readXMLInputData",
  function(object) standardGeneric("readXMLInputData"))

#' Read Input Data of Test Case for Default XML Definitions
#'
#' This functions reads the XML definitions for default RTest objects.
#'
#' Input datasets are contained below the XML element '\code{input-data}' and can be used by any
#' test packages and functions of the respective test case adapter (i.e. by the functions of the
#' objects inheriting the class '\code{RTestCase}'). By default major R data types are predefined
#' under the node and can be read by this method. Thereby, the type of the imported datasets is
#' determined by the XML element names. This method runs through all XML items, parses the
#' item and converts it into the respective R object.\cr
#' The following element types are supported:\cr
#' \itemize{
#'   \item{variable}{\code{XML: variable, XSD: RTestData_variable, RTest: xmlReadData_variable}}
#'   \item{vector}{\code{XML: vector, XSD: RTestData_vector, RTest: xmlReadData_vector}}
#'   \item{data.frame}{\code{XML: data.frame, XSD: RTestData_data.frame, RTest: xmlReadData_data.frame}}
#'   \item{list}{\code{XML: list, XSD: RTestData_list, RTest: xmlReadData_list}}
#' }
#'
#'
#' @name readXMLInputData
#' @aliases readXMLInputData,RTestCase-method
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object
#'
#' @return   (\code{ANY}) If multiple datasets are defined a \code{list} will be returned
#'           containing all datasets with the same order as in the XML file. Else, if only a single
#'           is defined, the dataset itself will be returned.
#'
#' @seealso  \code{RTestCase-class}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("readXMLInputData",
  signature  = "RTestCase",

  definition = function(object) {

    input.data <- xmlRead.default(object@xml.root[["input-data"]])
    input.data
  }
)




# test ############################################################################################



setGeneric("test",
  function(object, test.for = NULL, out.fPathPre = NULL, ...) standardGeneric("test"))
#' Execute Test Logic of the Test Case
#'
#' This method executes the test logic for a test case in a \code{testthat} reporter environment.
#'
#' This method performs the test logic by iterating through all testgroups as defined in the TC XML
#' definition file. For each test group it starts a seperate '\code{ListReporter}', which is
#' defined in the \code{testthat} package. Then, the different functions of a testgroup are
#' executed. Therefore, this function calls the method \code{execTCAdapter}, which needs to be
#' defined for each TestCase type separatly (e.g. for DSTAT, VCA, Calib, etc.). This method is the
#' adapter function and knows how to read the test case and how to execute the functions, which
#' should be tested. Thereby, all test results generated using \code{\link[testthat]{test_that}}
#' and the \code{expect_*} of the \code{testthat} package are recorded by the previously started
#' reporter object. The generated test results are stored (slot \code{test.result}) and the test
#' execution status set (slot \code{test.status}).
#'
#' @name test
#' @aliases test,RTestCase-method
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#' @param    test.for        (\code{vector(character)}) Specification for which elements to test,
#'                           NULL to test for all elements
#' @param    out.fPathPre    (\code{character}) Prefix incl. path to output files generated during
#'                           tetest
#' @param    ...            Additional arguments passed to the check function.
#'
#' @return   (\code{\link{RTestCase-class}})
#'
#' @seealso  \code{\link{RTestCase-class}}
#' @examples
#'
#' location <- find.package("RTest")
#'
#' TestCase <- RTestCase(xml.fPath =
#'   file.path(location,"xml-templates","RTest_TC-01.xml"))
#'
#' result <- test(TestCase)
#'
#' stopifnot(result@@test.result == "success")
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("test",
  signature  = "RTestCase",

  definition = function (object, test.for = NULL, ...) {
    RTest.cat("##### TEST CASE '",object@ID,"'\n\n")

    # Check test.for argument ---------------------------------------------------------------------

    # If NULL, test for all elements specified for this TC
    if(is.null(test.for))
      test.for <- XML::xmlSApply(object@xml.root[["tests"]], xmlName)


    # Use caching for multiple iterations, say, allow to say, which tests to should re-run
    # Check, if execution details changed (test.for)
    #if(!is.null(object@test.for) && !identical(test.for, object@test.for)) {
    #   object <- clearTest(object)
    #}
    # Re-Set cache and of the test environment
    object <- clearTest(object)

    # Re-Set 'test.for' class slot for current execution
    object@test.for <- test.for

    # Re-Set 'test.result' class slot for current execution
    object@test.result <- NA

    RTest.cat("Test for: ",paste(paste0("'",test.for,"'"), collapse=", "),"\n\n")


    # Execute tests / call test adapters / set test status ----------------------------------------

    # Get test environment  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tests <- object@tests

    # Execute all defined tests   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    for(tf.pkg in ls(tests)) {
      # Level 1: packages

      if(is.element(tf.pkg, test.for)) {
        # If the current package should be tested, execute all defined function test
        RTest.cat("Run test(s) for '",tf.pkg,"'\n")

        for(tf.pkg.i in 1:length(tests[[tf.pkg]])) {
          # Level 2: package-iterations

          for(tf.func in names(tests[[tf.pkg]][[tf.pkg.i]])) {
            # Level 3: functions of package

            for(tf.func.i in 1:length(tests[[tf.pkg]][[tf.pkg.i]][[tf.func]])) {
              # Level 4: iteration of a function

              tf.test <- tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]]

              RTest.cat("  Test '",tf.test[["test-code"]],"'  ...")

              # Open and start testthat reporter
              reporter     <- ListReporter$new()

              set_reporter(reporter)
              reporter$start_reporter()

              # Set testthat context
              context(tf.test[["test-code"]])

              # Execute the wrapper function
              tmpExec <- NULL

              with_reporter(reporter,
                tmpExec <- execAdapter(object, tf.pkg, tf.pkg.i, tf.func, tf.func.i,
                  out.fPathPre = out.fPathPre, ...)
              #tf.test[["cache"]] <- execAdapter(object, tf.pkg, tf.pkg.i, tf.func, tf.func.i)
              #execAdapter(object, tf.test[["test-func"]], tf.test[["xpath"]])
              )

              tf.test[["cache"]]     <- tmpExec[["result"]]
              tf.test[["execresid"]] <- tmpExec[["execresid"]]
              tf.test[["test-func"]] <- tmpExec[["function_name"]]

        `%:::%` = function(pkg, fun) get(fun, envir = asNamespace(pkg),
              inherits = FALSE)
        end_context <-  "testthat" %:::% "end_context"

              # Close testthat reporter
              end_context()
              reporter$end_reporter()

              # Set test name to reporter
              #unlockBinding("test", reporter)
              #reporter$test <- paste0(tf.func)

              #print(str(ls(reporter)))

#              reporter.results <- as.data.frame(reporter$get_results())
#              reporter.failed  <- length(which(reporter.results$failed > 0))
#
#              message("\n----------\n")
#              print(str(reporter$results$as_list()))
#              message("\n----------\n")

        if(length(reporter$results$as_list())>0){

        reporter.failed <- sum(sapply(reporter$results$as_list(),
                  function(repores) length(which(sapply(repores$results,
                          function(e) !"expectation_success" %in% class(e))))))
          }else{
          reporter.failed <- 0
        }


#              stop()
#              message("-")
#              print(str(reporter$get_results()[[1]][["results"]]))
#              message("--")
#              print(attributes(reporter$get_results()[[1]][["results"]][[1]]))
#              message("---")
#              print(reporter$failed)
#              #print(reporter$results)
#              stop()

              # Set test reporter
              tf.test[["reporter"]] <- reporter

              # Set test result based on reporter status
              tf.test[["result"]] <-
                #ifelse(length(reporter$results) == 0 || !reporter$failed, "failed", "success")
                #ifelse(length(reporter$results) == 0, "failed", ifelse(reporter$failed, "failed", "success"))
                ifelse(reporter.failed == 0, "success", "failed")

              RTest.cat(" ",tf.test[["result"]],"\n")

              # Set global test result for this test case
              if(is.na(object@test.result))
                # If it has not been set, use current result
                object@test.result <- tf.test[["result"]]
              else
              if(object@test.result != "failed")
                # As soon as failed has been set once, never change it again
                object@test.result <- tf.test[["result"]]


              # Change test entry in object@test slot (i.e. in test environment)
              tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]] <- tf.test
            }
          }
        }
      }
    }

    RTest.cat("\n")

    # Return
    return(object)
  }
)



# execAdapter #####################################################################################


setGeneric("execAdapter",
  function(object, tf.pkg, tf.pkg.i, tf.func, tf.func.i, out.fPathPre = NULL, ...)
    standardGeneric("execAdapter"))

#' Execute the Adapter Function of the Test Case
#'
#' This is an abstract method definition and specifies the adapter function of the individual
#' test case adapter, which understand the test case and knows how to execute it. It has to be
#' implemented in the specialized test classes separetly for each test project.
#'
#' @name execAdapter
#' @aliases execAdapter,RTestCase-method
#'
#' @param    object           (\code{object}) The \code{\link{RTestCase-class}} object
#' @param    tf.pkg         (\code{character}) The package name of the currently executed test
#'                          function.
#' @param    tf.pkg.i       (\code{integer}) The package iteration number of the currently executed
#'                          test function.
#' @param    tf.func        (\code{character}) The function name of the currently executed test
#'                          function.
#' @param    tf.func.i      (\code{character}) The function iteration number of the currently
#'                          executed test function.
#' @param    out.fPathPre   (\code{character}) Prefix incl. path to output files generated during
#'                           tests
#' @param    ...            Additional arguments passed to the check function.
#'
#' @return  (\code{ANY}) The cached result of the executed test function.
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("execAdapter",
  signature  = "RTestCase",

  definition = function (object, tf.pkg, tf.pkg.i, tf.func, tf.func.i, out.fPathPre = NULL, ...) {

    # Initialize test case execution --------------------------------------------------------------

    # Get test environment
    tf.test      <- object@tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]]

    # Get test function
    tf.testFunc  <- tf.test[["test-func"]]

    # Get XML xpath for current test function
    tf.xpath     <- tf.test[["xpath"]]

    # Get the execution cache
    tf.execCache <- execCache(object, tf.pkg, tf.pkg.i, tf.func, tf.func.i)

    tf.cacheid <- paste0(tf.pkg,"-",tf.pkg.i,"-",tf.func,"-",tf.func.i)

    # Check test function -------------------------------------------------------------------------

  use_generic <- FALSE

    if(!exists(tf.testFunc)){
    message("Using generic test interpreter function")
    use_generic <- TRUE
  }

    #   stop("No default test function '",tf.testFunc,"' defined for '",class(object),"'!")


    # Get XML definition --------------------------------------------------------------------------
    # Use the passed xpath to get XML definition of the test item
        # Try to access the XML definition and use the return xml node set
    tf.xmlItem <- suppressWarnings(getNodeSet(object@xml.root, path = tf.xpath)[[1]])

    # In R-3.4.0 it always produces warnings due to a change in the sturcture functions :(
        # Warnings, trough stop message (be very conservative)
    #      warning = function(w) {
    #
    #        stop("Warning occured during access of XML test element '",tf.xpath,"'.")
    #    },


    # Execute test function -----------------------------------------------------------------------
    # Call the test function and pass the TC's input data and current XML function definition

    if(use_generic){
    tf.testFunc <- "'generic' test function"
    tf.result <- do.call("generic",
        list(object, object@input.data, tf.execCache, tf.xmlItem, cacheid = tf.cacheid,
            paste0(out.fPathPre,"_",tf.cacheid), package = tf.pkg, ...))
  }else{
      tf.result <- do.call(tf.testFunc,
        list(object, object@input.data, tf.execCache, tf.xmlItem, cacheid = tf.cacheid,
          paste0(out.fPathPre,"_",tf.cacheid), ...))

  }

    # Return test function result -----------------------------------------------------------------
    # The returned value will be stored in the cache for this execution

    tf.return           <- list()

    tf.return$result    <- tf.result

    tf.return$execresid <-
      if("exec-res-id" %in% names(xmlAttrs(tf.xmlItem))) xmlAttrs(tf.xmlItem)[["exec-res-id"]]
      else                                               NA

  tf.return$function_name <- tf.testFunc

    return(tf.return)
  }
)



# execCache #####################################################################################


setGeneric("execCache",
  function(object, tf.pkg, tf.pkg.i, tf.func, tf.func.i) standardGeneric("execCache"))

#' Execution Cache for Test Function
#'
#' Creates the execution cache for the the currently executed test function.
#'
#' The execution cache includes all cached results of test functions that are part of the same
#' package iteration and were executed prior to the current test function. The cached results will
#' be stored in as a list() object with the following format:\cr
#' \code{XML:                                        execCache:}\cr
#' \code{<pkg_01>}\cr
#' \code{  <func_01> ... </func_01>                    $funct_01}\cr
#' \code{  <func_01> ... </func_01>                    $funct_01[[1]]  -> result}\cr
#' \code{  <func_01> ... </func_01>                    $funct_01[[2]]  -> result}\cr
#' \code{  <func_02> ... </func_02>  <- current tf ->  $funct_02       -> result}\cr
#' \code{  <func_03> ... </func_03>                    --- not included ---}\cr
#' \code{</pkg_01>}
#' @name execCache
#' @aliases execCache,RTestCase-method
#'
#' @param    object         (\code{object}) The \code{\link{RTestCase-class}} object
#' @param    tf.pkg         (\code{character}) The package name of the currently executed test
#'                          function.
#' @param    tf.pkg.i       (\code{integer}) The package iteration number of the currently executed
#'                          test function.
#' @param    tf.func        (\code{character}) The function name of the currently executed test
#'                          function.
#' @param    tf.func.i      (\code{character}) The function iteration number of the currently
#'                          executed test function.
#'
#' @return  (\code{list})
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("execCache",
  signature  = "RTestCase",

  definition = function (object, tf.pkg, tf.pkg.i, tf.func, tf.func.i) {

    # Get current test entivornment
    tf.tests <- object@tests

    # Get index of the current test function
    tf.funcIndex <- which(tf.func == names(tf.tests[[tf.pkg]][[tf.pkg.i]]))

  if(length(tf.funcIndex)<1){
    stop(paste0("no cache created due to unkown '",tf.func,"'"))
  }

    # Initialize execution cache
    tf.execCache <- NULL

    # Create execution cache
    if(tf.funcIndex == 1 && tf.func.i == 1) {
      # If first function and first iteration, initialize empty list
      tf.execCache <- list()

    } else {
      # Else, built a list with the cached results
      tf.execCache <- list()

      # Get index of the last function to be included in the cache ...
      maxFuncUseIndex <- if(tf.func.i == 1) tf.funcIndex - 1 else tf.funcIndex

      # ... and the cache of these
      for(f in names(tf.tests[[tf.pkg]][[tf.pkg.i]])[1:maxFuncUseIndex]) {

        # Get the index of the last function interation to be included ...
        maxIterUseIndex <-
          if(f == tf.func) tf.func.i-1 else length(tf.tests[[tf.pkg]][[tf.pkg.i]][[f]])

        # ... and collect the cache
        tf.execCache[[f]] <- list()
        for(i in 1:maxIterUseIndex) {
          i.index <-
            if(is.na(tf.tests[[tf.pkg]][[tf.pkg.i]][[f]][[i]][["execresid"]])) i
            else tf.tests[[tf.pkg]][[tf.pkg.i]][[f]][[i]][["execresid"]]

          tf.execCache[[f]][[i.index]] <- tf.tests[[tf.pkg]][[tf.pkg.i]][[f]][[i]][["cache"]]
        }

#        # If only one iteration is used, not return as single list
#        if(maxIterUseIndex == 1)
#          tf.execCache[[f]] <- tf.execCache[[f]][[1]]
      }
    }

    return(tf.execCache)
  })



# getExecSummary ################################################################################


setGeneric("getExecSummary",
  function(object) standardGeneric("getExecSummary"))
#' Summary of the Last Execution of the Test Case
#'
#' @name getExecSummary
#' @aliases getExecSummary,RTestCase-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{list})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getExecSummary",
  signature  = "RTestCase",

  definition = function (object) {

    if(is.na(object@test.result))
      stop("This test case has not been executed!")

    print(ls.str(object@tests), max.level=4)

  }
)




# getExecSummary.html #############################################################################

setGeneric("getExecSummary.html",
  function(object) standardGeneric("getExecSummary.html"))

#' Create HTML Summary of the Last Execution of the Test Case
#'
#' Generates a summary of the last test case execution for the overall test report.
#' @name getExecSummary.html
#' @aliases getExecSummary.html,RTestCase-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#'
#' @return   (\code{character})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getExecSummary.html",
  signature  = "RTestCase",

  definition = function (object) {

    if(is.na(object@test.result))
      return(c())
    #stop("This test case has not been executed!")


    out <- c()
    out.append <- function(...) { out <<- c(out, paste0(...)) }


    tc.tests  <- object@tests
    tc.status <- if(length(tc.tests) == 0) "NO-TESTS" else toupper(object@test.result)


    out.append("<table width=\"100%\" class=\"TCSummary\">")
    out.append("  <tr>")
    out.append("    <th width=\"100\">TC</th>")
    out.append("    <th width=\"70\">Version</th>")
    out.append("    <th width=\"100\">Type</th>")
    out.append("    <th width=\"100\">Label</th>")
    out.append("    <th>Description</th>")
    out.append("    <th width=\"150\">No. of Testgroups</th>")
    out.append("    <th width=\"250\">Input</th>")
    out.append("    <th width=\"150\">Status</th>")
    out.append("  </tr>")

    out.append("  <tr>")
    out.append("    <td align=\"center\">")
    out.append("      <a href=\"#TC-",getID(object),"\">",getID(object),"</a>")
    out.append("    </td>")
    out.append("    <td align=\"center\">",getSynopsis(object)[["version"]],"</td>")
    out.append("    <td align=\"center\">",getType(object),"</td>")
    out.append("    <td align=\"center\">",getSynopsis(object)[["label"]],"</td>")
    out.append("    <td>",getSynopsis(object)[["short-description"]],"</td>")
    out.append("    <td align=\"center\">",length(tc.tests),"</td>")
    out.append("    <td align=\"center\">",getXMLSourceFileName(object),"</td>")
    out.append("    <td align=\"center\" class=\"",tc.status,"\">",tc.status,"</td>")
    out.append("  </tr>")


    out.append("  <tr>")
    out.append("    <td colspan=\"8\">")

    out.append("      <table width=\"100%\" class=\"TCExpecSummary\">")
    out.append("        <tr>")
    out.append("          <th width=\"100\">Package</th>")
    out.append("          <th width=\"40\">#</th>")
    out.append("          <th width=\"200\">Description</th>")
    out.append("          <th width=\"100\">Function</th>")
    out.append("          <th width=\"40\">SpecID</th>")
    out.append("          <th width=\"40\">RiskID</th>")
    out.append("          <th width=\"40\">#</th>")
    out.append("          <th>Description</th>")
    out.append("          <th width=\"200\">No. of Tests</th>")
    out.append("          <th width=\"150\">Status</th>")
    out.append("        </tr>")

    #print(object@test.for)
    for(tf.pkg in unique(object@test.for)) {
      # Level 1: packages (write only those which were tested in the last execution)

      for(tf.pkg.i in 1:length(tc.tests[[tf.pkg]])) {

        # Level 2: package-iterations
        tf.pkg.info <- FALSE

        for(tf.func in names(tc.tests[[tf.pkg]][[tf.pkg.i]])) {
          # Level 3: functions of package
          tf.func.info <- FALSE

          for(tf.func.i in 1:length(tc.tests[[tf.pkg]][[tf.pkg.i]][[tf.func]])) {
            # Level 4: iteration of a function

            tf.test <- tc.tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]]

            tf.tests  <- nrow(as.matrix(tf.test$reporter$get_results()))

            tf.status <-
              if(tf.tests == 0) "NO-TESTS"
              else toupper(tf.test$result)

            out.append("        <tr>")

            if(!tf.pkg.info) {
              out.append("          <td>")
              out.append("            <a href=\"#TC-",getID(object),"-",tf.pkg,"_",tf.pkg.i,"\">")
              out.append("              ",tf.pkg,"")
              out.append("            </a>")
              out.append("          </td>")
              out.append("          <td align=\"center\">",tf.pkg.i,"</td>")
              out.append("          <td>",tf.test[["pkg-desc"]],"</td>")
              tf.pkg.info <- TRUE
            } else {
              out.append("          <td colspan=\"3\"></td>")
            }

            if(!tf.func.info) {
              out.append("          <td>")
              out.append("            <a href=\"#TC-",getID(object),"-",tf.pkg,"_",tf.pkg.i,"-",tf.func,"\">")
              out.append("              ",tf.func,"")
              out.append("            </a>")
              out.append("          </td>")
              out.append("          <td align=\"center\">",tf.test[["specid"]],"</td>")
              out.append("          <td align=\"center\">",tf.test[["riskid"]],"</td>")
              tf.func.info <- TRUE
            } else {
              out.append("          <td></td>")
              out.append("          <td></td>")
              out.append("          <td></td>")
            }

            out.append("          <td align=\"center\">")
            out.append("            <a href=\"#TC-",getID(object),"-",tf.pkg,"_",tf.pkg.i,"-",tf.func,"_",tf.func.i,"\">")
            out.append(               tf.func.i)
            out.append("            </a>")
            out.append("          </td>")

            out.append("          <td>",tf.test[["func-desc"]],"</td>")

            out.append("          <td align=\"center\">",tf.tests,"</td>")

            out.append("          <td align=\"center\" class=\"",tf.status,"\">",tf.status,"</td>")

            out.append("        </tr>")

          }
        }
      }
    }

    out.append("      </table>")
    out.append("    </td>")
    out.append("  </tr>")
    out.append("</table>")
    out.append("</br>")

    return(out)
  }
)



# execDetails.html ################################################################################

setGeneric("getExecDetails.html",
  function(object, report.onlyFailed = FALSE) standardGeneric("getExecDetails.html"))

#' Create Detailed HTML Summary of the Last Execution of the Test Case
#'
#' Generates a detailed summary of the last test case execution for the overall test report
#' including all tested expectations.
#'
#'
#' @name getExecDetails.html
#' @aliases getExecDetails.html,RTestCase-method
#'
#' @param    object             (\code{object}) The \code{\link{RTestCase-class}} object.
#' @param    report.onlyFailed  (\code{logical}) Report only failed exceptions (TRUE) or
#'                              all exceptions (FALSE, default).
#'
#' @return   (\code{character})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getExecDetails.html",
  signature  = "RTestCase",

  definition = function (object, report.onlyFailed = FALSE) {

    tc.ID <- getID(object)


    if(is.null(object@test.result) || is.na(object@test.result))
      return(c())
    #stop("This test case has not been executed!")

    out <- c()
    out.append <- function(...) { out <<- c(out, paste0(...)) }

    tc.tests  <- object@tests
    tc.status <- if(length(tc.tests) == 0) "NO-TESTS" else toupper(object@test.result)

    out.append("<a name=\"TC-",tc.ID,"\"></a>")
    out.append("<h2>",tc.ID,"</h2>")
    out.append("")

    out.append("<div class=\"abstract\">")
    out.append("  <table width=\"100%\">")
    out.append("    <tr>")
    out.append("      <td width=\"50%\" valign=\"top\">")
    out.append("        <table width=\"100%\">")
    out.append("          <tr>")
    out.append("            <td width=\"150\"><b>Version:</b></td>")
    out.append("            <td>",getSynopsis(object)[["version"]],"</td>")
    out.append("          </tr>")
    out.append("          <tr>")
    out.append("            <td width=\"150\"><b>Type:</b></td>")
    out.append("            <td>",getType(object),"</td>")
    out.append("          </tr>")
    out.append("        </table>")
    out.append("      </td>")
    out.append("      <td width=\"50%\" valign=\"top\">")
    out.append("        <table width=\"100%\">")
    out.append("          <tr>")
    out.append("            <td width=\"150\"><b>Author(s):</b></td>")
    out.append("            <td>",getSynopsis(object)[["author"]],"</td>")
    out.append("          </tr>")
    if(getSynopsis(object)[["creation-date"]] != "") {
      out.append("          <tr>")
      out.append("            <td width=\"150\"><b>Creation Date:</b></td>")
      out.append("            <td>",getSynopsis(object)[["creation-date"]],"</td>")
      out.append("          </tr>")
    }
    out.append("        </table>")
    out.append("      </td>")
    out.append("    </tr>")

    if(getSynopsis(object)[["label"]] != "") {
      out.append("    <tr>")
      out.append("      <td colspan=\"2%\">")
      out.append("        <table width=\"100%\">")
      out.append("          <tr>")
      out.append("            <td width=\"150\" valign=\"top\"><b>Label:</b></td>")
      out.append("            <td>",getSynopsis(object)[["label"]],"</td>")
      out.append("          </tr>")
      out.append("        </table>")
      out.append("      </td>")
      out.append("    </tr>")
    }

    out.append("    <tr>")
    out.append("      <td colspan=\"2%\">")
    out.append("        <table width=\"100%\">")
    out.append("          <tr>")
    out.append("            <td width=\"150\" valign=\"top\"><b>Short Description:</b></td>")
    out.append("            <td>",getSynopsis(object)[["short-description"]],"</td>")
    out.append("          </tr>")
    out.append("        </table>")
    out.append("      </td>")
    out.append("    </tr>")

    if(length(getSynopsis(object)[["description"]]) > 0 && getSynopsis(object)[["description"]] != "") {
      out.append("    <tr>")
      out.append("      <td colspan=\"2%\">")
      out.append("        <table width=\"100%\">")
      out.append("          <tr>")
      out.append("            <td width=\"150\" valign=\"top\"><b>Description:</b></td>")
      out.append("            <td>",getSynopsis(object)[["description"]],"</td>")
      out.append("          </tr>")
      out.append("        </table>")
      out.append("      </td>")
      out.append("    </tr>")
    }

    out.append("    <tr>")
    out.append("      <td colspan=\"2%\">")
    out.append("        <table width=\"100%\">")
    out.append("          <tr>")
    out.append("            <td width=\"150\"><b>Input File:</b></td>")
    out.append("            <td>",getXMLSourcePath(object),"</td>")
    out.append("          </tr>")
    out.append("        </table>")
    out.append("      </td>")
    out.append("    </tr>")
    out.append("  </table>")
    out.append("</div>")
    out.append("<br />")
    out.append("")

    out.tempfiles <- c()
    #for(tf.pkg in object@test.for) {
    lapply(unique(object@test.for), function(tf.pkg) {
        # Level 1: packages (write only those which were tested in the last execution)

        out.append("<a name=\"TC-",tc.ID,"-",tf.pkg,"\"></a>")

        out.append("<table width=\"100%\" class=\"TC-Level1\" style=\"margin-bottom: 15px\">")
        out.append("  <tr>")
        out.append("    <td class=\"TC-Level1-left\"></td>")
        out.append("    <td class=\"TC-Level1-head\">",tf.pkg,"</td>")
        out.append("  </tr>")
        out.append("  <tr>")
        out.append("    <td class=\"TC-Level1-left\"></td>")
        out.append("    <td class=\"TC-Level1-content\">")

        #for(tf.pkg.i in 1:length(tc.tests[[tf.pkg]])) {
        lapply(1:length(tc.tests[[tf.pkg]]), function(tf.pkg.i) {
            # Level 2: package-iterations
            out.append("  <table width=\"100%\" class=\"TC-Level2\">")
            out.append("    <tr>")
            out.append("      <td class=\"TC-Level2-left\"></td>")
            out.append("      <td class=\"TC-Level2-head\">")
            out.append("        <a name=\"TC-",tc.ID,"-",tf.pkg,"_",tf.pkg.i,"\"></a>")
            out.append("        ",tf.pkg.i,"")
            out.append("      </td>")
            out.append("    </tr>")
            out.append("    <tr>")
            out.append("      <td  class=\"TC-Level2-left\"></td>")
            out.append("      <td  class=\"TC-Level2-content\">")

            #for(tf.func in names(tc.tests[[tf.pkg]][[tf.pkg.i]])) {
            lapply(names(tc.tests[[tf.pkg]][[tf.pkg.i]]), function(tf.func) {
                # Level 3: functions of package

                out.append("    <a name=\"TC-",tc.ID,"-",tf.pkg,"_",tf.pkg.i,"-",tf.func,"\"></a>")

                out.append("    <table width=\"100%\" class=\"TC-Level3\">")
                out.append("      <tr>")
                out.append("        <td class=\"TC-Level3-left\"></td>")
                out.append("        <td class=\"TC-Level3-head\">",tf.func,"</td>")
                out.append("      </tr>")
                out.append("      <tr>")
                out.append("        <td class=\"TC-Level3-left\"></td>")
                out.append("        <td class=\"TC-Level3-content\">")

                #for(tf.func.i in 1:length(tc.tests[[tf.pkg]][[tf.pkg.i]][[tf.func]])) {
                lapply(1:length(tc.tests[[tf.pkg]][[tf.pkg.i]][[tf.func]]), function(tf.func.i) {
                    # Level 4: iteration of a function

                    tf.test      <- tc.tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]]
                    tf.reporter  <- tf.test$reporter$results$as_list()
                    tf.nTests    <- length(tf.reporter)

                    tf.reporter.results <- as.data.frame(tf.test$reporter$get_results())
                    tf.nTests    <- nrow(tf.reporter.results)
                    #                    print(tf.reporter.results)


                    RTest.cat("Write details for '",tf.pkg,"::",tf.func,"'\n")

          if(tf.nTests>0){
                      tf.exceptions.passed <- sum(sapply(tf.reporter,
                          function(repores) length(which(sapply(repores$results, function(e) "expectation_success" %in% class(e))))))
                      tf.expections.failed <- sum(sapply(tf.reporter,
                          function(repores) length(which(sapply(repores$results, function(e) !"expectation_success" %in% class(e))))))

                      tf.exceptions <- tf.exceptions.passed + tf.expections.failed

          }else{
            tf.exceptions <- tf.expections.failed <-  tf.exceptions.passed  <- 0
          }

                    tf.status <- if(tf.nTests == 0) "NO-TESTS" else toupper(tf.test$result)

                    tf.desc <- c()
                    if(tf.test[["pkg-desc"]] != "") tf.desc <- c(tf.desc, tf.test[["pkg-desc"]])
                    if(tf.test[["func-desc"]] != "") tf.desc <- c(tf.desc, tf.test[["func-desc"]])
                    tf.desc <- paste(tf.desc, collapse = "; ")

                    out.append("      <table width=\"100%\" class=\"TC-Level4\">")
                    out.append("        <tr>")
                    out.append("          <td class=\"TC-Level4-left\"></td>")
                    out.append("          <td class=\"TC-Level4-head\">")
                    out.append("            <a name=\"TC-",tc.ID,"-",tf.pkg,"_",tf.pkg.i,"-",tf.func,"_",tf.func.i,"\">")
                    out.append("              #",tf.func.i)
                    out.append("            </a>")
                    out.append("          </td>")
                    out.append("        </tr>")
                    out.append("        <tr>")
                    out.append("          <td class=\"TC-Level4-left\"></td>")
                    out.append("          <td class=\"TC-Level4-content\">")

                    out.append("        <div class=\"abstract-",tf.status,"\">")
                    out.append("        <table width=\"100%\" style=\"margin-bottom: 5px;\">")
                    out.append("          <tr>")
                    out.append("            <td width=\"150\"><b>Test</b></td>")
                    out.append("            <td>",tf.test[["test-code"]],"</td>")
                    out.append("            <td width=\"150\"><b>Description</b></td>")
                    out.append("            <td>",tf.desc,"</td>")
                    out.append("            <td width=\"150\" rowspan=\"4\" align=\"center\" valign=\"top\"></td>")
                    out.append("          </tr>")
                    out.append("          <tr>")
                    out.append("            <td><b>RTest Adapter</b></td>")
                    out.append("            <td>",tf.test[["test-adapter"]],"</td>")
                    out.append("            <td><b>No. of Tests</b></td>")
                    out.append("            <td>",tf.nTests,"</td>")
                    out.append("          </tr>")
                    out.append("          <tr>")
                    out.append("            <td><b>RTest Function</b></td>")
                    out.append("            <td>",tf.test[["test-func"]],"</td>")
                    out.append("            <td><b>No. of Exceptions</b></td>")
                    out.append("            <td>",tf.exceptions," (",
                      tf.expections.failed," failed / ",tf.exceptions.passed," passed)</td>")
                    out.append("          </tr>")
                    out.append("          <tr>")
                    out.append("            <td><b>XML Test Definition</b></td>")
                    out.append("            <td colspan=\"3\">",tf.test[["xpath"]],"</td>")
                    out.append("          </tr>")
                    out.append("        </table>")
                    out.append("        </div>")

                    if(tf.nTests == 0) {
                      out.append("          <table width=\"100%\" class=\"TCSummary\">")
                      out.append("            <tr>")
                      out.append("              <td align=\"center\">NO TESTS</td>")
                      out.append("            </tr>")
                      out.append("          </table>")

                    } else {

                      #for(repores.i in 1:length(tf.reporter$results)) {
                      lapply(1:tf.nTests, function(repores.i) {
                          repores        <- tf.reporter[[repores.i]]

                          repores.results.length <- length(repores$results)

                          RTest.cat("  ",repores$test,"\n")

#                          message("\n* repores\n")
#                          print(ls(repores))
#                          message("\n***\n")

                          repores.status <-
                            ifelse(length(which(sapply(repores$results, function(e) !"expectation_success" %in% class(e)))) > 0,
                              "FAILED", "SUCCESS")
                          out.append("          <table width=\"100%\" class=\"TCSummary\" style=\"margin-bottom: 5px\">")
                          out.append("            <tr>")
                          out.append("              <th>Test</th>")
                          out.append("              <th width=\"150\">No. of Exectations</th>")
                          out.append("              <th width=\"70\">User</th>")
                          out.append("              <th width=\"70\">System</th>")
                          out.append("              <th width=\"70\">Real</th>")
                          out.append("              <th width=\"150\">Status</th>")
                          out.append("            </tr>")

                          out.append("            <tr>")
                          out.append("              <td><b>",repores$test,"</b></td>")
                          out.append("              <td align=\"center\"><b>",repores.results.length,"</b></td>")
                          out.append("              <td align=\"center\"><b>",round(repores$user, 6),"</b></td>")
                          out.append("              <td align=\"center\"><b>",round(repores$system, 6),"</b></td>")
                          out.append("              <td align=\"center\"><b>",round(repores$real, 6),"</b></td>")
                          out.append("              <td align=\"center\" class=\"",repores.status,"\"><b>",repores.status,"</b></td>")
                          out.append("            </tr>")

                          out.append("            <tr>")
                          out.append("              <td colspan=\"6\">")


                          expec.info.pre <- c()

                          infostepsize <- max(250, round(repores.results.length/20))


                          RTest.cat("    0% ")

                          i.expec         <- 1
                          i.expec.written <- 1

              lapply(repores$results, function(expec) {
                              out.test <- ""

                              if(i.expec %% 50 == 0) {
                                RTest.cat(".")
                              }

                              if(i.expec %% infostepsize == 0) {
                                gc()
                                RTest.cat("\n    ",round(i.expec/repores.results.length*100),"% ")
                              }

#                              message("\n* expec\n")
#                              print(class(expec))
#                              print(str(expec))
#                              message("\n***\n")

                              expec.status <-
                                ifelse("expectation_success" %in% class(expec), "SUCCESS", "FAILED")

                              if(report.onlyFailed == FALSE ||
                                (report.onlyFailed && expec.status == "FAILED"))
                              {
                                expec.msg <- expec$message  #ifelse(expec$passed, expec$success_msg, expec$failure_msg)

#                                message("\n* expec.msg\n")
#                                print(expec.msg)
#                                message("\n***\n")

                                expec.msg.array  <- strsplit(expec.msg,"\\n")[[1]]
                                expec.msg.others <- c()

#                                message("\n* expec.msg.array\n")
#                                print(expec.msg.array)
#                                message("\n***\n")
                                if(length(expec.msg.array) <= 1) {
                                  expec.msg.main <- expec.msg
                                  expec.msg.info <- c(
                                    "Test"        = "Test Expected Behaviour",
                                    "Success"     = "expectation_success" %in% class(expec),
                                    "Failure"     = !"expectation_success" %in% class(expec))

                                } else {
                                  expec.msg.main   <- expec.msg.array[1]

                                  if(length(expec.msg.array) > 2) {
                                    expec.msg.others <- expec.msg.array[2:(length(expec.msg.array))]
                                  } else {
                                    expec.msg.others <- expec.msg.array[length(expec.msg.array)]
                                  }
                  `%:::%` = function(pkg, fun) get(fun, envir = asNamespace(pkg),
                        inherits = FALSE)
                  fromJSON_string <-  "jsonlite" %:::% "fromJSON_string"

                  if(grepl("\\{.*\\}", expec.msg.array[length(expec.msg.array)])) {
                                    expec.msg.info <- stringr::str_extract(expec.msg.array[length(expec.msg.array)],"\\{.*\\}")
                                    expec.msg.info <- fromJSON_string(expec.msg.info)
                                    expec.msg.others <- expec.msg.others[1:(length(expec.msg.others)-1)]
                                  } else {
                                    expec.msg.info <- c(noinfo="")

                                  }
                                }

#                             print(str(expec))
#                             message("array")
#                             print(expec.msg.array)
#                             message("main")
#                             print(expec.msg.main)
#                             message("info")
#                             print(expec.msg.info)
#                             message("others")
#                             print(expec.msg.others)
#                             message("***")

                                expec.msg.info.names <- names(expec.msg.info)
                                expec.msg.info.length <- length(expec.msg.info)

                                if(!identical(expec.info.pre, expec.msg.info.names)) {

                                  if(i.expec.written > 1) {
                                    out.test <- paste0(out.test, "              </table>")
                                  }

                                  out.test <- paste0(out.test, "              <table width=\"100%\" class=\"TCExpecSummary\">")
                                  out.test <- paste0(out.test, "                <tr>")
                                  out.test <- paste0(out.test, "                  <th width=\"25\">#</th>")
                  if(expec.msg.info.length > 0 && expec.msg.info.names[1]!="noinfo") {
                    lapply(1:expec.msg.info.length, function(i.info) {
                            if(i.info == 1)
                              out.test <<- paste0(out.test, "                  <th width=\"200\">",expec.msg.info.names[i.info],"</th>")
                            else
                              out.test <<- paste0(out.test, "                  <th>",expec.msg.info.names[i.info],"</th>")
                        })
                                    #lapply(expec.msg.info.names, function(info)
                                    #    paste0(out.test, "                  <th>",info,"</th>"))
                                  } else {
                                    out.test <- paste0(out.test, "                  <th>-</th>")
                                  }
                                  out.test <- paste0(out.test, "                  <th width=\"250\">Info</th>")
                                  out.test <- paste0(out.test, "                  <th width=\"150\">Status</th>")
                                  out.test <- paste0(out.test, "                </tr>")

                                  expec.info.pre <<- expec.msg.info.names
                                }

                                out.test <- paste0(out.test, "                <tr>")
                                out.test <- paste0(out.test, "                  <td align=\"center\">",i.expec.written,"</td>")
                                if(expec.msg.info.length > 0 && expec.msg.info.names[1]!="noinfo") {
                                  lapply(expec.msg.info,
                                    function(info)
                                      out.test <<- paste0(out.test, "                  <td align=\"center\">",info,"</td>"))
                                } else {
                                  out.test <- paste0(out.test, "                  <td align=\"center\"></td>")
                                }
                                out.test <- paste0(out.test, "                  <td>")
                                if(expec.status == "SUCCESS")
                                  out.test <- paste0(out.test, "                  ")
                                else
                                  out.test <- paste0(out.test, "                  ",expec.msg.main)
                                out.test <- paste0(out.test, "                  </td>")
                                out.test <- paste0(out.test, "                  <td align=\"center\" class=\"",expec.status,"\">")
                                out.test <- paste0(out.test, "                    ",expec.status)
                                out.test <- paste0(out.test, "                  </td>")
                                out.test <- paste0(out.test, "                </tr>")

                                if(expec.status == "FAILED" && length(expec.msg.others) > 0) {
                                  out.test <- paste0(out.test, "                <tr>")
                                  out.test <- paste0(out.test, "                  <td></td>")
                                  if(expec.msg.info.length > 0) {
                                    lapply(expec.msg.info,
                                      function(info)
                                        out.test <<- paste0(out.test, "                  <td></td>"))
                                  } else {
                                    out.test <- paste0(out.test, "                  <td></td>")
                                  }
                                  out.test <- paste0(out.test, "                  <td class=\"FAILURE-msg\">")
                                  out.test <- paste0(out.test, "                    ",paste0(expec.msg.others, collapse="<br/>"))
                                  out.test <- paste0(out.test, "                  </td>")
                                  out.test <- paste0(out.test, "                  <td align=\"center\" class=\"",expec.status,"\"></td>")
                                  out.test <- paste0(out.test, "                </tr>")
                                }


                                out.append(out.test)

                                i.expec.written <<- i.expec.written + 1
                              }

                              i.expec <<- i.expec + 1

                            })
                          # End of lapply(repores$results)
                          out.append("                </table>")

                          out.append("              </td>")
                          out.append("            </tr>")
                          out.append("          </table>")

                          RTest.cat("\n")
                        })
                    }

                    out.append("        </td>")
                    out.append("      </tr>")
                    out.append("    </table>")
                    out.append("    <a href=\"#pagetop\" class=\"backlink\">back to page top</a>")
                  })

                out.append("        </td>")
                out.append("      </tr>")
                out.append("    </table>")
              })

            out.append("      </td>")
            out.append("    </tr>")
            out.append("  </table>")

          })

        out.append("    </td>")
        out.append("  </tr>")
        out.append("</table>")
      })

    RTest.cat("\n")

    return(out)
  }
)




# getRTMInfos ##########################################################################################


setGeneric("getRTMInfos",
  function(object, test.for = NULL, cols = c("Version", "Type", "sDesc"))
    standardGeneric("getRTMInfos"))

#' Create RTM from executed test cases
#'
#' Creates the requirement traceability matrix for this test case.
#'
#' @name getRTMInfos
#' @aliases getRTMInfos,RTestCase-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object.
#' @param    test.for        (\code{character}) Vector with package names that should be tested or
#'                             NULL to get all packages available from the test case.
#' @param    cols            (\code{character}) Defines which information columns are shown for
#'                             the TCs in the RTM. The ordering in the vector is also the ordering
#'                             in the output ('Version','Type','Label','sDesc','lDesc').
#'
#' @return   (\code{list}) Listing of test case details (ID, Version, Type, sDesc) and
#'           assigned specification IDs (SpecIDs) and risk IDs (RiskIDs).
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getRTMInfos",
  signature  = "RTestCase",

  definition = function (object, test.for = NULL, cols = c("Version", "Type", "sDesc")) {

    if(is.null(test.for))
      test.for <- ls(object@tests)


    tc.tests  <- object@tests
    tc.status <- if(length(tc.tests) == 0) "NO-TESTS" else toupper(object@test.result)


    cols.contents <- list(
      Version = getSynopsis(object)[["version"]],
      Type    = getType(object),
      sDesc   = getSynopsis(object)[["short-description"]],
      lDesc   = getSynopsis(object)[["description"]],
      Label   = getSynopsis(object)[["label"]])

    RTM <- list(
      ID      = getID(object)
    )

    if(!is.null(cols) && length(cols) > 0) {
      for(col in cols) {
        RTM[[col]] <- cols.contents[[col]]
      }
    }

    tf.SpecIDs <- tf.RiskIDs <- c()

    for(tf.pkg in test.for) {
      # Level 1: packages (write only those which were tested in the last execution)

      tf.pkg.SpecIDs <- tf.pkg.RiskIDs <- c()

      for(tf.pkg.i in 1:length(tc.tests[[tf.pkg]])) {

        # Level 2: package-iterations
        tf.pkg.info <- FALSE

        for(tf.func in names(tc.tests[[tf.pkg]][[tf.pkg.i]])) {
          # Level 3: functions of package
          tf.func.info <- FALSE

          for(tf.func.i in 1:length(tc.tests[[tf.pkg]][[tf.pkg.i]][[tf.func]])) {
            # Level 4: iteration of a function

            tf.test <- tc.tests[[tf.pkg]][[tf.pkg.i]][[tf.func]][[tf.func.i]]

            specid <- tf.test[["specid"]]
            specid <- unlist(strsplit(specid, ","))
            specid <- gsub("\\ ","",specid)

            riskid <- tf.test[["riskid"]]
            riskid <- unlist(strsplit(riskid, ","))
            riskid <- gsub("\\ ","",riskid)

            tf.pkg.SpecIDs <- unique(c(tf.pkg.SpecIDs, specid))
            tf.pkg.RiskIDs <- unique(c(tf.pkg.RiskIDs, riskid))

          }
        }
      }

      tf.pkg.SpecIDs <- paste(tf.pkg.SpecIDs, collapse = ", ")
      tf.pkg.RiskIDs <- paste(tf.pkg.RiskIDs, collapse = ", ")

      if(length(test.for) > 1) {
        tf.SpecIDs <- c(tf.SpecIDs,paste0(tf.pkg,": ",tf.pkg.SpecIDs))
        tf.RiskIDs <- c(tf.RiskIDs,paste0(tf.pkg,": ",tf.pkg.RiskIDs))
      }else{
    tf.SpecIDs <- c(tf.SpecIDs,tf.pkg.SpecIDs)
    tf.RiskIDs <- c(tf.RiskIDs,tf.pkg.RiskIDs)
    }


    }

    RTM$SpecIDs <- paste(tf.SpecIDs, collapse = "; ")
    RTM$RiskIDs <- paste(tf.RiskIDs, collapse = "; ")

    return(RTM)
  }
)

