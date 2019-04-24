###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# Test Functions For 'RTestCase'                                                                  #
#                                                                                                 #
# Date:           08 - Mar - 2016                                                                 #
# Author:         Matthias Pfeifer (matthias.pfeifer@roche.com)                                   #
#                                                                                                 #
###################################################################################################


#' A simple function to Test the RTest package
#'
#' @param dat (\code{data.frame}) Any simple number dataframe with minimum one column
#' @param mult (\code{numeric}) Any simple number
#'
#' @return A Table with the number vector + a sum of the vector multiplied by \code{mult}
#'
#' @export
#'
#' @examples
#'
#' dat <- data.frame(x=c(1,1))
#' mult <- 1
#' test_fun(dat,mult)
#'
#' @author Sebastian Wolf
#'
test_fun <- function(dat, mult) {
  cbind(dat, "sum" = apply(dat, 1, sum)*mult)
}


#' Test Function For Testing Function 'RTest::test_returnValue_data.frame_cellbycell'
#'
#' @name     test.RTest.funct_01
#' @aliases  test.RTest.funct_01,RTestCase-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object
#' @param inputData (\code{list}) List of input values
#' @param execCache (\code{list}) list of already executed tests and their return values
#' @param xmlDef (\code{xmlNode}) xmlNode of the Test case
#' @param ... additional values can be given from \code{execAdapter}
#'
#' @return   (\code{list})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @author   Sebastian Wolf
setTestMethod(
    "test.RTest.funct_01",
    signature  = "RTestCase",
    definition = function(object, inputData, execCache, xmlDef, ...) {
      # Read parameters
      mult <- xmlReadData_variable(xmlDef[["params"]][["mult"]])

      # Calculate result
      result <- test_execution(
          what        = "test_fun",
          args        = list(dat=inputData[[1]], mult=mult),
          xmlTestSpec = xmlDef[["testspec"]][["execution"]])
      # Read reference
      reference <- xmlReadData_data.frame(xmlDef[["reference"]])

      # Execute test
      if(!is.null(xmlDef[["testspec"]][["return-value"]]))
        test_returnValue_data.frame_cellbycell(
            result,
            reference,
            xmlDef[["testspec"]][["return-value"]]
        )


      # Return result (will be cached)
      return(result)
    }
)

#' Read arguments from RTest 'param'-XML Node
#'
#' @details
#'
#'  This function will read in all parameters exept the one named "RTestData_input_data"
#'   into a list by using \link{xmlReadData_to_list}. The parameter "RTestData_input_data" is
#'  written into an additional item of the list. The name of this item is given
#'  by the "param" attribute of the XMLNode "RTestData_input_data". The value is given
#'  by the list item of the list "input_data" that can be found by the "name" attribute
#'  of the XMLNode "RTestData_input_data".
#'
#' @param parameters_xml_definition (\code{XMLNode}) An XML Node that has elements of
#'    kind RTestData_variable, RTestData_image, RTestData_vector, RTestData_list,
#'    RTestData_data.frame in it. Please see the \code{RTest.xsd} to be found in
#'   \code{file.path(find.package("RTest")/"xsd/RTest.xsd")}
#'
#'
#' @param input_data (\code{list}) A named list of values of kind data.frame, character,
#'   numeric or list of those
#'
#' @return A named list. For the name of the RTestData_input_data element, please see
#'   details.
#'
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
arguments_creator <- function(parameters_xml_definition, input_data=NULL){
  all_arguments <- list()

  if("RTestData_input_data" %in% names(parameters_xml_definition)){
    if(!is.null(input_data)){
      all_arguments[[
          xmlAttrs(parameters_xml_definition[["RTestData_input_data"]])["param"]
      ]] <- input_data[[
              xmlAttrs(parameters_xml_definition[["RTestData_input_data"]])["name"]
          ]]
      # Delete the input data parameters
      parameters_xml_definition[["RTestData_input_data"]] <- NULL
    }else{
      stop("Cannot read RTestData_input_data if no innput-data node was provided.")
    }
  }

  all_arguments <- append(
      all_arguments,
      suppressWarnings(xmlReadData_to_list(parameters_xml_definition))
  )#append
  return(all_arguments)
}

#' Find out if a function is available
#'
#' This function checks if a function name or method name is available in the global
#' namespace or the desired package namespace
#'
#' @param function_name (\code{character}) The name of the function to look up
#'
#' @param package  (\code{character}) The name of the package where this function
#'   might be hidden (not exported)
#'
#' @return "global" if it is available, "package" if it's available within the package
#'  or an Error if it is not available at all.
#'
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
get_existence_of_fun <- function(function_name, package){
  tryCatch({

        get(function_name,envir = .GlobalEnv)
        return("global")
      },
      error = function(e){
        tryCatch({

              get(function_name,envir = asNamespace(package))
              return("package")
            },

            error=function(e){
              stop(paste0("Function '",function_name,
                      "' is neither defined in .GlobaEnv nor in '",
                      package,
                      "' package."))
            }

        )
      })
}


setGeneric("generic",
    function(object, inputData, execCache, xmlDef, package, ...) standardGeneric("generic"))

#' Generic test adapter Method
#'
#' @name     generic
#' @aliases  generic,RTestCase-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object
#' @param inputData (\code{list}) List of input values
#' @param execCache (\code{list}) list of already executed tests and their return values
#' @param xmlDef (\code{xmlNode}) xmlNode of the Test case
#' @param package (\code{character}) Name of the package to be tested
#' @param ... additional values can be given from \code{execAdapter}
#'
#' @return   (\code{list})
#'
#' @seealso  \code{\link{RTestCase-class}}
#'
#' @export
#'
#' @examples
#' options("RTest_verbose" = TRUE)
#'
#' testCollection <- new("RTestCollection",
#'     project.name    = "RTest Vignette",
#'     project.details = "Example test exectuion",
#'     tester          = "Example tester",
#'     test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
#'
#' TCDir <- paste0(find.package("RTest"),"/xml-templates")
#'
#' testCollection <- RTest::importTCsFromDir(testCollection,
#'     xml.dPath = TCDir,f.pattern  = "RTest_TC-02.xml")
#'
#'
#' outf <- tempfile(fileext=".html")
#'
#' funct_02 <<- function(data, mult) {   cbind(data, "sum" = apply(data, 1, sum)*mult) }
#' environment(funct_02) <- asNamespace('RTest')
#'
#' testCollection <- RTest::exec(testCollection, out.fPath = outf, open=FALSE)
#'
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
setTestMethod(
    "generic",
    signature = "RTestCase",
    definition = function(object, inputData, execCache, xmlDef, package=NULL, ...) {

    arguments_call <- list()

    if(!is.null(xmlDef[["params"]])){
  ### ------ Derive call arguments ------ ######
      arguments_call <- xmlDef[["params"]] %>% arguments_creator(input_data = inputData)
    }

  ### ------ Create execution xmlTestSpec ------ ######
      # Double check that testSpec is existing
      xmlTestSpec <- xmlDef[["testspec"]][["execution"]]
      if(is.null(xmlTestSpec)){
        xmlTestSpec <- xmlNode("execution",attrs=list("execution-type"="silent"))
      }

  ### ------ Move function to .GlobalEnv if not already done ------ ######
      # Check if the function to test
      # was already exported to the global Namespace
      fun_existence <- get_existence_of_fun(
          function_name = xmlName(xmlDef),
          package = package
          )

      # If the function was not exported, assign it to the GLobal Namepsace
      if(fun_existence!="global" ){
        function_name <- paste0(package,":::",xmlName(xmlDef))

      }else{
        function_name <- xmlName(xmlDef)
      }
  ### ------ Text Execution ------ ######
      result <- test_execution(
          what = function_name,
          args = arguments_call,
          xmlTestSpec = xmlTestSpec
          )

  ### ------ Derive reference ------ ######

      reference <- xmlReadData_to_list(
          xmlNode("reference_data",xmlDef[["reference"]]))[["reference"]]

      if(!is.null(reference)){

  ### ------ Result vs Reference ----- #####
        # - Special case - Image
        if(!is.null(xmlAttrs(xmlDef[["reference"]])["image"]) &&
            !is.na(xmlAttrs(xmlDef[["reference"]])["image"])){
          if(!is.null(xmlAttrs(xmlDef[["reference"]])["exec_value"]) &&
              !is.na(xmlAttrs(xmlDef[["reference"]])["exec_value"])){
            reference <- eval(parse(text=reference))
          }
          test_returnValue_image(result, reference,  xmlDef[["testspec"]][["return-value"]],
              )
        # - All other cases -
        }else{
          test_returnValue_any(result, reference,  xmlDef[["testspec"]][["return-value"]]
              )
        }
      }

      return(result)
    })
