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


# test.RTest.test_returnValue_data.frame_cellbycell ###############################################

#' Test Function For Testing Function 'RTest::test_returnValue_data.frame_cellbycell'
#' 
#' @name     test.RTest.test_returnValue_data.frame_cellbycell
#' @aliases  test.RTest.test_returnValue_data.frame_cellbycell,RTestCase-method
#' 
#' @param    object          (\code{object}) The \code{\link{RTestCase-class}} object
#' 
#' @return   (\code{list})
#' 
#' @seealso  \code{\link{RTestCase-class}}
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setTestMethod(
  "test.RTest.test_returnValue_data.frame_cellbycell", 
  signature  = "RTestCase",
  definition = function(object, inputData, execCache, xmlDef, ...) {

    # Get function parameters ---------------------------------------------------------------------

    # ... not required ...
        
    
    # Get reference for test case -----------------------------------------------------------------
    
    reference <- xmlReadData_data.frame(xmlDef[["reference"]])
      

    # Calculate Result ----------------------------------------------------------------------------
    
    result <- inputData
    

    # Execute test --------------------------------------------------------------------------------
    
    test_returnValue_data.frame_cellbycell(result, reference, xmlDef[["testspec"]])    
    
    
    # Print some summary --------------------------------------------------------------------------
    
    RTest.cat("\n\n")
    RTest.cat("**************************\n")
    RTest.cat("    test.Pkg_1.funct_01   \n")
    
    RTest.cat("inputData (passed by function call)\n")
    RTest.print(inputData)
    
    RTest.cat("execCache (passed by function call)\n")
    RTest.print(execCache)
    
    RTest.cat("reference\n")
    RTest.print(reference)
    
    RTest.cat("result\n")
    RTest.print(result)
    
    RTest.cat("\n**************************\n")
    RTest.cat("\n\n")
    

    # Return result (will be cached) --------------------------------------------------------------
    
    return(result)
  }
)

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