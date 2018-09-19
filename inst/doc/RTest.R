## ---- eval = TRUE, echo = TRUE-------------------------------------------

library(RTest)


## ---- eval = TRUE, echo = TRUE-------------------------------------------

## Define the functions to be tested
test_fun <- function(dat, mult) {
  cbind(dat, "sum" = apply(dat, 1, sum)*mult)
}

assign("test_fun", test_fun, envir = .GlobalEnv)


# Create test adapter
setClass(
		Class          = "TestPackageTestCase",
		representation = representation(),
		prototype      = list(), 
		contains       = "RTestCase",
		where = .GlobalEnv
)


setTestMethod(
		"test.Pkg_1.funct_01", 
		signature  = "TestPackageTestCase",
		definition = function(object, inputData, execCache, xmlDef, ...) {
			
			# Read parameters
			mult <- xmlReadData_variable(xmlDef[["params"]][["mult"]])
			
			
			# Calculate result
			result <- test_execution(
					what        = test_fun,
					args        = list(c(inputData[[1]], mult)),
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
		},
		where = .GlobalEnv
)



## ---- eval = FALSE, echo = TRUE, message = FALSE-------------------------
#  
#  
#  # Create test collection
#  testCollection <- new("RTestCollection",
#  		project.name    = "RTest Vignette",
#  		project.details = "Example test exectuion",
#  		tester          = "Example tester",
#  		test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
#  
#  
#  # Import TCs
#  TCDir <- paste0(find.package("RTest"),"/xml-templates")
#  
#  testCollection <- importTCsFromDir(testCollection,
#  		xml.dPath = TCDir)
#  
#  
#  # Execute test cases
#  testCollection <- exec(testCollection)
#  
#  
#  # Write test report
#  outf <- tempfile(fileext=".html")
#  writeExecSummary.html(testCollection, out.fPath = outf)
#  
#  cat("Output written to ",outf,"", sep = "")

