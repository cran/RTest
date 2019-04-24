###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# RTest Uitility Functions                                                                        #
#                                                                                                 #
# Date:           25 - Jan - 2016                                                                 #
# Author:         Matthias Pfeifer (matthias.pfeifer@roche.com)                                   #
#                                                                                                 #
###################################################################################################

#' Function to generally execute a Test Case collection
#' 
#' @param testcase.directory (\code{character}) Location of the Test Case XML files
#' @param f.pattern (\code{character}) An additional pattern to just search
#' 	for specific files with certain names
#' @param project.name (\code{character}) Name of the project mentioned in all
#'   cover pages
#' @param project.details (\code{character}) Description of the project
#' @param project.tester (\code{character}) Name of the Test executer
#' @param report.file (\code{character}) Output file where to store the
#'   report
#' @param ... Additional arguments handed over to the \link{exec,RTestCollection-method} 
#' 	method
#' 
#' @return No return value, but the command line output will show where to fund the
#'   report. Using the additional argument \code{open=TRUE} will open the report
#'   directly after execution
#' 
#' @examples 
#' 
#' directory_with_tests <- list.dirs(find.package('RTest'),recursive=TRUE) %>% 
#' 				grep(pattern="xml-templates",value=TRUE)
#' 
#' RTest::RTest.execute(
#'  testcase.directory = directory_with_tests[1],
#' 	open=FALSE,
#'  f.pattern = "RTest_TC-generic.xml"
#' )
#' 
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
RTest.execute <- function(
		testcase.directory = list.dirs(find.package("RTest"),recursive=T) %>% 
				grep(pattern="xml-templates",value=T),
		f.pattern = "*.xml",
		project.name = "RTest Execution",
		project.details = "Example test exectuion",
		project.tester = "Example tester",
		report.file =  tempfile(fileext=".html"),
		...
){
	options("RTest_verbose" = TRUE)
	
    # Create test collection
	testCollection <- new(
			"RTestCollection", 
			project.name    = project.name, 
			project.details = project.details,
			tester          = project.tester,
			test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

	# Import TCs
	testCollection <- importTCsFromDir(testCollection,
			xml.dPath = testcase.directory,
			f.pattern	= f.pattern)
	
    # Execute test cases
	testCollection <- exec(testCollection, out.fPath = report.file, ...)
	
}

# RTest.cat #######################################################################################

#' Write Text To Console
#' 
#' A message is written to the console if the option \code{RunitTestSuite_verbose} is set TRUE.
#'
#' @param    ...          Passed directly to \code{\link[base]{paste0}}.
#' 
#' @seealso \code{\link{paste0}}, \code{\link[base]{cat}}
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
RTest.cat <- function(...) {
  if(getOption("RTest_verbose", default = FALSE))
    cat(paste0(paste0(...)))
}



# Rtest.print #####################################################################################

#' Print Text To Console
#' 
#' The arguments are printed if the option \code{RTest_verbose} is set TRUE.
#'
#' @param    ...          Passed directly to \code{\link[base]{print}}.
#' 
#' @seealso \code{\link[base]{print}}
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
RTest.print <- function(...) {
  if(getOption("RTest_verbose", default = FALSE))
    print(...)
}




# setTestMethod ###################################################################################

#' Create and Save a Default Test Method For a Test Case Adapter
#' 
#' This function creates and saves a default test method for a RTest adapter with predefined
#' arguments (see 'Details'). It will automatically create the generic as well as the method 
#' for the test case class using S4 style.
#' 
#' This function is a wrapper for standardized and simplified creation of S4-syle test case
#' adapters required for RTest. The passed function has to be assigned to the corresponding test 
#' case class, whichinherits \code{\link{RTestCollection-class}}) and represents the adapter for 
#' the respecive implementation of a test case type (i.e. XSD scheme). The passed function 
#' definition has to be in the following format with the following parameters.
#' \code{function(object, inputData, execCache, xmlDef, ... ) {
#'   # implementation goes here
#' }}
#' \itemize{
#'   \item{\code{object}}{(\code{object}) The \code{\link{RTestCase-class}} object.}
#'   \item{\code{inputData}}{(\code{list}) List with all input data provided in the XML test case.}
#'   \item{\code{execCache}}{(\code{list}) List with cached results determined in the predecessor 
#'     test functions of a test case execution.}
#'   \item{\code{xmlDef}}{(\code{XMLNode}) An object of class \code{XMLNode}, which defines the 
#'     current test function.}
#'   \item{\code{...}}{Additional parameters passed to the individual test functions.}
#' }
#'
#' @param    f	              (\code{character}) The name of the function.        
#' @param    signature       (\code{character}) The name of the corresponding test case adapter 
#'                           (i.e. the name of the class, which inherits 'RTestCase' and implements
#'                           all functions associated to a test case type and specifications).    
#' @param    definition      (\code{function}) The method, which will be called if the signature 
#'                           matches the definitions (see 'Details').
#' 
#' @param    where           (\code{env}) An environment where to set up the Method
#' 
#' @seealso  \code{\link[methods]{setGeneric}}, \code{\link[methods]{setMethod}}
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setTestMethod <- function(
  f, 
  signature  = "RTestCase", 
  definition = function(object, inputData, execCache, xmlDef, ... ) NULL,
  where = parent.frame()
  ) {
  
  # Register a new generic function, if it hasn't been done so far
  if(!isGeneric(f, where = where)) {
    setGeneric(
      name = f, 
      def  = definition,
      where = where
    )
  }
  
  # Register the passed method for the signature
  setMethod(f, 
    signature  = signature,
    definition = definition, where = where
  )  
}



# png2base64 ######################################################################################

#' Converts a PNG File Into a Base64 String for Using IT as Inline Image in HTML Files
#' 
#' This function creates a base64 string of a PNG  (e.g. png) directly into HTML via
#' the data function.
#' 
#' The input file is read and a base64 string encoded The returned file is the value of the 
#' \code{img} attribute \code{src}.
#' 
#' @param    file           (\code{character}) Path to PNG image.
#' @param    img.returnAsTag (\code{boolean})	TRUE, img is returned as HTML \code{img}-tag; FALSE,
#'                          raw image base64 content is returned.
#' @param    img.title      (\code{character})	Title of the HTML \code{img}-tag.
#' @param    img.width      (\code{character})	With for HTML \code{img}-tag.
#' 
#' @return   See paramter \code{img.returnAsTag}.
#' 
#' @author Matthias Pfeifer <matthias.pfeifer@@roche.com>
png2base64 <- function(file, img.returnAsTag = FALSE, img.title = "image", img.width = NULL) {
  tf <- tempfile()
  on.exit(unlink(tf))
  
  base64::encode(file, tf)
  
  src <- sprintf("data:image/png;base64,%s", paste(readLines(tf), collapse = ""))
  
  if(img.returnAsTag == TRUE) {
    if(!is.null(img.width))
      return(sprintf("<img src='%s' alt='%s' width='%s' />", src, img.title, img.width))
    else
      return(sprintf("<img src='%s' alt='%s' />", src, img.title))
  } else {
    return(src)
  }
}



# systemInfo.packages  ############################################################################

#' Summarize System Packages
#' 
#' This method creates a tabular listing of the packages, which are currently loaded and available
#' to the system
#'
#' @param    which           (\code{character}) Specifies, which packages to display. One of 
#'                           '\code{basePkgs}', '\code{loadedOnly}' or '\code{otherPkgs}' (for 
#'                           details see \code{\link[utils]{sessionInfo}}.
#' 
#' @return   (\code{data.frame}) A table containing the packages' names, versions and build dates.
#' 
#' @seealso  \code{\link[utils]{sessionInfo}}
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
systemInfo.packages <- function(which = "loadedOnly") {
  
  if(!which %in% c("basePkgs", "loadedOnly", "otherPkgs"))
    stop("Parameter 'which' not valid. Check method usage!")
  
  session.packages <- sessionInfo()[[which]]
  
  res <- data.frame(
    Package = rep("",length(session.packages)),
    Version = rep("",length(session.packages)),
    Build   = rep("",length(session.packages)),
    md5   = rep("",length(session.packages)),
    stringsAsFactors  = FALSE
  )
  
  for(i in 1:length(session.packages)){
    
    if(which == "basePkgs") {
      res[i,1] <- session.packages[i]
      res[i,2] <- res[i,3] <- res[i,4]<- NA
    } else {
      res[i,1] <- names(session.packages)[i]
      
      
      str <- ifelse(
        is.null(session.packages[[i]]$Date),
        ifelse(!is.null(session.packages[[i]]$Built),format(as.Date(strsplit(session.packages[[i]]$Built, ";")[[1]][3])), ""),
        session.packages[[i]]$Date)
      
      res[i,2] <- session.packages[[i]]$Version
      res[i,3] <- str
	  res[i,4] <- tryCatch({
				  
			  package_md5(session.packages[[i]]$Package)},error=function(e)"installed binary"
				)
    }
  }
  
  return(res)
}

#' Function to derive an md5 hash of a package in a current session
#' 
#' @param package (\code{character}) Name of the package to be scanned
#' 
#' @return \code{Namespace: } md5 hash or \code{tar: } md5 hash. Dependent
#' on whether the package can be loaded out of its current Namespace (Namespace) or
#' whether the installed binaries have to be scanned (tar) a different
#' md5 hash is given. Namespace packages where normally loaded using \code{library}
#' calls
#' 
#' @author Sebastian Wolf \email{sebastian.wolf.sw1@@roche.com}
#' 
#' @examples 
#' 
#' package_md5("testthat")
#' 
#' @export 
package_md5 <- function(package){
	
	in_env <- new.env()
	
	tryCatch({
				allfuns <- ls(envir = getNamespace(package))
				
				assign("outfile_name", file.path(tempdir(),paste0(package,'_R.txt')),
						envir=in_env)
				
				for(f in allfuns[1:2]){
					args <- capture.output(print(args(f)))[1]
					body <- paste(capture.output(print(body(f))), collapse = "\n")
					writeLines(sprintf("%s <- %s\n%s\n\n", f, args, body),get("outfile_name",in_env))
				}
				
				assign("what", "Namespace: ",envir=in_env)
				
			},error=function(e){
				assign("outfile_name", file.path(tempdir(),paste0(package,'_R.tar')),
						envir=in_env)
				
				tar(tarfile = get("outfile_name",in_env), files = file.path(find.package(package),"R"))
				
				assign("what", "tar(Package/R): ",envir=in_env)
				
			})
		
	out <- paste0(
			get("what",in_env),
			tools::md5sum(get("outfile_name",in_env)))
	
	file.remove(get("outfile_name",in_env))
	
	return(out)
	
	
}

# systemInfo.RInst ################################################################################

#' Summarize R Version Information 
#' 
#' This method creates a tabular listing of current R version information.
#'
#' @return   (\code{character}) A character vector containing the R version information.
#' 
#' @seealso  \code{\link[utils]{sessionInfo}}
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
systemInfo.RInst <- function() {
  
  session.R <- unlist(sessionInfo()$R.version)
  
  res <- session.R
  names(res) <- names(session.R)
  
  return(res)
}



# systemInfo.host  ################################################################################

#' Summarize Host System Information 
#' 
#' This method creates a tabular listing of current host system.
#'
#' @return   (\code{data.frame}) A character vector containing the R version information.
#' 
#' @seealso  \code{\link[base]{Sys.info}}
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
systemInfo.host <- function() {
  
  session.R <- Sys.info()
  
  res <- session.R
  names(res) <- names(session.R)
  
  return(res)
}



# normalizeDate ###################################################################################

#' Reformat a Date String
#' 
#' This method reformats a date string for R packages as these can be very heterogenous defined
#' in the DESCRIPTION files of packages.
#'
#' @param    d               (\code{character}) The date to be converted.
#' @param    asDate          (\code{boolean}) Return as R "\code{Date}" representation (TRUE) or 
#'                           as character string (FALSE).
#' @param    months          (\code{object}) The name of the year's month.
#' 
#' @return   (see Parameter \code{asDate}) Reformatted date.
#' 
#' @seealso  \code{\link{as.Date}}
#' 
#' @examples
#' 
#' new_date <- normalizeDate("15.September.2018",FALSE)
#' 
#' stopifnot(new_date=="15.09.2018")
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
normalizeDate <- function(
  d, 
  asDate = TRUE, 
  months = c("jan"="january", "feb"="februrary", "mar"="march", "apr"="april", 
    "may"="may", "jun"="june", "jul"="july", "aug"="august", "sep"="september", "oct"="october", 
    "nov"="november", "dec"="december"))
{
  
  d <- tolower(d)
  
  if(any(sapply(months, grepl, d))) {
    # If date has complete month as string, replace it
    
    # Find month
    month <- which(sapply(months, grepl, d))
    
    # Add leading '0' to 1-digit-month
    if(month < 10)
      month <- paste("0", month, sep="")
    
    # Convert month
    d <- sub(months[as.numeric(month)], month, d)
    
  } else if(any(sapply(names(months), grepl, d))) {
    # Else If date has abbreviation of month as string, replace it
    
    # Find month
    month <- which(sapply(names(months), grepl, d))
    
    # Add leading '0' to 1-digit-month
    if(month < 10)
      month <- paste("0", month, sep="")
    
    # Convert month
    d <- sub(names(months)[as.numeric(month)], month, d)
  }
  if(asDate){
	  if(as.numeric(R.Version()$major)+0.1*as.numeric(R.Version()$minor)<3.5){
		  return(as.Date(d,format = c("%d.%m.%Y"))) 
	  }else{
		  return(as.Date(d,tryFormats = c("%d.%m.%Y","%Y-%m-%d"))) 
	  }#R.Version
  }else {
	  return(d)
  }#else
}

#' function to make strings xml and html compatible
#' 
#' @param input_string (\code{character}) A simple character string
#' 
#' @return (\code{character}) The same string but incompatible characters
#' are exchanged by HTML Name characters such as &amp; for &
#' 
#' @examples
#' 
#' input_string <- "<5"
#' 
#' stopifnot(htmlify_string(input_string)=="&lt;5")
#' 
#' @author Sebastian Wolf <sebastian.wolf.sw1@@roche.com>
htmlify_string <- function(input_string){
#	print(input_string)
	hashlist <- list(
			"\"" = "&quot;",
			"&"  = "&amp;",
			"<"  = "&lt;",
			">"  = "&gt;",
			"\u00DF"  = "&szlig;",
			"\\" = "&#92;"
	)
	
	return(sapply(strsplit(as.character(input_string), split=character(0)), 
					function(x){ 
						paste(ifelse(x %in% names(hashlist),hashlist[x],x), collapse="")}))
	
	
}

#' function to derive external package functionalities
#' 
#' @param x \code{character} package :: function string
#' 
#' @return functionality of the wanted function
#' 
#' found at 
#' https://stackoverflow.com/questions/38983179/do-call-a-function-in-r-without-loading-the-package
getfun<-function(x) {
	if(length(grep("::", x))>0) {
		parts<-strsplit(x, "::")[[1]]
		getExportedValue(parts[1], parts[2])
	} else {
		x
	}
}

