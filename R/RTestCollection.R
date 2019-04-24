###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# Class Definition of 'RTestCollection'                                                           #
#                                                                                                 #
# Date:           25 - Jan - 2016                                                                 #
# Author:         Matthias Pfeifer (matthias.pfeifer@roche.com)                                   #
#                                                                                                 #
###################################################################################################



#' Class Definition 'RTestCollection'
#'
#' @slot     project.name  (\code{character}) Name of the project.
#' @slot     project.details (\code{character}) Further details of the project.
#' @slot     tester        (\code{character}) Name of the tester.
#' @slot     test.start    (\code{character}) Start date of the testing project.
#' @slot     collection    (\code{list}) The test case collection.
#'
#' @name RTestCollection-class
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setClass(
    Class          = "RTestCollection",

    representation = representation(
        project.name    = "character",
        project.details = "character",
        tester          = "character",
        test.start      = "character",
        collection      = "list"
    ),

    prototype      = list(
        project.name    = "RTestCollection",
        project.details = "n/a",
        tester          = "n/a",
        test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        collection      = list()
    ),

    validity       = function(object) {
      return(TRUE)
    }
)

#' Constructor for 'RTestCollection'
#'
#' @param project.name - Name of the test campaign
#' @param project.details - Details of the test campaign
#' @param project.tester - Name of the tester
#'
#' @return \link{RTestCollection-class} object
#'
#' @examples
#'
#' obj <- RTestCollection()
#' show(obj)
#'
#' @export
#' @author Sebastian Wolf
RTestCollection <- function(
    project.name = "RTest Execution",
    project.details = "Example test exectuion",
    project.tester = "Example tester"
    ){

    new(
    "RTestCollection",
    project.name    = project.name,
    project.details = project.details,
    tester          = project.tester,
    test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )

}

# exec ############################################################################################


setGeneric("exec",
    function(object, test.TCs = NULL, test.for = NULL, out.fPath = NULL, open = TRUE, ...)
      standardGeneric("exec"))

#' Tests imported Test Cases
#'
#' @name exec
#' @export
#' @aliases exec,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object.
#' @param    test.TCs        (\code{character}) Vector with the TCs to be executed or NULL if all
#'                           all TCs of the collection should be tested.
#' @param    test.for        (\code{vector(character)}) Specification for which elements to test,
#'                           NULL for test all elements
#' @param    out.fPath       (\code{character}) Path to output file.
#' @param    open            (\code{logical}) Should the generated file be opened (TRUE) or not
#'                           (FALSE) after report generation.
#' @param    ...             (\code{logical}) Additional parameters passed to function
#'                           \code{writeExecSummary.html}.
#'
#' @seealso \code{\link{RTestCollection-class}}
#'
#' @examples
#'
#' testCollection <- new("RTestCollection",
#'    project.name    = "RTest Vignette",
#'    project.details = "Example test exectuion",
#'    tester          = "Example tester",
#'    test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
#'
#' TCDir <- list.dirs(find.package("RTest"),recursive = TRUE) %>%
#'    grep(pattern = "xml-templates", value = TRUE)
#'
#' message("Test Adapter being used is defined in Function")
#' message("test.RTest.test_returnValue_data.frame_cellbycell")
#'
#' testCollection <- importTCsFromDir(testCollection,
#'     xml.dPath = TCDir[1],f.pattern  = "RTest_TC-01.xml")
#'
#' testCollection <- exec(testCollection)
#'
#' outf <- tempfile(fileext=".html")
#' writeExecSummary.html(testCollection, out.fPath = outf,open = FALSE)
#'
#' stopifnot(any(grepl("passed",readLines(outf))))
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("exec",
    signature  = "RTestCollection",

    definition = function (object, test.TCs = NULL, test.for = NULL, out.fPath = NULL, open = TRUE, ...) {

      if(as.numeric(stringr::str_extract(
                  as.character(packageVersion("testthat")),"[0-9]{1,2}\\.[0-9]{1,2}")) >=
          2){

        # not producing the reporting we need for pretty reports
      }

      # Initialize the list of TCs to perform -------------------------------------------------------
      test.TCs <- getValidTCs(object, test.TCs)

      if(length(test.TCs) == 0)
        stop("No of the TCs valid for execution!")


      # Execute test cases --------------------------------------------------------------------------

      RTest.cat("Execute following test cases:\n")
      RTest.print(test.TCs)
      RTest.cat("\n")

      for(tc in test.TCs)
        object@collection[[tc]] <- test(object@collection[[tc]], test.for,
            out.fPathPre = paste0(sub("\\.[^\\.]+$","",out.fPath),"_",tc))

      if(!is.null(out.fPath))
        writeExecSummary.html(object, out.fPath, test.TCs = test.TCs, open = open, ...)

      execStat <- sapply(test.TCs, function(tc) getTestResult(object@collection[[tc]]))
      execStat.failed <- length(which(execStat == "FAILURE"))

      if(execStat.failed)
        stop("EXECUTION OF UNIT TESTS FAILED FOR ",execStat.failed," TEST CASES!")

      return(object)
    }
)

# show ############################################################################################

#' Show Summary of RTestCollection Instance
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object.
#'
#' @seealso  \code{\link{RTestCollection-class}}
#'
#' @examples
#' location <- find.package("RTest")
#'
#' testCollection <- new("RTestCollection",
#'    project.name    = "RTest Vignette",
#'    project.details = "Example test exectuion",
#'    tester          = "Example tester",
#'    test.start      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
#'
#' show(testCollection)
#'
#' testCollection <- importTCsFromDir(testCollection,
#'    xml.dPath = paste0(location,"/xml-templates")
#' )
#'
#' # Now one test case shall be imported
#' show(testCollection)
#'
#' @name show
#' @aliases show,RTestCollection-method
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("show",
    signature  = "RTestCollection",

    definition = function (object) {
      cat("RTestCollection\n")
      cat("  @project.name:      ",object@project.name,"\n")
      cat("  @project.details:   ",object@project.details,"\n")
      cat("  @tester:            ",object@tester,"\n")
      cat("  @test.start:        ",object@test.start,"\n")
      cat("  @collection:        ",length(object@collection)," TCs\n")
      cat("\n")
    }
)



# importTC ########################################################################################


setGeneric("importTC",
    function(object, xml.fPath)
      standardGeneric("importTC"))

#' Import Test Case from XML File
#'
#' This function imports a test case XML definition and adds it to the test collection.
#' @name importTC
#' @aliases importTC,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object
#' @param    xml.fPath       (\code{character}) Path to XML definition file
#'
#' @seealso  \code{\link{RTestCollection-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("importTC",
    signature  = "RTestCollection",

    definition = function (object, xml.fPath) {

      if(missing(xml.fPath))
        stop("Argument 'xml.fPath' missing.")

      if(!file.exists(xml.fPath))
        stop("Input file '",xml.fPath,"' not exists.")


      # Create new test case ------------------------------------------------------------------------

      # Get name of TC adapter (determined by first node) - - - - - - - - - - - - - - - - - - - - - -

      # Read TC XML file
      tc.xmlDoc  <- xmlTreeParse(xml.fPath, getDTD = F)
      tc.xmlRoot <- xmlRoot(tc.xmlDoc)

      # Get element name of first node
      tc.adapter <- xmlName(tc.xmlRoot)

      # Check if adapter exists - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      tryCatch(
          { getClass("RTestCollection"); },
          error = function(e)
            stop("TC Adapter '",tc.adapter,"' is not a defined class."))


      # Create new instance of RTestCase class  - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RTest.cat("Create new TC using adapter definition '",tc.adapter,"'.")

    tc <- tryCatch(
        do.call(
          what = tc.adapter,
      args=list(
          xml.fPath = xml.fPath
          )
         ),error=function(e){
       new( tc.adapter,
           xml.fPath = xml.fPath)
     })


      RTest.print(tc)


      # Add TC to test suite collection -------------------------------------------------------------

      # Check if a TC with same ID has already been defined   - - - - - - - - - - - - - - - - - - - -
      if(getID(tc) %in% names(object@collection))
        stop("TC '", getID(tc), "' has already been defined.")

      # Add TC  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      object@collection[[getID(tc)]] <- tc



      # Return
      return(object)
    }
)



# importTCsFromDir ################################################################################

setGeneric("importTCsFromDir",
    function(object, xml.dPath, f.pattern = "\\.xml$", f.igncase = FALSE)
      standardGeneric("importTCsFromDir"))

#' Import all Test Cases from XML Files of a Directory
#'
#' This function imports a test case XML definition and adds it to the test collection.
#'
#' @name importTCsFromDir
#' @aliases importTCsFromDir,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object
#' @param    xml.dPath       (\code{character}) Path to directory containing the XML files
#' @param    f.pattern       (\code{character}) An optional regular expression to search the input
#'                           directory. Only file names which match the regular expression will be
#'                           returned (passed as argument to \code{\link[base]{list.files}}).
#' @param    f.igncase       (\code{logical}) Should pattern-matching be case-insensitive or nt
#'                           (passed as argument to \code{\link[base]{list.files}}).
#'
#' @seealso \code{\link{RTestCollection-class}}
#' @export
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("importTCsFromDir",
    signature  = "RTestCollection",

    definition = function (object, xml.dPath, f.pattern = "\\.xml$", f.igncase = FALSE) {

      if(missing(xml.dPath))
        stop("Argument 'xml.dPath' missing.")

      if(!file.exists(xml.dPath))
        stop("Input file '",xml.dPath,"' not exists.")

      # Read input directory
      xml.inFiles <- list.files(xml.dPath, pattern = f.pattern, ignore.case = f.igncase)

      RTest.cat(length(xml.inFiles)," files found in input directory:\n")
      RTest.print(xml.inFiles)
      RTest.cat("\n")

      # Import all XML files
      for(xml.fPath in file.path(xml.dPath, xml.inFiles)) {
        RTest.cat("Import TC file '",xml.fPath,"'\n\n")
        object <- importTC(object, xml.fPath)
      }
      RTest.cat("\n")

      # Return object
      return(object)
    }
)



# getValidTCs #####################################################################################


setGeneric("getValidTCs",
    function(object, test.TCs) standardGeneric("getValidTCs"))

#' Get a List of TCs
#'
#' @name getValidTCs
#' @aliases getValidTCs,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object.
#' @param    test.TCs        (\code{character}) Vector with the TCs to be executed or NULL if all
#'                           all TCs of the collection should be tested.
#'
#' @return  (\code{character}) Only the valid TCs
#'
#' @seealso \code{\link{RTestCollection-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getValidTCs",
    signature  = "RTestCollection",

    definition = function (object, test.TCs = NULL) {

      if(is.null(test.TCs)) {
        # If no attributes passed - by default -, show all imported TCs
        test.TCs <- names(object@collection)

      } else {
        # Else, check if the test cases to be perform exist in the collection
        for(tc in test.TCs[!(test.TCs %in% names(object@collection))])
          warning("TC '",tc,"' has not been imported to the collection")

        # Remove those, which are not in the collection
        test.TCs <- test.TCs[test.TCs %in% names(object@collection)]
      }

      return(test.TCs)
    }
)



# getTC ###########################################################################################

setGeneric("getTC",
    function(object, tc.id) standardGeneric("getTC"))

#' Return Imported Test Case
#'
#' @name getTC
#' @aliases getTC,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object
#' @param    tc.id           (\code{character}) TestCase ID
#'
#' @seealso \code{\link{RTestCase-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getTC",
    signature  = "RTestCollection",

    definition = function (object, tc.id) {

      if(missing(tc.id))
        stop("Argument 'tc.id' missing")

      # Return object
      if(tc.id %in% names(object@collection))
        return(object@collection[[tc.id]])
      else
        NULL
    }
)






# getExecStates ###################################################################################


setGeneric("getExecStates",
    function(object, test.TCs = NULL)
      standardGeneric("getExecStates"))
#' Execution Statues of TCs
#'
#' @name getExecStates
#' @aliases getExecStates,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object.
#' @param    test.TCs        (\code{character}) Vector with the TCs to be executed or NULL if all
#'                           all TCs of the collection should be tested.
#'
#' @return   \code{\link{character}} Vector with test result ('failed' or 'passed') for all TCs.
#'
#' @seealso  \code{\link{RTestCollection-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getExecStates",
    signature  = "RTestCollection",

    definition = function (object, test.TCs = NULL) {

      # Get TCs of collection
      test.TCs <- getValidTCs(object, test.TCs)

      if(length(test.TCs) == 0)
        stop("No of the TCs valid for execution!")

      # Get exec test status for all test cases that should be reporterd
      exec.teststates <-
          unlist(sapply(test.TCs, function(tc) getTestResult(object@collection[[tc]])))

      # Return
      return(exec.teststates)
    }
)


# summary #########################################################################################
# TODO: Implement or remove functin RTestCollection::summary

#' Execution Summary As R Data Object
#'
#' @aliases summary,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object.
#' @param    test.TCs        (\code{character}) Vector with the TCs to be executed or NULL if all
#'                           all TCs of the collection should be tested.
#'
#' @seealso \code{\link{RTestCollection-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
#setGeneric("summary",
#  function(object, test.TCs = NULL)
#    standardGeneric("summary"))

setMethod("summary",
    signature  = "RTestCollection",

    definition = function (object, test.TCs = NULL) {

      # Get TCs of collection
      test.TCs <- getValidTCs(object, test.TCs)

      if(length(test.TCs) == 0)
        stop("No of the TCs valid for execution!")

      # Get exec test status for all test cases that should be reporterd
      exec.summary <- lapply(
          test.TCs,
          function(tc) getExecSummary(object@collection[[tc]]))

      # Return
      return(exec.summary)
    }
)


# writeExecSummary.html ###########################################################################


setGeneric("writeExecSummary.html",
    function(object, out.fPath, test.TCs = NULL, open = TRUE, report.onlyFailed = FALSE, logo = NULL)
      standardGeneric("writeExecSummary.html"))

#' Write Summary of Last Test Case Executions as HTML
#' @name writeExecSummary.html
#' @aliases writeExecSummary.html,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object.
#' @param    out.fPath       (\code{character}) Path to output file.
#' @param    test.TCs        (\code{character}) Vector with the TCs to be executed or NULL if all
#'                           all TCs of the collection should be tested.
#' @param    open            (\code{logical}) Should the generated file be opened (TRUE) or not
#'                           (FALSE) after report generation.
#' @param    report.onlyFailed  (\code{logical}) Report only failed exceptions (TRUE) or
#'                              all exceptions (FALSE, default).
#' @param    logo            (\code{character}) Path to alternative logo file. To use the default
#'                              logo, use NULL. To use no logo use NA.
#'
#' @seealso \code{\link{RTestCollection-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("writeExecSummary.html",
    signature  = "RTestCollection",

    definition = function (object, out.fPath, test.TCs = NULL, open = TRUE, report.onlyFailed = FALSE, logo = NULL) {

      # Check function call -------------------------------------------------------------------------
      if(missing(out.fPath))
        stop("Argument 'out.fPath' missing.")


      # Get Test cases to be shown ------------------------------------------------------------------

      # Initialize the list of TCs to perform - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      test.TCs <- getValidTCs(object, test.TCs)

      if(length(test.TCs) == 0)
        stop("No of the TCs valid for execution!")


      # Analyse general execution status  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      # Get exec test status for all test cases that should be reporterd
      exec.teststates  <- getExecStates(object, test.TCs)


      # Check, if there are any test cases, that should be reported but have not been executed
      for(tc in test.TCs[is.null(exec.teststates)])
        if(is.null(getTestResult(object@collection[[tc]])))
          stop("Test Case '",getID(object@collection[[tc]]),"' has not been executed ",
              "(see RTest::exec(...) for details)!")


      # Create HTML contents ------------------------------------------------------------------------

      # Message
      RTest.cat("Write HTML summary for following test cases:\n")
      RTest.print(test.TCs)
      RTest.cat("\n")

      # General exec status and corresponding message
      exec.status           <- if("failed" %in% exec.teststates) "FAILED" else "SUCCESS"
      exec.status.msg       <- if("failed" %in% exec.teststates) "TEST FAILED" else "TEST PASSED"

      # Number of successfull / failed TCs
      exec.status.failures  <- length(which(exec.teststates == "failed"))
      exec.status.successes <- length(which(exec.teststates == "success"))


      # Initialize HTML content container - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      out <- c()
      out.append <- function(...) { out <<- c(out, paste0(...)) }


      # HTML contents - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      out.append("<html>")
      out.append("")

      out.append("<head>")
      out.append("")

      out.append("  <title>RTest - ",object@project.name,"</title>")
      out.append("")

      out.append("  <style>")

      css.fPath <-
          if(file.exists(file.path(find.package("RTest"),"css/style.css")))
            file.path(find.package("RTest"),"css/style.css")
          else
            file.path(find.package("RTest"),"css/style.css")

      out <- c(out, readLines(css.fPath))
      out.append("  </style>")
      out.append("")

      out.append("</head>")
      out.append("")

      out.append("<body>")
      out.append("<a name=\"pagetop\"></a>")
      out.append("")

      if(is.null(logo)) {
        img.fPath <-
            if(file.exists(file.path(find.package("RTest"),"/images/roche-logo.png")))
              file.path(find.package("RTest"),"/images/roche-logo.png")
            else
              file.path(find.package("RTest"),"/images/roche-logo.png")
      } else {
        img.fPath <- logo
      }

      out.append("<div id=\"header\">")
      out.append("  <table width=\"100%\">")
      out.append("    <tr>")
      out.append("      <td valign=\"bottom\" class=\"headline\">")
      out.append("        <div class=\"headline-1\"><i class=\"system\">RTest</i></div>")
      out.append("        <div class=\"headline-2\">",object@project.name,"</div>")
      out.append("      </td>")
      out.append("      <td width=\"100\">")
      if(!is.na(img.fPath)) {
        out.append("        ",
            png2base64(img.fPath,
                img.returnAsTag = TRUE, img.title = "Roche Logo", img.width = "100px"))
      }
      out.append("      </td>")
      out.append("    </tr>")
      out.append("  </table>")
      out.append("</div>")

      out.append("<br />")
      out.append("<table width=\"100%\" class=\"TCSummary\">")
      out.append("  <tr>")
      out.append("    <td width=\"50%\" valign=\"top\">")
      out.append("      <table width=\"100%\">")
      out.append("        <tr>")
      out.append("          <td width=\"150\"><b>Project</b></td>")
      out.append("          <td>",object@project.name,"</td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>Project Details</b></td>")
      out.append("          <td>",object@project.details,"</td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>Tester</b></td>")
      out.append("          <td>",object@tester,"</td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>Test Start</b></td>")
      out.append("          <td>",object@test.start,"</td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>Report Generated</b></td>")
      out.append("          <td>",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),"</td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>No. of Test Cases</b></td>")
      out.append("          <td>",length(test.TCs),"</td>")
      out.append("        </tr>")
      out.append("      </table>")
      out.append("    </td>")
      out.append("    <td width=\"50%\" valign=\"top\">")
      out.append("      <table width=\"100%\">")
      out.append("        <tr>")
      out.append("          <td width=\"150\"><b>Host</b></td>")
      out.append("          <td>",systemInfo.host()["sysname"]," ",systemInfo.host()["release"],"</td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>Host Version</b></td>")
      out.append("          <td>",systemInfo.host()["version"],"</td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>Host Name (User)</b></td>")
      out.append("          <td>")
      out.append("            ",systemInfo.host()["nodename"]," (",systemInfo.host()["user"],")")
      out.append("          </td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>R</b></td>")
      out.append("          <td>")
      out.append("            <a href=\"#REnvDetails\">",systemInfo.RInst()["version.string"],"</a>")
      out.append("          </td>")
      out.append("        </tr>")
      out.append("        <tr>")
      out.append("          <td><b>R Architecture</b></td>")
      out.append("          <td>",systemInfo.RInst()["arch"],"</td>")
      out.append("        </tr>")
      out.append("      </table>")
      out.append("    </td>")
      out.append("  </tr>")
      out.append("</table>")


      out.append("<h1>GLOBAL TEST STATUS</h1>")
      out.append("")

      out.append("<table width=\"100%\">")
      out.append("  <tr>")
      out.append("    <td align=\"center\">")
      out.append("      <div class=\"abstract-",exec.status,"\">")
      out.append("        <div class=\"exec-teststate\">",exec.status.msg,"</div>")
      out.append("        ",exec.status.failures," TCs failed ",
          "(",round(exec.status.failures / length(test.TCs) * 100, 2),"%)<br >")
      out.append("        ",exec.status.successes," TCs passed ",
          "(",round(exec.status.successes / length(test.TCs) * 100, 2),"%)<br >")
      out.append("      </div>")
      out.append("    </td>")
      out.append("  </tr>")
      out.append("<table>")
      out.append("")


      out.append("<h1>EXECUTION SUMMARY</h1>")
      out.append("")

      for(i.tc in 1:length(test.TCs)) {
        tc <- object@collection[[i.tc]]
        out.append(getExecSummary.html(tc))
      }


      out.append("<h1>EXECUTION DETAILS</h1>")
      out.append("")

      for(i.tc in 1:length(test.TCs)) {
        tc <- object@collection[[i.tc]]
        out.append(getExecDetails.html(tc, report.onlyFailed))
      }


      out.append("<a name=\"REnvDetails\"></a>")
      out.append("<h1>R TEST ENVIRONMENT</h1>")
      out.append("")

      for(p in c("basePkgs", "otherPkgs", "loadedOnly")) {
        pkgs <- systemInfo.packages(p)

        pkgs.head <- switch(p,
            "basePkgs"   = "R Base Packages",
            "otherPkgs"  = "Attached Packages",
            "loadedOnly" = "Loaded, but not attached Packages")

        out.append("<h2>",pkgs.head,"</h2>")
        out.append("")
        out.append("<table width=\"100%\" class=\"TCSummary\">")
        out.append("  <tr>")
        out.append("    <th align=\"center\" width=\"33%\">Package</th>")
        if(p != "basePkgs") {
          out.append("    <th align=\"center\" width=\"22%\">Version</th>")
          out.append("    <th align=\"center\" width=\"22%\">Build</th>")
          out.append("    <th align=\"center\" width=\"22%\">package_md5(Package)</th>")
        }
        out.append("  </tr>")
        for(r in 1:dim(pkgs)[1]) {
          out.append("  <tr>")
          out.append("    <td align=\"center\">",pkgs[r, 1],"</td>")
          if(p != "basePkgs") {
            out.append("    <td align=\"center\">",pkgs[r, 2],"</td>")
            out.append("    <td align=\"center\">",pkgs[r, 3],"</td>")
            out.append("    <td align=\"center\">",pkgs[r, 4],"</td>")
          }
          out.append("  </tr>")
        }
        out.append("<table>")
        out.append("<br />")
        out.append("")
      }


      out.append("</body>")
      out.append("</html>")


# Write HTML file -----------------------------------------------------------------------------

      write(out, out.fPath)

      RTest.cat("HTML summary written to file '",out.fPath,"'.\n\n")

      if(open){
     if(Sys.info()["sysname"]=="Windows"){
       shell.exec(out.fPath)
     }else{
       system(paste("open",out.fPath))
     }
   }
    }
)





# getRTM ##########################################################################################


setGeneric("getRTM",
    function(object, test.TCs = NULL, ...)
      standardGeneric("getRTM"))

#' Get RTM for all executed test cases
#'
#' This method returns the requirement traceability matrix (RTM) for all imported test cases.
#'
#' @name getRTM
#' @aliases getRTM,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object.
#' @param    test.TCs        (\code{character}) Vector with the TCs to be executed or NULL if all
#'                           all TCs of the collection should be tested.
#' @param    ...             Additional arguments passed to \code{\link{getRTMInfos}}.
#'
#' @return  (\code{data.frame}) The RTM as \code{data.frame} table object.
#'
#' @seealso \code{\link{RTestCollection-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getRTM",
    signature  = "RTestCollection",

    definition = function (object, test.TCs = NULL, ...) {

      # Check function call -------------------------------------------------------------------------

      # nothing to do


      # Get Test cases to be shown ------------------------------------------------------------------

      # Initialize the list of TCs to perform - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      test.TCs <- getValidTCs(object, test.TCs = test.TCs)

      if(length(test.TCs) == 0)
        stop("No of the TCs valid for execution!")


      # Create RTM ----------------------------------------------------------------------------------
      res <- do.call(rbind, lapply(1:length(test.TCs), function(i.tc) {
                getRTMInfos(object@collection[[i.tc]], ...)
              }))


      # Return output  ------------------------------------------------------------------------------

      return(res)
    }
)





# getRTMInMatrixShape #############################################################################


setGeneric("getRTMInMatrixShape",
    function(object, test.TCs = NULL, type = "function", ...)
      standardGeneric("getRTMInMatrixShape"))
#' Get RTM for all executed test cases in a matrix shape
#'
#' This method returns the requirement traceability matrix (RTM) in matrix representation for all
#' imported test cases of the test case collection. Thereby, the matrix can be created for the
#' relationship function to test case or risk to test case.
#' @name getRTMInMatrixShape
#' @aliases getRTMInMatrixShape,RTestCollection-method
#'
#' @param    object          (\code{object}) The \code{\link{RTestCollection-class}} object.
#' @param    test.TCs        (\code{character}) Vector with the TCs to be executed or NULL if all
#'                           all TCs of the collection should be tested.
#' @param    type            (\code{character}) Type of the returned matrix, either 'function'  or
#'                           'risk'.
#' @param    ...             Additional arguments passed to \code{getRTM}.
#'
#' @return  (\code{data.frame}) The RTM as \code{data.frame} table object.
#'
#' @seealso \code{\link{RTestCollection-class}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
setMethod("getRTMInMatrixShape",
    signature  = "RTestCollection",

    definition = function (object, test.TCs = NULL, type = "function", ...) {

      # Check function call -------------------------------------------------------------------------

      # nothing to do


      # Get Test cases to be shown ------------------------------------------------------------------

      # Initialize the list of TCs to perform - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      test.TCs <- getValidTCs(object, test.TCs = test.TCs)

      if(length(test.TCs) == 0)
        stop("No of the TCs valid for execution!")


      # Create RTM ----------------------------------------------------------------------------------

      RTMTab <- getRTM(object)

      column <- ifelse(type == "function", "SpecIDs", "RiskIDs")

      tc.specIDs <- lapply(RTMTab[,column], function(x) gsub(" ", "", strsplit(x, ",")[[1]]))
      names(tc.specIDs) <- RTMTab[,"ID"]

      allSpecs <- unique(unlist(as.vector(tc.specIDs)))

      res <- data.frame(
          SpecIDs = allSpecs,
          do.call(cbind,
              sapply(names(tc.specIDs), function(id) {
                    ifelse(allSpecs %in% tc.specIDs[[id]], "x", "")
                  }, simplify = FALSE, USE.NAMES = TRUE)
          ),
          stringsAsFactors = FALSE, check.names = FALSE)

      rownames(res) <- res[,1]


      # Return output  ------------------------------------------------------------------------------

      return(res)
    }
)



