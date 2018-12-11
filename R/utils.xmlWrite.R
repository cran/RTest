###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# RTest xmlWrite Uitility Functions                                                               #
#                                                                                                 #
# This file defines a set of utility functions to write XML entries of general (pre-defined)      #
# data types and other for RTest predefined XML elements.                                         #
#                                                                                                 #
# Date:           25 - Jan - 2016                                                                 #
# Author:         Matthias Pfeifer (matthias.pfeifer@roche.com)                                   #
#                                                                                                 #
###################################################################################################

# xmlFromList #################################################################################
#' Creat an XML Node from a list
#' 
#' This function appends a list as an XML object to an item. The function allows
#' setting attributes of XML items by using the "attributes" list name, therefore
#' it can never write tags with the name "attributes"
#' 
#' @param node (\code{XMLNode}) A Node created by XML package
#' @param sublist (\code{list}) Any list
#' 
#' @return node (\code{XMLNode}) A node where the list is attached to the first XML Node
#' @examples 
#' 
#' root <- XML::newXMLNode("root")
#' li <- list(a = list(aa = 1, ab=2), 
#' 			b=list(ba = 1,
#'                 bb=list(x=4,
#'                        attributes=c(value=3)),
#'                bb= 2,
#'               bc =3))
#' xmlFromList(root,li)
#' 
#' # The result is an XML Node like this
#' #<root>
#' #  <a>
#' #    <aa>1</aa>
#' #    <ab>2</ab>
#' #  </a>
#' #  <b>
#' #    <ba>1</ba>
#' #    <bb value="3">
#' #      <x>4</x>
#' #    </bb>
#' #    <bb>2</bb>
#' #    <bc>3</bc>
#' #  </b>
#' #</root> 
#' 
#' @export 
#' @author   Sebastian Wolf \email{sebastian.wolf.sw1@@roche.com}
xmlFromList <- function(node, sublist){
	for(i in 1:length(sublist)){
		child <- newXMLNode(names(sublist)[i], parent=node)
		
		if (typeof(sublist[[i]]) == "list"){				
			
			if("attributes" %in% names(sublist[[i]])){
				xmlAttrs(child)<-sublist[[i]]$attributes
				sublist[[i]]$attributes <- NULL
			}
			if(length(sublist[[i]])>0){
				
				xmlFromList(child, sublist[[i]])
			}
		}else{
			xmlValue(child) <- sublist[[i]]
		}
	} 
}


# xmlWriteContext #################################################################################

#' Write the Opening (Header, Root-Tag) and Closing for a RTestCase 
#'
#' @param    TCType      (\code{character}) TC Type
#' @param    id          (\code{character}) TC ID
#' @param    opening,closing (\code{logical}) Specify if the opening and/or the closing
#'                         tags should be written.
#' @param    xsd.scheme  (\code{character}) Path to XSD Scheme
#' @param    printXML    (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{list}) Opening [[1]] and Closing [[2]] of the Test Case
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteContext <- function(TCType, id, opening = TRUE, closing = TRUE,
    xsd.scheme = NULL, printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  if(missing(TCType)) stop("Argument 'TCType' is missing")
  if(missing(id))     stop("Argument 'id' is missing")
  
  if(!is.null(xsd.scheme))
    xsd.scheme <- paste0(" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"",xsd.scheme,"\"")
  
  
  # Generate XML ----------------------------------------------------------------------------------
  xml.opening <- c()
  if(opening) {
    xml.opening <- c(xml.opening, paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
    xml.opening <- c(xml.opening, paste0("<",TCType," ",xsd.scheme,">"))
    
    xml.opening <- c(xml.opening, "")
    xml.opening <- c(xml.opening, paste0("<ID>",id,"</ID>"))
    xml.opening <- c(xml.opening, "")
  }
  
  xml.closing <- c()
  if(closing) {
    xml.closing <- c(xml.closing, paste0("</",TCType,">"))
  }
  
  if(printXML)
    cat(paste(c(xml.opening, xml.closing), collapse="\n"))
  else
    return(c(xml.opening, xml.closing))
}



# xmlWriteSynopsis ################################################################################

#' Write the Synopsis for a RTestCase 
#'
#' @param    version     (\code{dcharacter}) Version Number
#' @param    author      (\code{character}) Author
#' @param    shortDescription (\code{character}) Short description
#' @param    description (\code{character}) Description
#' @param    creationDate (\code{character}) Creation Date
#' @param    changes     (\code{list}) A listing of lists each representing one change with
#'                         attributes 'author', 'date' and 'desc'
#' @param    label   (\code{character}) Labels.
#' @param    printXML    (\code{logical}) Print output or return xml as R object    
#' 
#' @return   (\code{character})
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteSynopsis <- function(version, author, 
    shortDescription = NULL, description = NULL, creationDate = NULL, 
    changes = list(list(author = author, date = creationDate, desc = "Initial Version")),
    label = NULL,
    printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  if(missing(version)) stop("Argument 'version' is missing")
  if(missing(author))  stop("Argument 'author' is missing")
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  xml <- c(xml, paste0("<synopsis>"))
  
  xml <- c(xml, paste0("  <version>",version,"</version>"))
  
  for(a in author) {
    xml <- c(xml, paste0("  <author>",author,"</author>"))
  }
  
  if(!is.null(shortDescription))
    xml <- c(xml, paste0(
            "  <short-description>\n",
            "    ",shortDescription,"\n",
            "  </short-description>"))
  
  if(!is.null(description))
    xml <- c(xml, paste0(
            "  <description>\n",
            "    ",description,"\n",
            "  </description>"))
  
  if(!is.null(label))
    xml <- c(xml, paste0(
            "  <label>\n",
            "    ",label,"\n",
            "  </label>"))
  
  if(!is.null(creationDate))
    xml <- c(xml, paste0("  <creation-date>",creationDate,"</creation-date>"))
  
  if(!is.null(changes)) {
    xml <- c(xml, paste0("  <change-history>"))
    for(change in changes) {
      xml <- c(xml, paste0(
              "    <change author=\"",change$author,"\" date=\"",change$date,"\">\n",
              "      ",change$desc,"\n",
              "    </change>"))   
    }
    xml <- c(xml, paste0("  </change-history>"))
  }
  
  xml <- c(xml, paste0("</synopsis>"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}


# xmlWriteInputData ################################################################################

#' Write the Input-Data section for a RTestCase 
#'
#' @param    ...         (\code{character}) Stuff to include in the input section
#' @param    printXML    (\code{logical}) Print output or return xml as R object    
#' 
#' @return   (\code{character})
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteInputData <- function(..., 
    printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  xml <- c(xml, paste0("<input-data>"))
  
  xml <- c(xml, unlist(sapply(c(...), function(x) paste0("  ",x))))
  
  xml <- c(xml, paste0("</input-data>"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}



# xmlWriteTests ###################################################################################

#' Write the Tests section for a RTestCase 
#'
#' @param    ...         (\code{character}) Stuff to include in the tests section
#' @param    printXML    (\code{logical}) Print output or return xml as R object    
#' 
#' @return   (\code{character})
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTests <- function(..., 
    printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  xml <- c(xml, paste0("<tests>"))
  
  xml <- c(xml, unlist(sapply(c(...), function(x) paste0("  ",x))))
  
  xml <- c(xml, paste0("</tests>"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}



# xmlWriteTest #####################################################################################

#' Write the Test section for a RTestCase 
#'
#' @param    elemname    (\code{character}) The name of the element's root tag
#' @param    testdesc    (\code{character}) The description of the test's root tag  
#' @param    ...         (\code{character}) Stuff to include in the test section
#' @param    printXML    (\code{logical}) Print output or return xml as R object    
#' 
#' @return   (\code{character})
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTest <- function(elemname, testdesc = NA, ..., 
    printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  if(!is.na(testdesc) && !is.character(testdesc))
    stop("Argument 'testdesc' has to be a character.")
  
  testdesc <- 
      if(!is.na(testdesc)) paste0(" test-desc=\"",testdesc,"\"")
      else ""
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  xml <- c(xml, paste0("<",elemname,"",testdesc,">"))
  
   xml <- c(xml, unlist(sapply(c(...), function(x) paste0("  ",x))))
  
  xml <- c(xml, paste0("</",elemname,">"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}


# xmlWriteTestFunction ############################################################################

#' Write the Test section for a RTestCase 
#'
#' @param    elemname    (\code{character}) The name of the element's root tag
#' @param    testdesc    (\code{character}) The description of the test's root tag  
#' @param    specid       (\code{character}) The Specification ID
#' @param    riskid       (\code{character}) The Risk ID
#' @param    execresid       (\code{character}) Executed Risk ID
#' @param    params       (\code{ANY}) The Parameters of the function
#' @param    reference       (\code{ANY}) The Reference tested against
#' @param    testspec       (\code{ANY}) The test specification do calculate
#' @param    printXML    (\code{logical}) Print output or return xml as R object    
#' 
#' @return   (\code{character})
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTestFunction <- function(
	elemname, 
    testdesc = NA,
	execresid = NA,
	specid = NA,
	riskid = NA,
    params = "",
	reference = "",
	testspec = "", 
    printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  if(!is.na(testdesc) && !is.character(testdesc))
    stop("Argument 'testdesc' has to be a character.")
  
  testdesc <- 
      if(!is.na(testdesc)) paste0(" test-desc=\"",testdesc,"\"")
      else ""
  
  
  if(!is.na(execresid) && !is.character(execresid))
    stop("Argument 'execresid' has to be a character.")
  
  execresid <- 
      if(!is.na(execresid)) paste0(" exec-res-id=\"",execresid,"\"")
      else ""
  
  
  if(!is.na(specid) && !is.character(specid))
    stop("Argument 'specid' has to be a character.")
  
  specid <- 
      if(!is.na(specid)) paste0(" spec-id=\"",specid,"\"")
      else ""
  
  
  if(!is.na(riskid) && !is.character(riskid))
    stop("Argument 'riskid' has to be a character.")
  
  riskid <- 
      if(!is.na(riskid)) paste0(" risk-id=\"",riskid,"\"")
      else ""
  
  
  # Internal functions-----------------------------------------------------------------------------
  
  transformTagvector <- function(name, tagvector) {
    
        if(length(tagvector) == 1 && nchar(tagvector) == 0)
              c(paste0("  <",name," />"))            
            else
              c(
                  paste0("  <",name,">"),
                  sapply(tagvector, function(x) paste0("    ",x)),
                  paste0("  </",name,">")
              )

    
  }
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  xml <- c(xml, paste0("<",elemname,"",testdesc,"",execresid,"",specid,"",riskid,">"))
  
  xml <- c(xml,
      transformTagvector("params", params)
  )
  
  xml <- c(xml,
      transformTagvector("reference", reference)
  )
  
  xml <- c(xml,
      transformTagvector("testspec", testspec)
  )
  
  
  xml <- c(xml, paste0("</",elemname,">"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}


# xmlWriteTestSpec ################################################################################

#' Write the testpsec section for a RTestCase 
#'
#' @param    ...         (\code{character}) Stuff to include in the tests section
#' @param    printXML    (\code{logical}) Print output or return xml as R object    
#' 
#' @return   (\code{character})
#' 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTestSpec <- function(..., 
    printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  xml <- c(xml, paste0("<testspec>"))
  
  xml <- c(xml, sapply(c(...), function(x) paste0("  ",x)))
  
  xml <- c(xml, paste0("</testspec>"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}





# xmlWriteData_data.frame #########################################################################

#' Write a R 'data.frame' as XML Data of Type 'xmlReadData_data.frame' 
#'
#' @param    elemname    (\code{character}) The name of the element's root tag 
#' @param    data        (\code{data.frame}) The data to write
#' @param    name        (\code{character}) The data name.
#' @param    printXML    (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteData_data.frame <- function(elemname = "data", data, name = NULL, printXML = TRUE) {
  
  # Check input -----------------------------------------------------------------------------------
  
  stopifnot(class(data) == "data.frame")
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  # Column definitions  
  xml.colDefs <- c(
      "<col-defs>",
      sapply(
          colnames(data),
          function(c) {
            paste0("  <coldef name=\"",c,"\" type=\"",
					
					if(typeof(data[[c]])=="integer"){
						if(grepl("Factor",capture.output(str(data[[c]])))){
							"factor"
						}else{
							"numeric"
						}
					}else if(typeof(data[[c]])=="double"){
						"numeric"
					}else{
						class(data[[c]])
					},"\" />")
          }, simplify = TRUE, USE.NAMES = FALSE),
      "</col-defs>")
  
  #transforme all variables to characters
  data[] <- lapply(data, as.character)
  
  
  # Content
  xml.content <- c()
  for(r in 1:dim(data)[1]) {
    r.name <- if(!is.null(rownames(data))) paste0(" name=\"",rownames(data)[r],"\"") else ""
    
    xml.content <- c(xml.content,
        paste0("<row",r.name,">"),
        sapply(data[r,], function(c) {
              if(grepl(">", c) || grepl("<", c))
                c <- paste0("<![CDATA[ ",c," ]]>")
              paste0("  <cell>",c,"</cell>")
            }, simplify = TRUE, USE.NAMES = FALSE),
        "</row>")
  }
  
  # Complete XML element
  xml <- c(
      paste0("<",elemname,"",if(!is.null(name)) paste0(" name=\"",name,"\""),">"),
      paste0("  ",xml.colDefs),
      paste0("  ",xml.content),
      paste0("</",elemname,">"))
  
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}



# xmlWriteData_vector #############################################################################

#' Write a R 'vector' as XML Data of Type 'xmlReadData_vector' 
#'
#' @param    elemname    (\code{character}) The name of the element's root tag 
#' @param    data        (\code{vector}) The vector data to write
#' @param    name        (\code{character}) The data name.
#' @param    printXML    (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteData_vector <- function(elemname = "vector", data, name = NULL,  printXML = TRUE) {
  
  # Check input -----------------------------------------------------------------------------------
  
  data.class <- class(data)
  
  stopifnot(class(data) %in% c("integer", "numeric", "character"))
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  # Content
  xml.elements <- c()
  for(e in 1:length(data)) {
    e.name <- if(!is.null(names(data))) paste0(" name=\"",names(data)[e],"\"") else ""
    
    xml.elements <- c(xml.elements,
        paste0("<element",e.name,">",
            if(is.numeric(data[e])) format(data[e], digits = 22) else data[e],
            "</element>"))

#    xml.elements <- c(xml.elements,
#        paste0("<element",e.name,">",data[e],"</element>"))
    
  }
  
  # Complete XML element
  xml <- c(
      paste0("<",elemname," ",
          ifelse(is.null(name), "", paste0("name=\"",name,"\""))," type=\"",data.class,"\">"),
      paste0("  ",xml.elements),
      paste0("</",elemname,">"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}



# xmlWriteData_variable ############################################################################

#' Write a R 'constat' as XML Data of Type 'xmlReadData_variable' 
#'
#' @param    elemname       (\code{character}) The name of the element's root tag
#' @param    data           (\code{ANY}) The variable to write
#' @param    name           (\code{character}) The name of the variable
#' @param    printXML       (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteData_variable <- function(elemname = "variable", data, name = NULL, printXML = TRUE) {
  
  # Check input -----------------------------------------------------------------------------------
  
  data.class <- class(data)
  
  stopifnot(class(data) %in% c("integer", "numeric", "character", "logical", "NULL"))
  
  
  # Generate XML ----------------------------------------------------------------------------------
  if(is.null(name)){
    xml <- paste0("<",elemname," value=\"", 
        if(is.numeric(data)) format(data, digits = 22) else data,
         "\" type=\"", data.class, "\"/>")
  } else {
    xml <- paste0("<",elemname," value=\"", 
        if(is.numeric(data)) format(data, digits = 22) else data,
        "\" type=\"", data.class,"\" name=\"",name,"\"/>")
  }
  
  if(printXML)
    cat(paste(xml, collapse="\n"), "\n")
  else
    return(xml)
}

# xmlWriteData_list ############################################################################

#' Write a R 'list' as XML Data of Type 'xmlReadData_list' 
#'
#' @param    elemname       (\code{character}) The name of the element's root tag
#' @param    data           (\code{ANY}) The list to write
#' @param    name           (\code{character}) The name of the list
#' @param    printXML       (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteData_list <- function(elemname = "list", data, name = NULL, printXML = TRUE) {
  
  # Check input -----------------------------------------------------------------------------------
  
  data.class <- class(data)
  
  stopifnot(class(data) %in% c("list"))
  
  xml <- paste0("<",elemname,
		  if(!is.null(name)){
			  if(name != ""){
				  paste0(" name=\"",name,"\"")
			  }
		  }
		  ,">")
  for(i in 1:length(data)){
	  listelement <- data[[i]]
	  listelementname <- names(data)[i]
			  
	  if(length(listelement)>0){
		  if(class(listelement)=="list"){
			  xml <- paste0(xml,xmlWriteData_list(
							  data = listelement,
							  name = listelementname,
							  printXML = F
							  ))
		  }else if(class(listelement)=="data.frame"){
			  xml <- paste0(xml,
					  paste(xmlWriteData_data.frame(
							  name=listelementname,
							  data = listelement,
							  printXML = F
					  ),collapse="\n"))
	  	  }else{
			  xml <- paste0(xml,					  
					  paste(xmlWriteData_vector(
							  name=listelementname,
							  data = listelement,
							  printXML = F
					  ),collapse = "\n"))
		  }
	  }else{
		  
		  xml <- paste0(xml,xmlWriteData_variable(
						  name=listelementname,
						  data = listelement,
						  printXML = F
				  ))
	  }
			  
			  
  }#for
  
  xml <- paste0(xml,paste0("\n</",elemname," >"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"), "\n")
  else
    return(xml)
}



# xmlWriteTest_execution ##########################################################################

#' Write XML Test Definition of Type 'RTestTest_execution' 
#'
#' @param    elemname    (\code{character}) The tag name of the test
#' @param    desc        (\code{character}) The testname 
#' @param    executionType (\code{character}) The execution mode to be checked (i.e. 'silent', 
#'             'warning', 'error').
#' @param    printXML       (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTest_execution <- function(elemname = "execution", desc = NULL, 
    executionType = "silent", printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  stopifnot(executionType %in% c("silent", "warning", "error","message"))
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  xml <- c(xml, paste0("<",elemname," desc=\"",desc,"\" execution-type=\"",executionType,"\" />"))
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}


# xmlWriteTest_variable ###########################################################################

#' Write XML Test Definition of Type 'RTestTest_variable' 
#'
#' @param    elemname    (\code{character}) The tag name of the test
#' @param    testname    (\code{character}) The testname 
#' @param    test        (\code{ANY}) Named vector with test, single entry if same for all.
#' @param    tolerance   (\code{ANY}) Named vector with tolerances, single entry if same for all.
#' @param    printXML       (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTest_variable <- function(elemname = "return-value", 
    testname = "variable", 
    test = "absolute", tolerance = 0, printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
    
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- paste0("<",elemname," desc=\"",testname,"\" diff-type=\"",test,"\" tolerance=\"",tolerance,"\" />")

  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}


# xmlWriteTest_vector_elementbyelement ############################################################

#' Write XML Test Definition of Type 'RTestTest_vector_elementbyelement' 
#'
#' @param    elemname    (\code{character}) The tag name of the test
#' @param    testname    (\code{character}) The testname 
#' @param    data        (\code{data.frame}) The reference data for which the test should be 
#'                       written.
#' @param    test        (\code{ANY}) Named vector with test, single entry if same for all.
#' @param    tolerance   (\code{ANY}) Named vector with tolerances, single entry if same for all.
#' @param    printXML       (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' @export
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTest_vector_elementbyelement <- function(elemname = "return-value", 
    testname = "vector_elementbyelement", data = NULL, 
    test = "absolute", tolerance = 0, printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  test0 <- if(length(test) > 1) test[1] else test
  tolerance0 <- if(length(tolerance) > 1) tolerance[1] else tolerance
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  opening <- paste0("",elemname," desc=\"",testname,"\" diff-type=\"",test0,"\" tolerance=\"",tolerance0,"\"")
  
  
  if(!is.null(data)) {
    xml <- c(xml, paste0("<",opening,">"))
    
    if(is.null(names(data)))
      names(data) <- as.character(1:length(data))
    
    for(c in names(data)) {
      
      c.attrs <- c()
      
      c.attrs <- c(c.attrs, paste0("name = \"",c,"\""))
      
      #c.attrs <- c(c.attrs, paste0("type = \"",class(data[,c]),"\""))
      
      if(length(test) != 1) {
        stopifnot(c %in% names(test))
        if(test[c] != test0)
          c.attrs <- c(c.attrs, paste0("desc = \"",test[c],"\""))
      }
      
      if(length(tolerance) != 1) {
        stopifnot(c %in% names(tolerance))
        if(tolerance[c] != tolerance0)
          c.attrs <- c(c.attrs, paste0("tolerance = \"",tolerance[c],"\""))
      }
      
      xml <- c(xml, paste0("  <element ",paste(c.attrs, collapse=" "),"/>"))
    }
    
    xml <- c(xml, paste0("</",elemname,">"))
    
  } else {
    xml <- c(xml, paste0("<",opening," />"))
  }
  
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}



# xmlWriteTest_data.frame_cellbycell ##############################################################

#' Write XML Test Definition of Type 'RTestTest_data.frame_cellbycell' 
#'
#' @param    elemname    (\code{character}) The tag name of the test
#' @param    desc        (\code{character}) Description
#' @param    data        (\code{data.frame}) The reference data for which the test should be 
#'                       written.
#' @param    diff_type    (\code{character}) Difference 'absolute' or 'relative' that is used for comparison.
#' @param    tolerance   (\code{ANY}) Named vector with tolerances, single entry if same for all.
#' @param    compare_type   (\code{character}) Comparator used in the XML spec.
#' @param    printXML       (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTest_data.frame_cellbycell <- function(elemname = "test",
    desc = "testname",
    data, 
    diff_type = "absolute",
    tolerance = 0,
    compare_type ="equal",
    printXML = TRUE) {
  
  

  # Check input -----------------------------------------------------------------------------------
  if(!is.null(data))
    stopifnot(class(data) == "data.frame")
  
  tolerance0 <- if(length(tolerance) > 1) tolerance[1] else tolerance
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  xml <- c(xml, paste0("<",elemname,
          " desc=\"",desc,
          "\" diff-type=\"",diff_type,
          "\" tolerance=\"",tolerance0,
          "\" compare-type=\"",compare_type,"\"",
          ifelse(is.null(data), " /", ""),">"))
  
  if(!is.null(data)) {
    for(c in colnames(data)) {
      
      c.attrs <- c()
      
      c.attrs <- c(c.attrs, paste0("name = \"",c,"\""))
      
      #c.attrs <- c(c.attrs, paste0("type = \"",class(data[,c]),"\""))
      
      if(length(test) != 1) {
        stopifnot(c %in% names(test))
		test0 <- if(length(test) > 1) test[1] else test
        if(test[c] != test0)
          c.attrs <- c(c.attrs, paste0("test = \"",test[c],"\""))
      }
      
      if(length(tolerance) != 1) {
        stopifnot(c %in% names(tolerance))
        if(tolerance[c] != tolerance0)
          c.attrs <- c(c.attrs, paste0("tolerance = \"",tolerance[c],"\""))
      }
      
      
      
      xml <- c(xml, paste0("  <column ",paste(c.attrs, collapse=" "),"/>"))
    }
    
    xml <- c(xml, paste0("</",elemname,">"))
  }
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}



# xmlWriteTest_list_nodebynode ####################################################################

#' Write XML Test Definition of Type 'RTestTest_list_nodebynode' 
#'
#' @param    elemname    (\code{character}) The tag name of the test
#' @param    testname    (\code{character}) The testname 
#' @param    data        (\code{data.frame}) The reference data for which the test should be 
#'                       written.
#' @param    test        (\code{ANY}) Named vector with test, single entry if same for all.
#' @param    tolerance   (\code{ANY}) Named vector with tolerances, single entry if same for all.
#' @param    printXML       (\code{logical}) Print output or return xml as R object
#' 
#' @return   (\code{character})
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlWriteTest_list_nodebynode <- function(elemname = "return-value", 
    testname = "list_nodebynode", data = NULL, 
    test = "absolute", tolerance = 0, printXML = TRUE) 
{
  
  # Check input -----------------------------------------------------------------------------------
  
  test0 <- if(length(test) > 1) test[1] else test
  tolerance0 <- if(length(tolerance) > 1) tolerance[1] else tolerance
  
  
  # Generate XML ----------------------------------------------------------------------------------
  
  xml <- c()
  
  opening <- paste0("",elemname," desc=\"",testname,"\" diff-type=\"",test0,"\" tolerance=\"",tolerance0,"\"")
  
  
  if(!is.null(data)) {
    xml <- c(xml, paste0("<",opening,">"))
    
    if(is.null(names(data)))
      names(data) <- as.character(1:length(data))
    
    for(c in names(data)) {
      
      c.attrs <- c()
      
      c.attrs <- c(c.attrs, paste0("name = \"",c,"\""))
      
      #c.attrs <- c(c.attrs, paste0("type = \"",class(data[,c]),"\""))
      
      if(length(test) != 1) {
        stopifnot(c %in% names(test))
        if(test[c] != test0)
          c.attrs <- c(c.attrs, paste0("desc = \"",test[c],"\""))
      }
      
      if(length(tolerance) != 1) {
        stopifnot(c %in% names(tolerance))
        if(tolerance[c] != tolerance0)
          c.attrs <- c(c.attrs, paste0("tolerance = \"",tolerance[c],"\""))
      }
      
      xml <- c(xml, paste0("  <element ",paste(c.attrs, collapse=" "),"/>"))
    }
    
    xml <- c(xml, paste0("</",elemname,">"))
    
  } else {
    xml <- c(xml, paste0("<",opening," />"))
  }
  
  
  if(printXML)
    cat(paste(xml, collapse="\n"))
  else
    return(xml)
}
