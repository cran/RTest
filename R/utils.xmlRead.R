###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# RTest xmlRead Uitility Functions                                                                #
#                                                                                                 #
# This file defines a set of utility functions to read XML entries of general (pre-defined)       #
# data types.                                                                                     #
#                                                                                                 #
# Date:           25 - Jan - 2016                                                                 #
# Author:         Matthias Pfeifer (matthias.pfeifer@roche.com)                                   #
#                                                                                                 #
###################################################################################################


# xmlReadData_variable ##############################################################################

#' Read XML Data From Type 'xmlReadData_variable' as R Variable
#'
#' @param    xmlItem        (\code{XMLNode}) Object of class \code{XMLNode} that defines the 
#'                          a simple variable and fullfills XSD definition 'xmlReadData_variable'.
#' 
#' @return   (\code{vector})
#' 
#' @seealso \code{\link[XML]{XMLNode-class}}
#' 
#' @examples
#' data <- '<variable name="myvar" value="4" type="numeric"/>'
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlReadData_variable(item)
#' print("5 shall be the outcome")
#' print(value + 1)
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlReadData_variable <- function(xmlItem) {
  
  # Return null if xml item is not defined --------------------------------------------------------
  if(is.null(xmlItem))
    return(NULL)
  
  
  # Parse XML -------------------------------------------------------------------------------------
  
  # Get type of variable
  variable.type <- 
    if("type" %in% names(xmlAttrs(xmlItem))) xmlAttrs(xmlItem)[["type"]] else "character"
  
  # Get value of variable
  variable.value <- xmlAttrs(xmlItem)[["value"]]
  # Cast the type variable
  variable.value <- switch(variable.type,
    "logical"   = as.logical(variable.value),
    "integer"   = as.integer(variable.value),
    "numeric"   = 
      if(variable.value == "NULL") { 
        NULL 
      } else if(variable.value  == ""){
        vector("numeric")
      } else {
        as.numeric(variable.value)
      },
    "character" = as.character(variable.value))  
  
  
  # Return
  return(variable.value)
}

# xmlReadData_variable ############################################################################

#' Read XML Data From Type 'xmlReadData_image' as R Variable
#'
#' @param    xmlItem        (\code{XMLNode}) Object of class \code{XMLNode} that defines the 
#'                          a simple variable and fullfills XSD definition 'xmlReadData_variable'.
#' 
#' @return   (\code{vector})
#' 
#' @seealso \code{\link[XML]{XMLNode-class}}
#' 
#' @examples
#' location <- find.package("RTest")
#' data <- paste0(
#'			'<variable name="myvar" value="',location,
#'           '/images/Roche_Logo.png" type="character" image="TRUE"/>'
#'			)
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlReadData_image(item)
#' stopifnot(grepl("png",value$address))
#' 
#' @export 
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlReadData_image <- function(xmlItem) {
	
	# Return null if xml item is not defined ------------------------------------------------------
	if(is.null(xmlItem))
		return(NULL)
	
	
	# Parse XML -----------------------------------------------------------------------------------
	
	# Get type of variable
	variable.type <- 
			if("type" %in% names(xmlAttrs(xmlItem))) xmlAttrs(xmlItem)[["type"]] else "character"
	
	# Get value of variable
	variable.value <- xmlAttrs(xmlItem)[["value"]]
	
	if(!file.exists(variable.value)){
		stop(paste0("Image '",variable.value,"' cannot be found."))
	}
	# Cast the type variable
	variable.value <-  as.character(variable.value)
	
	tf <- paste0(tempfile(fileext=".png"))
	
	image_m <- magick::image_convert( magick::image_read(variable.value),format="png")
	
	image_write(image_m, tf)

	
	# Return the image link
	return(
			list(
					image = TRUE,
					address = tf)
	)
}


# xmlReadData_vector ##############################################################################

#' Read XML Data From Type 'xmlReadData_vector' as R Vector
#'
#' @param    xmlItem        (\code{XMLNode}) Object of class \code{XMLNode} that defines the 
#'                          a vector and fullfills XSD definition 'xmlReadData_vector'.
#' 
#' @return   (\code{vector})
#' 
#' @seealso \code{\link[XML]{XMLNode-class}}
#' 
#' @examples
#' data <- '<testvector type="numeric"><element>1</element><element>2</element></testvector>'
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlReadData_vector(item)
#' print("2 shall be the outcome")
#' print(length(value))
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlReadData_vector <- function(xmlItem) {
  
  # Return null if xml item is not defined --------------------------------------------------------
  if(is.null(xmlItem))
    return(NULL)
  
  
  # Get vector settings ---------------------------------------------------------------------------
  
  vector.type <- 
    if("type" %in% names(xmlAttrs(xmlItem))) xmlAttrs(xmlItem)[["type"]] else "character"
  
  
  # Get vector elements ---------------------------------------------------------------------------
  
  vector <- xmlSApply(xmlItem, xmlValue)
  
  
  # Cast data type --------------------------------------------------------------------------------
  
  # Suppres warnings during casing (e.g. for 'as.numeric' if NAs are introduced)
  suppressWarnings(
    vector <- switch(vector.type,
      "logical"   = as.integer(vector),
      "integer"   = as.integer(vector),
      "numeric"   = as.numeric(vector),
      "character" = as.character(vector))
  )
  
  
  # Set vector names ------------------------------------------------------------------------------
  
  # Get vector element names 
  vector.names <- 
    xmlSApply(xmlItem, 
      function(e) { if("name" %in% names(xmlAttrs(e))) xmlAttrs(e)[["name"]] else NA })
  
  # If any element name has been specified in the XML, set vector names
  if(length(which(is.na(vector.names))) != length(vector)) {   
    # If not all vector element names have been specified, fill up with string "elem", 
    # where XX is element no. 
    tmp.vector.names <- paste0("elem", 1:length(vector))
    vector.names[is.na(vector.names)] <- tmp.vector.names[is.na(vector.names)]
    
    # Set vector names
    names(vector) <- vector.names
  }
  
  # Return vector ---------------------------------------------------------------------------------
  return(vector)
}



# xmlReadData_data.frame ##########################################################################

#' Read XML Data From Type 'xmlReadData_data.frame' as R 'data.frame'
#'
#' @param    xmlItem        (\code{XMLNode}) Object of class \code{XMLNode} that defines the 
#'                          a data frame and fullfills XSD definition 'xmlReadData_data.frame'.
#' @param    na_to_none    (\code{logical}) Convert NAs to empty characters (i.e. '').
#' 
#' @return   (\code{data.frame})
#' 
#' @seealso \code{\link[XML]{XMLNode-class}}
#' @examples
#' data <- '<data.frame><col-defs>
#' <coldef name="Column1" type="character"/>
#' <coldef name="Column2" type="numeric"/>
#' </col-defs>
#' <row name="1"><cell>ID1</cell><cell>1</cell></row>
#' <row name="2"><cell>ID2</cell><cell>2.1</cell></row>
#' <row name="3"><cell>ID3</cell><cell>3.1</cell></row>
#' </data.frame>'
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlReadData_data.frame(item)
#' stopifnot(dim(value)[1]==3)
#' stopifnot(dim(value)[2]==2)
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlReadData_data.frame <- function(xmlItem, na_to_none=FALSE) {
  
  # Return null if xml item is not defined --------------------------------------------------------
  if(is.null(xmlItem))
    return(NULL)
  
  # Filter specific elements of the XML item ------------------------------------------------------
  
  # Column definitions
  xmlItem.coldefs <- xmlElementsByTagName(xmlItem, "col-defs")
  # Content definitions
  xmlItem.rows    <- 
    if(!is.null(xmlItem[["row"]])) xmlElementsByTagName(xmlItem, "row")
    else                          NULL
  
  # Table attributes
  xmlItem.attrs   <- xmlAttrs(xmlItem)
  
  # Read XML contents -----------------------------------------------------------------------------
  
  # Read column definitions - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  table.coldefs <-
    unlist(sapply(
        xmlItem.coldefs,
        function(xmlDefs) { xmlApply(xmlDefs, xmlAttrs) },
        simplify = FALSE, USE.NAMES = TRUE), recursive = FALSE) 

  table.coldefs <- Filter(Negate(is.null),  table.coldefs)

  # Read data table contents: iterate trhough xmlItems to read row and cell entries   - - - - - - -
  if(!is.null(xmlItem.rows)) {
    # If rows are available, parse contents
    
    table.rows <- 
      lapply(														         
        xmlItem.rows, 
        function(xmlRow) { 		         # Parse rows
          row <- 
            xmlSApply(
              xmlRow, 
              function(xmlCell) {     # Parse cells
				if(xmlValue(xmlCell)!="\\n"){
					
	                cell        <- c(xmlValue(xmlCell))
	                names(cell) <- xmlAttrs(xmlCell)[["ID"]]
					if(na_to_none){					
						if(cell=="NA"){
							return("")
						}
					}
	                return(cell)
				}
              },
              USE.NAMES = TRUE
            )
		  row <- Filter(Negate(is.null),  row)
          return(t(row))
        }
      )
	
    table.matrix <- do.call(rbind, table.rows)
    
    
    # Read row attributes - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    table.rowsAttrs <- 
      sapply(
        xmlItem.rows, xmlAttrs,
        simplify = FALSE, USE.NAMES = TRUE) 
    
    
    
    # Define table attributes -----------------------------------------------------------------------
    
    # Define table row names  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    table.defs.rownames <- 
      sapply(table.rowsAttrs, function(r) if("name" %in% names(r) ) r["name"] else NA)
    
    # Set rownames to numeric entry, if it has not been specified by the name flag
    if(length(which(is.na(table.defs.rownames))) != dim(table.matrix)[1])
      table.defs.rownames[is.na(table.defs.rownames)] <- 
        (1:dim(table.matrix)[1])[is.na(table.defs.rownames)]
    else
      table.defs.rownames <- NULL
    
    
    # Define table column names   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    table.defs.colnames <- 
      sapply(table.coldefs, function(c) if("name" %in% names(c) ) c["name"] else NA)
	

    # If not all column names have been specified, fill up with string "cellXX", where XX is cell no.
    #if(length(table.defs.colnames) < dim(table.matrix)[2])
    #  table.defs.colnames <- c(table.defs.colnames, 
    #    paste0("cell", (length(table.defs.colnames)+1):dim(table.matrix)[2]))
    
    # Set column names to numeric entry, if it has not been specified by the name flag
    table.defs.colnames[is.na(table.defs.colnames)] <- 
      (1:dim(table.matrix)[2])[is.na(table.defs.colnames)]

    table.coldefs <- lapply(1:length(table.coldefs),function(i){
				value <- table.coldefs[[i]]
				if(is.na(value["name"])){
					value <- c("type"=as.character(value),
							"name"=as.character(table.defs.colnames[i]))
				}
				value
			})
    # Create table ----------------------------------------------------------------------------------
    
    # Create data frame object
    table <- data.frame(
      table.matrix,
      stringsAsFactors = FALSE)
    
    # Specify settings of data frame object
    rownames(table) <- table.defs.rownames
    colnames(table) <- table.defs.colnames
    
  } else {
    # If no rows are available, create empty dataframe
    tmp        <- lapply(table.coldefs, function(d) character())
    names(tmp) <- sapply(table.coldefs, function(d) d["name"])

    table <- data.frame(
      tmp,
      stringsAsFactors = FALSE, check.names = FALSE)
  }

  
  # Define column types
  sapply(1:length(table.coldefs), 
    function(i) {
      
	  c <- table.coldefs[[i]]
		
      if("type" %in% names(c)) {
        if       (c["type"] == "numeric" || c["type"] == "integer") {
          
          # Casting (complex way to avoid warning if NAs in table), 
          # as the simple 'as.numeric(table[,c["name"]])' not possible
          tmp <- table[,i]
          tmp[table[,i] == "NA"] <- 0
          tmp <- suppressWarnings({ as.numeric(tmp) })
          tmp[table[,i] == "NA"] <- NA
          if(c["type"] == "integer") tmp <- as.integer(tmp)
          table[,i] <<- tmp
          
        } else if(c["type"] == "factor") {
          table[,i] <<- factor(table[,c["name"]])
          
        } else if(c["type"] == "logical") {
          table[,i] <<- as.logical(table[,c["name"]])
          
        } else if(c["type"] == "character") {
          
          # Casting (complex way to avoid warning if NAs in table), 
          # as the simple 'as.numeric(table[,c["name"]])' not possible
          tmp <- table[,i]
          tmp[table[,i] == "NA"] <- NA
          tmp[table[,i] == "character(0)"] <- ""
          
          table[,i] <<- as.character(tmp)
          
        } else {
          stop("Column definition type '",c["type"],"' is not defined.")
        }
      }
    })
  
  # Add data metaiformation / attributes to data.frame object
  for(a in names(xmlItem.attrs)) {
    attributes(table)[[a]] <- xmlItem.attrs[a]
  }
  
  # Return table ----------------------------------------------------------------------------------
  return(table)
}





# xmlReadData_list ################################################################################


#' Read XML Data From Type 'xmlReadData_list' as R 'list' (DUMMY)
#' 
#' DUMMY: IMPLEMENTATION HAS TO BE DONE!!
#'
#' @param    xmlItem        (\code{XMLNode}) Object of class \code{XMLNode} that defines the 
#'                          a list object and fullfills XSD definition 'xmlReadData_list'.
#' 
#' @return   (\code{data.frame})
#' 
#' @seealso \code{\link[XML]{XMLNode-class}}
#' @examples
#' data <- '<mylist>
#' 
#' <data.frame name="ITEM1">
#' <col-defs>
#'   <coldef name="Column1" type="character"/>
#'   <coldef name="Column2" type="numeric"/>
#' </col-defs>
#' <row name="1"><cell>ID1</cell><cell>1</cell></row>
#' <row name="2"><cell>ID2</cell><cell>2.1</cell></row>
#' <row name="3"><cell>ID3</cell><cell>3.1</cell></row>
#' </data.frame>
#' <vector name="ITEM2" type="numeric"><element>1</element><element>2</element></vector>
#' 
#' </mylist>
#' '
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlReadData_list(item)
#' stopifnot(names(value)[1]=="ITEM1")
#' stopifnot(length(value[[2]])==2)
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlReadData_list <- function(xmlItem) {
  
  # Return null if xml item is not defined --------------------------------------------------------
  if(is.null(xmlItem))
    return(NULL)
  
  
  # TODO: implement xmlReadData_list
  res <- xmlRead.default(xmlItem)
  
  
  # Return list -----------------------------------------------------------------------------------  
  return(res)
}
# xmlReadData_text ##############################################################################

#' Read XML Data From Type 'xmlReadData_text' as R Variable
#'
#' @param    xmlItem        (\code{XMLNode}) Object of class \code{XMLNode} that defines the 
#'                          a simple variable and fullfills XSD definition 'xmlReadData_text'.
#' 
#' @return   (\code{vector})
#' 
#' @seealso \code{\link[XML]{XMLNode-class}}
#' @examples
#' data <- '<variable type="character">My text is awesome</variable>'
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlReadData_text(item)
#' stopifnot(value=="My text is awesome")
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlReadData_text <- function(xmlItem) {
  
  # Return null if xml item is not defined --------------------------------------------------------
  if(is.null(xmlItem))
    return(NULL)
  
  
  # Parse XML -------------------------------------------------------------------------------------
  
  # Get type of variable
  variable.type <- 
    if("type" %in% names(xmlAttrs(xmlItem))) xmlAttrs(xmlItem)[["type"]] else "character"
  
  # Get value of variable
  variable.value <- xmlValue(xmlItem)
  
  # Cast the type variable
  variable.value <- switch(variable.type,
    "logical"   = as.logical(variable.value),
    "integer"   = as.integer(variable.value),
    "numeric"   = 
      if(variable.value == "NULL") { 
        NULL 
      } else if(variable.value  == ""){
        vector("numeric")
      } else {
        as.numeric(variable.value)
      },
    "character" = as.character(variable.value))  
  
  
  # Return
  return(variable.value)
}





# xmlRead.default ################################################################################


#' General import function to reads XML data of different types
#' 
#' This function controls the import of input data set. 
#' 
#' Based on the tag name of the input data definition in the XML file, the corresponding 
#' \code{readXMLData_*} function is called, whereby \code{*} is a placeholder for the data type
#' definition in the XML scheme. For example, for XML definitions following the \code{data.frame} 
#' specification, a function \code{readXMLData_data.frame <- function(xmlDataItem)} is expected, 
#' which implements the XML parser for \code{data.frame}s and returns the data as R object. 
#'
#' @param    xmlItem        (\code{XMLNode}) Object of class \code{XMLNode} that defines the 
#'                          a list object and fullfills XSD definition 'xmlReadData_list'.
#' 
#' @return   (\code{data.frame}) or (\code{variable}) or (\code{vector}) or a named list of 
#'   all imported input datasets.
#' 
#' @seealso \code{\link[XML]{XMLNode-class}}
#' @examples
#' data <- '<text type="character">My text is awesome</text>'
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlRead.default(item)
#' stopifnot(value=="My text is awesome")
#' 
#' data <- '<list><data.frame><col-defs>
#' <coldef name="Column1" type="character"/>
#' <coldef name="Column2" type="numeric"/>
#' </col-defs>
#' <row name="1"><cell>ID1</cell><cell>1</cell></row>
#' <row name="2"><cell>ID2</cell><cell>2.1</cell></row>
#' <row name="3"><cell>ID3</cell><cell>3.1</cell></row>
#' </data.frame></list>'
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlRead.default(item)
#' stopifnot(dim(value)[1]==3)
#' stopifnot(dim(value)[2]==2)
#' 
#' data <- '<variable type="character" value="My text is awesome"/>'
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlRead.default(item)
#' stopifnot(value=="My text is awesome")
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
xmlRead.default <- function(xmlItem)
{  		
  result <- xmlApply(xmlItem, 
    function(xmlDataItem){
      
      datatypename <- xmlName(xmlDataItem)
      
      readfun <- 
        if(datatypename == "list") "xmlRead.default"
        else                       paste0("xmlReadData_",datatypename)
      
      if(exists(readfun)) {
        do.call(readfun, args = list(xmlDataItem))
      } else { 
        stop("No XML read function defined for XML element '",datatypename,"' ",
          "(function '",readfun,"' expected).")
      }
      
    }
  )
  
  #if(length(result) == 1){
  #  return(result[[1]])
  #}else{
  names(result) <- 
    xmlSApply(xmlItem, function(e) 
        if("name" %in% names(xmlAttrs(e))) xmlAttrs(e)[["name"]] else "data"
    )
  return(result)
  #}
}

#' Read an unidentified List of Data Types from TestCase params
#' 
#' @param xmlItem     Object of class \code{XMLNode} that defines a list
#' 		of xmlTags that contain just elements defined in
#'      RTest XSD (list, variable, text, data.frame, vector)
#' 
#' @return args (\code{list}) All the elements named by their tag and containing 
#' 		the value defined in the xml
#' 
#' @export
#' @examples
#' data <- '<mylist>
#' 
#' <inputitem1>
#' <col-defs>
#'   <coldef name="Column1" type="character"/>
#'   <coldef name="Column2" type="numeric"/>
#' </col-defs>
#' <row name="1"><cell>ID1</cell><cell>1</cell></row>
#' <row name="2"><cell>ID2</cell><cell>2.1</cell></row>
#' <row name="3"><cell>ID3</cell><cell>3.1</cell></row>
#' </inputitem1>
#' <inputitem2 type="numeric"><element>1</element><element>2</element></inputitem2>
#' 
#' </mylist>
#' '
#' item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))
#' value <- RTest::xmlReadData_to_list(item)
#' 
#' stopifnot(names(value)[1]=="inputitem1")
#' stopifnot(names(value)[2]=="inputitem2")
#' stopifnot(length(value[[2]])==2)
#' @author Sebastian Wolf <sebastian.wolf@@roche.com>
xmlReadData_to_list <- function(xmlItem){
	
	if(is.null(xmlItem)){
		return(list())
	}
	
	param_list <- xmlChildren(xmlItem)
	
	args <- list()
	
	for(param in param_list){
		if(length(xmlChildren(param))>=1){
			if(names(xmlChildren(param,addNames=T))[1]=="element"){
				entry <- ifelse(xmlName(param)=="vector",
					xmlAttrs(param)[["name"]],
					xmlName(param))
				args[[entry]] <- xmlReadData_vector(param) 
				
			}else if(names(xmlChildren(param,addNames=T))[1]=="col-defs"){
				entry <- ifelse(xmlName(param)=="data.frame",
						xmlAttrs(param)[["name"]],
						xmlName(param))
				args[[entry]] <- xmlReadData_data.frame(param)
			}else if(names(xmlChildren(param,addNames=T))[1]=="text"){
				entry <- ifelse(xmlName(param)=="text",
						xmlAttrs(param)[["name"]],
						xmlName(param))
				args[[entry]] <- xmlReadData_text(param)
			}else{
				entry <- ifelse(xmlName(param)=="list",
						xmlAttrs(param)[["name"]],
						xmlName(param))
				if(xmlName(param)=="list"){
					args[[entry]] <- xmlReadData_list(param)
				}else{
					args[[entry]] <- xmlReadData_to_list(param)
					
				}
			}
		}else{
			if(xmlName(param)=="image"){
				args[[xmlName(param)]] <- xmlReadData_image(param)
			}else if(!is.null(xmlAttrs(param)["value"])){
				entry <- ifelse(xmlName(param)=="variable",
						xmlAttrs(param)[["name"]],
						xmlName(param))
				args[[entry]] <- xmlReadData_variable(param)
			}else{
				args[[xmlName(param)]] <- xmlReadData_text(param)
			}
		}
	}
	
	return(args)
}






