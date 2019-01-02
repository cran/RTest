###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# Example functions for "generic" adapter                                                     #
#                                                                                                 #
# Date:           09 - Dec - 2018                                                                 #
# Author:         Sebastian Wolf (sebastian@mail-wolf.de)                                   #
#                                                                                                 #
###################################################################################################

#' Function changing a data frame by adding a column
#' 
#' @param data (\code{data.frame}) Any data frame with numeric values
#' @param mult (\code{numeric}) Any numeric value (length == 1)
#' 
#' @return A data.frame with an additional column sum that
#'   is the rowwise sum multiplied by \code{mult}
#' 
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
example_data_frame <- function(
		data = data.frame(x=c(1,2),y=c(1,2)),
		mult = 1
		){
	
	data[,"sum"] <- apply(data,1,function(x){sum(x)*mult})
	return(data)
}

#' Function returning the Roche logo as an image at tempdir
#' 
#' @param name \code{character} The name of the output image
#' 
#' @return The file path to a temporar file with the given
#' 	name that will contain the Roche_Logo.png that
#'  comes with RTest/images
#' 
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
example_image <- function(name="Roche_logo.png"){
	
	input <- list.files(find.package("RTest"),recursive = T,pattern="Roche_Logo",full.names=T)[1]
	
		
	output <- file.path(tempdir(),name)
	
	file.copy(from = input, to=output,overwrite=T)
	
	return(output)
}

#' Function returning a list with three values
#' 
#' @param name_1 (\code{character}) Name of the first list element
#' @param value_2 (\code{numeric}) Value of the second list element
#' 
#' @return A list with three elements, a generic data frame
#'  inside the element \code{data.frame} a list element with
#'  the value "VALUE1" inside the element with name of
#'  parameter \code{name_1} and an item with the name "NAME2"
#'  and the value of \code{value_2} inside.
#' 
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
example_list <- function(name_1 = "NAME1",value_2 = 1){
	
	return_value <- list()
	
	return_value[[name_1]] <- "VALUE1"
	return_value[["NAME2"]] <- value_2
	return_value[["data.frame"]] <- data.frame(x=c(1,2),y=c(1,2))
	
	return(return_value)
	
}

#' Function returning a list with three values and large DF
#' 
#' @param name_1 (\code{character}) Name of the first list element
#' @param value_2 (\code{numeric}) Value of the second list element
#' 
#' @return A list with three elements, a generic data frame
#'  inside the element \code{data.frame} a list element with
#'  the value "VALUE1" inside the element with name of
#'  parameter \code{name_1} and an item with the name "NAME2"
#'  and the value of \code{value_2} inside.
#' 
#' @name example_list_large
#' 
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
example_list_large <- function(name_1 = "NAME1",value_2 = 1){
	
	warning('{"Testing":["might take long"]}')
	
	return_value <- list()
	
	return_value[[name_1]] <- "VALUE1"
	return_value[["NAME2"]] <- value_2
	return_value[["data.frame"]] <- data.frame(x=rep(1,290),y=rep(2,290))
	
	return(return_value)
	
}

#' Function returning a character vector of length "rep"
#'
#' @param rep (\code{numeric}) Number of repetitions
#' 
#' @return character vector containing \code{rep} times
#'   the word "RTest"
#'  
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
example_vector <- function(rep = 5){
	rep("RTest",times=rep)
}

#' Function returning relative difference of X and Y
#' 
#' @param x (\code{numeric}) X-value
#' @param y (\code{numeric}) Y-value
#' 
#' @return \code{(X-Y)/(X)}
#' 
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
example_variable <- function(x=1.2,y=1){
	(x-y)/(x)
}