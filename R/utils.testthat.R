###################################################################################################
#                                              RTest                                              #
###################################################################################################
#                                                                                                 #
# Changing testthat (>2.0) functions to work in RTest                                             #
#                                                                                                 #
# Date:           22 - Dec - 2018                                                                 #
# Author:         Sebastian Wolf sebastian@mail-wolf.de                                           #
#                                                                                                 #
###################################################################################################

#' testthat function: Function to return expectation after being executed
#' 
#' In comparison to \code{testthat} this function always exports the message, even
#' in case of success
#' 
#' @param x object to test for class membership
#' @param message \code{character} character string to be the execution message
#' @param info Additional information. Included for backward compatibility
#'   only and new expectations should not use it.
#' @param srcref Only needed in very rare circumstances where you need to
#'   forward a srcref captured elsewhere.
#' @param ... Unused, just defined inside testthat
#' 
#' @author Sebastian Wolf
#' @export
as.expectation.logical <- function(x, message, ..., srcref = NULL, info = NULL) {
	type <- if (x) "success" else "failure"
	`%:::%` = function(pkg, fun) get(fun, envir = asNamespace(pkg),
				inherits = FALSE)
	
	add_info <-  "testthat" %:::% "add_info"
	
	message <- add_info(message, info)
	expectation(type, message, srcref = srcref)
}

#' Method capturing the run
#' 
#' @param quo an \code{rlang} quo
#' @param capture A function to derive the output / warnings / messages
#' 	of the function as e.g. \code{evaluate_promise}
#' @param label \code{character} A label for the evaluated value
#' @return \code{act} A list including the label (\code{lab}),
#' 	a caputre of the function (\code{cap}) and the code call
#'  itself as (\code{..})
#' 
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de})
quasi_capture <- function(quo, capture, label = NULL) {
	
	force_implementation <- if(!is.null(options("force_implementation")[[1]])){
		as.logical(options("force_implementation"))
	}else{
		FALSE
	}
	
	# In case quasi_capture is existing as a function
	# from testthat (>2.0)
	if(as.numeric(
					stringr::str_extract(
							as.character(packageVersion("testthat")),"[0-9]{1,2}\\.[0-9]{1,2}")) >=
			2 && !force_implementation){
		
		# Import quasi capture from testthat
		`%:::%` = function(pkg, fun) get(fun, envir = asNamespace(pkg),
					inherits = FALSE)
		
		quasi_capture <-  "testthat" %:::% "quasi_capture"
		
		return(quasi_capture(quo=quo,capture=capture,label=label))
	}else{
		
		`%||%` = rlang::`%||%`
		# quasi capture source code from testthat 2.0.0.9000
		act <- list()
		act$lab <- label %||% rlang::quo_label(quo)
		act$cap <- capture(act$val <- rlang::eval_bare(rlang::get_expr(quo), rlang::get_env(quo)))
		return(act)
		
	}
	
}

#' Expect a function call to run silent
#' 
#' In case the function call is not silent, a message including all outputs, messages, warnings
#' is given.
#' 
#' @export
#' @importFrom glue glue glue_collapse
#' @importFrom rlang enquo
#' 
#' @param object executable function call
#' 
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
expect_silent_RTest <- function(object) {
	
	act <- quasi_capture(
			rlang::enquo(object),
			testthat::evaluate_promise)

	outputs <- c(
			outputs = if (!identical(act$cap$output, "")) act$cap$output,
			warnings = if (length(act$cap$warnings) > 0) act$cap$warnings %>% 
						strsplit(split="\n") %>% unlist(),
			messages = if (length(act$cap$messages) > 0) act$cap$messages %>% 
						strsplit(split="\n") %>% unlist()
	)
	
	# Create a phrase that contains which outputs and warnings were created
	outputs_string <- glue::glue("{names(outputs)} = '{outputs}'") %>%
			glue::glue_collapse(sep="\n")
	
	# Create the output for the test
	expect(
			length(outputs) == 0,
			#sprintf("%s produced %s.", act$lab, paste(outputs, collapse = ", ")),
			glue::glue("{act$lab} produced:\n {outputs_string}")
	)
	
	invisible(act$cap$result)
}


#'@rdname expect_less_more
expect_less_than <- function(...){
	expect_lt(...)
} 

#'@rdname expect_less_more
expect_more_than <- function(...){
	expect_gt(...)
} 

#'@rdname expect_less_more
expect_lt <- function(...){
	force_implementation <- if(!is.null(options("force_implementation")[[1]])){
				as.logical(options("force_implementation"))
			}else{
				FALSE
			}
	
	if(as.numeric(
					stringr::str_extract(
							as.character(packageVersion("testthat")),"[0-9]{1,2}\\.[0-9]{1,2}")) >=
			2 && !force_implementation){
		a <- list(...)
		a$info <- NULL
		do.call(getfun("testthat::expect_lt"),a)
		
	}else{
		testthat::expect_less_than(...)
	}
}
#'@name expect_less_more
#'@aliases expect_gt
#'@param ... Any parameter sent to expect_gt or expect_lt of testthat
#'@rdname expect_less_more
expect_gt <- function(...){
	force_implementation <- if(!is.null(options("force_implementation")[[1]])){
				as.logical(options("force_implementation"))
			}else{
				FALSE
			}
	
	if(as.numeric(
					stringr::str_extract(
							as.character(packageVersion("testthat")),"[0-9]{1,2}\\.[0-9]{1,2}")) >=
			2 && !force_implementation){
		a <- list(...)
		a$info <- NULL
		do.call(getfun("testthat::expect_gt"),a)
	}else{
		testthat::expect_more_than(...)
	}
}