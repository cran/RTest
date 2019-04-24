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

#' Method capturing the run
#'
#' @param quo an \code{rlang} quo
#' @param capture A function to derive the output / warnings / messages
#'   of the function as e.g. \code{evaluate_promise}
#' @param label \code{character} A label for the evaluated value
#' @return \code{act} A list including the label (\code{lab}),
#'   a caputre of the function (\code{cap}) and the code call
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

    return(quasi_capture(.quo=quo, .capture=capture, .label = deparse(quo)))
  }else{

      act <- list()
      `%||%` = rlang::`%||%`
      act$lab <- deparse(quo) %||% quo_label(quo)
      act$cap <- capture(act$val <- eval_bare( rlang::get_expr(quo), rlang::get_env(quo)))

      return(act)


    # quasi capture source code from testthat 2.0.0.9000
#    act <- list()
#    act$lab <- label %||% rlang::quo_label(quo)
#    act$cap <- capture(act$val <- rlang::eval_bare(rlang::get_expr(quo), rlang::get_env(quo)))
#    return(act)

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
      testthat::evaluate_promise,
      label = deparse(object)
      )

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
#'Expect less or more
#'
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
