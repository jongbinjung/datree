#' Evaluate the value of a node
#'
#' \code{evaluate} uses \code{lazyeval} to evaluate the value function of any
#' given node. Essentially a shortcut for calling \code{lazyeval::f_eval} on the
#' \code{$value} object of a node.
#'
#' @param node Node list. The node to evaluate.
#' @param data List-like. The data to be used when evaluating the value
#'   function. Depending on the value function, omitting the data may result in
#'   an error.
#' @param verbose Logical. Whether or not to print the full value formula.
#' @return Evaluated numeric value.
#' @family node processing functions
#' @examples
#' v <- o_node(v = ~ .data$x + y)
#' y <- 10
#' evaluate(v, data = list(x = 10))
#' evaluate(v, data = list(x = 1))
#' @export
evaluate <- function(node, data = NULL, verbose = FALSE) {
  fv <- node$value
  if (!lazyeval::is_formula(fv)) {
    stop(sprintf("Cannot evaluate non-formula value in node %s", node$name))
  }

  return(lazyeval::f_eval(fv, data))
}

#' Generate data frame of alternative values for a decision node
#'
#' \code{decide} uses \code{lazyeval} to evaluate the value functions of a
#' decision node's children, i.e., alternatives.
#'
#' @param node \code{d_node}. The decision node to evaluate.
#' @param data List-like. The data to be used when evaluating the value
#'   functions. Depending on the value functions, omitting the data may result
#'   in an error.
#' @return Data frame of alternatives and respective values.
#' @family node processing functions
#' @examples
#' a <- o_node(v = ~ .data$x + y, "Add")
#' b <- o_node(v = ~ .data$x * y, "Multiply")
#' d <- d_node(children = list(a, b))
#' y <- 10
#' decide(v, data = list(x = 10))
#' decide(v, data = list(x = 1))
#' @export
decide <- function(node, data = NULL) {
  children <- node$children
  purrr::map_df(children, ~ tibble::tibble(
    alternative = .x$name,
    evalue = evaluate(.x, data)
  ))
}
