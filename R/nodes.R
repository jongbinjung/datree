#' Create an outcome (value) node.
#'
#' \code{o_node} returns a value node that usually acts as a outcome leaf.
#' A value node should only be associated with a single outcome value.
#'
#' @param v Numeric. The value assigned to this outcome.
#' @param name Character. The name of this outcome
#' @return A node (\code{list}) that has a \code{value} and \code{name}.
#' @family node generating functions
#' @examples
#' v <- o_node(v = 5)
#' str(v)
#' @export
o_node <- function(v, name = "Outcome") {
  if (!lazyeval::is_formula(v)) {
    v = as.formula(paste("~", as.character(v)))
  }
  list(
    name = name,
    value = v
    )
}

#' Create an uncertainty node.
#'
#' \code{u_node} returns an uncertainty node that takes the weighted average of
#' it's children (e-value) as the value. An uncertainty node can have multiple
#' children.
#'
#' @param children List. A list of nodes that are the possibilities of this
#'   uncertainty.
#' @param probs List. The probabilities associated to each of the children as a
#'   list of functions. Should have the same number of elements as
#'   \code{children}. If the number of elements is equal to \code{children},
#'   they should sum to one.
#'   Otherwise, the u_node with silently do very bad things ...
#' @inheritParams o_node
#' @return A node (\code{list}) that has a \code{value} function and \code{name}.
#' @family node generating functions
#' @examples
#' # Generate a list of some outcome nodes to be children
#' children <- list(
#'   o_node(v = -10, name = "Rain"),
#'   o_node(v = 10, name = "Sunny")
#' )
#' x <- runif(1)
#' u <- u_node(children = children, probs = list(~ x, ~ 1 - x),
#'             name = "Weather")
#' str(u)
#' @export
u_node <- function(children, probs, name = "Uncertainty") {
  # if (any(probs < 0)) {
  #   stop(sprintf("Negative probabilities for u_node %s", name))
  # }
  #
  # if (sum(probs) > 1) {
  #   stop(sprintf("Probabilities for u_node %s sum to greater than 1", name))
  # }
  #
  n_possibilities <- length(children)

  if (length(probs) > n_possibilities) {
    stop(sprintf("Too many probabilities for u_node %s", name))
  }

  if (length(probs) < n_possibilities) {
    stop(sprintf("Not enough probabilities for u_node %s", name))
  #   remainder <- 1 - sum(probs)
  #   n_fills <- n_possibilities - length(probs)
  #   probs <- c(probs, rep(remainder / n_fills, n_fills))
  }
  #
  # if (any(probs == 0)) {
  #   # message(sprintf("Zero probabilities for u_node %s", name))
  # }

  c_values <- paste(purrr::map_chr(children, ~ lazyeval::f_text(.x$value)),
                    collapse = ", ")

  if (!lazyeval::is_formula(probs[[1]])) {
    # TODO: figure out a way to deal with non-formula probabilities.
    # For now, just stop
    stop(sprintf("Cannot deal with non-formula probabilities: u_node %s", name))
  }
  c_probs <- paste(purrr::map_chr(probs, ~ lazyeval::f_text(.x)),
                   collapse = ", ")
  fv <- as.formula(paste("~", paste("sum(c(", c_values, ")",
                                          "*",
                                          "c(", c_probs, "))")
                                    ))

  list(
    name = name,
    # type = "uncertainty",
    # children = children,
    # probabilities = probs,
    value = fv
    )
}

#' Create a decision node.
#'
#' \code{d_node} returns a decision node that takes the max value of it's
#' children as its value. A decision node can have multiple children.
#'
#' @param children List. A list of nodes that are the
#'   alternatives of this decision.
#' @inheritParams o_node
#' @return A node (\code{list}) that has a \code{value}, \code{name}, and the
#'   selected \code{decision}.
#' @family node generating functions
#' @examples
#' # Generate a list of some outcome nodes to be children
#' children <- list(
#'   o_node(v = -10, name = "Rain"),
#'   o_node(v = 10, name = "Sunny")
#' )
#' d <- d_node(children = children, name = "Weather")
#' str(d)
#' @export
d_node <- function(children, name = "Decision") {
  n_alternatives <- length(children)

  if (n_alternatives < 2) {
    stop(sprintf("Less than two alternatives for d_node %s", name))
  }

  # values <- purrr::map_dbl(children, "value")
  c_values <- purrr::map_chr(children, ~ lazyeval::f_text(.x$value))
  fv <- as.formula(paste("~", paste("max(", paste(c_values,
                                    collapse = ","), ")")))
  list(
    name = name,
    type = "decision",
    children = children,
    value = fv
    )
}
