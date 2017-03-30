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
  list(
    value = v,
    name = name
    )
}

#' Create an uncertainty node.
#'
#' \code{u_node} returns an uncertainty node that takes the weighted average of
#' it's children (e-value) as the value.
#' An uncertainty node can have multiple children.
#'
#' @param children List. A list of nodes that are the
#'   possibilities of this uncertainty.
#' @param probs Numeric vector. The probabilities associated to each of the
#'   children. Should have the equal or less elements than \code{children}.
#'   If the number of elements is equal to \code{children}, they should sum to
#'   one. If the number of elements is less than that of \code{children}, the
#'   remaining values are filled out equally, such that the total probability
#'   sums to one.
#' @inheritParams o_node
#' @return A node (\code{list}) that has a \code{value} and \code{name}.
#' @family node generating functions
#' @examples
#' # Generate a list of some outcome nodes to be children
#' children <- list(
#'   o_node(v = -10, name = "Rain"),
#'   o_node(v = 10, name = "Sunny")
#' )
#' u <- u_node(children = children, probs = c(.2, .8), name = "Weather")
#' str(u)
#' @export
u_node <- function(children, probs, name = "Uncertainty") {
  if (any(probs < 0)) {
    stop(sprintf("Negative probabilities for u_node %s", name))
  }

  if (sum(probs) > 1) {
    stop(sprintf("Probabilities for u_node %s sum to greater than 1", name))
  }

  n_possibilities <- length(children)

  if (length(probs) > n_possibilities) {
    stop(sprintf("Too many probabilities for u_node %s", name))
  }


  if (length(probs) < n_possibilities) {
    warning(sprintf("Adding extra probabilities for u_node %s", name))
    remainder <- 1 - sum(probs)
    n_fills <- n_possibilities - length(probs)
    probs <- c(probs, rep(remainder / n_fills, n_fills))
  }

  if (any(probs == 0)) {
    warning(sprintf("Zero probabilities for u_node %s", name))
  }

  values <- purrr::map_dbl(children, "value")
  v <- sum(values * probs)

  list(
    value = v,
    name = name,
    type = "uncertainty",
    children = children,
    probabilities = probs
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
#' str(u)
#' @export
d_node <- function(children, name = "Decision") {
  n_alternatives <- length(children)

  if (n_alternatives < 2) {
    stop(sprintf("Less than two alternatives for d_node %s", name))
  }

  values <- purrr::map_dbl(children, "value")

  decision <- children[[which.max(values)]]

  list(
    value = decision$value,
    name = name,
    type = "decision",
    children = children,
    decision = decision
    )
}
