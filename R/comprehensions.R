# https://gist.github.com/klmr/35a13344080e71bf8c34
# Dummy object, only required for name resolution.


#' @export
set<- structure(list(), class = 'set')

print.set = function (x, ...) invisible(x)

#' Title
#'
#' @param set
#' @param expr
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
`[.set` = function (set, expr, filter) {
  expr = substitute(expr)
  filter = substitute(filter)
  stopifnot(identical(expr[[1]], quote(`<-`)))
  stopifnot(identical(expr[[2]][[1]], quote(`|`)))
  map = expr[[2]][[2]]
  gen = expr[[3]]
  var = expr[[2]][[3]]
  stopifnot(length(var) == 1 && is.name(var))
  var = deparse(var, backtick = TRUE)
  range = eval.parent(gen)

  closure = function (formals, body, envir)
    eval(call('function', as.pairlist(formals), body), envir)

  formals = as.pairlist(setNames(c(quote(expr = )), var))

  f = closure(formals, filter, parent.frame())
  g = closure(formals, map, parent.frame())
  sapply(Filter(f, range), g)
}


# Example:

#set[x + 1 | x <- 1 : 10, x %% 2 == 0]
# [1]  3  5  7  9 11
