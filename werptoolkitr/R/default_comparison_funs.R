# default functions for comparisons. These are mostly just easily-callable
# wrappers for basic arithmetic, though they could be more complex. The main
# thing is that they always have the data argument first as x and the reference
# argument second as y.

difference <- function(x,y) {x-y}

# Relative provides an add_eps argument to prevent divide by zero. need to add
# to both, because otherwise at low flows the others drop below when they
# shouldn't.
relative <- function(x,y, add_eps = 0) {(x + add_eps)/(y + add_eps)}
