print.xtable <- function(x,...){

  UseMethod('print.xtable', x)

}

print.xtable.default <- function(x,...){

  xtable::print.xtable(x,...)

}
