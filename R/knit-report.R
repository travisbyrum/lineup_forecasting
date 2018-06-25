args <- commandArgs(TRUE)

if (!length(args))
  stop('Directory path not provided')

RNW_PATH <- as.character(args[1])

knitr::knit(RNW_PATH)
