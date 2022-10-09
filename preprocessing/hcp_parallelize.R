# https://furrr.futureverse.org/articles/progress.html
require(furrr)
require(progressr)
options(future.globals.maxSize= (8*1024*1024)^2) #4GB

plan(multisession, workers = 2)