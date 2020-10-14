# Function to select on how many cores next function should be executed
autoStopCluster <- function(cl) {	
  stopifnot(inherits(cl, "cluster"))	
  env <- new.env()	
  env$cluster <- cl	
  attr(cl, "gcMe") <- env	
  reg.finalizer(env, function(e) {	
    message("Finalizing cluster ...")	
    message(capture.output(print(e$cluster)))	
    try(parallel::stopCluster(e$cluster), silent = FALSE)	
    message("Finalizing cluster ... done")	
  })	
  cl	
}	
