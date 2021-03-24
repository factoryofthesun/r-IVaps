.onLoad <- function(libname, pkgname){
  op <- options()
  op.devtools <- list(
    devtools.name = "qps",
    devtools.desc.author = "Richard Liu <richard.liu@yale.edu> [aut, cre]"
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}
