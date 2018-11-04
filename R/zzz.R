# .First.lib <- function(libname, pkgname, where) {
#   where <- match(paste("package:", pkgname, sep=""), search())
#   .initClasses(where)
# }
# nocov start
.onLoad <- function(libname, pkgname) {
  .initClasses()
}
# nocov end
