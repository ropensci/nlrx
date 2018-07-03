# .First.lib <- function(libname, pkgname, where) {
#   where <- match(paste("package:", pkgname, sep=""), search())
#   .initClasses(where)
# }

.onLoad <- function(libname, pkgname) {
  .initClasses()
}
