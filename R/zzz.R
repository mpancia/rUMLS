.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to rUMLS. This package requires a UMLS username and password. Run auth_UMLS() to authenticate.")
}
