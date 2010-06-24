\name{setLibraryPaths}
\alias{setLibraryPaths}
\title{Set Library Paths}
\description{
  Makes specific packages available for loading by setting the R library paths.
  Will recursively search for packages in the directories pointed to by
  R's default library paths. Call \code{setLibraryPaths} before calling
  \code{library} and \code{require}.
}
\usage{
setLibraryPaths(spec = NULL, versions = NULL, noaction = FALSE)
}
\arguments{
  \item{spec}{character string or NULL (default). Valid values are "dev" and "rel".}
  \item{versions}{named character or numeric vector with versions to use
    or NULL (default).}
  \item{noaction}{logical. If \code{TRUE} do not change the library
    paths --- just print what the change would be.}
}
\details{
  The packages that are made available thought the library paths is
  controlled by \code{spec} or \code{versions} or both.

  If \code{spec} is \code{NULL} the lastest packages will be made
  available. All packages found by recursively searching the in
  the directories pointed to by R's default library paths
  (see \code{resetLibraryPaths}) will be concidered.
  If \code{spec} is "dev", the latest development packages
  will be made available. All packaged with version numbers less
  that "1.0-0" are considered development packages. If \code{spec}
  is "rel", the latest released packages will be made available.
  A released package has a version number of "1.0-0" or higher.
  The latest package is defined as the package with the highest
  version number . The version numbers are read from the package
  \file{DESCRIPTION} files.

  \code{versions} gives a way to override what is specified in
  \code{spec}. Pass a named vector of versions that must be made
  available. The names are package names and the values are partially
  or fully specified version numbers (see examples below).
}
\value{
  A character vector with the new library paths.
}
\author{Lars Hansen}
\seealso{
  \code{\link{getLibraryPaths}},
  \code{\link{resetLibraryPaths}},
  \code{\link{.libPaths}},
  \code{\link{installedPackages}}  
}
\examples{
## Use latest packages.
setLibraryPaths()

## Use latest released packages.
setLibraryPaths("rel")

## Use latest development packages.
setLibraryPaths("dev")

\dontrun{
## In general use released packages, but if they exist
## use version "1.1-1" of "a" package, the latest "0.5.*" version
## of "b" and the latest "0.*-*" version or "c".
setLibraryPaths("rel", versions = c(a = "1.1-1", b = 0.5, c = 0))

## First call a function in the latest development version of
## "Rdrmaa" package; then in the latest released version.
setLibraryPaths("dev")
library(Rdmaa)

drmaa_version()

unloadNamespace("Rdrmaa")
setLibraryPaths("rel")
library(Rdmaa)

drmaa_version()
}
}
\keyword{utilities}
