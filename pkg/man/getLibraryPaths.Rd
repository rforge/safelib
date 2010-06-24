\name{getLibraryPaths}
\alias{getLibraryPaths}
\title{Get Library Paths}
\description{
  Return the library tree paths that R knows about.
}
\usage{
getLibraryPaths()
}
\details{
  This is where R finds packages to load. Only packages
  in these paths can be loaded. Identical to \code{.libPaths}
  function. Included in this package for completeness. It is the
  complement of \code{setLibraryPaths} function.
}
\value{
  The paths are returned in a character vector.
}
\author{Lars Hansen}
\seealso{
  \code{\link{setLibraryPaths}},
  \code{\link{resetLibraryPaths}},
  \code{\link{.libPaths}}
}
\examples{
## Set library paths to point to all the lastest packages.
setLibraryPaths()

## Check the library paths.
getLibraryPaths()
}
\keyword{utilities}
