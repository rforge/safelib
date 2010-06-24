\name{resetLibraryPaths}
\alias{resetLibraryPaths}
\title{Reset Library Paths}
\description{
  Set R's library paths back to the value it had before
  \code{setLibraryPaths} was called.
}
\usage{
resetLibraryPaths(noaction = FALSE, resetOriginal = FALSE)
}
\arguments{
  \item{noaction}{logical. If \code{TRUE} do not change the library
    paths --- just print what the change would be.}
  
  \item{resetOriginal}{logical. Only intended for testing purpose.}
}
\details{
  Use this function to undo changes made with \code{setLibraryPaths}.
}
\value{
  A character vector with the reset library paths. Will always return
  R's default library paths as they are initialized at the beginning of
  a R session, typically
  \code{c(Sys.getenv("R_LIBS"), .Library, .Library.site)}.
}
\author{Lars Hansen}
\seealso{
  \code{\link{setLibraryPaths}},
  \code{\link{getLibraryPaths}},
  \code{\link{.libPaths}}
}
\examples{
## Set library paths to point to all the latest packages.
setLibraryPaths()

## Go back to default R library path.
resetLibraryPaths()
}
\keyword{utilities}
