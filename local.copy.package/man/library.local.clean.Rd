\name{library.local.clean}
\alias{library.local.clean}
\title{
  Clean out obselete local copies of packages.
}
\description{
  Clean out obselete local copies of packages.  Obselete local copies
  are copies of a package that are not the most recent and are older
  than a specified age.  The default age is the time of the last reboot,
  because non-most recent packages that old should definitely not be
  in use by any running R session.
}
\usage{
library.local.clean(older.than = NULL, local.lib.locs = c(Sys.getenv("TMPDIR"), Sys.getenv("TMP"), Sys.getenv("TEMP")), verbose = TRUE)
}
\arguments{
  \item{older.than}{
    Specification of the age of packages to clean out.  Either a
    relative time, like '5 days', or a date/time parseable by
    \code{as.POSIXct}.
    The default value is the time of the most recent reboot.
}
\item{local.lib.locs}{
  Where to look for local copies of packages.
}
\item{verbose}{
  Output information about which packages are being removed.
}
}
\value{
  An invisible NULL value.
}
\author{
  Tony Plate \email{tplate@acm.org}
}
\examples{
\dontrun{local.library.clean()}
}
\keyword{ misc }
