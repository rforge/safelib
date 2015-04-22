\name{library.local}
\alias{library.local}
\alias{require.local}
\title{
Make a local copy of a package and load it.
}
\description{
  Make a local copy of a package and load
  the copy.  Loading a package this way makes it
  possible to update packages in the common library
  while running R sessions have the package attached.
  Only makes a new local copy of the package if the package
  has been updated in the central library.
}
\usage{
library.local(package, character.only=FALSE, ..., lib.loc = NULL,
              binary.only=getOption('library.local.binary.only', TRUE),
              compare.method = c('cached.info', 'md5sum'), local.deps = TRUE,
              local.lib.locs = c(Sys.getenv("TMPDIR"), Sys.getenv("TMP"), Sys.getenv("TEMP")),
              pkg.subdirs = c("R", "libs", "data"),
              verbose = FALSE, dry.run = FALSE)
require.local(package, lib.loc = NULL, quietly = FALSE, warn.conflicts = TRUE,
              keep.source = getOption("keep.source.pkgs"), character.only = FALSE,
              binary.only=getOption('library.local.binary.only', TRUE),
              local.deps = TRUE, local.lib.locs=c(Sys.getenv('TMPDIR'), Sys.getenv('TMP'), Sys.getenv('TEMP')))
}
\arguments{
  \item{package}{ name of the package (name or quoted string).  As with
    \code{library} if the argument is a variable whose value is the name
    of the package, supplied \code{character.only=TRUE}.}
  \item{character.only}{ a logical indicating whether \code{package} can be
          assumed to be a character string}
  \item{\dots}{ arguments to pass to \code{library()}}
  \item{binary.only}{ only make copies of packages with binaries in them (e.g., DLLs)}
  \item{lib.loc}{ where to look for packages}
  \item{compare.method}{ how to check whether an existing local copy is
    the same as the library version.  For \code{cached.info}, file size
    and creation times are compared.  If these change in the central
    library, a new copy will be created.}
  \item{local.deps}{ if \code{TRUE}, work out dependencies, and load
    them using \code{library.local()} too}
  \item{local.lib.locs}{ directory where to make local copies}
  \item{pkg.subdirs}{ which subdirectories of a package to check when
    seeing if an existing copy of a package is up-to-date}
  \item{verbose}{ Can be \code{TRUE/FALSE}, or a level 1, 2, or 3.
    \code{TRUE} is equivalent to the highest level (3).}
  \item{dry.run}{ if \code{TRUE}, do everything but load the pacakge,
    including making a copy}
  \item{quietly,warn.conflicts,keep.source}{see docs for \code{require()}}
}
\details{
  Intended to 'just work' with no argument other than the package name.

  If \code{local.deps=FALSE}, any package dependencies will be loaded
  with the ordinary \code{library()} function.

  \code{require.local()} is just like \code{require()} except that it calls
  \code{library.local()} instead of \code{library()}. The code for
  \code{require.local()} was adapted from \code{require()} from R-2.14.1.

  The argument \code{binary.only} is provided because packages without
  binaries (e.g., DLLs) generally can be updated while
  attached to a running R session.  If it is wanted to make local copies
  of all packages, then do options(library.local.binary.only=TRUE) in
  the R startup file.

  The library will end up having structure like the following:
\preformatted{
    c:/Temp/R_local_libs/TTR_local_copy_4b1135b3/TTR/
    c:/Temp/R_local_libs/xts_local_copy_2be1478a/xts/
    c:/Temp/R_local_libs/zoo_local_copy_4eac48af/zoo/
}
This means that each package is in its own private 'library' (in the
strict R sense, and 'library' is a directory in which packages are
stored.)
The actual library directories used are not stored in \code{.libPaths()}.  The
library from which an attached packaged has been installed can always be
found with \code{path.package()}.

File size and creation time of the files in the central library are
recorded as a saved R object in the local copy in the file
\code{<library>/<package>/pkginfo.rda}, e.g., in
\code{c:/Temp/R_local_libs/xts_local_copy_2be1478a/xts/pkginfo.rda}.

}
\note{
  \code{library.local()} attempts to find dependencies of packages and
  attach those using \code{library.local()} too.  However, if it fails
  to find some dependencies (e.g., because of difficulties parsing
  'Depends:' lines for packages), the dependencies may end up being
  loaded by \code{library()} and local copies may not be made.
  }
\value{
  Returns the value from \code{library()}
}
\author{
  Tony Plate \email{tplate@acm.org}.  R-core for the original \code{require()}.
}
\examples{
\dontrun{library.local('some.package')}
# see where all loaded packages come from
path.package(.packages())
}
\keyword{ misc }
