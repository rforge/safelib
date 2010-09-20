\name{safelibs-package}
\alias{safelibs-package}
\alias{safelibs}
\docType{package}
\title{

  A package versioning system in which a library can store multiple
  versions of a package, and packages can be updated without interfering
  with active R sessions.

}
\description{

  A package versioning system in which a library can store multiple
  versions of a package, and packages can be updated, manually or
  automatically, without interfering with running R processes.

  Note that this system is quite different to R's "versioned install"
  mechanism, which was intended to allow a single R session to load
  multiple different versions of a package, and which may be removed
  from future versions of R.

}
\details{
\tabular{ll}{
Package: \tab safelibs\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-12-08\cr
License: \tab GLP\cr
LazyLoad: \tab yes\cr
}

  This system stores packages of different versions by encoding the
  version number in the library path, and then storing the package in
  the conventional way with no version number.  This differs from R's
  versioned package install mechanism, which stores packages in
  directories whose names are the concatenation of the package name and
  the version number.

  This system provides two major capabilities:
  \itemize{
    \item Multiple versions of packages can be stored in a library, and an R
    session can request a particular version, or just the latest version.
    \item A package can be updated at any time without interfering with
    any active R session that might be using the package.  This is possible because
  existing installed packages are never overwritten; when a package is installed it is
  always placed into a new directory in the library tree.  This allows
  safe automated updating of R packages in a centrally managed system
  without the need to try to find "safe" periods of low activity.
  }

  This package has a
  function 'installedPackages that recursively searches through
  R's library paths '.libPaths()' and their sub-directories. It support
  filtering by various search criteria, like whether or not packages
  are loaded, are latest version, are in a site path
  and much more. 'setLibraryPath()' sets R's library paths to include
  directories of packages that match certain version criteria. By
  default it sets the library paths to point to all the latest versions
  of the installed packages.

}
\author{
  Lars Hansen and Tony Plate

Maintainer: Tony Plate <tplate@blackmesacapital.com>
}
\keyword{ package }
