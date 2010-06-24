\name{installedPackages}
\alias{installedPackages}
\title{Find Installed Packages}
\description{
  Return information on installed packages that satisfy search criteria.
}
\usage{
installedPackages(lib.loc = NULL,
                  ## Trinary filters (TRUE/FALSE/NA).
                  loaded = NA, subdirs = NA, latest = NA,
                  masked = NA, loadable = NA, system = FALSE,
                  site = FALSE,
                  ## Value based filters
                  package = NULL, version = NULL,
                  minVer = NULL, maxVer = NULL,
                  priority = NULL,
                  ## Performance control
                  noCache = FALSE,
                  ## Outout control
                  fields = NULL,
                  columns = c("Version", "LibPath", fields),
                  sort = TRUE, dup.rm = TRUE)
}
\arguments{
  \item{lib.loc}{
    character vector describing the top of \R library trees to
    search through or if \code{NULL} (default) use current library
    paths.
  }
  \item{loaded}{
    logical or \code{NA} (default). Find packages that
    are loaded in to memory (\code{TRUE}), not loaded (\code{FALSE}) or both
    (\code{NA}).
  }
  \item{subdirs}{
    logical or \code{NA} (default). Find packages that
    are in sub-directories (\code{TRUE}), not in sub-directories
    (\code{FALSE}) or both (\code{NA}).
  }
  \item{latest}{
    logical or \code{NA} (default). Find packages that
    are the latest version, i.e. highest version number (\code{TRUE}),
    not in the latest (\code{FALSE}) or both (\code{NA}).
  }
  \item{masked}{
    logical or \code{NA} (default). Find packages that
    are masked, i.e. not accessible because of and other version
    (\code{TRUE}), not masked (\code{FALSE}) or both
    (\code{NA}).
  }
  \item{loadable}{
    logical or \code{NA} (default). Find packages that
    are loadable, i.e. in the directories pointed to by the library paths
    or below (\code{TRUE}), not loadable (\code{FALSE}) or both
    (\code{NA}).
  }
  \item{system}{
    logical or \code{NA}. Find system packages, i.e.
    in \code{.Library} (\code{TRUE}), non-system packages (\code{FALSE})
    (default) or both (\code{NA}).
  }
  \item{site}{
    logical or \code{NA}. Find site packages, i.e.
    in \code{.Library.site} (\code{TRUE}), non-site packages
    (\code{FALSE}) (default) or both (\code{NA}).
  }
  \item{package}{
    character string with a regular expression of package names to
    find, or if \code{NULL} (default) all names.
  }
  \item{version}{
    character string with a regular expression of package versions to
    find, or if \code{NULL} (default) all versions.
  }
  \item{minVer}{
    character string or numeric value of minimal version to find
    (non-inclusive), or if \code{NULL} (default) no minimun.
  }
  \item{maxVer}{
    character string or numeric value of maximal version to find
    (inclusive), or if \code{NULL} (default) no maximum.
  }
  \item{priority}{
    character vector or \code{NULL} (default).  If non-null, used to
    select packages; \code{"high"} is equivalent to
    \code{c("base", "recommended")}.  To select all packages without an
    assigned priority use \code{priority = "NA"}.
  }
  \item{noCache}{
    logical. If \code{FALSE} (default) cache information.
  }
  \item{fields}{
    character vector giving the fields to extract from
    each package's \code{DESCRIPTION} file in addition to the default
    ones, or \code{NULL} (default).  Unavailable fields result in
    \code{NA} values.
  }
  \item{columns}{
    character vector with columns to include in output, or
    or if \code{NULL} all columns. Defaults to \code{c("Version",
    "LibPath")}.
  }
  \item{sort}{
    logical. If \code{TRUE} (default) sort output by package name
    followed by version number.
  }
  \item{dup.rm}{
    logical. If \code{TRUE} (default) remove duplicate packages
    from output.
  }
}
\details{
  \code{installedPackages} scans the \file{DESCRIPTION} files of each
  package found in and below \code{lib.loc} the meetes the search
  criteria give by the arguments and returns a matrix of package
  related information like name, library path and various information
  from the \file{DESCRIPTION} file like version number and dependencies.
  This function extends R's \code{installed.packages} with search criteria
  and more output control.
}
\value{
  Same as R's \code{installed.packages} with the addition of the
  following columns:
  \item{Loaded}{logical. Is the package loaded into the current R session?}
  \item{Loadable}{logical. Is it possible to load the package? It must
    be in the library paths (\code{getLibraryPaths}) to be loadable.}
  \item{Masked}{logical. Is the package masked? A masked package cannot
    be loaded because an other version is already loaded or there is an
    other version earlier in the library paths.
  }
}
\author{Lars Hansen}
\seealso{\code{\link{installed.packages}},
  \code{\link{setLibraryPaths}},
  \code{\link{getLibraryPaths}}
}
\examples{
## Full information about all the package that are installed in or
## below the current library paths (potentially a lot of output!).
installedPackages(system = NA, site = NA, columns = NULL)

## Abbreviated information on the currently loadable user installed
## packages.
installedPackages(loadable = TRUE)

## What is loaded?
installedPackages(loaded = TRUE)

## What are the latest package available with a version number higher
## than or equal to "1.0-0"
installedPackages(latest = TRUE, minVer = 1)

## Find all installed "timeseries" packages that has a version number
## below but no equal to "3.2-0"
installedPackages(package = "^(ts|TS)", maxVer = 3.2)

## What do we know about the "graphics" package?
installedPackages(package = "^graphics$", system = TRUE, site = TRUE,
                  columns = NULL)

## Do we have any installed packages with version number
## between 1.0-0 and 1.5-3?
installedPackages(minVer = 1, maxVer = "1.5-3")

## Do we have any "*.1-1" versions installed?
installedPackages(version = "..1-1")

## Who wrote the 1.0-0 versions and when was that?
installedPackages(version = "^1.0-0$", fields = c("Author", "Date"))
}
\keyword{utilities}
