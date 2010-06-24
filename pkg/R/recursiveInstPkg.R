##
## Recursively Find Installed Packages
##
## Description:
##
##      Find all packages installed at the specified paths and their
##      subdirectories.
##
## Usage:
##
##      .recursiveInstPkgs(lib.loc = NULL, priority = NULL,
##                         noCache = FALSE, fields = NULL,
##                         recursive = TRUE, sort = TRUE,
##                         dup.rm = TRUE)
##
## Arguments:
##
##  lib.loc: character vector describing the top of R library trees to
##           search through or if 'NULL' (default) use current library
##           paths.
##
## priority: character vector or 'NULL' (default).  If non-null, used to
##           select packages; '"high"' is equivalent to 'c("base",
##           "recommended")'.  To select all packages without an assigned
##           priority use 'priority = "NA"'.
##
##  noCache: logical. If 'FALSE' (default) cache information.
##
##   fields: character vector giving the fields to extract from each
##           package's 'DESCRIPTION' file in addition to the default ones,
##           or 'NULL' (default).  Unavailable fields result in 'NA'
##           values.
##
## recursive: logical. If 'TRUE' (default) include subdirectories in
##           search.
##
##     sort: logical. If 'TRUE' (default) sort output by package name
##           followed by version number.
##
##   dup.rm: logical. If 'TRUE' (default) remove duplicate packages from
##           output.
##
## Details:
##
##      '.recursiveInstPkgs' scans the 'DESCRIPTION' files of each package
##      found in and below 'lib.loc' and returns a matrix of package
##      related information like name, library path and various
##      information from the 'DESCRIPTION' file like version number and
##      dependencies. This function extends 'installed.packages' with
##      recursive search and more output control.
##
## Value:
##
##      Same as R's 'installed.packages'.
##
## Author(s):
##
##      Lars Hansen
##
## See Also:
##
##      'installed.packages', 'installedPackages', 'setLibraryPaths'
##
## Examples:
##
##      ## What is installed by user?
##      .recursiveInstPkgs(priority = "NA")[, c("LibPath", "Version"), drop = FALSE]
##
.recursiveInstPkgs <- function(lib.loc = NULL, priority = NULL,
                               noCache = FALSE, fields = NULL,
                               recursive = TRUE, sort = TRUE,
                               dup.rm = TRUE)
{
    if (is.null(lib.loc))
        lib.loc <- .libPaths()

    require(utils)

    packages <- installed.packages(lib.loc = lib.loc, priority = priority,
                                   noCache = noCache, fields = fields)

    if (recursive)
    {
        ## Trim subdirectories that are already covered by parent
        ## directories.
        dup <- lapply(seq(along = lib.loc),
                     function(i)
                     {
                         tmp <- lib.loc; tmp[i] = "@";
                         grep(paste("^", lib.loc[i], sep = ""), tmp)
                     })
        dup <- do.call("c", dup)
        if (length(dup))
            lib.loc <- lib.loc[-dup]

        ## Look for DESCRIPTION files two levels down or lower.
        destFiles <- list.files(.list.dirs(.list.dirs(lib.loc,
                                                      full.names = TRUE),
                                           full.names = TRUE),
                                full.names = TRUE, pattern = "DESCRIPTION",
                                recursive = TRUE)

        ## The "library" is two levels above DESCRIPTION file.
        libs <- dirname(dirname(destFiles))

        if (length(libs))
        {
            subdirPackages <- installed.packages(libs, priority = priority,
                                                 noCache = noCache,
                                                 fields = fields)

            packages <- do.call("rbind", list(packages, subdirPackages))
        }
    }

    ## Convert Windows network paths use forward slashes.
    ## They do cause some warnings that backslashes
    ## prevent, but unlike backslashes they work
    ## in all cases (backslashes in R_LIBS does not
    ## work with 'Rcmd.exe INSTALL').
    packages <- sub("^\\\\\\\\", "//", packages)

    if (dup.rm)
        packages <- packages[!duplicated(packages), , drop = F]

    if (sort && dim(packages)[[1]])
    {
        sortKey <- paste(dimnames(packages)[[1]], packages[, "Version"],
                         sep = "")
        packages[order(sortKey), , drop = F]
    }
    else
        packages
}

