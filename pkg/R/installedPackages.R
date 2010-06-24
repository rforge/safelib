installedPackages <- function(lib.loc = NULL,
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
{
    if (is.null(lib.loc))
        lib.loc <- .libPaths()

    if (is.na(site))
        NULL
    else if (!site)
    {
        remove <- match(c(.Library, .Library.site), lib.loc)
        if (any(!is.na(remove)))
            lib.loc <- lib.loc[-remove]
    }
    else
    {
        keep <- match(c(.Library, .Library.site), lib.loc)
        if (any(!is.na(keep)))
            lib.loc <- lib.loc[keep]
    }

    ## Give warning about using both system and priority.
    if (!is.null(priority))
    {
        ## 'system' default is FALSE.
        if (is.na(system) || system)
        {
            warning("'priority' and 'system' arguments are mutually exclusive. ",
                    "'system' ignored.")
        }
    }
    else
    {
        if (is.na(system))
            priority <- NULL
        else if (!system)
            priority <- "NA"
        else
            priority <- "high"
    }

    if (is.na(subdirs))
        packages = .recursiveInstPkgs(lib.loc, priority = priority,
                                      noCache = noCache,
                                      recursive = TRUE, fields = fields,
                                      sort = sort, dup.rm = dup.rm)
    else if (!subdirs)
        packages = .recursiveInstPkgs(lib.loc, priority = priority,
                                      noCache = noCache,
                                      recursive = FALSE, fields = fields,
                                      sort = sort, dup.rm = dup.rm)
    else
    {
        ## Look for DESCRIPTION files two levels down or lower.
        destFiles <- list.files(.list.dirs(.list.dirs(lib.loc,
                                                      full.names = TRUE),
                                           full.names = TRUE),
                                full.names = TRUE, pattern = "DESCRIPTION",
                                recursive = TRUE)

        ## The "library" is two levels above DESCRIPTION file.
        libs <- dirname(dirname(destFiles))

        packages = .recursiveInstPkgs(libs, priority = priority,
                                      noCache = noCache,
                                      recursive = FALSE, fields = fields,
                                      sort = sort, dup.rm = dup.rm)
    }

    ## Search .path.package() for loaded packages.
    ## Convert Windows network paths use forward slashes.
    ## They do cause some warnings that backslashes
    ## prevent, but unlike backslashes they work
    ## in all cases (backslashes in R_LIBS does not
    ## work with 'Rcmd.exe INSTALL').
    loadedPackages <- sub("^\\\\\\\\", "//", .path.package())
    
    Loaded <- !is.na(match(file.path(packages[, "LibPath"],
                                     packages[, "Package"]),
                           loadedPackages,
                           nomatch = NA))

    ## Path must be in .libPaths() to be loadable.
    Loadable <- !is.na(match(packages[, "LibPath"],
                             .libPaths(),
                             nomatch = NA))

    ## Unloaded package versions can be masked by
    ## 1) loaded packages, or
    ## 2) [TODO] packages earlier in library path.
    Masked <- !Loaded & !is.na(match(packages[, "Package"],
                                     basename(.path.package()),
                                     nomatch = NA))

    packages <- cbind(packages, Loaded, Loadable, Masked)


    if (!is.na(loaded) && dim(packages)[1])
        packages <- packages[packages[, "Loaded"] == as.character(loaded), ,
                             drop = FALSE]

    if (!is.na(loadable) && dim(packages)[1])
        packages <- packages[packages[, "Loadable"] == as.character(loadable),
                             , drop = FALSE]

    if (!is.na(masked) && dim(packages)[1])
        packages <- packages[packages[, "Masked"] == as.character(masked), ,
                             drop = FALSE]

    if (!is.null(package) && dim(packages)[1])
    {
        package <- as.character(package)
        packages <- packages[grep(package, packages[, "Package"]), , drop = FALSE]
    }

    if (!is.null(version) && dim(packages)[1])
    {
        version <- as.character(version)

        if (length(grep("[a-z|A-Z]", version)))
            stop("Invalid version regex: '", version, "'")

        packages <- packages[grep(version, packages[, "Version"]), , drop = FALSE]
    }

    if (!is.null(minVer) && dim(packages)[1])
    {
        minVer <- as.character(minVer)
        if (!length(grep("[.]", minVer)))
            minVer <- paste(minVer, ".0", sep = "")
        minVer <- package_version(minVer)
        packages <- packages[package_version(packages[, "Version"]) >=  minVer,
                             , drop = FALSE]
    }

    if (!is.null(maxVer) && dim(packages)[1])
    {
        maxVer <- as.character(maxVer)
        if (!length(grep("[.]", maxVer)))
            maxVer <- paste(maxVer, ".0", sep = "")
        maxVer <- package_version(maxVer)
        packages <- packages[package_version(packages[, "Version"]) <  maxVer,
                             , drop = FALSE]
    }

    if (!is.na(latest) && dim(packages)[1])
    {
        findLatest <- function(pkg)
        {
            indecies <- which(packages[, "Package"] == pkg)
            ver <- package_version(packages[indecies, "Version"])
            pos <- min(which(ver == max(ver)))
            indecies[pos]
        }

        names <- sort(unique(packages[, "Package"]))

        theLatest <- sapply(names, findLatest)

        if (latest)
            packages <- packages[theLatest, , drop = FALSE]
        else
            packages <- packages[-theLatest, , drop = FALSE]
    }

    if (is.null(columns))
        packages
    else
        packages[, columns, drop = FALSE]
}

