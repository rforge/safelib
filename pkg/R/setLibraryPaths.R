setLibraryPaths <- function(spec = NULL, versions = NULL, noaction = FALSE)
{
    ## Retrieve original library paths.
    if (!exists(".originalLibraryPaths", env = .GlobalEnv))
    {
        original <- .libPaths()
        assign(".originalLibraryPaths", original, env = .GlobalEnv)
    }
    else
        original <- get(".originalLibraryPaths", env = .GlobalEnv)

    current <- .libPaths()

    ## 'spec == NULL' default values. Specify every single
    ## 'installedPackage' argument to be safe.
    lib.loc = original
    loaded = NA
    subdirs = NA
    latest = TRUE
    masked = NA
    loadable = NA
    system = NA
    site = NA
    package = NULL
    version = NULL
    minVer = NULL
    maxVer = NULL
    columns = "LibPath"

    if (!is.null(spec))
    {
        if (spec == "rel")
        {
            minVer = 1
            maxVer = NULL
        }
        else if (spec == "dev")
        {
            minVer = NULL
            maxVer = 1
        }
        else
            stop("Unknow spec argument. Must be either NULL, 'res' or 'dev'.")
    }

    expanded <- installedPackages(lib.loc = lib.loc, loaded = loaded,
                                  subdirs = subdirs, latest = latest,
                                  masked = masked, loadable = loadable,
                                  system = system, site = site,
                                  package = package, version = version,
                                  minVer = minVer, maxVer = maxVer,
                                  columns = columns)[ ,"LibPath"]

    ## Reset to default values.
    minVer = NULL
    maxVer = NULL

    if (!is.null(versions))
    {
        if (any(duplicated(names(versions))))
            stop("Duplicated package names in 'versions' argument")

        paths <-
            lapply(seq(along=versions),
                   function(i, names)
                   {
                       package = paste("^", names[i], "$", sep = "")
                       version = paste("^", versions[i], sep = "")

                       installedPackages(lib.loc = lib.loc, loaded = loaded,
                                         subdirs = subdirs, latest = latest,
                                         masked = masked, loadable = loadable,
                                         system = system, site = site,
                                         package = package, version = version,
                                         minVer = minVer, maxVer = maxVer,
                                         columns = columns)[ ,"LibPath"]
                   },
                   names(versions))

        names(paths) <- names(versions)

        ## Make sure each version is found.
        for (i in seq(along = paths))
            if (!length(paths[[i]]))
                stop("Version '", versions[[i]], "' of package '",
                     names(paths)[[i]], "' was not found.")

        overrides <- do.call("c", paths)

        ## Replace 'expanded' with 'overrides'.
        if (length(setdiff(names(overrides), names(expanded))))
            stop("'expanded' should be a superset of 'overrides'")

        for (name in names(overrides))
            expanded[name] <- overrides[name]
    }

    ## Convert Windows network paths use forward slashes.
    ## They do cause some warnings that backslashes
    ## prevent, but unlike backslashes they work
    ## in all cases (backslashes in R_LIBS does not
    ## work with 'Rcmd.exe INSTALL').
    original <- sub("^\\\\\\\\", "//", original)

    ## Merge 'original' and 'expanded' paths. Make sure original path
    ## order is maintained.
    new <- unique(c(original, expanded))

    paths <- lapply(original, ## 'original' works here because all 'expanded' contains it.
                    function(p)
                    {
                        ## All these backslashes are needed
                        ## in order for the regular expression
                        ## to work right.

                        ## Convert Windows network paths use forward slashes.
                        ## See comment above.
                        p <- sub("^\\\\\\\\", "//", p)

                        ## Find the original paths in 'new'.
                        idx <- grep(paste("^", p, sep = ""), new)
                        sort(new[idx])
                    })

    new <- do.call("c", paths)

    if (noaction)
    {
        if (identical(new, current))
            cat("No change!\n")
        else
        {
            cat("Current library path:\n ",
                paste(current, collapse = "\n  "), "\n")

            if (!identical(original, current))
                cat("Original library path:\n ",
                    paste(original, collapse = "\n  "), "\n")

            cat("New library path:\n ",
                paste(new, collapse = "\n  "), "\n")
        }
    }
    else
    {
        ## It get a bit tricky here. We need to use '\\\\' network paths
        ## instead of '//' for Windows, in order to avoid this warning:
        ##   "In .find.package(package, lib.loc, quiet = TRUE) :
        ##    package 'safelibs' found more than once,
        ##    using the one found in '\\jacona/home/lhansen/R/libs/safelibs'"
        ## when loading packages. Unfortunately, the '.libPaths' functions does
        ## not work with '\\\\' paths. It calls a function 'Sys.glob' that removes
        ## these paths. Seems like a bug. We do not actually need or want any
        ## globbing to happen, so we set '.lib.loc' directly here instead
        ## of calling '.libPaths(new)'.
        assign(".lib.loc", new, envir = environment(base::.libPaths))
    }
}

