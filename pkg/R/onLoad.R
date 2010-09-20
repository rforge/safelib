.onLoad <- function(lib, pkg)
{
    safelibs <- Sys.getenv("R_SAFELIBS")

    if (nchar(safelibs))
    {
        args <- eval(parse(text = paste("list(", safelibs, ")")))

        if (class(args) != "list" ||
            length(args) < 1 ||
            !all(names(args) %in% c("spec", "versions")))
            stop("incorrect format of R_SAFELIBS environment variable:",
                 safelibs)

        do.call("setLibraryPaths", args)
    }
}
