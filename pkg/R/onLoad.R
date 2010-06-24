.onLoad <- function(lib, pkg)
{
    versioning <- Sys.getenv("R_VERSIONING")

    if (nchar(versioning))
    {
        args <- eval(parse(text = paste("list(", versioning, ")")))

        if (class(args) != "list" ||
            length(args) < 1 ||
            !all(names(args) %in% c("spec", "versions")))
            stop("incorrect format of R_VERSIONING environment variable:",
                 versioning)

        do.call("setLibraryPaths", args)
    }
}
