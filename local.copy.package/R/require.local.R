require.local <- function (package, lib.loc = NULL, quietly = FALSE, warn.conflicts = TRUE,
                           keep.source = getOption("keep.source.pkgs"), character.only = FALSE,
                           binary.only=getOption('library.local.binary.only', TRUE),
                           local.deps = TRUE, local.lib.locs=c(Sys.getenv('TMPDIR'), Sys.getenv('TMP'), Sys.getenv('TEMP')))
{
    # The code for require.local() adapted from require() from R-2.14.1.
    if (!missing(keep.source))
        warning("'keep.source' is deprecated and will be ignored")
    if (!character.only)
        package <- as.character(substitute(package))
    loaded <- paste("package", package, sep = ":") %in% search()
    if (!loaded) {
        if (!quietly)
            packageStartupMessage(gettextf("Loading required package: %s",
                package), domain = NA)
        value <- tryCatch(library.local(package, lib.loc = lib.loc, binary.only = binary.only,
                     local.deps = local.deps, local.lib.locs = local.lib.locs,
                     character.only = TRUE, logical.return = TRUE, warn.conflicts = warn.conflicts,
                     quietly = quietly), error = function(e) e)
        if (inherits(value, "error")) {
            if (!quietly) {
                msg <- conditionMessage(value)
                cat("Failed with error:  ", sQuote(msg), "\n", file = stderr(), sep = "")
                # .Internal(printDeferredWarnings())
                # codetools doesn't like use of .Internal(), so do it differently, but
                # this might result in duplicate printing of warning messages...
                if (length(warnings())) print(warnings())
            }
            return(invisible(FALSE))
        }
        if (!value)
            return(invisible(FALSE))
    }
    else value <- TRUE
    invisible(value)
}
