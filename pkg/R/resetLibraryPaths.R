resetLibraryPaths <- function(noaction = FALSE, resetOriginal = FALSE)
{
    current <- .libPaths()

    if (resetOriginal)
    {
        ## Only supposed to be use in testing.
        original <- current
        assign(".originalLibraryPaths", original, env = .GlobalEnv)
    }
    else
    {
        ## Retrieve original library paths.
        if (exists(".originalLibraryPaths", env = .GlobalEnv))
            original <- get(".originalLibraryPaths", env = .GlobalEnv)
        else
            original <- current
    }


    if (noaction)
    {
        if (identical(original, current))
            cat("No change!\n")
        else
        {
            cat("Current library path:\n ",
                paste(current, collapse = "\n  "), "\n")

            cat("New library path:\n ",
                paste(original, collapse = "\n  "), "\n")
        }
    }
    else
        .libPaths(original)
}

