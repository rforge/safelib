## Just like 'list.files' only it produces a character vector
## of the names of directories in the named directory and optionally
## its subdirectories.
.list.dirs <- function(path = ".", pattern = NULL, all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE)
{
    files <- list.files(path = path, full.names = TRUE, pattern = pattern,
                        all.files = all.files, recursive = recursive,
                        ignore.case = ignore.case)

    dirs <- files[file.info(files)$isdir]

    if (!full.names)
        basename(dirs)
    else
        dirs
}

