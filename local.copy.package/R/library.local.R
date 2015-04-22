library.local <- function(package, character.only=FALSE,
                          ...,
                          lib.loc=NULL, binary.only=getOption('library.local.binary.only', TRUE),
                          compare.method=c('description.built', 'cached.info', 'md5sum'), local.deps=TRUE,
                          local.lib.locs=c(Sys.getenv('TMPDIR'), Sys.getenv('TMP'), Sys.getenv('TEMP')),
                          pkg.subdirs=c('R','libs','data'),
                          verbose=FALSE, dry.run=FALSE) {
    if (is.logical(verbose) && verbose)
        verbose <- 3
    non.null <- function(x, y) if (is.null(x)) y else x
    # Hack to make library.local(foo) work like library.local('foo')
    # Same ugly hack as used in library().  Would prefer not to, but
    # consistency with library() > prettiness.
    package.orig <- substitute(package)
    if (is.name(substitute(package)) && !character.only)
        package <- as.character(substitute(package))
    compare.method <- match.arg(compare.method)
    if (!is.character(package) || length(package)!=1)
        stop("'package' must be a single package name as a character vector")
    # package already loaded
    if (verbose>2)
        cat('library.local: Starting on', package, 'at', format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), '\n')
    if (is.element(package, .packages())) {
        if (verbose > 2)
            cat("package ", package, ' is already loaded\n')
        return(.packages())
    }
    if (!is.element(package, .packages(lib.loc=lib.loc, all.available=TRUE)))
        stop("package ", package, ' is not found anywhere with lib.loc=', paste(non.null(lib.loc, 'NULL'), collapse=';'),
             if (!character.only && is.name(package.orig))
             paste(" (if '", as.character(package.orig), "' is a var, do library.local(",
                   as.character(package.orig), ', character.only=TRUE, ...))', sep=''))
    priority <- utils:::packageDescription(package, fields='Priority', lib.loc=lib.loc)
    if (is.na(priority))
        priority <- 'NA'
    if (verbose > 2)
        cat('library.local:', package, 'priority:', priority, '\n')
    # lib.loc seems to be ignored for base packages, so don't even try with them...
    if (   (package %in% loadedNamespaces())
        || (priority == 'base')
        || (binary.only && system.file(package=package, 'libs')=='')) {
        if (verbose > 2) {
            if (package %in% loadedNamespaces())
                cat('library.local: using ordinary library() for', package, 'because it is already loaded as a namespace\n')
            else if (priority=='base')
                cat('library.local: using ordinary library() for', package, 'because it is a base package\n')
            else
                cat('library.local: using ordinary library() for', package, 'because it has no binary and binary.only=TRUE\n')
        }
        # codetools complains that '... may be used in an incorrect context' if ... is not the 1st actual arg to library()?
        return(library(..., package=package, character.only=TRUE, lib.loc=lib.loc))
    }
    if (local.deps) {
        deps <- setdiff(all.pkg.depends(package, lib.loc=lib.loc), c(package, .packages()))
        if (length(deps) && verbose > 2)
            cat('library.local: for', package, 'need deps:', paste(deps, collapse=', '), '\n')
        for (dep in deps) {
            library.local(package=dep, character.only=TRUE, ..., local.deps=FALSE, lib.loc=lib.loc, compare.method=compare.method,
                          local.lib.locs=local.lib.locs, pkg.subdirs=pkg.subdirs, verbose=verbose, dry.run=dry.run)
        }
    }
    # Check again whether the desired package is loaded.
    # Could be strange/unusual circumstances where it got loaded as
    # a result of loading the dependencies
    if (is.element(package, .packages())) {
        if (verbose > 2)
            cat("package ", package, ' is already loaded (found after loading dependencies)\n')
        return(.packages())
    }
    local.lib.locs <- setdiff(local.lib.locs, '')
    local.lib.locs <- gsub('[/\\\\]$', '', local.lib.locs)
    local.lib.locs <- local.lib.locs[order(basename(local.lib.locs)=='R_local_libs', decreasing=TRUE)]
    local.lib.loc <- NULL
    # find the first writable component of local.lib.loc, and create 'R_local_libs' there
    for (dir in local.lib.locs) {
        if (isTRUE(file.info(dir)$isdir) && isTRUE(as.logical(file.access(dir, 4)==0))) {
            local.lib.loc <- dir
            if (verbose > 2)
                cat('found writeable component of local.lib.locs at', dir, '\n')
            break
        }
    }
    if (is.null(local.lib.loc))
        stop('cound not find any writable temp directories, tried ', paste(local.lib.locs, collapse=';'))
    if (basename(local.lib.loc) != 'R_local_libs') {
        local.lib.loc <- file.path(local.lib.loc, 'R_local_libs')
        if (!file.exists(local.lib.loc))
            dir.create(local.lib.loc, recursive=TRUE)
    }
    orig.pkg.dir <- find.package(package, lib.loc=lib.loc)
    # look for candidate copies already existing
    copy.lib.dir <- list.files(local.lib.loc, pattern=paste(package, '_local_copy_*', sep=''), full.names=TRUE)
    # copy.lib.dir is a folder where <package> can be found, so
    # copy.lib.dir/<package> is at the same level as orig.pkg.dir
    # Choose the most recent copy.lib.dir there is more than one.
    if (length(copy.lib.dir) > 1)
        copy.lib.dir <- copy.lib.dir[order(file.info(copy.lib.dir)$ctime, decreasing=TRUE)[1]]
    if (verbose>2)
        cat('library.local:', package, 'comparing with', compare.method, 'at', format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), '\n')
    # This test shouldn't be needed, but have it here for robustness
    if (normalizePath(dirname(dirname(orig.pkg.dir))) == normalizePath(local.lib.loc)
        && regexpr(paste('^', package, '_local_copy_', sep=''), basename(dirname(orig.pkg.dir)), fixed=TRUE) > 0) {
        # The original library is already in the place where we make copies, and looks like a copy,
        # so don't go making another a copy of it.
        if (verbose>2)
            cat('library.local: original library', orig.pkg.dir, 'looks like it is already a copy; not making another copy\n')
        copy.lib.dir <- dirname(orig.pkg.dir)
    } else if (length(copy.lib.dir)==1) {
        found <- FALSE
        if (compare.method=='description.built' && file.exists(file.path(orig.pkg.dir, 'DESCRIPTION'))) {
            # Can use this method if the DESCRIPTION file has a 'Built:' line (which contains
            # the timestamp (to the second) of when the package was built).
            desc.orig <- try(readLines(file.path(orig.pkg.dir, 'DESCRIPTION')))
            if (any(regexpr('^Built:', desc.orig) > 0)) {
                desc.copy <- try(readLines(file.path(copy.lib.dir, package, 'DESCRIPTION')))
                if (length(desc.orig) == length(desc.copy) && all(desc.orig == desc.copy)) {
                    if (verbose>1)
                        cat('Existing copy in ', copy.lib.dir, ' is same as ', orig.pkg.dir, ' by DESCRIPTION file\n', sep='')
                } else {
                    cat('Existing copy in ', copy.lib.dir, ' differs from ', orig.pkg.dir, ' by DESCRIPTION file\n', sep='')
                    copy.lib.dir <- NULL
                }
                found <- TRUE
            } else {
                # Don't have a "Built:" line, use different compare method
                compare.method <- 'cached.info'
            }
        }
        if (!found && compare.method=='cached.info' && file.exists(file.path(copy.lib.dir, package, 'pkginfo.rda'))) {
            tt1 <- proc.time()[3]
            load(file.path(copy.lib.dir, package, 'pkginfo.rda'))
            if (!exists('pkginfo', inherits=FALSE)) {
                if (verbose)
                    cat('Existing copy in ', copy.lib.dir, " doesn't have value pkginfo object\n", sep='')
                copy.lib.dir <- NULL
            } else {
                tt2 <- proc.time()[3]
                copyInfo <- pkginfo$files
                pkg.files <- list.files(orig.pkg.dir, recursive=TRUE, full.names=FALSE)
                tt3 <- proc.time()[3]
                pkg.files.want <- grep(paste('^(',paste(pkg.subdirs, collapse='|'),')(/|$)',sep=''), pkg.files, value=TRUE)
                origInfo <- file.info(file.path(orig.pkg.dir, pkg.files.want))[,c('size','mtime')]
                if (colnames(origInfo)[2]=='ctime') colnames(origInfo)[2] <- 'mtime'
                rownames(origInfo) <- gsub('\\', '/', pkg.files.want, fixed=TRUE)
                origInfo <- origInfo[order(rownames(origInfo)),]
                tt4 <- proc.time()[3]
                copyInfo <- copyInfo[grep(paste('^(',paste(pkg.subdirs, collapse='|'),')(/|$)',sep=''), rownames(copyInfo)),]
                copyInfo <- copyInfo[order(rownames(copyInfo)),]
                # This is a little odd, but on some systems, file.info() returns GMT times, and
                # on others it returns local times.  E.g., in Windows XP 64 bit,
                # running in the R console, and Rterm under command window, we see:
                # > file.info('c:/Windows')$mtime
                # [1] "2011-05-07 15:57:48 EDT"
                # while running from R.exe under cygwin, we see:
                # > file.info('c:/Windows')$mtime
                # [1] "2011-05-07 10:57:48 EDT"
                # What makes thing even more diabolical is that the above difference is 5 hours,
                # but the current GMT/EDT difference is 4 hours, so it seems like the cygwin one
                # is getting a EST offset.
                # So, find the the GMT offset, and try it with +1 hour too.
                st <- Sys.time()
                st.local <- as.numeric(as.POSIXct(format(as.POSIXlt(st))))
                st.gmt <- as.numeric(as.POSIXct(format(as.POSIXlt(st, 'GMT'))))
                gmt.offset <-st.gmt - st.local
                same.n <- nrow(copyInfo) == nrow(origInfo)
                same.size <- same.n && all(same.size.i <- (copyInfo$size == origInfo$size))
                same.names <- same.n && all(rownames(copyInfo) == rownames(origInfo))
                same.mtime <- (same.n &&
                              all(same.mtime.i <- (copyInfo$mtime == origInfo$mtime
                                                   | copyInfo$mtime + gmt.offset == origInfo$mtime
                                                   | copyInfo$mtime - gmt.offset == origInfo$mtime
                                                   | copyInfo$mtime + gmt.offset + 3600 == origInfo$mtime
                                                   | copyInfo$mtime - (gmt.offset + 3600) == origInfo$mtime)))
                if (!same.n || !same.size || !same.names || !same.mtime) {
                    if (verbose) {
                        if (!same.n) {
                            cat('Existing copy in ', copy.lib.dir, ' differs from ', orig.pkg.dir, ' by number of files\n', sep='')
                        } else if (!same.names) {
                            cat('Existing copy in ', copy.lib.dir, ' differs from ', orig.pkg.dir, ' by names of files\n', sep='')
                        } else if (!same.size) {
                            i <- which(same.size.i)[1]
                            cat('Existing copy in', copy.lib.dir, 'differs from', orig.pkg.dir, 'by size, e.g.,',
                                rownames(copyInfo)[i], copyInfo[i,'size'], origInfo[i, 'size'], '\n')
                        } else if (same.mtime) {
                            i <- which(same.mtime.i)[1]
                            cat('Existing copy in', copy.lib.dir, 'differs from', orig.pkg.dir, 'by mtime, e.g.,',
                                rownames(copyInfo)[i], as.character(copyInfo[i,'mtime']), as.character(origInfo[i, 'mtime']), '\n')
                        } else {
                            cat('Existing copy in ', copy.lib.dir, ' differs from ', orig.pkg.dir, ' by some(?) size/create-time\n', sep='')
                        }
                    }
                    copy.lib.dir <- NULL
                } else {
                    if (verbose>1)
                        cat('Existing copy in ', copy.lib.dir, ' is same as ', orig.pkg.dir, ' by size/create-time\n', sep='')
                }
            }
            found <- TRUE
        } else if (!found) {
            # compare md5sums
            orig.pkg.files <- c(file.path(orig.pkg.dir, 'DESCRIPTION'),
                                list.files(file.path(orig.pkg.dir, pkg.subdirs), recursive=TRUE, full.names=TRUE))
            orig.md5sum <- md5sum(orig.pkg.files)
            # strip of dirs and get in a cananoical order for comparing
            names(orig.md5sum) <- basename(orig.pkg.files)
            orig.md5sum <- orig.md5sum[order(names(orig.md5sum), orig.md5sum)]
            copy.pkg.files <- c(file.path(copy.lib.dir, package, 'DESCRIPTION'),
                               list.files(file.path(copy.lib.dir, package, pkg.subdirs), recursive=TRUE, full.names=TRUE))
            copy.md5sum <- md5sum(copy.pkg.files)
            names(copy.md5sum) <- basename(copy.pkg.files)
            copy.md5sum <- copy.md5sum[order(names(copy.md5sum), copy.md5sum)]
            if (!isTRUE(all.equal(orig.md5sum, copy.md5sum))) {
                if (verbose)
                    cat('Existing copy in ', copy.lib.dir, ' differs from ', orig.pkg.dir, ' by md5sums\n', sep='')
                copy.lib.dir <- NULL
            } else {
                if (verbose>1)
                    cat('Existing copy in ', copy.lib.dir, ' is same as ', orig.pkg.dir, ' by md5sums\n', sep='')
            }
            # if we don't have cached file info, create it
            if (length(copy.lib.dir) && !file.exists(file.path(copy.lib.dir, package, 'pkginfo.rda'))) {
                pkg.files <- list.files(orig.pkg.dir, recursive=TRUE, full.names=FALSE)
                fileInfo <- file.info(file.path(orig.pkg.dir, pkg.files))
                rownames(fileInfo) <- gsub('\\', '/', pkg.files, fixed=TRUE)
                pkginfo <- list(pkg.dir=normalizePath(orig.pkg.dir), files=fileInfo[,c('size','mtime')])
                save(list='pkginfo', file=file.path(copy.lib.dir, package, 'pkginfo.rda'))
            }
            found <- TRUE
        }
    }
    if (verbose>2)
        cat('library.local: Finished comparison at', format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), '\n')
    # if we don't have copy.lib.dir, create a new one and copy the library there
    if (length(copy.lib.dir)==0) {
        copy.lib.dir <- tempfile(pattern=paste(package, '_local_copy_', sep=''), tmpdir=local.lib.loc)
        dir.create(copy.lib.dir)
        if (verbose)
            cat('Making local copy of ', package, ' in ', copy.lib.dir, '\n', sep='')
        if (!isTRUE(file.copy(orig.pkg.dir, copy.lib.dir, recursive=TRUE)))
            stop('failed to copy from ', orig.pkg.dir, ' to ', copy.lib.dir)
        pkg.files <- list.files(orig.pkg.dir, recursive=TRUE, full.names=FALSE)
        fileInfo <- file.info(file.path(orig.pkg.dir, pkg.files))
        rownames(fileInfo) <- gsub('\\', '/', pkg.files, fixed=TRUE)
        pkginfo <- list(pkg.dir=normalizePath(orig.pkg.dir), files=fileInfo[,c('size','mtime')])
        save(list='pkginfo', file=file.path(copy.lib.dir, package, 'pkginfo.rda'))
    } else {
        if (verbose)
            cat('Loading existing copy of ', package, ' in ', copy.lib.dir, '\n', sep='')
    }
    if (verbose>2)
        cat('library.local: Loading at', format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), 'using lib.loc=',
            paste(c(copy.lib.dir, lib.loc), collapse=';'), '\n')
    if (!dry.run)
        library(..., package=package, character.only=TRUE, lib.loc=c(copy.lib.dir, lib.loc))
    else
        cat('Not loading ', package, ' because dry.run==TRUE\n', sep='')
    if (verbose>2)
        cat('library.local: Finished load',  package, 'at', format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), '\n')
    return(.packages())
}

path.package.local <- function(package, original=TRUE) {
    path <- path.package(package)
    if (!original)
        return(path)
    pkginfo <- NULL # to shut up code checks
    remove('pkginfo', inherits=FALSE)
    if (regexpr(paste(package, '_local_copy_', sep=''), path) && file.exists(file.path(path, 'pkginfo.rda'))) {
        try(load(file.path(path, 'pkginfo.rda')))
        if (exists('pkginfo', inherits=FALSE))
            return(pkginfo$pkg.dir)
        else
            stop('could not read pkginfo from pkginfo.rda')
    } else {
        return(path)
    }
}

all.pkg.depends <- function(pkgs, lib.loc=NULL) {
    all.pkgs <- character(0)
    deplist <- list()
    while (length(pkgs)) {
        deps <- pkg.depends(pkgs[1], lib.loc=lib.loc)
        all.pkgs <- c(all.pkgs, pkgs[1])
        if (length(deps))
            deplist[[pkgs[1]]] <- deps
        pkgs <- c(pkgs[-1], setdiff(deps, c(pkgs, all.pkgs)))
    }
    pkgs <- all.pkgs
    # now get them in order in all.pkgs
    all.pkgs <- character(0)
    while (length(pkgs)) {
        pkg <- NULL
        for (i in seq(along=pkgs)) {
            if (!is.element(pkgs[i], names(deplist)) || all(deplist[[pkgs[i]]] %in% all.pkgs)) {
                pkg <- pkgs[i]
                break
            }
        }
        if (is.null(pkg))
            stop('cannot find pkg with all dependencies loaded in ', paste(pkgs, collapse=', '))
        all.pkgs <- c(all.pkgs, pkg)
        pkgs <- pkgs[-i]
    }
    all.pkgs
}

pkg.depends <- function(pkg, lib.loc=NULL) {
    pfile <- system.file("Meta", "package.rds", package=pkg, lib.loc=lib.loc)
    pkgInfo <- readRDS(pfile)
    as.character(sapply(pkgInfo$Depends, '[[', 'name'))
}

library.local.clean <- function(older.than=NULL,
                                local.lib.locs=c(Sys.getenv('TMPDIR'), Sys.getenv('TMP'), Sys.getenv('TEMP')),
                                verbose=TRUE) {
    # Clean out obselete packages that are older than specified
    # The default value for the cleanout time is the last reboot
    if (is.null(older.than)) {
        if (.Platform$OS == 'windows') {
            # systeminfo returns a line like this:
            # "System Up Time:            11 Days, 15 Hours, 16 Minutes, 59 Seconds"
            sys.info <- system('systeminfo /FO LIST', intern=TRUE)
            older.than <- grep('^system up time:', sys.info, ignore.case=TRUE, value=TRUE)
            if (length(older.than) < 1)
                stop('no up time in system info; info was:', sys.info)
            # if too many lines, prefer the one with 'sec' or 'min' in it
            if (length(older.than) > 1)
                older.than <- older.than[order(regexpr('(sec|min)', older.than, ignore.case=TRUE)>0, decreasing=TRUE)][1]
            older.than <- gsub('^[^:]*: *', '', older.than, ignore.case=)
        } else {
            # There are many different styles of uptime results. I try to parse them all. Yeee!
            # Examples from different machines:
            # [x86] Linux 2.4 (Redhat 7.3)
            #  2:06pm  up 63 days, 18 min,  3 users,  load average: 0.32, 0.08, 0.02
            # [x86] Linux 2.4.18-14 (Redhat 8.0)
            #  3:07pm  up 29 min,  1 user,  load average: 2.44, 2.51, 1.57
            # [PPC - G4] MacOS X 10.1 SERVER Edition
            #  2:11PM  up 3 days, 13:50, 3 users, load averages: 0.01, 0.00, 0.00
            # [powerpc] Darwin v1-58.corefa.com 8.2.0 Darwin Kernel Version 8.2.0
            #  10:35  up 18:06, 4 users, load averages: 0.52 0.47 0.36
            # [Sparc - R220] Sun Solaris (8)
            #  2:13pm  up 22 min(s),  1 user,  load average: 0.02, 0.01, 0.01
            # [x86] Linux 2.4.18-14 (Redhat 8)
            #  11:36pm  up 4 days, 17:58,  1 user,  load average: 0.03, 0.01, 0.00
            # AIX jwdir 2 5 0001DBFA4C00
            #  09:43AM   up  23:27,  1 user,  load average: 0.49, 0.32, 0.23
            # OpenBSD box3 2.9 GENERIC#653 i386
            #  6:08PM  up 4 days, 22:26, 1 user, load averages: 0.13, 0.09, 0.08
            # (AIX6, uptime > 1 day)
            #  01:29PM up 316 days, 3:54, 2 users, load average: 1.15, 1.06, 1.05
            # (AIX6, uptime very shortly after reboot)
            #  01:33PM up 1 user, load average: 0.21, 0.08, 0.03
            # (AIX6, uptime 1 min. after reboot)
            #  01:34PM up 1 min, 2 users, load average: 0.35, 0.13, 0.05
            # (AIX6, uptime 1 hour after reboot)
            #  02:43PM up 1:10, 2 users, load average: 0.01, 0.04, 0.03
            # (Solaris 9, very short uptime)
            #  12:07pm 1 user, load average: 0.87, 1.06, 1.38
            uptime <- system('LANG=C LC_ALL=C uptime', intern=TRUE)
            older.than <- grep('(^| )up ', uptime, ignore.case=TRUE, value=TRUE)
            if (length(older.than) < 1)
                if (regexpr('user', older.than, ignore.case=TRUE)>0)
                    older.than <- '0s'
                else
                    stop("no 'up' time in 'uptime' output was:", uptime)
            # if too many lines, prefer the one with 'sec' or 'min' in it
            if (length(older.than) > 1)
                older.than <- older.than[order(regexpr('(sec|min)', older.than, ignore.case=TRUE)>0, decreasing=TRUE)][1]
            older.than <- gsub('(^|.* )up ', '', older.than)
            older.than <- gsub(', *[0-9,]+ users.*$', '', older.than)
        }
    }
    if (inherits(older.than, 'difftime')) {
        older.than <- Sys.time() - older.than
    } else if (inherits(older.than, 'Date')) {
        older.than <- as.POSIXct(as.character(older.than))
    } else if (is.character(older.than)) {
        # Try to parse older.than
        x <- try(as.POSIXct(older.than), silent=TRUE)
        if (!is(x, 'try-error')) {
            older.than <- x
        } else {
            # parse a relative time,
            # try to handle strings like 14d17h3m
            x <- gsub('([0-9])([A-Z])', '\\1 \\2', older.than, ignore.case=TRUE)
            x <- gsub('([A-Z])([0-9])', '\\1, \\2', x, ignore.case=TRUE)
            x <- strsplit(x, ', *')[[1]]
            if (length(x)==0)
                x <- 0
            else
                x <- sum(sapply(x, function(tt) {
                    if (regexpr('^[0-9]+:[0-9]+$', tt)>0) {
                        return(as.numeric(as.difftime(tt,'%H:%M', 'secs')))
                    } else if (regexpr('^[0-9]+:[0-9]+:[0-9]+$', tt)>0) {
                        return(as.numeric(as.difftime(tt,'%H:%M:%S', 'secs')))
                    } else {
                        tt1 <- gsub('(s|m|h|d|w).*', '\\1', casefold(tt, upper=FALSE))
                        tt2 <- strsplit(tt1, ' +')[[1]]
                        if (regexpr('^[.0-9]*$', tt2[1])>0 && is.element(tt2[2], c('s','m','d','h','w'))) {
                            return(as.numeric(tt2[1]) * switch(tt2[2], s=1,m=60,h=3600,d=24*3600,w=7*24*3600))
                        } else {
                            warning("library.local.clean: could not parse component '", tt, "' of up time specification: ", x)
                            return(-0.0001)
                        }
                    }
                }))
            if (x >= 0 || regexpr('^ *$', older.than)>0) {
                older.than <- Sys.time() - as.difftime(x, units='secs')
            } else {
                # only stop with an error if there was something non-blank in the string and we didn't parse anything
                stop('could not parse anything sensible out of older.than value: ', older.than)
            }
        }
    } else if (!inherits(older.than, 'POSIXt')) {
        stop("unrecognized class for older.than:", class(older.than))
    }
    if (verbose)
        cat('Removing old local copies of packages created before ', format(older.than), '\n', sep='')
    if (is.na(older.than))
        stop('ended up with a NA value for older.than')
    local.lib.locs <- unique(setdiff(local.lib.locs, ''))
    local.lib.locs <- gsub('[/\\\\]$', '', local.lib.locs)
    local.lib.locs <- local.lib.locs[order(basename(local.lib.locs)=='R_local_libs', decreasing=TRUE)]
    for (dir in local.lib.locs) {
        if (basename(dir) != 'R_local_libs')
            dir <- file.path(dir, 'R_local_libs')
        # just work in writable component of local.lib.locs
        if (file.exists(dir) && isTRUE(file.info(dir)$isdir) && isTRUE(as.logical(file.access(dir, 4)==0))) {
            pkg.paths <- Sys.glob(file.path(dir, '*', '*'))
            pkg.mtime <- file.info(pkg.paths)$mtime
            if (any(i <- (is.na(pkg.mtime) | pkg.mtime > older.than))) {
                pkg.paths <- pkg.paths[!i]
                pkg.mtime <- pkg.mtime[!i]
            }
            if (length(pkg.paths)) {
                pkg.names <- basename(pkg.paths)
                # for each package, remove the most recent from the list
                pkg.del <- unlist(tapply(seq(length=length(pkg.paths)), pkg.names, function(ii) ii[-order(pkg.mtime[ii], decreasing=TRUE)[1]]))
                for (x in pkg.paths[pkg.del]) {
                    if (   file.exists(file.path(x, 'libs'))
                        && unlink(file.path(x, 'libs'), force=TRUE, recursive=TRUE)==0
                        && file.exists(file.path(x, 'libs'))) {
                        warning("unable to remove 'libs' subdir of ", x, '; package still in use?')
                        next
                    }
                    y <- dirname(x)
                    if (unlink(y, force=TRUE, recursive=TRUE)==0 && !file.exists(y)) {
                        if (verbose)
                            cat('Successfully removed old package directory ', y, '\n', sep='')
                    } else {
                        warning("unable to remove directory ", y, '; package still in use?')
                    }
                }
            }
        }
    }
    invisible(NULL)
}
