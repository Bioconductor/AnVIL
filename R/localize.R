#' Discover the binary based on a user defined path
.user_setting <- function(option) {
    val <- Sys.getenv(option, unset = NA)
    if (!is.na(val))
        val
    else
        NULL
}

#' Get gsutil binary
.get_gsutil_binary <- function() {
    user_path <- .user_setting('GSUTIL_BINARY_PATH')
    if (!is.null(user_path))
        return(normalizePath(user_path))
    ## Discover binary automatically if user doesn't give path
    binary_name <- 'gsutil'
    bin_path <- if (.Platform$OS.type == "windows") {
                    appdata <- normalizePath(Sys.getenv("localappdata"), winslash = "/")
                    binary_name <- paste(binary, "cmd", sep = ".")
                    c(function() file.path(appdata, "Google/Cloud SDK/google-cloud-sdk/bin", binary_name),
                      function() file.path(Sys.getenv("ProgramFiles"),
                                           "/Google/Cloud SDK/google-cloud-sdk/bin", binary_name),
                      function() file.path(Sys.getenv("ProgramFiles(x86)"),
                                           "/Google/Cloud SDK/google-cloud-sdk/bin", binary_name))
                } else {
                    c(function() Sys.which(binary_name),
                      function() paste("~/google-cloud-sdk/bin", binary_name, sep = "/"),
                      function() file.path(gcloud_binary_default(), "bin", binary_name))
                }
    ## Return appropriate path for 'gsutil'
    for (path in bin_path) {
        if (file.exists(path())) {
            return(normalizePath(path()))
        }
    }
    stop("failed to find 'gsutil' binary")
}

## ## TODO
## #' User is supposed to set the GCE_AUTH file
## .get_option <- function(option) {
##     gce_auth <- Sys.getenv("GCE_AUTH_FILE", unset=NA)
##     if (is.na(gce_auth))
##         stop("missing env variable 'GCE_AUTH_FILE'.")
## }

#' @title helper function to pass arguments to `gsutil`
#'
#' @param args arguments to helper function
#'
#' @details private helper function to pass arguments to `gsutil`
#'     command line utility and throw the appropriate errors.
#'

.gsutil_do <-
    function(args, dry_run = FALSE)
{
    gsutil <- .get_gsutil_binary()
    ## TODO: replace 'gsutil' with gsutil binary
    err <- system2(gsutil, args, wait=TRUE)
    if (err) {
        stop(
            "\n", sprintf("gsutil %s", paste(args, collapse=" ")),
            "\n failed: error code ", err
        )
    }
}


#' @rdname is_gsutil_uri
#'
#' @title Check if the google bucket has a gs:// prefix
#'
#' @param path character, a path to a storage bucket
is_gsutil_uri <-
    function(path)
{
    is.character(path) && grepl("^gs://.+$", path)
}


#' @rdname gsutil_ls
#'
#' @title List contents of a google cloud bucket, or if you run
#'     'gsutil_ls' without URLs, it lists all of the Cloud Storage
#'     buckets under your default project ID
#'
#' @param source character, a valid path to a google storage
#'     bucket
#'
#' @param path character, a path or regular expression listing files
#'     of paths.
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @return Exit status of `gsutil_ls()`, invisibly

gsutil_ls <-
    function(source = character(1), path = character(1), recursive = TRUE)
{
    args <- c(
        "ls",
        if (recursive) "-r",
        file.path(if (nchar(source) > 0) source,
                  if (nchar(path) > 0) path)
    )
    .gsutil_do(args)
}


#' @rdname gsutil_cp
#'
#' @title Copy contents of a gcs bucket
#'
#' @param source character, a valid path to a google storage bucket
#'
#' @param destination character vector, representing a destination path
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @return Exit status of `gsutil_cp()`, invisibly
#'
#' @examples
#'
#' \dontrun{
#'    # for a single file
#'    gsutil_cp("gs://anvil-bioc/blah", "/tmp/blah-copy", recursive=FALSE, parallel=FALSE)
#'
#'    # for a folder with all contents
#'    # "/tmp/foobar-copy" must be an existing destination or must be created
#'    gsutil_cp("gs://anvil-bioc/foobar", "/tmp/foobar-copy", recursive=TRUE, parallel=TRUE)
#' }
#'
#' TODO: Add an argument 'local' which can have one of three values, c(source, destination, NULL)
#' stopifnot(local == source/destination/NULL) , if !file.exists(local), create.dir(local) to avoid error.
gsutil_cp <-
    function(source, destination,
             recursive = TRUE, parallel = TRUE)
{
    args <- c(
        if (parallel) "-m", ## Makes the operations faster
        "cp", ## cp command
        if (recursive) "-r",
        source,
        destination
    )
    .gsutil_do(args)
}


#' @rdname gsutil_stat
#'
#' @title Check if a bucket's subdirectory/file is present and get
#'     information regarding file.
#'
#' @param source character, a valid path to a google storage
#'     bucket. The path, needs the prefix 'gs://'
#'
#' @return Exit status of `gsutil_stat()`, invisibly
#'
#' @examples
#'
#' \dontrun{
#'   gsutil_stat('gs://anvil-bioc', 'blah')
#'
#' gs://anvil-bioc/blah:
#' Creation time:          Tue, 11 Jun 2019 15:24:56 GMT
#' Update time:            Tue, 11 Jun 2019 15:24:56 GMT
#' Storage class:          STANDARD
#' Content-Language:       en
#' Content-Length:         7
#' Content-Type:           application/octet-stream
#' Metadata:
#'       goog-reserved-file-mtime:1560266597
#'   Hash (crc32c):          Ww1IQg==
#'   Hash (md5):             JGp1Xnjm4whB2lXhL3hziw==
#'   ETag:                   CInT4bHe4eICEAE=
#'   Generation:             1560266696845705
#'   Metageneration:         1
#' }
#'
gsutil_stat <-
    function(source, sub_directory)
{
    ## source needs to be a valid gs uri
    stopifnot(is_gsutil_uri(source))
    ## make path
    path <- file.path(source, sub_directory)
    args <- c(
        "stat",
        path
    )
    .gsutil_do(args)
}


#' @rdname gsutil_rm
#'
#' @title Remove contents of a google cloud  bucket
#'
#' @param source character, a valid path to a google storage
#'     bucket
#'
#' @param sub_directory character vector, representing a sub directory
#'     path inside the google bucket
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @return Exit status of `gsutil_rm()`, invisibly
#'
#' @examples
#'
#' \dontrun{
#'    ## Remove a single file
#'    gsutil_rm('gs://anvil-bioc', 'blah', recursive = FALSE)
#'
#'    ## Remove a folder
#'    gsutil_rm('gs://anvil-bioc', 'foo-bar', recursive=TRUE, parallel=TRUE)
#' }
#'
gsutil_rm <-
    function(source, sub_directory, recursive = TRUE, parallel = TRUE)
{
    ## make sure source if a google cloud bucket
    stopifnot(is_gsutil_uri(source))
    ## remove
    args <- c(
        if (parallel) "-m",
        "rm",
        if (recursive) "-r",
        file.path(source, sub_directory)
    )
    .gsutil_do(args)
}


#' @rdname gsutil_rsync
#'
#' @title rsync contents of a gcs bucket
#'
#' @param source character, a path to a source url. Either a google
#'     bucket or a local path
#'
#' @param destination character, a path to a destination url. Either a
#'     google bucket or a local path.
#'
#' @param delete logical, a flag which matches the source to the
#'     destination exactly. (rsync -d)
#'
#'     The rsync -d option is very useful and commonly used, because
#'     it provides a means of making the contents of a destination
#'     bucket or directory match those of a source bucket or
#'     directory. However, please exercise caution when you use this
#'     option: It's possible to delete large amounts of data
#'     accidentally if, for example, you erroneously reverse source
#'     and destination.
#'
#' @param recursive logical, if the operation should be recursive
#'
#' @param parallel logical, if the operation should be in parallel
#'
#' @details
#'
#' To make gs://mybucket/data match the contents of the local
#'     directory "data" you could do:
#'        `gsutil rsync -d data gs://mybucket/data`
#'
#' To make the local directory "data" the same as the contents of
#'    gs://mybucket/data:
#' `gsutil rsync -d -r gs://mybucket/data data`
#'
#' @return Exit status of `gsutil_rsync()`, invisbly
#'
#' @examples
#'
#' \dontrun{
#'    ## Rsync from google bucket source to local destination
#'    gsutil_rsync('gs://anvil-bioc', '/tmp/local-copy')
#'
#'    ## Rsync from local source to google bucket
#'    gsutil_rsync('/tmp/local-copy', 'gs://anvil-bioc')
#' }

gsutil_rsync <-
    function(source, destination, delete = TRUE,
             recursive = TRUE, parallel = TRUE)
{
    ## if destination is not a google cloud repo, and does not exist
    if (!is_gsutil_uri(destination) && !dir.exists(destination))
        dir.create(destination)

    ## rsync operation
    args <- c(
        ##  -m option, to perform parallel (multi-threaded/multi-processing)
        if (parallel) "-m",
        "rsync",
        if (delete) "-d",
        if (recursive) "-r",
        source,
        destination
    )
    .gsutil_do(args)
}


#' @rdname localize
#'
#' @title Localize a Google storage bucket to the container.
#'
#' @param source character, a valid path to a google storage
#'     bucket.
#'
#' @param destination character vector, representing a local path.
#'
#' @param ... additional arguments to `gsutil_rsync()`
#'
#' @return Exit status of function, `gsutil_rsync()`
#'
#' @details localize a bucket from the google cloud storage to a local
#'     path on the machine.
#'
#' @examples
#'
#' \dontrun{
#'    localize("gs://anvil-bioc", "/tmp/test-localize")
#' }
#'
#' @export
localize <-
    function(source, destination, ...)
{
    ## make sure source is valid google
    if (!is_gsutil_uri(source))
        stop("'source' must be a google cloud storage location with prefix 'gs://'.")
    ## rsync
    gsutil_rsync(source, destination, ...)
}

#' @rdname delocalize
#'
#' @title Delocalize a Google storage bucket from the container.
#'
#' @param source character vector, representing a local path
#'
#' @param destination character, a valid path to a google storage
#'     bucket.
#'
#' @param remove_local_volume logical, remove the local mount volume.
#'
#' @param ... arguments for gsutil_rsync like, delete, recursive,
#'     parallel.
#'
#' @return Exit status of function, `gsutil_rsync()`
#'
#' @details delocalize the volume which is mounted with the help of
#'     gsutil. This syncs the library a final time and then removes
#'     the volume from the directory. Once a path is delocalized, it
#'     has to be localized again to regain access.
#'
#' @examples
#'
#' \dontrun{
#'     delocalize("/tmp/test-localize/", "gs://anvil-bioc")
#' }
#'
#' @export
delocalize <-
    function(source, destination, remove_local_volume = TRUE, ...)
{
    ## validate google bucket
    if (!is_gsutil_uri(destination))
        stop("'destination' must be a valid google cloud storage location with prefix 'gs://'.")
    ## First sync
    gsutil_rsync(source, destination, ...)
    ## then remove volume
    if (remove_local_volume) {
        unlink(source, recursive=TRUE)
    }
}


#' @rdname add_libs
#'
#' @title Add to .libPaths from a local path
#'
#' @param path character, identifying a local path
#'
#' @return character with latest .libPaths()
#'
#' @details Add the synced google bucket path(which is now local) to
#'     the library path .libPaths() so that R can see the hosted
#'     libraries.
#'
#' @examples
#'
#' \dontrun{
#'    add_libs('/tmp/my_library')
#'    localize('gs://bioconductor-full-devel', '/tmp/my_library')
#' }
#'
#' @export
add_libs <-
    function(path)
{
    ## Create directory if it doesn't exist
    ## otherwise .libPaths() doesn't work
    if (!dir.exists(path)) {
        dir.create(path)
    }
    .libPaths(c(local_path, .libPaths()))
}


#' @rdname sync
#'
#' @title Sync libraries from google bucket(source) and
#'     local_path(destination).
#'
#' @param source character, name of google bucket.
#'
#' @param destination character, represnting a local path.
#'
#' @details The sync function allows the user to sync a source and
#'     destination between a google bucket and a local directory. The
#'     'sync' function needs the correct inputs for source and
#'     destination, to do the right thing.
#'
#'     case 1: sync a google bucket to a local directory
#'
#'             All the files and folders in the google bucket (source)
#'             will be sync-ed with a local directory(destination).
#'
#'             sync("gs://anvil-bioc", "my-devel-lib")
#'
#'     case 2: sync a local directory to a google bucket
#'
#'             If you want to upload files from your local
#'             directory(source) to your google bucket(destination),
#'             you must use
#'
#'             sync("my-devel-lib", "gs://anvil-bioc")
#'
#'             If you do not use case 2, and run case 1 again, then
#'             the sync function will delete the new files which were
#'             modified in the local directory.
#'
#' @export
sync <-
    function(source, destination, ...)
{
    if (is_gsutil_uri(source)) {
        ## No need to .gcs_pathify as "localize" will call it.
        localize(source, destination, ...)
    }
    ## Do not remove local volume
    if (is_gsutil_uri(destination)) {
        delocalize(source, destination,
                   remove_local_volume = FALSE, ...)
    }
}

.install_find_dependencies <-
    function(packages, lib)
{
    ## find dependencies
    db <- available.packages(repos = BiocManager::repositories())
    deps <- unlist(tools::package_dependencies(packages, db), use.names=FALSE)
    installed <- rownames(installed.packages(lib.loc = lib))
    unique(c(packages, deps[!deps %in% installed]))
}

## TODO: write test cases for testing .choose_google_bucket
.choose_google_bucket <-
    function()
{
    version <- as.character(BiocManager::version())
    ## If the version is devel
    if (BiocManager:::isDevel())
        google_bucket <- "gs://bioconductor-full-devel"
    else {
        release_version <- sub(".", "-", version, fixed=TRUE)
        google_bucket <- paste0(
            "gs://bioconductor-full-release", release_version
        )
    }
    google_bucket
}

.package_exists <-
    function(google_bucket, pkgs = character())
{
    for (pkg in pkgs) {
        gsutil_stat(source = google_bucket, sub_directory = pkg)
    }
}


#' @rdname install
#'
#' @title Install libraries and it's dependencies from a bucket.
#'
#' @param packages character, names of packages to install from a
#'     bucket.
#'
#' @param google_bucket character, a valid path to a google storage
#'     bucket.
#'
#' @param lib character, file path to .libPaths()[1], primary location
#'     to install packages.
#'
#' @details Given a pre-existing google bucket with a host of
#'     bioconductor packages built on a specific matching version of R
#'     and Bioconductor, the user is able to install packages his
#'     container using the 'install()` function from the google
#'     bucket.
#'
#' eg:
#'
#' add_libs("/tmp/host-site-library")
#'
#' install(packages = c('BiocParallel', ='BiocGenerics'),
#'             google_bucket = "bioc_release_volume",
#'             lib = .libPaths()[1])
#'
#' @return character, location of packages installed invisibly
#'
#' @export
install <-
    function(pkgs = character(), lib = .libPaths()[1], lib.loc = NULL)
{
    ## Validate arguments
    stopifnot(is.character(pkgs), !anyNA(pkgs))

    packages <- .install_find_dependencies(pkgs, lib = lib.loc)
    ## copy from bucket to .libPaths()[1]
    ## Assumes packages are already in the google bucket

    google_bucket <- .choose_google_bucket()

    ## TODO: check if the package is in the bucket, if not throw error
    if (!.package_exists(google_bucket, pkgs))
        stop("package does not exist")

    if (!is_gsutil_uri(google_bucket))
        stop("not a valid google bucket which exists in the GCP account")
    packages <- sprintf("%s/%s", google_bucket, packages)

    ## if there is more than one "source" i.e package(s)
    ## FIXME: gsutil_rsync should be vectorized, including 0-length source
    for (source in packages) {
        gsutil_rsync(
            source = source, desination = lib, delete = TRUE,
            recursive = TRUE
        )
    }
    invisible(file.path(lib, basename(packages)))
}


## TODO: Figure out 3.8 or 3.9 from BiocVersion, (what version?)

## No argument to google bucket in AnVIL::install() -- > signature is
## going to be

## install(packages, lib = .libPaths()[1], lib.loc) (do the right
## thing if given just 'packages')
