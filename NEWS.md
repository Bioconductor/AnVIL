# AnVIL 1.18.0

USER VISIBLE CHANGES

- (v 1.17.3) Added Terra Data Repository (TDR) service as `TDR()`. See service
  at https://data.terra.bio.

- (v 1.17.1) Gen3 services, `avworkflow*_configuration()` functions, `install()`,
  `repository()`, and `repositories()` have been removed.

- (v 1.17.1) Defunct `repository_stats` function in favor of
`BiocPkgTools::repositoryStats` (@LiNk-NY)

BUG FIXES AND MINOR IMPROVEMENTS

- (v 1.17.2) Use `application/json` as default `Content-Type`.

# AnVIL 1.16.0

USER VISIBLE CHANGES

- (v 1.15.10) Validate API versions against hardcoded variables; produce warning
  when discordant (@LiNk-NY, #101).

- (v 1.15.8) Add `gcloud_storage()` and `gcloud_storage_buckets()` to
  create and manage Google Cloud Storage buckets (@LiNk-NY, #72).

- Gen3 services, `avworkflow*_configuration()` functions, `install()`,
  `repository()`, and `repositories()` are defunct.

- (v 1.15.5) Catch `avtable_import_status()` errors in the response object.

- (v 1.15.1) Update vignette with examples for `avworkflow_info()` (@mtmorgan,
  @yubocheng).
  
BUG FIXES AND MINOR IMPROVEMENTS

- (v 1.15.11) Update Dockstore API file, version, and URL

- (v 1.15.9) Use assertions from `BiocBaseUtils`

- (v 1.15.7) Use `URLencode` for table in `avtable` and direct request to
  Rawls endpoint (@LiNk-NY, #98)

- (v 1.15.6) Update the Dockstore API reference URL and use
  `api_referenc_url` instead of API file (@LiNk-NY).

- Update namespace in vignette and examples (@kozo2, #54)

# AnVIL 1.14.0

NEW FEATURES

- (v 1.13.1) Add paged support for large tables in `avtable_import()`
  and `avtable_import_set()`.

- (v 1.13.2) Only show `avtable_paged()` and `avtable_import*()`
  progress bar in interactive() sessions

- (v 1.13.4) Report messages when `avtable_import_status()`
  contains one. <https://github.com/Bioconductor/AnVIL/issues/79>

- (v 1.13.3) Use 'op' when .avworkflow_response() calls avstop_for_status().
  <https://github.com/Bioconductor/AnVIL/issues/80>
  
- (v 1.13.7) Check `requester pays` for destination URIs when using
  `gsutil_cp` (@smgogarten, #82)

USER VISIBLE CHANGES

- (v 1.13.8) Update documentation on updating workflow configurations.
  (@amstilp, #84)

- (v 1.13.11) Added workflowId to `avworkflow_files()` and
  `avworkflow_localize()` to allow for filtering by workflow
  (@yubocheng, #90).


# AnVIL 1.12.0

USER VISIBLE CHANGES

- (v 1.11.2) update workflow file discovery to use API, rather than 'scraping'
  google bucket. https://github.com/Bioconductor/AnVIL/issues/69

- (v 1.11.3) Gen3 services deprecated

- (v 1.11.5) Add `na =` to handle NA encoding in `avtable()` /
  `avtable_import()`. Changes default behavior.
  <https://github.com/Bioconductor/AnVIL/issues/75>

BUG FIXES

- (v 1.11.1) consistently URLencode workspace and workflow `name`, to allow
  for spaces. https://github.com/Bioconductor/AnVIL/issues/67

# AnVIL 1.10.0

NEW FEATURES

- (v 1.9.1) add `drs_access_url()` to returned signed `https://` URLs
  from `drs://` URIs. Enhance `drs_cp()`.

- (v 1.9.4) add `auto_unbox=` argument to Service class, allowing
  other developers flexibility in unboxing values passed to REST
  APIs.

- (v 1.9.7) add developer facilities for tracking API changes in
  Rawls, Terra, and Leonardo services

USER VISIBLE CHANGES

- (v 1.9.2) Deprecate AnVIL::install() & friends in favor of
  BiocManager::install(), which now knows about container binary
  repositories.

- (v 1.9.8) Update Rawls, Terra, and Leonardo services. Changed
  endpoints include:

    ```
    ## Rawls
    $removed
    [1] admin_delete_refresh_token admin_statistics_get
    [3] refreshToken               refreshTokenDate

    $updated
    [1] listUserBillingAccounts createWorkspace         getTags
    [4] clone                   entity_type_metadata    get_entity
    [7] entityQuery             createSubmission        validateSubmission

    ## Terra
    $removed
    [1] userTrial         listImportPFBJobs importPFBStatus

    $updated
     [1] deleteBillingProject       billingAccounts
     [3] createWorkspace            cloneWorkspace
     [5] entityQuery                flexibleImportEntities
     [7] importEntities             createSubmission
     [9] validateSubmission         browserDownloadEntitiesTSV
    [11] setProfile

    ## Leonardo
    $removed
    [1] batchNodepoolCreate

    $updated
     [1] listApp                listAppByProject       deleteApp
     [4] createApp              listDisks              listDisksByProject
     [7] createDisk             updateRuntime          createRuntime
    [10] setCookie              proxyClusterJupyter    proxyClusterJupyterLab
    [13] proxyClusterRStudio
    ```

- (v 1.9.9) add 'gadgets' (simple graphical interfaces) to key
  functions, `avworkspace_gadget()`, `avtable_gadget()`,
  `avworkflow_gadget()`. Also `browse_workspace()` for opening a terra
  workspace in the browser.

BUG FIXES

- (v 1.9.3 / 1.8.2) `avworkflow_localize()` looks for `submissionId`
  files correctly.

- (v 1.9.5 / 1.8.3) `drs_stat()` works when `accessUrl` is included in
  response.

- (v 1.9.6 / 1.8.5) `gsutil_cp()` and `gsutil_rsync()` use
  `normalizePath()` on source and destination arguments to avoid
  creating directories in unexpected locations when provided with
  paths containing `~`, `.` or `..`.

- (v 19.10 / v 1.8.6) `gcloud_account("<new account>")` did not
  invalidate cached access tokens.
  https://github.com/Bioconductor/AnVIL/issues/66

- (v 1.9.11 / v 1.8.7) avoid changing status of 'Done' workflows to
  'Aborted' <https://github.com/Bioconductor/AnVIL/issues/64>

- (v 1.9.11 / v 1.8.7) allow 'NULL' for entity arguments of
  avworkflow_run() <https://github.com/Bioconductor/AnVIL/issues/65>

# AnVIL 1.8.0

NEW FEATURES

- (v 1.7.4) add `avworkflow_configuration_*()` functions for
  manipulating workflow configurations, and a vignette describing use.

- (v 1.7.5) add `avdata_import()` to import 'REFERENCE DATA' and
  'OTHER DATA' tables.

- (v 1.7.9) export `repository_stats()` to summarize binary package
  availability.

USER VISIBLE CHANGES

- (v 1.7.4) Deprecate `avworkflow_configuration()`,
  `avworkflow_import_configuration()`.

- (v 1.7.4) Update Dockstore md5sum.

- (v 1.7.5) `avdata()` is re-implemented to more faithfully report
  only 'REFERENCE DATA' and 'OTHER DATA' workspace attributes;
  previously, other attributes such as the description and tags (from
  the workspace landing page) were also reported.

BUG FIXES

- (v 1.7.4) `avworkflow_files()` and `avworkflow_localize()` do not
  fail when the workflow has produced no files.

- (v 1.7.6) improve handling of authentication token for gcloud
  utilities.

- (v 1.8.2) `avworkflow_localize()` looks for `submissionId` files
  correctly.

- (v 1.8.3) `drs_stat()` works when `accessUrl` is included in
  response.

# AnVIL 1.6.7 / 1.7.13

BUG FIXES

- Correct gcloud_project() when user environment variable set.
  https://github.com/Bioconductor/AnVIL/pull/52

# AnVIL 1.6.6

BUG FIXES

- Correct gsutil_pipe() argument mis-match, see
  https://support.bioconductor.org/p/9141780/

# AnVIL 1.6.0

NEW FEATURES

- (v. 1.5.5) add `repository()` to return the binary repository
  location, if available.

- (v. 1.5.7) `drs_stat()` and `drs_cp()` support signed URLs

USER VISIBLE CHANGES

- (v. 1.5.2) `drs_stat()` uses multiple cores (on non-Windows) to enhance
  performance

- (v. 1.5.6) `install()` delegates to `BiocManager::install()`,
  providing more flexibility (e.g., installing from GitHub) and
  robustness.

- (v. 1.5.7) `drs_stat()` returns fields more selectively.

# AnVIL 1.4.1

BUG FIXES

- Only install binary packages on Bioconductor docker images

# AnVIL 1.4.0

NEW FEATURES

- (v 1.3.1) support `Rawls()` service (more fine-grained implementation
  / extension of the 'Terra()' orchestration API).

- (v 1.3.2) introduce `avworkspace_*()` functions for viewing and updating
  workflow configurations.

- (v 1.3.3) introduce `avnotebooks_()` functions for managing notebooks
  on workspaces and runtimes.

- (v 1.3.11) introduce `avtable_paged()` for page-wise access to tables

- (v 1.3.14) introduce `avworkspace_clone()` for cloning existing
  workspaces.

- (v 1.3.21) `avworkspaces()` returns a tibble of available workspaces.

- (v 1.3.24) `gsutil_rsync()` supports a regular expresion `exclude =`
  to exclude files from synchronization.

- (v 1.3.24) `avworkflow_localize()` copies workflow control and / or
  output files to the local disk.

USER VISIBLE CHANGES

- (v 1.3.1) service functions have signatures like `fun(x, ...,
  .__body__ = list(y))`, where `x` is a argument for the 'URL' of the
  RESTful interface, and `y` is an argument for the 'BODY' of POST and
  similar requests. The `...` provide backward compatibility, and is
  used to populate elements of `.__body__`; the full interface is
  required when URL and BODY have identically named arguments.

- (v 1.3.10, 1.3.11) return 'entity' column with name `'table_id'`,
  rather than `'name'`.

- (v 1.3.22) `localize()` / `delocalize()` warn when `dry = TRUE`, so that
  lack of localization is more apparent.

- (v 1.3.24) `gsutil_stat()` returns a tibble summaring bucket status,
  rather than character().

- (v 1.3.30) Add Referer: header to all Leonardo requests

BUG FIXES

- (v 1.3.6) when `.__body__` consists of 1 argument, it is represented
  as an unnamed set.

- (v 1.3.7) allow positional matching for `.__body__` arguments

- (v. 1.2.1 / 1.3.31) drs_stat() returns a single record per URL when
  multiple hashes available.

# AnVIL 1.2.0

NEW FEATURES

- (v 1.1.3) introduce .deprecated flag in `operations()` / `tags()`; don't include
  deprecated APIs by default; warn on use of deprecated APIs.

- (v 1.1.4) add `repositories()` to return binary (if available),
  Bioconductor, and CRAN repository paths.

- (v 1.1.6) provide md5sum as check on service version.

- (v 1.1.9) add `avfiles_*()` for managing workspace bucket files.

- (v 1.1.15) add `avtable_import_set()` to create subsets of tables,
  following the Terra data model.

- (v 1.1.16) add `avruntimes()`, `avworkspace_jobs()` to query for runtimes
  and jobs associated with the active billing account.

- (v 1.1.17) add `avdisks()` to query for persistent disks associate
  with the active billing account.

- (v 1.1.21) add `avworkflow_*()` for interacting with workflow jobs
  and outputs.

# AnVIL 1.0.x

BUG FIXES

- (v 1.0.1) collapse 'produces' vectors to scalars, for httr::accept()

- (v 1.0.3) access correct binary repository, more robustly

USER VISIBLE CHANGES

- (v 1.0.2) support updated Leonardo `listRuntimes()` and friends
  (`listClusters()` deprecated)

# AnVIL 1.0.0

- AnVIL is _finally on Bioconductor!
- Support OpenAPI Specification version 2 (aka Swagger 2.0)
- `av`, `gcloud`, `gsutil` type functions added to interface with AnVIL
  and the cloud
- Support `leonardo`, `terra`, `dockstore` and `gen3*` APIs

# AnVIL 0.0.20

- Support untagged swagger

# AnVIL 0.0.17

- `leonardo`, `terra`, `dockstore` and `gen3*` symbols not defined; users must
  create these themselves, e.g., `leonardo <-

- Added a `NEWS.md` file to track changes to the package.
