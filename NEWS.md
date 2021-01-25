# AnVIL 1.4.0

NEW FEATURES

- (v 1.3.1) support Rawls() service (more fine-grained implementation
  / extension of the 'Terra()' orchestration API).

- (v 1.3.2) introduce avworkspace_*() functions for viewing and updating
  workflow configurations.

- (v 1.3.3) introduce avnotebooks_() functions for managing notebooks
  on workspaces and runtimes.

- (v 1.3.11) introduce avtable_paged() for page-wise access to tables

- (v 1.3.14) introduce avworkspace_clone() for cloning existing
  workspaces.

USER VISIBLE CHANGES

- (v 1.3.1) service functions have signatures like `fun(x, ...,
  .__body__ = list(y))`, where `x` is a argument for the 'URL' of the
  RESTful interface, and `y` is an argument for the 'BODY' of POST and
  similar requests. The `...` provide backward compatibility, and is
  used to populate elements of `.__body__`; the full interface is
  required when URL and BODY have identically named arguments.
  
- (v 1.3.10, 1.3.11) return 'entity' column with name 'table_id',
  rather than 'name'

BUG FIXES

- (v 1.3.6) when `.__body__` consists of 1 argument, it is represented
  as an unnamed set.

- (v 1.3.7) allow positional matching for `.__body__` arguments

# AnVIL 1.2.0

NEW FEATURES

- (v 1.1.3) introduce .deprecated flag in operations() / tags(); don't include
  deprecated APIs by default; warn on use of deprecated APIs.

- (v 1.1.4) add repositories() to return binary (if available),
  Bioconductor, and CRAN repository paths.

- (v 1.1.6) provide md5sum as check on service version.

- (v 1.1.9) add avfiles_*() for managing workspace bucket files.

- (v 1.1.15) add avtable_import_set() to create subsets of tables,
  following the Terra data model.

- (v 1.1.16) add avruntimes(), avworkspace_jobs() to query for runtimes
  and jobs associated with the active billing account.

- (v 1.1.17) add avdisks() to query for persistent disks associate
  with the active billing account.

- (v 1.1.21) add avworkflow_*() for interacting with workflow jobs
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
