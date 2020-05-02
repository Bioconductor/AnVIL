# AnVIL 1.0.x

BUG FIXES

- (v 1.0.1) collapse 'produces' vectors to scalars, for httr::accept()

USER VISIBLE CHANGES

- (v 1.0.2) support updated Leonardo `listRuntimes()` and friends
  (`listClusters()` deprecated)

# AnVIL 1.0.0

- AnVIL is _finally_ on Bioconductor!
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

