# AnVIL R / Bioconductor package

This is a work in progress. The package requires authorized access to
AnVIL resources, and therefore is not useful to the general public.

Current functionality includes:

- OAuth 2.0 authentication against the `anvil-leo-dev` google cloud app.
- Proof-of-concept implementation of [REST endpoints][1]

Before installation, the cloned package [needs to be completed][2] by
adding application credentials to `inst/extdata/leonardo_access.json`;
this requires project access and hence is not available to the general
public.

[1]: https://leonardo.dev.anvilproject.org/api-docs.yaml
[2]: https://github.com/Bioconductor/AnVIL/blob/fb21da593ac09f3a62dcee542734b6eb421240ed/man/anvil.Rd#L31
