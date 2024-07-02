The dockstore service uses openapi 3.0, but our software (currently)
uses rapiclient, with a restriction to Swagger 2.0. The file in this
directory was created using the [LucyBot][] api-spec-converter from
the command line.

On a macOS and following the README instructions on the git repository, I did

```
$ brew install npm
$ npm install -g api-spec-converter
```

and then ran the command

```
wget -O openapi.yaml https://dockstore.org/api/openapi.yaml
## Forbidden 403 on direct URL
api-spec-converter -f openapi_3 -t swagger_2 \
  openapi.yaml > \
  api.yaml
```

[LucyBot]: https://github.com/LucyBot-Inc/api-spec-converter
