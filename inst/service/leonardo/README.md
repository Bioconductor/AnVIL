The leonardo service uses openapi 3.0, but our software (currently)
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
api-spec-converter -f openapi_3 -t swagger_2 \
  https://raw.githubusercontent.com/DataBiosphere/leonardo/develop/http/src/main/resources/swagger/api-docs.yaml > \
  api.yaml
```

[LucyBot]: https://github.com/LucyBot-Inc/api-spec-converter
