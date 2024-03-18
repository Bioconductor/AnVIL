#' @rdname avworkflow_configuration
#'
#' @name avworkflow_configurations
#'
#' @title Workflow configuration
#'
#' @inheritParams avworkflow
#'
#' @description Funtions on this help page facilitate getting,
#'     updating, and setting workflow configuration parameters. See
#'     `?avworkflow` for additional relevant functionality.
#'
#' @seealso The help page `?avworkflow` for discovering, running,
#'     stopping, and retrieving outputs from workflows.
NULL


##
## namespace / name management
##

.avworkflow <- local({
    hash <- new.env(parent = emptyenv())
    function(key, value) {
        if (!is.null(value))
            hash[[key]] <- value
        hash[[key]]
    }
})

#' @rdname avworkflow_configuration
#'
#' @description `avworkflow_namespace()` and `avworkflow_name()` are
#'     utility functions to record the workflow namespace and name
#'     required when working with workflow
#'     configurations. `avworkflow()` provides a convenient way to
#'     provide workflow namespace and name in a single command,
#'     `namespace/name`.
#'
#' @param workflow_namespace character(1) AnVIL workflow namespace, as
#'     returned by, e.g., the `namespace` column of `avworkflows()`.
#'
#' @param workflow_name character(1) AnVIL workflow name, as returned
#'     by, e.g., the `name` column of `avworkflows()`.
#'
#' @param workflow character(1) representing the combined workflow
#'     namespace and name, as `namespace/name`.
#'
#' @return `avworkflow_namespace()`, and `avworkflow_name()` return
#'     `character(1)` identifiers. `avworkflow()` returns the
#'     character(1) concatenated namespace and name. The value
#'     returned by `avworkflow_name()` will be percent-encoded (e.g.,
#'     spaces `" "` replaced by `"%20"`).
#'
#' @export
avworkflow_namespace <-
    function(workflow_namespace = NULL)
{
    .avworkflow("NAMESPACE", workflow_namespace)
}

#' @rdname avworkflow_configuration
#'
#' @export
avworkflow_name <-
    function(workflow_name = NULL)
{
    value <- .avworkflow("NAME", workflow_name)
    URLencode(value)
}

#' @rdname avworkflow_configuration
#'
#' @export
avworkflow <-
    function(workflow = NULL)
{
    stopifnot(
        is.null(workflow) || .is_scalar_character(workflow)
    )

    if (!is.null(workflow)) {
        wkflow <- strsplit(workflow, "/")[[1]]
        if (length(wkflow) != 2L)
            stop(
                "'workflow' must be fo the form 'namespace/name', ",
                "with a single '/'"
            )
        avworkflow_namespace(wkflow[[1]])
        avworkflow_name(wkflow[[2]])
    }
    paste0(avworkflow_namespace(), "/", avworkflow_name())
}

.avworkflow_response <-
    function(config)
{
    response <- Rawls()$method_inputs_outputs(
        config$name,
        config$namespace,
        config$methodRepoMethod$methodPath,
        config$methodRepoMethod$methodUri,
        config$methodRepoMethod$methodVersion,
        config$methodRepoMethod$sourceRepo
    )
    .avstop_for_status(response, "avworkflow_response")
}

#' @importFrom dplyr left_join
.avworkflow_configuration_template_io <-
    function(template, config)
{
    if (length(config)) {
        config <-
            tibble::enframe(config, value = "attribute") |>
            tidyr::unnest("attribute")
        template <- left_join(
            template,
            config,
            by = "name"
        )
    } else {
        template <- cbind(template, attribute = character(nrow(template)))
    }
    as_tibble(template)
}

#' @rdname avworkflow_configuration
#'
#' @description `avworkflow_configuration_get()` returns a list structure
#'     describing an existing workflow configuration.
#'
#' @inheritParams av
#'
#' @return `avworkflow_configuration_get()` returns a list structure
#'     describing the configuration. See
#'     `avworkflow_configuration_template()` for the structure of a
#'     typical workflow.
#'
#' @export
avworkflow_configuration_get <-
    function(workflow_namespace = avworkflow_namespace(),
             workflow_name = avworkflow_name(),
             namespace = avworkspace_namespace(),
             name = avworkspace_name())
{
    stopifnot(
        .is_scalar_character(workflow_name),
        .is_scalar_character(workflow_namespace),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    config <- Rawls()$get_method_configuration(
        namespace, URLencode(name),
        workflow_namespace, workflow_name
    )
    .avstop_for_status(config, "avworkflow_methods")
    config_list <- config %>% as.list()
    class(config_list) <- c("avworkflow_configuration", class(config_list))
    config_list
}

#' @rdname avworkflow_configuration
#'
#' @description `avworkflow_configuration_inputs()` returns a
#'     data.frame template for the inputs defined in a workflow
#'     configuration.  This template can be used to provide custom
#'     inputs for a configuration.
#'
#' @return `avworkflow_configuration_inputs()` returns a data.frame
#'     providing a template for the configuration inputs, with the
#'     following columns:
#'
#' - inputType
#' - name
#' - optional
#' - attribute
#'
#' @return The only column of interest to the user is the `attribute`
#'     column, this is the column that should be changed for
#'     customization.
#'
#' @export
avworkflow_configuration_inputs <-
    function(config)
{
    stopifnot(inherits(config, "avworkflow_configuration"))
    response <- .avworkflow_response(config)
    inputs_tmpl <- as.list(response)$inputs
    inputs_config <- config$inputs
    .avworkflow_configuration_template_io(inputs_tmpl, inputs_config)
}

#' @rdname avworkflow_configuration
#'
#' @description `avworkflow_configuration_outputs()` returns a
#'     data.frame template for the outputs defined in a workflow
#'     configuration.  This template can be used to provide custom
#'     outputs for a configuration.
#'
#' @return `avworkflow_configuration_outputs()` returns a data.frame
#'     providing a template for the configuration outputs, with the
#'     following columns:
#' - name
#' - outputType
#' - attribute
#'
#' @return The only column of interest to the user is the `attribute`
#'     column, this is the column that should be changed for
#'     customization.
#'
#' @export
avworkflow_configuration_outputs <-
    function(config)
{
    stopifnot(inherits(config, "avworkflow_configuration"))
    response <- .avworkflow_response(config)
    outputs_tmpl <- as.list(response)$outputs
    outputs_config <- config$outputs
    .avworkflow_configuration_template_io(outputs_tmpl, outputs_config)
}

.avworkflow_MethodRepoMethod_validate <-
    function(methodRepoMethod, .schema)
{
    args <- formals(.schema)
    unknown <- setdiff(names(methodRepoMethod), names(args))
    if (length(unknown))
        stop(
            "unknown 'methodRepoMethod' names: '",
            paste(unknown, collapse = "' '"),
            "'"
        )
    idx <- !names(methodRepoMethod) %in% "methodVersion"
    ok <- vapply(methodRepoMethod[idx], .is_scalar_character, logical(1))
    if (!all(ok))
        stop(
            "'methodRepoMethod' values must be character(1); bad values: '",
            paste(names(ok)[!ok], collapse = "' '"),
            "'"
        )
    methodVersion <- methodRepoMethod$methodVersion
    stopifnot(
        `'methodRepoMethod$methodVersion' must be character(1) or integer(1)` =
            .is_scalar_character(methodVersion) ||
            .is_scalar_integer(as.integer(methodVersion))
    )

    ## all elements are unboxed
    lapply(methodRepoMethod, jsonlite::unbox)
}

.avworkflow_configuration_set_validate_response <-
    function(response, op)
{
    lens <- lengths(response)
    bad <- grepl("^(extra|invalid)", names(lens)) & lens != 0L
    if (any(bad)) {
        warning(
            "'avworkflow_configuration_set' bad values:", immediate. = TRUE
        )
        sink(stderr())
        on.exit(sink(NULL))
        print(response[bad])
    }

    invisible(response)
}

#' @rdname avworkflow_configuration
#'
#' @description `avworkflow_configuration_update()` returns a list structure
#'     describing a workflow configuration with updated inputs and / or outputs.
#'
#' @param config an `avworkflow_configuration` object to be updated.
#'
#' @param inputs the new inputs to be updated in the workflow configuration. If
#'     none are specified, the inputs from the original configuration will be
#'     used and no changes will be made.
#'
#' @param outputs the new outputs to be updated in the workflow configuration.
#'     If none are specified, the outputs from the original configuration will
#'     be used and no changes will be made.
#'
#' @return `avworkflow_configuration_update()` returns a list structure
#'     describing the updated configuration.
#'
#' @export
avworkflow_configuration_update <-
    function(config,
             inputs = avworkflow_configuration_inputs(config),
             outputs = avworkflow_configuration_outputs(config))
{
    stopifnot(
        inherits(config, "avworkflow_configuration"),
        all(c("name", "attribute") %in% names(inputs)),
        all(c("name", "attribute") %in% names(outputs)),
        is(inputs, "data.frame"), is(outputs, "data.frame")
    )
    config$inputs <-
        lapply(setNames(as.list(inputs$attribute), inputs$name), unbox)
    config$outputs <-
        lapply(setNames(as.list(outputs$attribute), outputs$name), unbox)
    config
}

#' @rdname avworkflow_configuration
#'
#' @description `avworkflow_configuration_set()` updates an existing
#'     configuration in Terra / AnVIL, e.g., changing inputs to the
#'     workflow.
#'
#' @param config a named list describing the full configuration, e.g.,
#'     created from editing the return value of
#'     `avworkflow_configuration_set()` or
#'     `avworkflow_configuration_template()`.
#'
#' @param dry logical(1) when `TRUE` (default), report the
#'     consequences but do not perform the action requested. When
#'     `FALSE`, perform the action.
#'
#' @return `avworkflow_configuration_set()` returns an object
#'     describing the updated configuration. The return value includes
#'     invalid or unused elements of the `config` input. Invalid or
#'     unused elements of `config` are also reported as a warning.
#'
#' @examples
#' if (
#'     gcloud_exists() && identical(avplatform_namespace(), "AnVILGCP") &&
#'     nzchar(avworkspace_name())
#' ) {
#' ## set the namespace and name as appropriate
#' avworkspace("bioconductor-rpci-anvil/Bioconductor-Workflow-DESeq2")
#'
#' ## discover available workflows in the workspace
#' avworkflows()
#'
#' ## record the workflow of interest
#' avworkflow("bioconductor-rpci-anvil/AnVILBulkRNASeq")
#'
#' ## what workflows are available?
#' available_workflows <- avworkflows()
#'
#' ## retrieve the current configuration
#' config <- avworkflow_configuration_get()
#' config
#'
#' ## what are the inputs and outputs?
#' inputs <- avworkflow_configuration_inputs(config)
#' inputs
#'
#' outputs <- avworkflow_configuration_outputs(config)
#' outputs
#'
#' ## update inputs or outputs, e.g., this input can be anything...
#' inputs <-
#'     inputs |>
#'     mutate(attribute = ifelse(
#'         name == "salmon.transcriptome_index_name",
#'         '"new_index_name"',
#'         attribute
#'     ))
#' new_config <- avworkflow_configuration_update(config, inputs)
#' new_config
#'
#' ## set the new configuration in AnVIL; use dry = FALSE to actually
#' ## update the configuration
#' avworkflow_configuration_set(config)
#' }
#'
#' ## avworkflow_configuration_template() is a utility function that may
#' ## help understanding what the inputs and outputs should be
#' avworkflow_configuration_template() |>
#'     str()
#'
#' @export
avworkflow_configuration_set <-
    function(config,
             namespace = avworkspace_namespace(),
             name = avworkspace_name(),
             dry = TRUE)
{
    stopifnot(
        inherits(config, "avworkflow_configuration"),
        .is_scalar_character(namespace),
        .is_scalar_character(name)
    )

    rawls <- Rawls()

    config$methodRepoMethod <- .avworkflow_MethodRepoMethod_validate(
        config$methodRepoMethod, schemas(rawls)$MethodRepoMethod
    )

    if (dry) {
        message(
            "'avworkflow_configuration_set()' arguments validated; ",
            "use 'dry = FALSE' to update ",
            "workflow ", paste0(config$namespace, "/", config$name), " in ",
            "workspace ", paste0(namespace, "/", name)
        )
        return(invisible(config))
    }

    response <- rawls$update_method_configuration(
        namespace, URLencode(name), config$namespace, config$name,
        .__body__ = config
    )
    .avstop_for_status(response, "avworkflow_configuration_set")

    response <-
        response %>%
        as.list()
    .avworkflow_configuration_set_validate_response(response)
    return(invisible(config))
}

#' @rdname avworkflow_configuration
#'
#' @description `avworkflow_configuration_template()` returns a
#'     template for defining workflow configurations. This template
#'     can be used as a starting point for providing a custom
#'     configuration.
#'
#' @details The exact format of the configuration is important.
#'
#' One common problem is that a scalar character vector `"bar"` is
#' interpretted as a json 'array' `["bar"]` rather than a json string
#' `"bar"`. Enclose the string with `jsonlite::unbox("bar")` in the
#' configuration list if the length 1 character vector in R is to be
#' interpretted as a json string.
#'
#' A second problem is that an unquoted unboxed character string
#' `unbox("foo")` is required by AnVIL to be quoted. This is reported
#' as a warning() about invalid inputs or outputs, and the solution is
#' to provide a quoted string `unbox('"foo"')`.
#'
#' @return `avworkflow_configuration_template()` returns a list
#'     providing a template for configuration lists, with the
#'     following structure:
#'
#' - namespace character(1) configuration namespace.
#' - name character(1) configuration name.
#' - rootEntityType character(1) or missing. the name of the table
#'   (from `avtables()`) containing the entitites referenced in
#'   inputs, etc., by the keyword 'this.'
#' - prerequisites named list (possibly empty) of prerequisites.
#' - inputs named list (possibly empty) of inputs. Form of input
#'   depends on method, and might include, e.g., a reference to a
#'   field in a table referenced by `avtables()` or a character string
#'   defining an input constant.
#' - outputs named list (possibly empty) of outputs.
#' - methodConfigVersion integer(1) identifier for the method
#'   configuration.
#' - methodRepoMethod named list describing the method, with
#'   character(1) elements described in the return value for `avworkflows()`.
#'   - methodUri
#'   - sourceRepo
#'   - methodPath
#'   - methodVersion. The REST specification indicates that this has
#'     type `integer`, but the documentation indicates either
#'     `integer` or `string`.
#' - deleted logical(1) of uncertain purpose.
#'
#' @examples
#' avworkflow_configuration_template()
#'
#' @export
avworkflow_configuration_template <-
    function()
{
    list(
        ## method namespace and name, not workspace namespace and name
        namespace = character(1),
        name = character(1),
        rootEntityType = character(0),
        prerequisites = setNames(list(), character()),
        inputs = setNames(list(), character()),
        outputs = setNames(list(), character()),
        methodConfigVersion = integer(1),
        methodRepoMethod = list(
            methodUri = unbox(character(1)),
            sourceRepo = unbox(character(1)),
            methodPath = unbox(character(1)),
            methodVersion = unbox(character(1))
        ),
        deleted = logical(1)
    )
}

#' @rdname avworkflow_configuration
#'
#' @param x Object of class `avworkflow_configuration`.
#'
#' @param ... additional arguments to `print()`; unused.
#'
#' @export
print.avworkflow_configuration <-
    function(x, ...)
{
    str(x)
}
