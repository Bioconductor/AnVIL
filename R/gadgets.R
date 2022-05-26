#' @importFrom DT renderDT datatable formatStyle
.gadgets_renderDT <-
    function(tbl)
{
    force(tbl) # necessary/
    renderDT(
        datatable(
            tbl,
            fillContainer = TRUE,
            selection = list(mode = "single", target = "row"),
            options = list(dom = "ftp")
        ) |>
        formatStyle(seq_len(NROW(tbl) + 1L) - 1L, 'vertical-align'='top')
    )
}

##
## workspaces
##

.workspaces <- local({
    ## a little more responsive -- only retrieve workspaces once per session
    workspaces <- NULL
    function() {
        if (is.null(workspaces))
            workspaces <<- avworkspaces()
        workspaces
    }
})

#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom DT DTOutput renderDT
.workspace_ui <-
    function()
{
    miniPage(
        miniContentPanel(
            DTOutput("workspaces", height = "100%")
        ),
        gadgetTitleBar("AnVIL Workspaces")
    )
}

#' @importFrom shiny observeEvent stopApp
.workspace_server <-
    function(input, output, session)
{
    workspaces <- .workspaces()
    output$workspaces <- .gadgets_renderDT(workspaces)

    observeEvent(input$done, {
        selected <- input$workspaces_rows_selected
        if (is.integer(selected)) {
            workspace <- workspaces |> slice(selected)
            returnValue <- paste0(workspace$namespace, "/", workspace$name)
        } else {
            returnValue <- character()
        }
        stopApp(returnValue)
    })

    observeEvent(input$cancel, {
        stopApp(character())
    })
}

#' @importFrom shiny runGadget
.workspace_impl <-
    function(use_avworkspace = TRUE)
{
    suppressMessages({
        workspace <- runGadget(
            .workspace_ui, .workspace_server,
            stopOnCancel = FALSE
        )
    })

    if (length(workspace) && use_avworkspace) {
        avworkspace(workspace) # set workspace to selected value
        message("workspace set to '", avworkspace(), "'")
    }
    invisible(workspace)
}

#' @rdname gadgets
#'
#' @title Graphical user interfaces for common AnVIL operations
#'
#' @description `workspace()` allows choice of workspace for
#'     subsequent use. It is the equivalent of displaying workspaces
#'     with `avworkspaces()`, and setting the selected workspace with
#'     `avworkspace()`.
#'
#' @return `workspace()` returns the selected workspace as a
#'     character(1) using the format namespace/name, or character(0)
#'     if no workspace is selected.
#'
#' @examples
#' \dontrun{
#' workspace()
#' browse_workspace(use_avworkspace = FALSE)
#' tbl <- table()
#' wkflw <- workflow()
#' }
#'
#' @export
workspace <-
    function()
{
    .workspace_impl()
}

.workspace_get <-
    function(use_avworkspace = TRUE)
{
    if (use_avworkspace && nzchar(avworkspace_name(warn = FALSE))) {
        workspace <- avworkspace()
    } else {
        ## no workspace selected
        workspace <- .workspace_impl(use_avworkspace = FALSE)
    }
    if (!length(workspace))
        stop("select a workspace to visit", call. = FALSE)

    workspace
}

#' @rdname gadgets
#'
#' @description `browse_workspace()` uses `browseURL()` to open a
#'     browser window pointing to the Terra workspace.
#'
#' @param use_avworkspace logical(1) when `TRUE` (default), use the
#'     selected workspace (via `workspace()` or `avworkspace()` if
#'     available. If `FALSE` or no workspace is currently selected,
#'     use `workspace()` to allow the user to select the workspace.
#'
#' @return `browse_workspace()` returns the status of a `system()`
#'     call to launch the browser, invisibly.
#'
#' @export
browse_workspace <-
    function(use_avworkspace = TRUE)
{
    stopifnot(.is_scalar_logical(use_avworkspace))

    workspace <- .workspace_get(use_avworkspace)
    url <- paste0("https://app.terra.bio/#workspaces/", workspace)
    browseURL(url)
}

#
# tables
#

.table_ui <-
    function(input, output, session)
{
    miniPage(
        miniContentPanel(
            p(strong("Workspace:"), textOutput("workspace", inline = TRUE)),
            DTOutput("tables", height = "100%")
        ),
        gadgetTitleBar("AnVIL Tables")
    )
}

.table_server <-
    function(input, output, session)
{
    tables <- avtables()

    output$workspace <-renderText({
        avworkspace()
    })

    output$tables <- .gadgets_renderDT(tables)

    observeEvent(input$done, {
        selected <- input$tables_rows_selected
        if (is.integer(selected)) {
            returnValue <- tables |> slice(selected) |> pull(.data$table)
        } else {
            returnValue <- character()
        }
        stopApp(returnValue)
    })

    observeEvent(input$cancel, {
        stopApp(character())
    })
}

#' @rdname gadgets
#'
#' @description `table()` allows choice of table in the current
#'     workspace (selected by `avworkspace()` or `workspace()`) to be
#'     returned as a tibble. It is equivalent to invoking `avtables()`
#'     to show available tables, and `avtable()` to retrieve the
#'     selected table.
#'
#' @return `table()` returns a `tibble` representing the selected
#'     AnVIL table.
#'
#' @export
table <-
    function()
{
    workspace <- .workspace_get()
    suppressMessages({
        table <- runGadget(.table_ui, .table_server, stopOnCancel = FALSE)
    })

    if (length(table)) {
        avtable(table)
    } else {
        invisible()
    }
}

##
## workflows()
##

.workflow_ui <-
    function()
{
    miniPage(
        miniContentPanel(
            p(strong("Workspace:"), textOutput("workspace", inline = TRUE)),
            DTOutput("workflows", height = "100%")
        ),
        gadgetTitleBar("AnVIL Workflows")
    )
}

#' @importFrom shiny observeEvent stopApp
.workflow_server <-
    function(input, output, session)
{
    output$workspace <-renderText({
        avworkspace()
    })
    workflows <- avworkflows()
    output$workflows <- .gadgets_renderDT(workflows)

    observeEvent(input$done, {
        selected <- input$workflows_rows_selected
        if (is.integer(selected)) {
            workflow <- workflows |> slice(selected)
            returnValue <- paste0(workflow$namespace, "/", workflow$name)
        } else {
            returnValue <- character()
        }
        stopApp(returnValue)
    })

    observeEvent(input$cancel, {
        stopApp(character())
    })
}

#' @rdname gadgets
#'
#' @description `workflow()` allows choice of workflow for
#'     retrieval. It is the equivalent of `avworkflows()` for listing
#'     available workflows, and `avworkflow_configuration_get()` for
#'     retrieving the workflow.
#'
#' @return `workflow()` returns an `avworkflow_configuration` object
#'     representing the inputs and outputs of the selected
#'     workflow. This can be edited and updated as described in the
#'     "Running an AnVIL workflow within R" vigenette.
#'
#' @export
workflow <-
    function()
{
    .workspace_get()
    suppressMessages({
        workflow <- runGadget(
            .workflow_ui, .workflow_server, stopOnCancel = FALSE
        )
    })

    if (length(workflow)) {
        avworkflow(workflow) # set workflow to selected value
        message("workflow set to '", avworkflow(), "'")
        avworkflow_configuration_get()
    }
}
