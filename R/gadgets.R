#' @importFrom DT renderDT datatable formatStyle
.gadget_renderDT <-
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

#' @importFrom htmltools p strong
#'
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#'
#' @importFrom DT DTOutput renderDT
#'
#' @importFrom shiny textOutput
.gadget_ui <-
    function(title)
{
    force(title)
    function() {
        miniPage(
            p(
                strong("Current workspace:"),
                textOutput("workspace", inline = TRUE)
            ),
            miniContentPanel(
                DTOutput("gadget_tibble", height = "100%")
            ),
            gadgetTitleBar(title)
        )
    }
}

#' @importFrom shiny observeEvent stopApp renderText
.gadget_server <-
    function(tibble, DONE_FUN)
{
    force(tibble) # using force() improves display rendering
    force(DONE_FUN)
    function(input, output, session) {
        output$workspace <-renderText({
            if (nzchar(avworkspace_name(warn = FALSE))) {
                avworkspace()
            }
        })

        output$gadget_tibble <- .gadget_renderDT(tibble)

        observeEvent(input$done, {
            row_selected <- input$gadget_tibble_rows_selected
            if (is.integer(row_selected)) {
                returnValue <- DONE_FUN(tibble, row_selected)
            } else {
                returnValue <- character()
            }
            stopApp(returnValue)
        })

        observeEvent(input$cancel, {
            stopApp(NULL)
        })
    }
}

#' @rdname gadgets_developer
#'
#' @title Functions to implement AnVIL gadget interfaces
#'
#' @description Functions documented on this page are primarily
#'     intended for package developers wishing to implement gadgets
#'     (graphical interfaces) to navigating AnVIL-generated tables.
#'
#' @description `.gadget_run()` presents the user with a
#'     tibble-navigating gadget, returning the value of `DONE_FUN` if
#'     a row of the tibble is selected, or NULL.
#'
#' @param title character(1) (required) title to appear at the base of
#'     the gadget, e.g., "AnVIL Workspaces".
#'
#' @param tibble a `tibble` or `data.frame` to be displayed in the
#'     gadget.
#'
#' @param DONE_FUN a function of two arguments, `tibble` and
#'     `row_selected`. The tibble is the `tibble` provided as an
#'     argument to `.gadget_run()`. `row_selected` is the row
#'     selected in the gadget by the user. The function is only
#'     invoked when the user selects a valid row.
#'
#' @return `.gadget_run()` returns the result of `DONE_FUN()` if a row
#'     has been selected by the user, or `NULL` if no row is selected
#'     (the user presses `Cancel`, or `Done` prior to selecting any
#'     row).
#'
#' @examples
#' \dontrun{
#' tibble <- avworkspaces()
#' DONE_FUN <- function(tibble, row_selected) {
#'     selected <- slice(tibble, row_selected)
#'     with(selected, paste0(namespace, "/", name))
#' }
#' .gadget_run("AnVIL Example", tibble, DONE_FUN)
#' }
#' @importFrom shiny runGadget
#'
#' @export
.gadget_run <-
    function(title, tibble, DONE_FUN)
{
    stopifnot(
        isScalarCharacter(title),
        is.data.frame(tibble)
    )
    suppressMessages({
        runGadget(
            .gadget_ui(title),
            .gadget_server(tibble, DONE_FUN),
            stopOnCancel = FALSE
        )
    })
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
#' wkflw <- avworkflow_gadget()
#' }
#'
#' @export
avworkspace_gadget <-
    function()
{
    .workspace_impl()
}

.workspaces <- local({
    ## a little more responsive -- only retrieve workspaces once per session
    workspaces <- NULL
    function() {
        if (is.null(workspaces))
            workspaces <<- avworkspaces()
        workspaces
    }
})

.workspace_impl <-
    function(use_avworkspace = TRUE)
{
    DONE_FUN <- function(tibble, row_selected)
        paste0(tibble$namespace[row_selected], "/", tibble$name[row_selected])

    workspace <- .gadget_run("AnVIL Workspaces", .workspaces(), DONE_FUN)

    if (length(workspace)) {
        avworkspace(workspace) # set workflow to selected value
        message("workspace set to '", avworkspace(), "'")
    }

    invisible(workspace)
}

.workspace_get <-
    function(use_avworkspace = TRUE)
{
    if (use_avworkspace && nzchar(avworkspace_name(warn = FALSE))) {
        workspace <- avworkspace()
    } else {
        ## no workspace currently selected
        workspace <- .workspace_impl(use_avworkspace = TRUE)
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
#' @importFrom utils browseURL
#'
#' @export
browse_workspace <-
    function(use_avworkspace = TRUE)
{
    stopifnot(isScalarLogical(use_avworkspace))

    workspace <- .workspace_get(use_avworkspace)
    url <- paste0("https://app.terra.bio/#workspaces/", workspace)
    browseURL(url)
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
avtable_gadget <-
    function()
{
    DONE_FUN <- function(tibble, row_selected)
        tibble$table[row_selected]

    workspace <- .workspace_get() # maybe prompt for workspace

    table <- .gadget_run("AnVIL Tables", avtables(), DONE_FUN)

    if (length(table)) {
        avtable(table)
    } else {
        invisible()
    }
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
avworkflow_gadget <-
    function()
{
    DONE_FUN <- function(tibble, row_selected)
        paste0(tibble$namespace[row_selected], "/", tibble$name[row_selected])

    workspace <- .workspace_get()

    workflow <- .gadget_run("AnVIL Workflows", avworkflows(), DONE_FUN)

    if (length(workflow)) {
        avworkflow(workflow) # set workflow to selected value
        message("workflow set to '", avworkflow(), "'")
        avworkflow_configuration_get()
    }
}
