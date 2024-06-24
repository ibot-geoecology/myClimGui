.app_const_SETTINGS_PLOTLY_KEY <- "plotly"
.app_const_SETTINGS_MULTI_SELECT_KEY <- "multi_select"
.app_const_SETTINGS_COLOR_BY_LOGGER_KEY <- "color_by_logger"

#' Start shiny app
#'
#' Main function, which start shiny application.
#'
#' @param data myClim data
#' @export
mcg_run <- function (data, ...) {
    data_loggers <- .data_get_locality_logger_type(data)
    ui <- .ui_get_main(data, data_loggers)

    server <- function (input, output, session) {
        shared <- .app_get_initialized_shared(data, data_loggers)
        .server_plot_get_main(input, output, session, shared)
        .server_states_get_main(input, output, session, shared)
    }

    app <- shiny::shinyApp(ui, server)
    shiny::runApp(app, ...)
}

.app_get_initialized_shared <- function(data, data_loggers) {
    result <- new.env()
    result$data <- data
    result$data_loggers <- data_loggers
    result$selection_table <- NULL
    result$selected_data <- NULL
    return(result)
}