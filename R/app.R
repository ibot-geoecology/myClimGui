.app_const_SIDEBAR_WIDTH <- 300
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
        .server_plot_get_main(data, data_loggers, input, output, session)
    }

    app <- shiny::shinyApp(ui, server)
    shiny::runApp(app, ...)
}
