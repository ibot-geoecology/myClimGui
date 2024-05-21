.ui_get_main <- function(data, data_loggers) {
    header <- shinydashboard::dashboardHeader(title = "myClimGui")
    sidebar <- .ui_get_sidebar_ui(data, data_loggers)
    body <- .ui_get_body_ui(data)

    return(shinydashboard::dashboardPage(header, sidebar, body))
}
.ui_get_sidebar_ui <- function(data, data_loggers) {
    shinydashboard::dashboardSidebar(
        shinyjs::useShinyjs(),
        shiny::tags$style(
            shiny::HTML(".sidebar {height: calc(100vh - 50px); overflow-y: auto; }")
        ),
        shiny::checkboxGroupInput("settings_checkboxes", label=NULL,
                                  choices=c("Multi select" = .app_const_SETTINGS_MULTI_SELECT_KEY,
                                            "Plotly" = .app_const_SETTINGS_PLOTLY_KEY,
                                            "Color by logger" = .app_const_SETTINGS_COLOR_BY_LOGGER_KEY),
                                  selected=.app_const_SETTINGS_MULTI_SELECT_KEY),
        shiny::selectInput("sensor_select", "Sensors", sort(.data_get_sensors(data)), width="100%",
                           multiple=TRUE),
        shinyTree::shinyTree("data_tree", checkbox=TRUE, theme="proton", themeIcons=FALSE),
        shiny::radioButtons("data_loggers", label=NULL, choices=sort(names(data_loggers))),
        width=stringr::str_glue("{.app_const_SIDEBAR_WIDTH}px")
    )
}

.ui_get_body_ui <- function(data) {
    date_range <- .data_get_date_range(data, "day")
    shinydashboard::dashboardBody(
        shiny::fluidRow(
            shiny::column(
                shiny::actionButton("refresh_button", "Show", width="100%",
                                    style="background-color: green; color: white; font-weight: bold"),
                width = 2
            ),
            shiny::column(
                shiny::selectInput("facet_select", NULL, c("NULL", "locality", "physical"), selected="physical",
                                   width="100%"),
                width = 2),
            shiny::tagAppendAttributes(shiny::column(
                shiny::textOutput("datetime_range"),
                width = 4), class="text-center", style="font-weight: bold; "),
            shiny::column(
                shiny::dateRangeInput("date_range", NULL, start=date_range[[1]], end=date_range[[2]], width="100%"),
                width = 3
            ),
            shiny::column(
                shiny::actionButton("reset_button", NULL, icon=shiny::icon("refresh"), width="100%"),
                width = 1
            )
        ),
        shiny::fluidRow(
            shiny::column(
                shiny::tags$style(type = "text/css",
                                  paste0("#plot_plotly {height: calc(100vh - 140px) !important;} ",
                                         "#plot_ggplot {height: calc(100vh - 140px) !important;}")),
                plotly::plotlyOutput("plot_plotly", width="100%"),
                shiny::plotOutput("plot_ggplot", width="100%",
                                  dblclick="plot_dblclick",
                                  brush=shiny::brushOpts(id="plot_brush", resetOnNew=TRUE)),
                width = 12
            )
        )
    )
}

