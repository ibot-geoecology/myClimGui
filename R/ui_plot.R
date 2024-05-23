.ui_plot_tab <- function(data, data_loggers) {
    shiny::tabPanel("Plot",
                    shiny::tags$style(
                        shiny::HTML("#sidebar {height: calc(100vh - 95px); overflow-y: auto; }")),
                    shiny::sidebarLayout(
                        .ui_plot_sidebar(data, data_loggers),
                        .ui_plot_body(data),
                        position = c("left", "right"),
                        fluid = TRUE)
    )
}

.ui_plot_sidebar <- function(data, data_loggers) {
    shiny::sidebarPanel(
        shinyjs::useShinyjs(),
        shiny::checkboxGroupInput("settings_checkboxes", label=NULL,
                                  choices=c("Multi select" = .app_const_SETTINGS_MULTI_SELECT_KEY,
                                            "Plotly" = .app_const_SETTINGS_PLOTLY_KEY,
                                            "Color by logger" = .app_const_SETTINGS_COLOR_BY_LOGGER_KEY),
                                  selected=.app_const_SETTINGS_MULTI_SELECT_KEY),
        shiny::selectInput("sensor_select", "Sensors", sort(.data_get_sensors(data)), width="100%",
                           multiple=TRUE),
        shinyTree::shinyTree("data_tree", checkbox=TRUE, theme="proton", themeIcons=FALSE),
        shiny::radioButtons("data_loggers", label=NULL, choices=sort(names(data_loggers))),
        id="sidebar",
        width=3
    )
}

.ui_plot_body <- function(data) {
    date_range <- .data_get_date_range(data, "day")
    return(shiny::mainPanel(
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
        ),
        width = 9
    ))
}