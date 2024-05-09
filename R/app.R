.app_const_SIDEBAR_WIDTH <- 300

#' Start shiny app
#'
#' Main function, which start shiny application.
#'
#' @param data myClim data
#' @export
mcg_run <- function (data, ...) {
    data_loggers <- .data_get_locality_logger_type(data)

    header <- shinydashboard::dashboardHeader(title = "myClimGui")
    sidebar <- .app_get_sidebar_ui(data, data_loggers)
    body <- .app_get_body_ui(data)

    ui <- shinydashboard::dashboardPage(header, sidebar, body)

    server <- function (input, output, session) {
        return(.app_get_server(data, data_loggers, input, output, session))
    }

    app <- shiny::shinyApp(ui, server)
    shiny::runApp(app, ...)
}

.app_get_sidebar_ui <- function(data, data_loggers) {
    shinydashboard::dashboardSidebar(
        shinyjs::useShinyjs(),
        shiny::tags$style(
            shiny::HTML(".sidebar {height: calc(100vh - 50px); overflow-y: auto; }")
        ),
        shiny::checkboxInput("multi_select_checkbox", "Multi select", value=TRUE),
        shiny::selectInput("sensor_select", "Sensors", sort(.data_get_sensors(data)), width="100%",
                           multiple=TRUE),
        shinyTree::shinyTree("data_tree", checkbox=TRUE, theme="proton", themeIcons=FALSE),
        shiny::radioButtons("data_loggers", label=NULL, choices=names(data_loggers)),
        width=stringr::str_glue("{.app_const_SIDEBAR_WIDTH}px")
    )
}

.app_get_body_ui <- function(data) {
    date_range <- .data_get_date_range(data)
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
            shiny::column(
                shiny::checkboxInput("plotly_checkbox", "Use plotly", value=FALSE),
                width = 2),
            shiny::column(
                shiny::checkboxInput("color_by_logger_checkbox", "Color by logger", value=FALSE),
                width = 2),
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

.app_get_server <- function(data, data_loggers, input, output, session) {
    previous_sensors <- shiny::reactiveVal()
    zoom_range <- shiny::reactiveVal()

    shiny::observeEvent(input$plotly_checkbox, {
        .app_plotly_checkbox_event(input)
    })

    shiny::observeEvent(input$multi_select_checkbox, .app_multi_select_checkbox_event(input))

    shiny::observeEvent(input$reset_button, {
        date_range <- .data_get_date_range(data)
        shiny::updateDateRangeInput(session, "date_range",
                                    start=date_range[[1]], end=date_range[[2]])
    })

    shiny::observeEvent(input$sensor_select, {
            .app_sensor_select_event(input, previous_sensors)
        })
    
    shiny::observeEvent(input$plot_dblclick, {
       .app_plot_dblclick_event(input, zoom_range) 
    })

    output$data_tree <- shinyTree::renderTree({.tree_get_list(data)})

    output$plot_plotly <- plotly::renderPlotly({
        plot <- .app_render_plot_common(data, data_loggers, input)
        if(is.null(plot)) {
            return(NULL)
        }
        return(plotly::ggplotly(plot))
    })

    output$plot_ggplot <- shiny::renderPlot({
        zoom_range_value <- zoom_range()
        plot <- .app_render_plot_common(data, data_loggers, input, zoom_range_value)
        if(is.null(plot)) {
            return(NULL)
        }
        return(plot)
    })
}

.app_plotly_checkbox_event <- function(input) {
    use_plotly <- input$plotly_checkbox
    if(use_plotly) {
        shinyjs::show(id="plot_plotly")
        shinyjs::hide(id="plot_ggplot")
    } else {
        shinyjs::show(id="plot_ggplot")
        shinyjs::hide(id="plot_plotly")
    }
}

.app_multi_select_checkbox_event <- function(input) {
    multi_select <- input$multi_select_checkbox
    if(multi_select) {
        shinyjs::show(id="sensor_select")
        shinyjs::show(id="data_tree")
        shinyjs::hide(id="data_loggers")
        shinyjs::enable(id="refresh_button")
    } else {
        shinyjs::hide(id="sensor_select")
        shinyjs::hide(id="data_tree")
        shinyjs::show(id="data_loggers")
        shinyjs::disable(id="refresh_button")
    }
}

.app_sensor_select_event <- function(input, previous_sensors) {
    tree <- shiny::isolate(input$data_tree)
    if(is.null(tree)) {
        tree <- .tree_get_list(data)
    }
    add_sensor <- length(input$sensor_select) > length(previous_sensors())
    if(add_sensor) {
        sensor <- lubridate::setdiff(input$sensor_select, previous_sensors())
    }
    else {
        sensor <- lubridate::setdiff(previous_sensors(), input$sensor_select)
    }
    shinyTree::updateTree(session, "data_tree", .tree_change_selection(tree, sensor, add_sensor))
    previous_sensors(input$sensor_select)
}

.app_plot_dblclick_event <- function(input, zoom_range) {
    brush <- input$plot_brush
    if(is.null(brush)) {
        zoom_range(NULL)
    } else {
        zoom_range(myClim:::.common_as_utc_posixct(c(brush$xmin, brush$xmax)))
    }
}

.app_render_plot_common <- function(data, data_loggers, input, zoom_range_value=NULL)
{
    input$data_loggers
    input$refresh_button
    input$plotly_checkbox
    input$multi_select_checkbox
    shiny::isolate(plot <- .app_get_plot(data, data_loggers, input, zoom_range_value))
    return(plot)
}

.app_get_plot <- function(data, data_loggers, input, zoom_range_value)
{
    data_tree <- input$data_tree
    input_data_loggers <- input$data_loggers
    date_range <- input$date_range
    if(!is.null(zoom_range_value)) {
        date_range <- zoom_range_value
    }
    selected_facet_text <- input$facet_select
    multi_select <- input$multi_select_checkbox
    if((multi_select && is.null(data_tree)) || (!multi_select && is.null(data_loggers))) {
        return(NULL)
    }
    if(multi_select) {
        slices <- shinyTree::get_selected(data_tree, format="slices")
        if(length(slices) == 0) {
            return(NULL)
        }
        filtered_data <- .tree_filter_data(data, slices)
    } else {
        selected_loggers <- data_loggers[[input_data_loggers]]
        logger_type <- NULL
        if(length(selected_loggers) == 2) {
            logger_type <- selected_loggers[[2]]
        }
        filtered_data <- myClim::mc_filter(data, localities=selected_loggers[[1]], logger_types=logger_type)
    }
    facet <- if(selected_facet_text == "NULL") NULL else selected_facet_text
    plot <- myClim::mc_plot_line(filtered_data, facet=facet, start_crop=date_range[[1]], end_crop=date_range[[2]],
                                 color_by_logger=input$color_by_logger_checkbox)
    return(plot)
}

