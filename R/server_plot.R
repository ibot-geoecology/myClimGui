.server_plot_get_main <- function(data, data_loggers, input, output, session) {
    previous_sensors <- shiny::reactiveVal()
    zoom_range <- shiny::reactiveVal()
    previous_selected_settings <- shiny::reactiveVal("init")
    render_plot_number <- shiny::reactiveVal(0)
    last_datetime_range <- shiny::reactiveVal(NULL)
    last_filtered_data_table <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$settings_checkboxes, {
        .server_plot_process_settings_change(input, previous_selected_settings, render_plot_number,
                                             zoom_range)
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$reset_button, {
        date_range <- .data_get_date_range(data, "day")
        shiny::updateDateRangeInput(session, "date_range",
                                    start=date_range[[1]], end=date_range[[2]])
    })

    shiny::observeEvent(input$sensor_select, {
        .server_plot_sensor_select_event(data, input, session, previous_sensors)
    })

    shiny::observeEvent(input$plot_dblclick, {
        .server_plot_dblclick_event(input, zoom_range)
    })

    output$data_tree <- shinyTree::renderTree({.tree_get_list(data)})

    output$plot_plotly <- plotly::renderPlotly({
        plot <- .server_plot_render_plot_common(data, data_loggers, session, input, render_plot_number,
                                                last_datetime_range, zoom_range, last_filtered_data_table)
        if(is.null(plot)) {
            return(NULL)
        }
        return(plotly::ggplotly(plot))
    })

    output$plot_ggplot <- shiny::renderPlot({
        zoom_range_value <- zoom_range()
        plot <- .server_plot_render_plot_common(data, data_loggers, session, input, render_plot_number,
                                                last_datetime_range, zoom_range, last_filtered_data_table)
        if(is.null(plot)) {
            return(NULL)
        }
        return(plot)
    }, res=96)

    output$datetime_range <- shiny::renderText({
        .server_plot_get_datetime_range(last_datetime_range)
    })
}

.server_plot_process_settings_change <- function(input, previous_selected_settings, render_plot_number,
                                                 zoom_range) {
    is_init <- length(previous_selected_settings()) == 1 && previous_selected_settings() == "init"
    changed_settings <- NULL
    if(!is_init){
        changed_settings <- .server_plot_changed_settings(input, previous_selected_settings())
        render_plot_number(render_plot_number()+1)
        if(changed_settings$key == .app_const_SETTINGS_PLOTLY_KEY && changed_settings$value) {
            zoom_range(NULL)
        }
    }
    if(is_init || changed_settings$key == .app_const_SETTINGS_PLOTLY_KEY) {
        .server_plot_plotly_checkbox_event(input)
    }
    if(is_init || changed_settings$key == .app_const_SETTINGS_MULTI_SELECT_KEY) {
        .server_plot_multi_select_checkbox_event(input)
    }
    previous_selected_settings(input$settings_checkboxes)
}

.server_plot_plotly_checkbox_event <- function(input) {
    use_plotly <- .server_plot_selected_settings(input, .app_const_SETTINGS_PLOTLY_KEY)
    if(use_plotly) {
        shinyjs::show(id="plot_plotly")
        shinyjs::hide(id="plot_ggplot")
    } else {
        shinyjs::show(id="plot_ggplot")
        shinyjs::hide(id="plot_plotly")
    }
}

.server_plot_multi_select_checkbox_event <- function(input) {
    multi_select <- .server_plot_selected_settings(input, .app_const_SETTINGS_MULTI_SELECT_KEY)
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

.server_plot_sensor_select_event <- function(data, input, session, previous_sensors) {
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

.server_plot_dblclick_event <- function(input, zoom_range) {
    brush <- input$plot_brush
    if(is.null(brush)) {
        zoom_range(NULL)
    } else {
        zoom_range(myClim:::.common_as_utc_posixct(c(brush$xmin, brush$xmax)))
    }
}

.server_plot_render_plot_common <- function(data, data_loggers, session, input, render_plot_number,
                                            last_datetime_range, zoom_range, last_filtered_data_table)
{
    input$data_loggers
    input$refresh_button
    input$facet_select
    render_plot_number()
    shiny::isolate(filtered_data <- .server_plot_get_plot_data(data, data_loggers, input))
    if(is.null(filtered_data)) {
        return(NULL)
    }
    if(.server_plot_reset_zoom_range_if_need(filtered_data, last_filtered_data_table, zoom_range)) {
        return(NULL)
    }
    filtered_data <- .server_plot_crop_plot_data(filtered_data, input, zoom_range)
    last_datetime_range(.data_get_date_range(filtered_data))
    shiny::isolate(plot <- .server_plot_get_plot(filtered_data, input))
    return(plot)
}

.server_plot_crop_plot_data <- function(filtered_data, input, zoom_range)
{
    date_range <- input$date_range
    date_range[[2]] <- date_range[[2]] + lubridate::days(1)
    if(!is.null(zoom_range())) {
        date_range <- zoom_range()
    }
    filtered_data <- myClim::mc_prep_crop(filtered_data, start=date_range[[1]], end=date_range[[2]])
    return(filtered_data)
}

.server_plot_get_plot_data <- function(data, data_loggers, input)
{
    data_tree <- input$data_tree
    input_data_loggers <- input$data_loggers
    multi_select <- .server_plot_selected_settings(input, .app_const_SETTINGS_MULTI_SELECT_KEY)
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
    return(filtered_data)
}

.server_plot_get_plot <- function(data, input)
{
    selected_facet_text <- input$facet_select
    facet <- if(selected_facet_text == "NULL") NULL else selected_facet_text
    color_by_logger <- .server_plot_selected_settings(input, .app_const_SETTINGS_COLOR_BY_LOGGER_KEY)
    plot <- myClim::mc_plot_line(data, facet=facet, color_by_logger=color_by_logger)
    return(plot)
}

.server_plot_reset_zoom_range_if_need <- function(filtered_data, last_filtered_data_table, zoom_range) {
    filtered_data_table <- .data_get_filtered_data_table(filtered_data)
    if(isTRUE(all.equal(filtered_data_table, last_filtered_data_table()))){
        return(FALSE)
    }
    last_filtered_data_table(filtered_data_table)
    if(!is.null(zoom_range())){
        zoom_range(NULL)
        return(TRUE)
    }
    return(FALSE)
}

.server_plot_selected_settings <- function(input, key) {
    result <- key %in% input$settings_checkboxes
    return(result)
}

.server_plot_changed_settings <- function(input, previous_selected_settings) {
    new_selected <- setdiff(input$settings_checkboxes,  previous_selected_settings)
    if(length(new_selected) == 1){
        return(list(key=new_selected, value=TRUE))
    }
    new_unselected <- setdiff(previous_selected_settings, input$settings_checkboxes)
    return(list(key=new_unselected, value=FALSE))
}

.server_plot_get_datetime_range <- function(last_datetime_range){
    if(is.null(last_datetime_range())){
        return("")
    }
    return(stringr::str_glue("{last_datetime_range()[[1]]} - {last_datetime_range()[[2]]}"))
}
