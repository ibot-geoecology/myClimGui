.server_plot_get_main <- function(input, output, session, shared) {
    previous_sensors <- shiny::reactiveVal()
    zoom_range <- shiny::reactiveVal()
    previous_selected_settings <- shiny::reactiveVal("init")
    render_plot_number <- shiny::reactiveVal(0)
    last_datetime_range <- shiny::reactiveVal(NULL)
    last_filtered_data_table <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$settings_checkboxes, {
        .server_plot_process_settings_change(input, shared, previous_selected_settings, render_plot_number,
                                                 last_datetime_range, zoom_range, last_filtered_data_table)
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$reset_button, {
        date_range <- .data_get_date_range(shared$data, "day")
        shiny::updateDateRangeInput(session, "date_range",
                                    start=date_range[[1]], end=date_range[[2]])
        .server_plot_change_selected_data(input, shared, last_datetime_range,
                                          zoom_range, last_filtered_data_table, render_plot_number, FALSE)
    })

    shiny::observeEvent(input$date_range, {
        .server_plot_change_selected_data(input, shared, last_datetime_range,
                                          zoom_range, last_filtered_data_table, render_plot_number, TRUE)
    })

    shiny::observeEvent(input$sensor_select, {
        .server_plot_sensor_select_event(shared$data, input, session, previous_sensors)
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$plot_dblclick, {
        .server_plot_dblclick_event(input, zoom_range)
        .server_plot_change_selected_data(input, shared, last_datetime_range,
                                          zoom_range, last_filtered_data_table, render_plot_number, TRUE)
    })

    shiny::observeEvent(input$data_loggers, {
        .server_plot_change_selected_data(input, shared, last_datetime_range,
                                          zoom_range, last_filtered_data_table, render_plot_number, TRUE)
    })
    
    shiny::observeEvent(input$data_tree, {
        data_tree <- input$data_tree
        slices <- shinyTree::get_selected(data_tree, format="slices")
        if(length(slices) == 0) {
            shared$selection_table <- NULL
            return()
        }
        selection_table <- .tree_get_selection_table(shared$data, slices)
        if(!is.null(shared$selectopn_table) && isTRUE(all.equal(selection_table, shared$selection_table))) {
            return()
        }
        shared$selection_table <- selection_table
        .server_plot_change_selected_data(input, shared, last_datetime_range,
                                          zoom_range, last_filtered_data_table, render_plot_number, FALSE)
    })
    
    shiny::observe({
        d <- plotly::event_data("plotly_relayout")
        if(is.null(d) || !("xaxis.range[0]" %in% names(d))) {
            return()
        }
        shiny::isolate({
            zoom_range(myClim:::.common_as_utc_posixct(c(d[["xaxis.range[0]"]], d[["xaxis.range[1]"]])))
            .server_plot_change_selected_data(input, shared, last_datetime_range,
                                              zoom_range, last_filtered_data_table, render_plot_number, FALSE)
        })
    })

    output$data_tree <- shinyTree::renderTree({.tree_get_list(shared$data)})

    output$plot_plotly <- plotly::renderPlotly({
        render_plot_number()
        if(!shiny::isolate(.server_plot_is_plotly(input))) {
            return(plotly::ggplotly(ggplot2::ggplot()))
        }
        crop_data <- NULL
        if(!is.null(shared$selection_table)) {
            filter_data <- .data_filter_by_selection_table(shared$data, shared$selection_table)
            crop_data <- myClim::mc_prep_crop(filter_data, shared$crop_range[[1]], shared$crop_range[[2]])
        }
        plot <- .server_plot_render_plot_common(session, input, render_plot_number, crop_data)
        if(is.null(plot)) {
            p <- plotly::ggplotly(ggplot2::ggplot())
        } else {
            p <- plotly::ggplotly(plot)
        }
        p <- plotly::event_register(p, "plotly_relayout")
        return(p)
    })

    output$plot_ggplot <- shiny::renderPlot({
        if(shiny::isolate(.server_plot_is_plotly(input))) {
            return(NULL)
        }
        zoom_range_value <- zoom_range()
        crop_data <- NULL
        if(!is.null(shared$selection_table)) {
            filter_data <- .data_filter_by_selection_table(shared$data, shared$selection_table)
            crop_data <- myClim::mc_prep_crop(filter_data, shared$crop_range[[1]], shared$crop_range[[2]])
        }
        plot <- .server_plot_render_plot_common(session, input, render_plot_number, crop_data)
        if(is.null(plot)) {
            return(NULL)
        }
        plot
    }, res=96)

    output$datetime_range_text <- shiny::renderText({
        .server_plot_get_datetime_range(last_datetime_range)
    })

    output$selected_item_text <- shiny::renderText({
        .server_plot_get_selected_item_text(shared, last_filtered_data_table)
    })
}

.server_plot_process_settings_change <- function(input, shared, previous_selected_settings, render_plot_number,
                                                 last_datetime_range, zoom_range, last_filtered_data_table) {
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
    if(!is_init && changed_settings$key == .app_const_SETTINGS_MULTI_SELECT_KEY) {
        refresh_plot <- !.server_plot_selected_settings(input, .app_const_SETTINGS_MULTI_SELECT_KEY)
        .server_plot_change_selected_data(input, shared, last_datetime_range,
                                          zoom_range, last_filtered_data_table, render_plot_number, refresh_plot)
    }
    previous_selected_settings(input$settings_checkboxes)
}

.server_plot_plotly_checkbox_event <- function(input) {
    if(.server_plot_is_plotly(input)) {
        shinyjs::show(id="plot_plotly")
        shinyjs::hide(id="plot_ggplot")
    } else {
        shinyjs::show(id="plot_ggplot")
        shinyjs::hide(id="plot_plotly")
    }
}

.server_plot_multi_select_checkbox_event <- function(input, session) {
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
    if(is.null(sensor)) {
        return()
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

.server_plot_change_selected_data <- function(input, shared, last_datetime_range, zoom_range, last_filtered_data_table,
                                              render_plot_number, refresh_plot) {
    shiny::isolate(.server_plot_set_selected_data(input, shared))
    if(is.null(shared$selection_table)) {
        shared$crop_range <- NULL
        return()
    }
    if(.server_plot_reset_zoom_range_if_need(shared$selection_table, last_filtered_data_table, zoom_range)) {
        shared$crop_range <- NULL
        return()
    }
    
    shared$crop_range <- .server_plot_get_crop_range(input, shared, zoom_range)
    last_datetime_range(shared$crop_range)
    if(refresh_plot) {
        render_plot_number(render_plot_number()+1)
    }
}

.server_plot_render_plot_common <- function(session, input, render_plot_number, selected_data)
{
    input$refresh_button
    input$facet_select
    render_plot_number()
    if(is.null(selected_data)) {
        return(NULL)
    }
    shiny::isolate(plot <- .server_plot_get_plot(selected_data, input))
    return(plot)
}

.server_plot_get_crop_range <- function(input, shared, zoom_range)
{
    crop_range <- input$date_range
    crop_range[[2]] <- crop_range[[2]] + lubridate::days(1)
    if(crop_range[[2]] > shared$data_range[[2]]) {
        crop_range[[2]] <- shared$data_range[[2]]
    }
    if(!is.null(zoom_range())) {
        crop_range <- zoom_range()
    }
    if(!is.null(shared$last_crop_range_params$crop_range) &&
       !is.null(shared$last_crop_range_params$selection_table) &&
       isTRUE(all.equal(crop_range, shared$last_crop_range_params$crop_range)) &&
       isTRUE(all.equal(shared$selection_table, shared$last_crop_range_params$selection_table))) {
        return(shared$crop_range)
    }
    shared$last_crop_range_params$crop_range <- crop_range
    shared$last_crop_range_params$selection_table <- shared$selection_table
    filter_data <- .data_filter_by_selection_table(shared$data, shared$selection_table)
    crop_data <- myClim::mc_prep_crop(filter_data, crop_range[[1]], crop_range[[2]])
    return(.data_get_date_range(crop_data))
}

.server_plot_set_selected_data <- function(input, shared)
{
    data_tree <- input$data_tree
    input_data_loggers <- input$data_loggers
    multi_select <- .server_plot_selected_settings(input, .app_const_SETTINGS_MULTI_SELECT_KEY)
    if((multi_select && is.null(data_tree)) || (!multi_select && is.null(shared$data_loggers))) {
        return()
    }
    if(!multi_select) {
        selected_loggers <- shared$data_loggers[[input_data_loggers]]
        logger_type <- NULL
        if(length(selected_loggers) == 2) {
            logger_type <- selected_loggers[[2]]
        }
        shared$selection_table <- .data_get_selection_table(shared$data, selected_loggers[[1]], logger_type)
    }
}

.server_plot_get_plot <- function(data, input)
{
    selected_facet_text <- input$facet_select
    facet <- if(selected_facet_text == "NULL") NULL else selected_facet_text
    color_by_logger <- .server_plot_selected_settings(input, .app_const_SETTINGS_COLOR_BY_LOGGER_KEY)
    plot <- myClim::mc_plot_line(data, facet=facet, color_by_logger=color_by_logger)
    return(plot)
}

.server_plot_reset_zoom_range_if_need <- function(selection_table, last_filtered_data_table, zoom_range) {
    filtered_data_table <- .data_get_filtered_data_table(selection_table)
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

.server_plot_get_selected_item_text <- function(shared, last_filtered_data_table){
    filtered_data_table <- last_filtered_data_table()
    localities <- unique(filtered_data_table$locality_id)
    if(length(localities) > 1){
        return("")
    }
    locality_id <- localities[[1]]
    if(myClim:::.common_is_agg_format(shared$data)) {
        return(stringr::str_glue("{locality_id}"))
    }
    logger_names <- unique(filtered_data_table$logger_name)
    locality_item <- shared$data$localities[[locality_id]]
    serial_numbers <- unique(purrr::map_chr(logger_names, ~ locality_item$loggers[[.x]]$metadata@serial_number))
    serial_numbers <- serial_numbers[!is.na(serial_numbers)]
    if(length(serial_numbers) != 1){
        return(stringr::str_glue("{locality_id}"))
    }
    return(stringr::str_glue("{locality_id} - {serial_numbers}"))
}

.server_plot_is_plotly <- function(input) {
    return(.server_plot_selected_settings(input, .app_const_SETTINGS_PLOTLY_KEY))
}

