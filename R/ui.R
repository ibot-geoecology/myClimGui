.ui_const_PLOT_TITLE <- "Plot"
.ui_const_STATES_TITLE <- "States"
.ui_const_DATA_TITLE <- "Data"

.ui_get_main <- function(shared) {
    shiny::fluidPage(theme = shinythemes::shinytheme("flatly"),
        style = "padding: 0px;",
        shiny::tags$script(shiny::HTML('
            $(document).on("keydown", function (e) {
                if (e.key == "ArrowUp" || e.key == "ArrowDown") {
                    var states_table = $("#states_table table.dataTable").DataTable();
                    var selected = states_table.row(".selected")[0];
                    if(selected.length == 0) {
                        return;
                    }
                    var selected_index = selected[0];
                    if(e.key == "ArrowUp") {
                        selected_index = selected_index - 1;
                    } else {
                        selected_index = selected_index + 1;
                    }
                    if(selected_index < 0) {
                        selected_index = 0;
                    }
                    nrows = states_table.rows().count();
                    if(selected_index >= nrows) {
                        selected_index = nrows - 1;
                    }
                    states_table.rows().deselect();
                    states_table.row(selected_index).select();
                }
            });')),    
        shiny::actionButton("return_button", "Return", icon = shiny::icon("door-open"),
                            style = "position: absolute; top: 8px; right: 8px; z-index:10000;"),
        shiny::tagAppendAttributes(
            shiny::textOutput("selected_item_text"), style="font-weight: bold; color: white; position: absolute; top: 10px; right: 120px; z-index:10000;"),
        shiny::tagAppendAttributes(
            shiny::textOutput("datetime_range_text"), style="font-weight: bold; color: white; position: absolute; top: 30px; right: 120px; z-index:10000;"),
        shinyjs::useShinyjs(),
        shiny::navbarPage("myClimGui",
                          .ui_plot_tab(shared),
                          .ui_states_tab(shared),
                          .ui_data_tab(sahred),
                          id="navbar_page"))
}

