.ui_data_tab <- function(shared) {
    delete_fluid_row <- NULL
    if(shared$is_uncleaned_raw) {
        delete_fluid_row <- shiny::fluidRow(
            shiny::column(
                shiny::actionButton("delete_states_button", .texts_data_delete),
                width = 2,
            ),
            shiny::column(
                width = 5,
            ),
            shiny::column(
                shiny::actionButton("save_delete_table_button", .texts_data_save_button),
                width = 2,
                style="padding-right: 0px; text-align: right; ",
            ),
            shiny::column(
                shiny::textInput("file_delete_textinput", NULL, value="delete_table.rds", width="100%"),
                width = 3,
            ),
        )
    }

    shiny::tabPanel(.ui_const_DATA_TITLE,
                    icon = shiny::icon("table"),
       delete_fluid_row,
       shiny::fluidRow(
            shiny::column(
                DT::dataTableOutput("data_table"),
                width = 12
            )
        ),
    )
}