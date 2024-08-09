if(getRversion() >= "2.15.1")  globalVariables(c(".data"))

#' @description
#' # Shiny app for myClim
#' 
#' This package provides a graphical interface for interactive browsing of *myClim* objects,
#' primarily designed to assist with visual inspection of time-series data,
#' joining multiple downloads, and managing states (tags).
#' 
#' Currently, there is a single function, `mcg_run`, which takes a single parameter
#' representing the myClim object you wish to inspect. This function runs the Shiny application
#' either in the RStudio viewer or a web browser.
#' 
#' The main interactive menu consists of:
#' * Plot
#' * States
#' * Data
#' 
#' # Plot
#' 
#' Default plotting is done using ggplot. Selecting localities, loggers, sensors, and 
#' time span in the *Plot* section affects what you see in the *States* and *Data* sections. 
#' States are shown only for selected sensors; data are shown only for selected 
#' sensors and zoom levels. The more you zoom, the less data you see in the table. The data
#' table zooms together with the plot region (not for Plotly).   
#' 
#' ## Localities, Loggers, Sensors
#' 
#' On the left side of the running Shiny myClimGui, you can see a tree structure 
#' of your localities, loggers, and sensors. Here, you can select which time-series you 
#' wish to browse in the viewer.
#' Only selected time-series are shown. After selecting the elements you wish to browse, 
#' you have to hit the "show" button.
#' 
#' ## Browsing Options
#' There are several options in **Plot** mode, including *Multi Select*, *Plotly*, 
#' and *Color by Logger* in the left panel. On the right side of the screen, 
#' you can also set the time span of the data you wish to browse, 
#' i.e., limit the plot region to certain dates.
#' 
#' * If none of the options is selected, all loggers and sensors in the 
#' selected localities are plotted.
#' * Multi-select: allows selecting any combination of locality, logger, and sensor.
#' * Plotly: switches from the ggplot graphical device to Plotly (allows better zooming, 
#' image export, querying microclimate data values by clicking on the plot; 
#' it is much slower than the default ggplot).
#' * Color by Logger: useful for repeated downloads at the same locality or 
#' when there are multiple loggers at a locality, to distinguish which sensor 
#' line in the plot belongs to a specific logger. Otherwise, lines are colored by sensor type.
#' 
#' ## Facet Options
#' You can use the dropdown menu at the top of the screen to drive the facet options. 
#' You can choose to facet by *physical*, *locality*, or *NULL*.
#' 
#' * Physical: facets by physical elements (each physical element has its own 
#' plotting window; identical physical elements from multiple loggers are plotted together).
#' * Locality: facets by locality, with each locality having its own plotting window. 
#' This allows plotting a maximum of 2 physical elements together. When plotting 
#' a single physical element, data are not scaled, but when plotting 2 physical 
#' elements, data are scaled to fit the plotting region.
#' * NULL: a single plotting window for all time-series, allowing plotting a 
#' maximum of 2 physical elements together. When plotting a single physical element, 
#' data are not scaled, but when plotting 2 physical elements, data are scaled to fit the plotting region.
#' 
#' ## User Interaction
#' When you draw a rectangle (select region) with the mouse in the plotting window and double-click in 
#' the selected region, the plot will zoom in. You can zoom multiple times. To return
#' to see the full data range, you can hit refresh on the right-hand side or
#' double-click outside the selected region. 
#' 
#' # States
#' In myClim, any part of a time-series can hold "states," e.g., data quality tags. 
#' You can add "states" to a specific region of your time-series, e.g., error, 
#' sensor out of soil, compromised shielding. You can manage tags directly in 
#' myClim from the console (see the `mc_states_` function family in myClim) or interactively
#' in myClimGui. After clicking on a specific state (row), a plot showing the data region will 
#' pop up under the states table. You can click multiple rows, and they will be plotted in different 
#' colors on the plot. You can edit the text of the tag in the table after double-clicking the cell, 
#' the same as the text of the value. You can also edit the range of the tag by 
#' clicking the "edit range" button and selecting a new start/end from the popup calendar.
#' You can also add a new tag by clicking the "New" button, where you specify the start, end, and tag.
#' 
#' Note that you will see only the states of the sensors you have selected in the *Plot* section.
#' 
#' # Data
#' This is the section where you can browse your time-series row by row, column by column in 
#' a table. For now, you are not allowed to modify data in the table. You can use the table view, e.g., to 
#' find the exact datetime when some issue occurred to place the quality tag precisely. 
#' Note that you will see only the data of the sensors you have selected in the *Plot* section 
#' and only in the range your plot is zoomed to. 
#' 
#' ## WARNINGS
#' * Once you call `mcg_run` and Shiny starts, the RStudio console is "frozen," 
#' handling commands from Shiny. You are not able to work in the console simultaneously 
#' with running Shiny.
#' * Be patient; it is Shiny, and it is relatively slow, performing all your 
#' commands one by one. This means if you start nervously clicking the buttons,
#' thinking it is not responding, you will eventually see Shiny 
#' performing all your actions one by one.
#' 
#' * When using Plotly mode, combining two physical elements (e.g., temperature and soil
#' moisture) in a single plotting window, be mindful of the scaling. Plotly will show 
#' you scaled values instead of your real data. The solution is to plot by physical 
#' element, i.e., plot a single physical element in a single plotting window, avoiding the combination
#' of two variables with different value ranges.

"_PACKAGE"
