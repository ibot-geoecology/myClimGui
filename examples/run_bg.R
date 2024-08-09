library(myClim)
library(callr)

p <- r_bg(function(data) {myClimGui::mcg_run(data, port=8080, launch.browser=FALSE)}, args = list(data=mc_data_example_clean))

# open in browser http://localhost:8080

# can be called after click on Return button
data <- p$get_result()
