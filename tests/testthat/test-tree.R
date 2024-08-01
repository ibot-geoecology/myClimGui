test_that(".tree_get_list", {
    tree_list <- .tree_get_list(myClim::mc_data_example_raw)
    expect_equal(names(tree_list$A1E05), c("[1] 91184101", "[2] 92201058"))
    expect_equal(names(tree_list$A1E05$`[1] 91184101`), "Thermo_T")
    tree_list <- .tree_get_list(myClim::mc_data_example_agg)
    expect_equal(names(tree_list$A1E05), c("Thermo_T", "Dendro_T", "Dendro_raw"))
})

test_that(".tree_change_sensor", {
    tree <- readRDS("../data/tree/tree.rds")
    expect_true(is.null(attr(tree$A2E32$TMS_T1, "stselected")))
    tree <- .tree_change_selection(tree, "TMS_T1", TRUE)
    expect_true(attr(tree$A2E32$TMS_T1, "stselected"))
    expect_true(attr(tree$A6W79$TMS_T1, "stselected"))
    tree <- .tree_change_selection(tree, "TMS_T1", FALSE)
    expect_false(attr(tree$A2E32$TMS_T1, "stselected"))
    expect_false(attr(tree$A6W79$TMS_T1, "stselected"))
})
