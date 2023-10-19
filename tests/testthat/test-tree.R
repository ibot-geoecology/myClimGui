test_that(".tree_get_list", {
    tree_list <- .tree_get_list(myClim::mc_data_example_raw)
    expect_equal(names(tree_list$A1E05), c("1_91184101", "2_92201058"))
    expect_equal(names(tree_list$A1E05$`1_91184101`), "Thermo_T")
    tree_list <- .tree_get_list(myClim::mc_data_example_agg)
    expect_equal(names(tree_list$A1E05), c("Thermo_T", "Dendro_T", "Dendro_raw"))
})
