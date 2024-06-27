library(shinytest2)

test_that("Migrated shinytest test: UI_tests.R", {
  app <- AppDriver$new(load_timeout = 1e+05)

  listInputs <- c("LA_choice", "navlistPanel", "phase_choice", "tabs")


  # 1. Does it load  -------------------------------------------------------------------------------------------------------------------
  message("Running test 1.")
  Sys.sleep(1)
  app$expect_values(input = listInputs)

  # 2. Is England, Primary the default?--------------------------------------------
  message("Running test 2.")
  app$set_inputs(navlistPanel = "la_scorecards")
  app$set_inputs(LA_choice = "England")
  app$expect_values(input = listInputs, output = c(
    "data_description",
    "total_funding_box", "pupil_growth", "estimated_additional_places",
    "estimated_spare_places", "places_chart", "label_estimate_y1",
    "forecast_1y", "label_estimate_y3", "forecast_3y", "forecast_3y"
  ))

  # 3. Do the values change when you select a different LA?-------------------------------
  message("Running test 3.")
  app$set_inputs(LA_choice = "Hertfordshire")
  app$expect_values(input = listInputs, output = c(
    "data_description",
    "total_funding_box", "pupil_growth", "estimated_additional_places",
    "estimated_spare_places", "places_chart", "label_estimate_y1",
    "forecast_1y", "label_estimate_y3", "forecast_3y", "forecast_3y"
  ))

  # 4. Do the values change when you select a different phase?-------------------------------
  app$set_inputs(phase_choice = "Secondary")
  app$expect_values(input = listInputs, output = c(
    "data_description",
    "total_funding_box", "pupil_growth", "estimated_additional_places",
    "estimated_spare_places", "places_chart", "label_estimate_y1",
    "forecast_1y", "label_estimate_y3", "forecast_3y", "forecast_3y"
  ))


  # 5. Do the values stay the same when you select a different quality measure?-------------------------------
  message("Running test 5.")
  app$set_inputs(tabs = "forecast")
  app$expect_values(input = listInputs, output = c(
    "data_description",
    "total_funding_box", "pupil_growth", "estimated_additional_places",
    "estimated_spare_places", "places_chart", "label_estimate_y1",
    "forecast_1y", "label_estimate_y3", "forecast_3y", "forecast_3y"
  ))

  # 6. Does the preference tab load correctly? ------------------------------
  message("Running test 6.")
  app$set_inputs(tabs = "preference")
  app$expect_values(input = listInputs, output = c(
    "prefT3_ENG",
    "PrefT3_LA", "preference_p"
  ))

  # 7. Does the values in the preference tab change when you select England? ------------------------------
  message("Running test 7.")
  app$set_inputs(LA_choice = "England")
  app$expect_values(input = listInputs, output = c(
    "prefT3_ENG",
    "PrefT3_LA", "preference_p"
  ))

  # 8. Does the values in the preference tab change when you select a different Phase? ------------------------------
  message("Running test 8.")
  app$set_inputs(phase_choice = "Primary")
  app$expect_values(input = listInputs, output = c(
    "prefT3_ENG",
    "PrefT3_LA", "preference_p"
  ))

  # 9. Does the values in the preference tab stay the same when you select a different quality measure? ------------------------------
  # app$setInputs(chart_choice = "Ofsted",wait_=FALSE, values_=FALSE)
  message("Running test 9.")
  app$expect_values(input = listInputs, output = c(
    "prefT3_ENG",
    "PrefT3_LA", "preference_p"
  ))

  # 10. Does the quality tab load correctly? ------------------------------
  message("Running test 10.")
  app$set_inputs(tabs = "quality")
  app$expect_values(input = listInputs, output = c(
    "England_GO_places",
    "quality_chart", "no_rating_line"
  ))

  # 11. Do the values in the quality tab change when you select Progress 8? ------------------------------
  message("Running test 11.")
  app$set_inputs(phase_choice = "Secondary")
  # app$setInputs(chart_choice = "Progress 8")
  app$expect_values(input = listInputs, output = c(
    "England_GO_places",
    "quality_chart", "no_rating_line"
  ))

  # 12. Do the values in the quality tab change when you select Reading? Are these options available for Primary?------------------------------
  message("Running test 12.")
  app$set_inputs(phase_choice = "Primary")
  # app$setInputs(chart_choice = "Reading Progress")
  app$expect_values(input = listInputs, output = c(
    "England_GO_places",
    "quality_chart", "no_rating_line"
  ))

  # 13. Do the values in the quality tab change when you select Maths?  Are these options available for Primary?------------------------------
  # app$setInputs(chart_choice = "Maths Progress")
  message("Running test 13.")
  app$expect_values(input = listInputs, output = c(
    "England_GO_places",
    "quality_chart", "no_rating_line"
  ))

  # 14. Do the values in the quality tab change when you select an LA?------------------------------
  message("Running test 14.")
  app$set_inputs(LA_choice = "Cheshire East")
  app$expect_values(input = listInputs, output = c(
    "LA_GO_places",
    "LA_GO_ran", "quality_chart", "no_rating_line"
  ))

  # 15. Does the cost tab render correctly?-----------------
  app$set_inputs(tabs = "cost")
  app$expect_values(input = listInputs, output = c(
    "perm_box",
    "temp_box", "new_box", "cost_table", "cost_plot"
  ))

  # 16. Do outputs change when you select "England"?-----------------
  app$set_inputs(LA_choice = "England")
  app$expect_values(input = listInputs, output = c(
    "perm_box",
    "temp_box", "new_box", "cost_table", "cost_plot"
  ))

  # 17. Do outputs change when you select "Secondary"?-----------------
  app$set_inputs(phase_choice = "Secondary")
  app$expect_values(input = listInputs, output = c(
    "perm_box",
    "temp_box", "new_box", "cost_table", "cost_plot"
  ))

  # 18. Do outputs stay the same when you select a different quality measure?-----------------
  # app$setInputs(chart_choice = "Progress 8",wait_=FALSE, values_=FALSE)
  app$expect_values(input = listInputs, output = c(
    "perm_box",
    "temp_box", "new_box", "cost_table", "cost_plot"
  ))
})
