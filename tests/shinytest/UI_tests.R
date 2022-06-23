app <- ShinyDriver$new("../../", loadTimeout = 1.e5)
app$snapshotInit("UI_tests", screenshot = FALSE)

listInputs <- c("LA_choice", "navbar", "phase_choice", "tabs")

# 1. Does it load  -------------------------------------------------------------------------------------------------------------------
Sys.sleep(1)
app$snapshot()

# 2. Is England, Primary the default?--------------------------------------------
app$setInputs(navbar = "la_scorecards")
app$setInputs(LA_choice = "England")
app$snapshot(list(
  input = listInputs,
  output = c(
    "data_description", "total_funding_box", "pupil_growth", "estimated_additional_places",
    "estimated_spare_places", "places_chart", "label_estimate_y1", "forecast_1y",
    "label_estimate_y3", "forecast_3y", "forecast_3y"
  )
))

# 3. Do the values change when you select a different LA?-------------------------------
app$setInputs(LA_choice = "Hertfordshire")
app$snapshot(list(
  input = listInputs,
  output = c(
    "data_description", "total_funding_box", "pupil_growth", "estimated_additional_places",
    "estimated_spare_places", "places_chart", "label_estimate_y1", "forecast_1y",
    "label_estimate_y3", "forecast_3y", "forecast_3y"
  )
))

# 4. Do the values change when you select a different phase?-------------------------------
app$setInputs(phase_choice = "Secondary")
app$snapshot(list(
  input = listInputs,
  output = c(
    "data_description", "total_funding_box", "pupil_growth", "estimated_additional_places",
    "estimated_spare_places", "places_chart", "label_estimate_y1", "forecast_1y",
    "label_estimate_y3", "forecast_3y", "forecast_3y"
  )
))


# 5. Do the values stay the same when you select a different quality measure?-------------------------------
app$setInputs(tabs = "forecast")
app$snapshot(list(
  input = listInputs,
  output = c(
    "data_description", "total_funding_box", "pupil_growth", "estimated_additional_places",
    "estimated_spare_places", "places_chart", "label_estimate_y1", "forecast_1y",
    "label_estimate_y3", "forecast_3y", "forecast_3y"
  )
))

# 6. Does the preference tab load correctly? ------------------------------
app$setInputs(tabs = "preference")
app$snapshot(list(
  input = listInputs,
  output = c("prefT3_ENG", "PrefT3_LA", "preference_p")
))

# 7. Does the values in the preference tab change when you select England? ------------------------------
app$setInputs(LA_choice = "England")
app$snapshot(list(
  input = listInputs,
  output = c("prefT3_ENG", "PrefT3_LA", "preference_p")
))

# 8. Does the values in the preference tab change when you select a different Phase? ------------------------------
app$setInputs(phase_choice = "Primary")
app$snapshot(list(
  input = listInputs,
  output = c("prefT3_ENG", "PrefT3_LA", "preference_p")
))

# 9. Does the values in the preference tab stay the same when you select a different quality measure? ------------------------------
# app$setInputs(chart_choice = "Ofsted",wait_=FALSE, values_=FALSE)
app$snapshot(list(
  input = listInputs,
  output = c("prefT3_ENG", "PrefT3_LA", "preference_p")
))

# 10. Does the quality tab load correctly? ------------------------------
app$setInputs(tabs = "quality")
app$snapshot(list(
  input = listInputs,
  output = c("England_GO_places", "quality_chart", "no_rating_line")
))

# 11. Do the values in the quality tab change when you select Progress 8? ------------------------------
app$setInputs(phase_choice = "Secondary")
# app$setInputs(chart_choice = "Progress 8")
app$snapshot(list(
  input = listInputs,
  output = c("England_GO_places", "quality_chart", "no_rating_line")
))

# 12. Do the values in the quality tab change when you select Reading? Are these options available for Primary?------------------------------
app$setInputs(phase_choice = "Primary")
# app$setInputs(chart_choice = "Reading Progress")
app$snapshot(list(
  input = listInputs,
  output = c("England_GO_places", "quality_chart", "no_rating_line")
))

# 13. Do the values in the quality tab change when you select Maths?  Are these options available for Primary?------------------------------
# app$setInputs(chart_choice = "Maths Progress")
app$snapshot(list(
  input = listInputs,
  output = c("England_GO_places", "quality_chart", "no_rating_line")
))

# 14. Do the values in the quality tab change when you select an LA?------------------------------
app$setInputs(LA_choice = "Cheshire East")
app$snapshot(list(
  input = listInputs,
  output = c("LA_GO_places", "LA_GO_ran", "quality_chart", "no_rating_line")
))

# 15. Does the cost tab render correctly?-----------------
app$setInputs(tabs = "cost")
app$snapshot(list(
  input = listInputs,
  output = c("perm_box", "temp_box", "new_box", "cost_table", "cost_plot")
))

# 16. Do outputs change when you select "England"?-----------------
app$setInputs(LA_choice = "England")
app$snapshot(list(
  input = listInputs,
  output = c("perm_box", "temp_box", "new_box", "cost_table", "cost_plot")
))

# 17. Do outputs change when you select "Secondary"?-----------------
app$setInputs(phase_choice = "Secondary")
app$snapshot(list(
  input = listInputs,
  output = c("perm_box", "temp_box", "new_box", "cost_table", "cost_plot")
))

# 18. Do outputs stay the same when you select a different quality measure?-----------------
# app$setInputs(chart_choice = "Progress 8",wait_=FALSE, values_=FALSE)
app$snapshot(list(
  input = listInputs,
  output = c("perm_box", "temp_box", "new_box", "cost_table", "cost_plot")
))
