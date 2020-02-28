
output[["newAnalysis"]] <- renderUI({
  sliderInput(
    "integer",
    "Integer:",
    min = 0, 
    max = 10000,
    value = 500
  )
})
