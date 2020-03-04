
output[["currentSession"]] <- renderPrint({
  current <- sessionInfo()
  current
})
