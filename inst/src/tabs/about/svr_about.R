
## Get sessionInfo from current session
output[["currentSession"]] <- renderPrint({
  current <- sessionInfo()
  current
})
