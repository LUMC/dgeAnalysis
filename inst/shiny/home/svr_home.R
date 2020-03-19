
output[["welcome"]] <- renderUI({
  tagList(
    div(
      br(),
      span(
        "With the development of next-generation sequencing techniques, RNA-Seq is becoming a
        more commonly applied method in various fields, e.g., cancer genomics, cell biology, etc.
        RNA-Seq gives great insights into a biological species at a specific moment. To find out
        which genes are playing an important function at a specific moment or under a specific
        condition, differential gene expression analysis is used. With the use of this type of analysis,
        research between different conditions can be conducted.",
        style = "text-align: justify; display: block; width: 90%"
      ),
      span(
        "Using the application for differential gene analysis can be very helpful in many projects. Next
        to checking data quality, it also provides a platform to make a quick analysis. This makes it
        an easy to use platform capable of performing a variety of analysis possibilities.",
        style = "text-align: justify; display: block; width: 90%"
      ),
      br(),
      style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%"
    )
  )
})

output[["get_started"]] <- renderUI({
  fluidRow(
    column(
      width = 6,
      h2("New analysis"),
      div(
        br(),
        span(
          "1. Go to the 'Data upload' page and select 'New analysis'.", tags$br(),
          "2. Upload files (both CSV and TSV is accepted):", tags$br(),
          HTML("&nbsp;&nbsp;"), "- Samplesheet (metadata), Contains information about each sample.", tags$br(),
          HTML("&nbsp;&nbsp;"), "- Count data, Contains the read alignment results (read counts per feature).", tags$br(),
          HTML("&nbsp;&nbsp;"), "- Annotation, Contains more information about genes (location, length, etc.).", tags$br(),
          "3. Go to the 'Run analysis' page.", tags$br(),
          "4. Adjust the settings for to the desired analysis.", tags$br(),
          "5. Run the analysis.", tags$br(),
          "6. View the results.",
          style = "text-align: justify; display: flex; justify-content: center; width: 90%"
        ),
        br(),
        style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%"
      )
    ),
    column(
      width = 6,
      h2("View analysis"),
      div(
        br(),
        span(
          "1. Go to the 'Data upload' page and select 'View analysis'.", tags$br(),
          "2. Upload files (both CSV and TSV is accepted):", tags$br(),
          HTML("&nbsp;&nbsp;"), "- Samplesheet (metadata), Contains information about each sample.", tags$br(),
          HTML("&nbsp;&nbsp;"), "- Count data, Contains the read alignment results (read counts per feature).", tags$br(),
          HTML("&nbsp;&nbsp;"), "- Normalized data, Contains the normalization results (read counts per feature).", tags$br(),
          HTML("&nbsp;&nbsp;"), "- DE table, Contains all analysis results.", tags$br(),
          "3. View the results.",
          style = "text-align: justify; display: flex; justify-content: center; width: 90%"
        ),
        br(), br(), br(),
        style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%"
      )
    )
  )
})
