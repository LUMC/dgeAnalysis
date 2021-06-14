
## Welcome text
output[["welcome"]] <- renderUI({
  tagList(
    div(
      br(),
      span(
        "With the development of next-generation sequencing techniques, RNA-Seq is becoming a
        more widely applied method in various fields, e.g. cancer genomics, cell biology, etc.
        RNA-Seq gives great insights into a biological species at a specific time. Find out
        which genes play an important function at a certain time or under a certain
        condition, differential gene expression analysis is used. Using this type of analysis,
        research between different conditions can be performed.",
        style = "text-align: justify; display: block; width: 90%"
      ),
      span(
        "Using the dgeAnalysis application for differential gene analysis can be very
        helpful in many projects. In addition to checking data quality, it also provides a platform
        to make a quick analysis. This makes it a user-friendly platform capable of performing
        various analysis options.",
        style = "text-align: justify; display: block; width: 90%"
      ),
      br(),
      style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%"
    )
  )
})

## Basic explanation of New and View analysis
output[["get_started"]] <- renderUI({
  fluidRow(column(
    width = 6,
    h2("New analysis"),
    div(
      br(),
      span(
        "1. Go to the 'Data upload' page and select 'New analysis'.",
        tags$br(),
        "2. Upload files (both CSV and TSV are accepted):",
        tags$br(),
        HTML("&nbsp;&nbsp;"),
        "- Samplesheet (metadata), Contains information about each sample.",
        tags$br(),
        HTML("&nbsp;&nbsp;"),
        "- Count data, Contains the read alignment results (read counts per feature).",
        tags$br(),
        HTML("&nbsp;&nbsp;"),
        "- Annotation, Contains more information about genes (location, length, etc.).",
        tags$br(),
        "3. Go to the 'Run analysis' page.",
        tags$br(),
        "4. Adjust the settings for the desired analysis.",
        tags$br(),
        "5. Run the analysis.",
        tags$br(),
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
        "1. Go to the 'Data upload' page and select 'View analysis'.",
        tags$br(),
        "2. Upload files (both CSV and TSV are accepted):",
        tags$br(),
        HTML("&nbsp;&nbsp;"),
        "- Samplesheet (metadata), Contains information about each sample.",
        tags$br(),
        HTML("&nbsp;&nbsp;"),
        "- Count data, Contains the read alignment results (read counts per feature).",
        tags$br(),
        HTML("&nbsp;&nbsp;"),
        "- Normalized data, Contains the normalization results (read counts per feature).",
        tags$br(),
        HTML("&nbsp;&nbsp;"),
        "- DE table, Contains all analysis results.",
        tags$br(),
        "3. View the results.",
        style = "text-align: justify; display: flex; justify-content: center; width: 90%"
      ),
      br(),
      br(),
      br(),
      style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%"
    )
  ))
})
