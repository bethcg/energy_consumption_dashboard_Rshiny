# RShiny version of the GEO4CIVHIC energy-demand dashboard.
#
# Same use case and expected results as the Streamlit example, served through
# Renku Apps ("bring your own web app"): the Procfile runs
#   shiny::runApp('src', host = '0.0.0.0',
#                 port = as.integer(Sys.getenv('RENKU_SESSION_PORT')))
# so this file only needs to define `ui`, `server`, and return a shinyApp().
#
# Data: Zenodo record https://zenodo.org/records/10568762 (mounted via a Renku
# data connector). We don't hard-code one mount folder name — we look for the
# Excel file under the connector mount root, with env-var overrides.

library(shiny)
library(readxl)
library(dplyr)
library(lubridate)
library(plotly)

DATA_MOUNT_ROOT <- Sys.getenv("DATA_MOUNT_ROOT", unset = "/home/renku/work")
EXPECTED_FILENAME <- "Energy demand_GEO4CIVHIC demo sites.xlsx"

find_data_file <- function() {
  # (1) explicit override
  env_path <- Sys.getenv("DATA_PATH", unset = "")
  if (nzchar(env_path) && file.exists(env_path)) return(env_path)

  # (2) exact filename anywhere under the mount root
  hits <- list.files(DATA_MOUNT_ROOT, pattern = EXPECTED_FILENAME,
                     recursive = TRUE, full.names = TRUE, all.files = TRUE)
  if (length(hits) > 0) return(hits[[1]])

  # (3) fallback: any GEO4CIVHIC / energy .xlsx under the mount root
  loose <- list.files(DATA_MOUNT_ROOT, pattern = "GEO4CIVHIC.*\\.xlsx$",
                      recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(loose) == 0)
    loose <- list.files(DATA_MOUNT_ROOT, pattern = "nergy.*\\.xlsx$",
                        recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(loose) > 0) return(loose[[1]]) else return(NA_character_)
}

load_data <- function(path) {
  df <- readxl::read_excel(path)
  names(df) <- c("Hour", "Load_kW")
  # 8760 hourly rows over a non-leap year starting Jan 1st (matches Streamlit).
  df$Timestamp <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") +
    (df$Hour - 1) * 3600
  df
}

DATA_PATH <- find_data_file()
energy <- if (!is.na(DATA_PATH)) load_data(DATA_PATH) else NULL

# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("body{max-width:1100px;margin:0 auto;}"))),
  titlePanel("⚡ GEO4CIVHIC demo sites: Energy demand"),
  tags$p("This dashboard visualizes annual load profiles from the ",
         "Renku-connected dataset stemming from Zenodo."),

  if (is.null(energy)) {
    div(
      style = "padding:1rem;border:1px solid #d33;border-radius:8px;",
      strong("Data not found!"),
      p(sprintf("Looked for '%s' under '%s'.", EXPECTED_FILENAME, DATA_MOUNT_ROOT)),
      p("Check that the Zenodo data connector is mounted, then set ",
        code("DATA_MOUNT_ROOT"), " or ", code("DATA_PATH"), " accordingly.")
    )
  } else {
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Filter options"),
        selectInput("month", "Select month",
                    choices = month.name[month.name %in%
                      unique(month(energy$Timestamp, label = TRUE, abbr = FALSE,
                                   locale = "C") |> as.character())],
                    selected = "January")
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(4, wellPanel(strong("Peak load"), textOutput("peak"))),
          column(4, wellPanel(strong("Avg load"), textOutput("avg"))),
          column(4, wellPanel(strong("Total consumption"), textOutput("total")))
        ),
        plotlyOutput("profile", height = "460px"),
        tags$p(style = "opacity:.6;margin-top:.5rem",
               textOutput("source", inline = TRUE))
      )
    )
  }
)

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  if (is.null(energy)) return(invisible(NULL))

  month_name_col <- reactive({
    as.character(month(energy$Timestamp, label = TRUE, abbr = FALSE, locale = "C"))
  })

  filtered <- reactive({
    energy[month_name_col() == input$month, , drop = FALSE]
  })

  # Metrics are computed over the full year, matching the Streamlit app.
  output$peak  <- renderText(sprintf("%s kW", max(energy$Load_kW)))
  output$avg   <- renderText(sprintf("%s kW", round(mean(energy$Load_kW), 2)))
  output$total <- renderText(sprintf("%s kWh", as.integer(sum(energy$Load_kW))))
  output$source <- renderText(sprintf("Data source: %s", DATA_PATH))

  output$profile <- renderPlotly({
    d <- filtered()
    plot_ly(d, x = ~Timestamp, y = ~Load_kW, type = "scatter", mode = "lines",
            line = list(color = "#2ca02c")) |>
      layout(
        title = sprintf("Demand profile for %s", input$month),
        xaxis = list(title = "Time of day"),
        yaxis = list(title = "Load [kW]"),
        paper_bgcolor = "white", plot_bgcolor = "white"
      )
  })
}

shinyApp(ui, server)
