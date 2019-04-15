library(shiny)
library(tidyverse)
library(plotly)

load("pca_data.RData")

shinyUI(fixedPage(

  # App title ----
  titlePanel("Mycotype PCA plots"),
  h3("Sequencing Run 181107"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      h2("PCA Data"),
      p("The data selected here are used to calculate the principal component loadings and scores."),
      selectInput("inProjects",
                  label = "Include Project(s):",
                  choices = gt_seqtab %>% select(project) %>% distinct(),
                  selected = gt_seqtab$project[1],
                  multiple = TRUE),
     selectInput("inExclude",
                  label = "Exclude Sample(s):",
                  choices = NULL,
                  multiple = TRUE),
      h2("Plot Options"),
      p("These options modify what scores are plotted, and how they are labeled, but do not alter the underlying PCA."),
      radioButtons("inColorGroup",
                   label = "Color:",
                   choices = c("Outside / Inside", "Date", "Project", "Sub-Location", "Name", "Room"),
                   selected = "Outside / Inside"
                   ),
      radioButtons("inLabelGroup",
                   label = "Label:",
                   selected = "None",
                   choices = c("None",  "Outside / Inside", "Date", "Project", "Sub-Location", "Name", "Room")
                   ),
      # selectInput("inLineGroup",
      #              label = "Line:",
      #              choices = c("Location", "Sub-Location", "Name", "Outside / Inside"),
      #              multiple = FALSE),
      selectInput(inputId  = "inPlotLocations",
                  label    = "Projects to Plot:",
                  choices  = "All",
                  selected = NULL,
                  multiple = TRUE),
      selectInput("inPlotSubLocations",
      label = "Sub-Locations to Plot:",
      choices = "All",
      selected = NULL,
      multiple = TRUE),
      h4("Include all Outside"),
      radioButtons(inputId = "inRadioButtonsOutside",
      label = "Plot all outside points in the background",
      choices = c("None", "All", "34, 56 and 160", "All but 34, 56 and 160", "All but 34, 56, 160 and 180"),
      selected = NULL)),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotlyOutput(outputId = "loadingsPlot"),
      br(),
      hr(),
      plotlyOutput(outputId = "scoresPlot")
    )
  )
))
