library(shiny)
library(tidyverse)
library(plotly)

load("pca_data.RData")

filter_pca_data <- function(projects = NULL, exclude = NULL) {
  if(is.null(projects)) {return(pca_data)}
  if(is.null(exclude)) {exclude = "NOTANAME"}
  # Filter based on user input, project and exclusions
  pca_data <- gt_seqtab %>% filter(project %in% projects, subproject != "control") %>% filter(!(name %in% exclude))

  # Remove Taxa that do not vary across samples
  pca_data <- left_join(pca_data %>% group_by(seqid) %>% dplyr::summarise(v = var(nhits)) %>% filter(v > 0) %>% select(seqid), pca_data, by = "seqid")

  # Spread into PCA matrix

  return(pca_data)
}

run_pca <- function(pca_data) {
  pca_matrix <- pca_data %>% select(sample, seqid, nhits) %>% spread(seqid, nhits)
  pca_results <- summary(prcomp(pca_matrix %>% select(-sample), scale = TRUE, center = TRUE))
  samples_df <- left_join(pca_matrix %>% select(sample), metadata, by = "sample")
  scores_df <- as_tibble(pca_results$x)
  scores_df$sample <- pca_matrix$sample
  loadings_df <- as_tibble(pca_results$rotation)
  loadings_df$seqid <- as.integer(colnames(pca_matrix[-1]))
  loadings_df <- left_join(loadings_df, df_taxa %>% select(Kingdom, Phylum, Class, Order, Family, Genus, Species, seqid), by = "seqid")
  importance_df <- as_tibble(pca_results$importance)
  return(list(pca = pca_results, scores_df = scores_df, loadings_df = loadings_df, importance_df = importance_df, samples_df = samples_df))
}


plot_loadings <- function(loadings_df, PCs = c("PC1", "PC2"), cutoff = 0.1, locations) {
  plot_title = paste("PC Loadings:", paste(locations, collapse = " "))
  PCs_df <- loadings_df[ , c(PCs, "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")]
  colnames(PCs_df) <- c("x", "y", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  p_df <- PCs_df %>% filter(abs(.[[1]]) > 0.1 | abs(.[[2]]) > 0.1)
  p <- plot_ly(data = PCs_df,
               type="scatter",
               mode = "markers",
               marker = list(size = 10),
               x = ~x,
               y = ~y,
               color = ~Phylum,
               text = ~paste("Kingdom:", Kingdom, "</br>Phylum:", Phylum, "</br>Class:", Class, "</br>Order:",
                              Order, "</br>Family:", Family, "</br>Genus:", Genus, "</br>Species:", Species)) %>%
       layout(title = plot_title, xaxis = list(title = PCs[1]), yaxis = list(title = PCs[2]))
  p
}

filter_plot_data <- function(pca_data, projects = NULL, subprojects = NULL, only_outside = FALSE) {

  if(!is.null(projects)) {
    f_samples_df <- pca_data$samples_df %>% filter(project %in% projects)
  } else {
    f_samples_df <- pca_data$samples_df
  }

  if(!is.null(subprojects)) {
    f_samples_df <- f_samples_df %>% filter(subproject %in% subprojects)
  }

  if(only_outside) {
    f_samples_df <- f_samples_df %>% filter(outside == "Outside")
  }


  f_scores_df <- left_join(f_samples_df %>% select(sample), pca_data$scores_df, by = "sample")

  # print(paste("Location:", projects))
  # print(f_samples_df$project)
  # print(paste("Sub Location:", subprojects))
  # print(f_samples_df$subproject)

  pca_data$samples_df <- f_samples_df
  pca_data$scores_df <- f_scores_df

  return(pca_data)
}


plot_scores <- function(scores_df, samples_df, color_group, label_group, background_data_list = NULL, plot_background_data = "None") {

#  pca_data <- filter_pca_data(c("High School", "Montecito"))
#  pca_results <- run_pca(pca_data)
#  scores_df             = filter_plot_data(pca_data = pca_results, projects = "High School", subprojects = NULL, only_outside = FALSE)$scores_df
#  background_data_list  = filter_plot_data(pca_data = pca_results, projects = NULL, subprojects = NULL, only_outside = TRUE)
#  samples_df            = filter_plot_data(pca_data = pca_results, projects = "High School", subprojects = NULL, only_outside = FALSE)$samples_df
#  color_group           = "Sub-Location"
#  label_group           = "Name"
#  plot_title            = "PCA Scores"

  marker_text <- samples_df %>% mutate(text = paste("Name:", name, "</br>Date:", date, "</br>In/Out:", outside, "</br>Project:", project, "</br>Sub-Loc:", subproject))

  c_group <- recode(color_group, "Date" = "date", "Project" = "project", "Sub-Location" = "subproject", "Name" = "name", "Outside / Inside" = "outside", "Room" = "room")
  t_group <- recode(label_group, "Date" = "date", "Project" = "project", "Sub-Location" = "subproject", "Name" = "name", "Outside / Inside" = "outside", "Room" = "room")

  projects <- samples_df %>% select(project) %>% distinct() %>% pull(project)
  plot_title <- paste("PCA Scores:", paste(c(projects), collapse = " "))

  if(!is.null(background_data_list) & plot_background_data != "None") {
    background_scores_df = background_data_list$scores_df
    background_samples_df = background_data_list$samples_df
    background_marker_text <- background_samples_df %>% mutate(text = paste("Name:", name, "</br>Date:", date, "</br>In/Out:", outside, "</br>Project:", project, "</br>Sub-Loc:", subproject))
    p <- plot_ly(name = "All Outside",
                 type="scatter",
                 mode = "markers",
                 marker = list(size = 10, color = 'rgba(165, 165, 165, .5)'),
                 data = background_scores_df,
                 x = ~PC1,
                 y = ~PC2,
                 text = background_marker_text$text
                 ) %>%
         add_trace(name = samples_df[ , c_group] %>% pull(),
                   type="scatter",
                   mode = "markers",
                   marker = list(size = 10, color = NULL),
                   data = scores_df,
                   x = ~PC1,
                   y = ~PC2,
                   text = marker_text$text,
                   color = samples_df[ , c_group] %>% pull(),
                   opacity = 1
                   ) %>% 
         layout(title = plot_title)
  } else {
    p <- plot_ly(type="scatter",
                 mode = "markers",
                 marker = list(size = 10),
                 data = scores_df,
                 x = ~PC1,
                 y = ~PC2,
                 text = marker_text$text,
                 color = samples_df[ , c_group] %>% pull()
                 ) %>%
          layout(title = plot_title)
  }


  if(t_group != "None") {
    p <- p %>% add_text(text = samples_df[ , t_group] %>% pull(), textposition = "top")
  }

  p
}


shinyServer( function(input, output, session){
  #browser()
  ### PCA
  # Filter locations.
  pca_locations <- reactive({ filter_pca_data(input$inProjects) })

  # Update exclusion select with available names from filtered locations.
  observe({
    updateSelectInput(session, "inExclude", 
      choices = pca_locations() %>% select(name) %>% distinct()
    )
  })

  # Filter data for location and exclusions
  current_data <- reactive({ filter_pca_data(input$inProjects, input$inExclude) })

  # Run PCA with current data
  current_results <- reactive({ run_pca(current_data()) })

  ### PLOTTING

  # Update plot location selector from filtered locations.
  observe({
    updateSelectInput(session, "inPlotLocations", 
      choices = c(pca_locations() %>% select(project) %>% distinct() %>% pull())
    )
  })

  plot_locations <- reactive({ filter_plot_data(pca_data = current_results(), projects = input$inPlotLocations, subprojects = NULL, only_outside = FALSE)})

  # Update sublocation selector from plot location selector
  observe({
    updateSelectInput(session, "inPlotSubLocations", 
      choices = c(plot_locations()$samples_df %>% select(subproject) %>% distinct() %>% pull())
    )
  })

  # Filter plot data for location and sublocation selections.
  current_plot_data    <- reactive({ filter_plot_data(pca_data = current_results(), projects = input$inPlotLocations, subprojects = input$inPlotSubLocations, only_outside = FALSE)})
  
  background_plot_data <- reactive({
    if(input$inRadioButtonsOutside == "None"){
      out = filter_plot_data(pca_data = current_results(), projects = NULL, subprojects = NULL, only_outside = TRUE)
    } else if(input$inRadioButtonsOutside == "All") {
      out = filter_plot_data(pca_data = current_results(), projects = NULL, subprojects = NULL, only_outside = TRUE)
    } else if(input$inRadioButtonsOutside == "34, 56 and 160") {
      out = filter_plot_data(pca_data = current_results(), projects = "Montecito", subprojects = c("34","56","160"), only_outside = TRUE)
      print(out$samples_df %>% select(subproject) %>% distinct() %>% pull())
    } else if(input$inRadioButtonsOutside == "All but 34, 56 and 160") {
      out = filter_plot_data(
        pca_data = current_results(),
        projects = "Montecito",
        subprojects = current_results()$samples_df %>% filter(!subproject %in% c(34, 56, 160)) %>% select(subproject) %>% distinct() %>% pull(),
        only_outside = TRUE)
    } else if(input$inRadioButtonsOutside == "All but 34, 56, 160 and 180") {
      out = filter_plot_data(
        pca_data = current_results(),
        projects = "Montecito",
        subprojects = current_results()$samples_df %>% filter(!subproject %in% c(34, 56, 160, 180)) %>% select(subproject) %>% distinct() %>% pull(),
        only_outside = TRUE)
  }
  out
  })

  # Plot PCA scores
  output$scoresPlot <- renderPlotly({
    plot_scores(
      scores_df            = current_plot_data()$scores_df,
      samples_df           = current_plot_data()$samples_df,
      color_group          = input$inColorGroup,
      label_group          = input$inLabelGroup,
      background_data_list = background_plot_data(),
      plot_background_data = input$inRadioButtonsOutside)
  })

  # Plot PCA Loadings
  output$loadingsPlot <- renderPlotly({
    locations <- current_results()$samples_df %>% select(project) %>% distinct() %>% pull(project)
    plot_loadings(
      loadings_df = current_results()$loadings_df,
      PCs = c("PC1", "PC2"),
      cutoff = 0.1,
      locations = locations)
  })
})
