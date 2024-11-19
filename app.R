# Shiny UI for the "plot_scciimap_srv.R" map plotting script for the Swiss Canopy Crane site
# Code by D.Basler 2024

packages <- c("shiny","colourpicker","dplyr","plotrix","shape","png","readxl")
install.packages(setdiff(packages, rownames(installed.packages())))

library(shiny)
library(colourpicker)

# Check path settings in "plot_scciimap_srv.R"
source("plot_scciimap_srv.R")

ui <- fluidPage(
  titlePanel("SCC-II Map Generation Tool"),
  sidebarLayout(
    sidebarPanel(
      h3("Preconfigured Maps"),
      selectInput("predefined_maps", "Select a Preconfigured Map", 
                  choices = NULL, # c("Overview","Target Trees","Campaign: Broadleaf", "Campaign: Conifer","Campaign: Flash","Custom"),
                  selected = "Overview"),
      h4("Output Options"),
      textInput("title", "Title", value = "Overview"),
      checkboxInput("presentation", "Presentation Mode", value = FALSE),
      selectInput("output_format", "Output Format", choices = c("JPEG", "PDF"), selected = "PDF"),
      downloadButton("generate_and_download_map", "Generate and Download Map"),
      br(),
      h3("Basic Map Options"),
      checkboxInput("crane", "Crane", value = TRUE),
      checkboxInput("grid", "Grid", value = TRUE),
      checkboxInput("road", "Road", value = TRUE),
      checkboxInput("footpaths", "Footpaths", value = TRUE),
      checkboxInput("gates", "Gates", value = TRUE),
      checkboxInput("fence", "Fence", value = TRUE),
      checkboxInput("containers", "Containers", value = TRUE),
      checkboxInput("roofs", "Roofs", value = TRUE),
      checkboxInput("roof_labels", "Roof Labels", value = TRUE),
      br(),
      h4("Measurements"),
      checkboxInput("soilpits", "Soil Pits", value = TRUE),
      checkboxInput("suctionplates", "Suction Plates", value = FALSE),
      checkboxInput("littertraps", "Litter Traps", value = TRUE),
      checkboxInput("raintraps", "Rain Traps", value = TRUE),
      checkboxInput("sapflow_log", "Sapflow Logger", value = FALSE),
      checkboxInput("sapflow_cable", "Sapflow Cables", value = FALSE),
      br(),
      h3("Tree Layer(s)"),
      selectInput("year", "Year", choices = 2018:2038, selected = as.numeric(Sys.Date(),1,4)),
      actionButton("add_tree", "Add Tree Configuration"),
      uiOutput("trees_ui"),
      br(),
      h4("Campaign"),
      selectInput("craneroute", "Crane Route", choices = c("None", "broadleaf", "conifer", "flash"), selected = NA),
      checkboxInput("craneroutearrows", "Crane Route Arrows", value = TRUE),
      br(),
      br(),
      h4("Configuration Management"),
      downloadButton("save_config", "Save Configuration"),
      fileInput("load_config", "Load Configuration"),
      br(),
      # Footer Section Below the Plot
      tags$div(
        class = "footer",
        style = "position: relative; margin-top: 20px; text-align: center; font-size: 0.9em; color: gray;",
        HTML("<hr>"),
        HTML("<p>SCCII is a research site of the PPE group at the University of Basel <br>"),
        tags$a(href = "https://ppe.duw.unibas.ch/en/sccii/", target = "_blank", "About the research site"),
        HTML("</p>"),
        HTML("<p>copyright &copy; 2024 David Basler / DaBaScientific</p>"),
        tags$a(href = "https://dabascientific.com/hoelstein/phenocam/", target = "_blank", "Visit the SCC-II Dashboard")
      ) 
    ),
    mainPanel(
      h3("Map Preview"),
      plotOutput("mapPlot")
      
 
    )
  )
)


server <- function(input, output, session) {
  
  tree_counter <- reactiveVal(1)
  trees <- reactiveValues(list = list())
  
  # Flag to control debouncing
  debouncingEnabled <- reactiveVal(FALSE)
  
  # Regular (non-debounced) reactive for global map options
  globalMapOptions <- reactive({
    list(
      title = input$title,
      presentation = input$presentation,
      year = input$year,
      crane = input$crane,
      grid = input$grid,
      footpaths = input$footpaths,
      road = input$road,
      gates = input$gates,
      fence = input$fence,
      containers = input$containers,
      soilpits = input$soilpits,
      roofs = input$roofs,
      roof_labels = input$roof_labels,
      raintraps = input$raintraps,
      sapflow_log = input$sapflow_log,
      sapflow_cable = input$sapflow_cable,
      suctionplates = input$suctionplates,
      littertraps = input$littertraps,
      craneroute = input$craneroute,
      craneroutearrows = input$craneroutearrows,
      trees = trees$list
    )
  })
  
  # Debounced version of global map options
  debouncedGlobalMapOptions <- globalMapOptions %>% debounce(3000)
  
  # Reactive to determine which options to use (debounced or not)
  activeGlobalMapOptions <- reactive({
    if (debouncingEnabled()) {
      debouncedGlobalMapOptions()
    } else {
      globalMapOptions()
    }
  })
  
  
  
  
  template_path<-"./templates"
  rds_files <- list.files(template_path, pattern = "\\.rds$", full.names = FALSE)
  predefined_maps <- sub(".rds","",rds_files)
  # Reactive value to store the loaded configuration
  tree_config <- reactiveVal(NULL)
  updateSelectInput(session, "predefined_maps",
                    choices = predefined_maps,
                    selected = "Overview")
  
  # Reactive flag to control the observer
  disable_global_observer <- reactiveVal(FALSE)
  
  disable_global_observer(TRUE)  # Disable the observer during reset
  
  
  addTree <- function(tree_id, tree_config = NULL, session, trees) {
    message(sprintf("  adding tree layer %s",tree_id))
    # Default tree configuration if none is provided
    if (is.null(tree_config)) {
      tree_config <- list(
        speciescolor = TRUE,
        customcolor = "#7C7D7F",
        size_dbh = TRUE,
        customsize = 1,
        flt = "all",
        alpha = 255,
        highlight_shape = NA,
        highlight_color = "#FF0000",
        measurements = list(
          pointdm = TRUE,
          banddm = TRUE,
          soilmt = TRUE,
          #leafwp = FALSE,
          #nsc = FALSE,
          #phenocam = FALSE,
          sapflow = TRUE
          
        )
      )
    }
    # Dynamically add UI for the tree configuration
    insertUI(
      selector = "#trees_ui",
      ui = tags$div(
        id = tree_id,
        wellPanel(
          h4(paste("Tree Layer", gsub("tree_", "", tree_id))),
          checkboxInput(paste0(tree_id, "_size"), "use DBH", value = tree_config$size_dbh),
          uiOutput(paste0(tree_id, "_csize")),
          checkboxInput(paste0(tree_id, "_numbers"), "Numbers", value =  tree_config$number),
          checkboxInput(paste0(tree_id, "_speciescolor"), "Species Color", value = tree_config$speciescolor),
          colourInput(paste0(tree_id, "_customcolor"), "Custom Color", value =  tree_config$customcolor),
          textInput(paste0(tree_id, "_flt"), "Filter", value = tree_config$flt),
          tags$div(
            style = "display: flex; align-items: center;",  # Flexbox for alignment
            h5("Filter Builder", style = "margin-right: 10px;"),  # Add margin for spacing
            actionButton("filter_info", "", icon = icon("info-circle"))
          ),
          #h5("Dynamic Filter Builder"),actionButton("filter_info", "", icon = icon("info-circle")),
          wellPanel(
            selectInput(paste0(tree_id, "_filter_col"), "Column", choices = setdiff(unique(sub("_\\d{4}$", "", colnames(metadata))),c("xcor","ycor","nr_pos","dm_pos","newx","newy","angle","dist","corrangle","masl","death_date","death_comment","defx","defy","comment_2022_03")), selected = NULL),
            #selectInput(paste0(tree_id, "_filter_col"), "Column", choices = colnames(metadata), selected = NULL),
            selectInput(paste0(tree_id, "_filter_op"), "Operator", choices = c("==", "!=", ">", "<", ">=", "<="), selected = "=="),
            uiOutput(paste0(tree_id, "_filter_val_ui")),
            actionButton(paste0(tree_id, "_update_filter"), "Update Filter"),
            actionButton("add_and", "AND"),
            actionButton("add_or", "OR"),
            actionButton(paste0(tree_id, "_remove_filter"), "Remove Filter"),
            br(),
          ),
          sliderInput(paste0(tree_id, "_alpha"), "Transparency", min = 0, max = 100, value = (tree_config$alpha/255-1)*100 ),
          selectInput(paste0(tree_id, "_highlight_shape"), "Highlight Shape", choices = c("none", "circle", "cross" ,"rect","underline"), selected = ifelse(is.na(tree_config$highlight_shape),"none",tree_config$highlight_shape)),
          colourInput(paste0(tree_id, "_highlight_color"), "Highlight Color", value = tree_config$highlight_color),
          h5("Measurements"),
          checkboxInput(paste0(tree_id, "_pointdm"), "Point DM", value = tree_config$measurements$pointdm),
          checkboxInput(paste0(tree_id, "_banddm"), "Band DM", value = tree_config$measurements$banddm),
          checkboxInput(paste0(tree_id, "_soilmt"), "Soil MT", value = tree_config$measurements$soilmt),
          #checkboxInput(paste0(tree_id, "_leafwp"), "Leaf WP", value = FALSE),
          #checkboxInput(paste0(tree_id, "_nsc"), "NSC", value = FALSE),
          #checkboxInput(paste0(tree_id, "_phenocam"), "Phenocam", value = FALSE),
          checkboxInput(paste0(tree_id, "_sapflow"), "Sapflow", value = tree_config$measurements$sapflow),
          actionButton(paste0(tree_id, "_remove"), "Remove Tree Configuration"),
          br(), br()
        )
      )
    )
    
    
    
    observeEvent(input$filter_info, {
      showModal(modalDialog(
        title = "Filter Values Explanation",
        p("Filter values allow you to refine data displayed on the map based on specific criteria."),
        p("The filter is applied for the selected year"),
        tags$ul(
          tags$li("Numeric columns: Use operators like '==', '<', '>' to filter data numerically."),
          tags$li("Categorical columns: Select specific categories from the dropdown."),
          tags$li("Combine filters: Use 'AND' and 'OR' to build complex filter conditions.")
        ),
        p("Example Filters:"),
        tags$pre("ColumnName == 'Category1' AND AnotherColumn > 10"),
        footer = modalButton("Close")  # Add a close button
      ))
    })
    
    # Add the tree configuration to the reactive list
    trees$list[[tree_id]] <- tree_config
    
    # Attach remove logic for the tree UI
    observeEvent(session$input[[paste0(tree_id, "_remove")]], {
      removeUI(selector = paste0("#", tree_id))
      trees$list[[tree_id]] <- NULL
    })
    
    
    
    # Update trees$list with the current configuration of this tree
    observeEvent({ lapply(names(input), function(name) if (startsWith(name, paste0(tree_id, "_"))) input[[name]]) },
                 {
                   # Some options have changed in the tree settings ... read UI and update data
                   if (disable_global_observer() || length(trees$list) == 0) return()
                   disable_global_observer(TRUE)
                   message(sprintf("TREE LIST TRIGGERED %s", tree_id))
                   #isolate({
                     trees$list[[tree_id]] <- list(
                       flt = input[[paste0(tree_id, "_flt")]],
                       speciescolor = input[[paste0(tree_id, "_speciescolor")]],
                       customcolor = input[[paste0(tree_id, "_customcolor")]],
                       size_dbh = input[[paste0(tree_id, "_size")]],
                       customsize = ifelse(is.null(input[[paste0(tree_id, "_csize")]]), 1,input[[paste0(tree_id, "_csize")]]),
                       numbers = input[[paste0(tree_id, "_numbers")]],
                       alpha = 255 - as.numeric((input[[paste0(tree_id, "_alpha")]]) / 100 * 255),
                       highlight_shape = ifelse(input[[paste0(tree_id, "_highlight_shape")]] == "none", NA, input[[paste0(tree_id, "_highlight_shape")]]),
                       highlight_color = input[[paste0(tree_id, "_highlight_color")]],
                       measurements = list(
                         pointdm = input[[paste0(tree_id, "_pointdm")]],
                         banddm = input[[paste0(tree_id, "_banddm")]],
                         soilmt = input[[paste0(tree_id, "_soilmt")]],
                         sapflow = input[[paste0(tree_id, "_sapflow")]]
                       )
                     )
                     message("TREE LIST UPDATED")
                     disable_global_observer(FALSE)
                   #})
                 })

    
    # # Dynamic Color Picker UI
    observe({
      output[[paste0(tree_id, "_csize")]] <- renderUI({
        if (!is.null(session$input[[paste0(tree_id, "_size")]]) && !session$input[[paste0(tree_id, "_size")]]) {
          sliderInput(paste0(tree_id, "_csize"), "custom size", min = 0.01, max = 2, value = 1)
        }
      })
    })
    
    # ********************    FILTER  ******************** 
    # Dynamically update "Value" UI based on selected column
    observeEvent(session$input[[paste0(tree_id, "_filter_col")]], {
      selected_col <- session$input[[paste0(tree_id, "_filter_col")]]
      if (!is.null(selected_col)){
        if (!selected_col %in% colnames(metadata)) selected_col<-sprintf("%s_%s",selected_col,input$year)
      }
      if (!is.null(selected_col) && selected_col %in% colnames(metadata)) {
        column_data <- metadata[[selected_col]]
        if (is.numeric(column_data)) {
          # If numeric, show text session$input
          output[[paste0(tree_id, "_filter_val_ui")]] <- renderUI({
            textInput(paste0(tree_id, "_filter_val"), "Value", value = "")
          })
        } else {
          # If non-numeric, show select session$input with unique values
          output[[paste0(tree_id, "_filter_val_ui")]] <- renderUI({
            selectInput(paste0(tree_id, "_filter_val"), "Value", choices = sprintf("%s",unique(column_data)), selected = NULL)
          })
        }
      }
    })
    observeEvent(session$input[[paste0(tree_id, "_update_filter")]], {
      current <- session$input[[paste0(tree_id, "_flt")]]
      column <- session$input[[paste0(tree_id, "_filter_col")]]
      operator <- session$input[[paste0(tree_id, "_filter_op")]]
      value <- session$input[[paste0(tree_id, "_filter_val")]]
      filter_string <- paste(column, operator, value)
      if (grepl("&$", current) || grepl("\\|$", current)) {
        updateTextInput(session, paste0(tree_id, "_flt"), value = paste(current,filter_string))
      } else {
        updateTextInput(session, paste0(tree_id, "_flt"), value = filter_string)  
      }
    })
    observeEvent(session$input[[paste0(tree_id, "_remove_filter")]], {
      updateTextInput(session, paste0(tree_id, "_flt"), value = "all")
    })
    observeEvent(session$input[[paste0(tree_id, "_remove")]], {
      removeUI(selector = paste0("#", tree_id))
    })
    # Add AND (&) to the filter string
    observeEvent(session$input$add_and, {
      current <- session$input[[paste0(tree_id, "_flt")]]
      if (!grepl("&$", current) && !grepl("\\|$", current)) {
        updateTextInput(session, paste0(tree_id, "_flt"), value = paste(current," &"))
      }
    })
    # Add OR (|) to the filter string
    observeEvent(session$input$add_or, {
      current <- session$input[[paste0(tree_id, "_flt")]]
      if (!grepl("&$", current) && !grepl("\\|$", current)) {
        updateTextInput(session, paste0(tree_id, "_flt"), value = paste(current," |"))
      }
    })
    # ******************** END FILTER ******************** 
  }
  
  
  # Observe changes to predefined maps
  observeEvent(input$predefined_maps, {
    selected_map <- input$predefined_maps
    selected_map_file <- file.path(template_path,paste0(selected_map,".rds"))
    if (file.exists(selected_map_file)) {
      
      disable_global_observer(TRUE)  # Disable the observer
      debouncingEnabled(TRUE)        # Enable debouncing
      
      loaded_config <- readRDS(selected_map_file)
      loaded_config$global_options$predefined_maps<-selected_map
      # Validate the loaded configuration file
      if (!is.null(loaded_config) && all(c("global_options", "tree_configurations") %in% names(loaded_config))) {
        # Update global options in the UI
        for (name in names(loaded_config$global_options)) {
          if (name %in% names(input)) {
            # Dynamically use the appropriate update function
            if (is.logical(loaded_config$global_options[[name]])) {
              updateCheckboxInput(session, name, value = loaded_config$global_options[[name]])
            } else if (is.character(loaded_config$global_options[[name]])) {
              updateTextInput(session, name, value = loaded_config$global_options[[name]])
            } else if (is.numeric(loaded_config$global_options[[name]])) {
              updateNumericInput(session, name, value = loaded_config$global_options[[name]])
            } else if (is.factor(loaded_config$global_options[[name]])) {
              updateSelectInput(session, name, selected = as.character(loaded_config$global_options[[name]]))
            }
          }
        }
      } else {
        message("invalid file")
      }
      # #config <- predefined_configs[[selected_map]]
      message(sprintf("***************\nloading predefined map: %s (%i tree layers)\n***************",selected_map,length(loaded_config$tree_configurations)))
      
      # Update tree configurations
      tree_config<-NULL
      trees$list<-list() # Reset the tree list
      tree_counter(1)    # Reset the tree counter
      removeUI(selector = "#trees_ui *", multiple = TRUE) #Clear the existing tree UI
      
      
      # Reload the configuration
      if (!is.null(loaded_config) && "tree_configurations" %in% names(loaded_config)) {
        for (i in seq_along(loaded_config$tree_configurations)) {
          tree_id <- paste0("tree_", i)
          tree_counter(tree_counter() + 1)
          addTree(tree_id, tree_config = loaded_config$tree_configurations[[i]], session = session, trees = trees)
        }
      }
      
      disable_global_observer(FALSE)  # Re-enable the observer
    }
  })

  
  # Reset debouncing after preset loading
  observe({
    if (!disable_global_observer()) {
      debouncingEnabled(FALSE)  # Disable debouncing for manual edits
    }
  })
  # Manually adding a tree layer
  observeEvent(input$add_tree, {
    new_tree_id <- paste0("tree_", tree_counter())
    tree_counter(tree_counter() + 1)
    addTree(new_tree_id, session = session, trees = trees)
  })
  
  
 #Update preview when something is changed on the UI (or after preset is loaded)
  observe({
    # Check if the observer is disabled
    if (disable_global_observer()) return()

    print ("***** collect values and plot map ***** ")
    
    mapOptions <- activeGlobalMapOptions()

    incomplete<-any(sapply(mapOptions, is.null)) | is.null(mapOptions$trees) || any(sapply(mapOptions$trees, function(tree) any(sapply(tree, is.null))))
    
    if (!incomplete)  {
      isolate({
        if (mapOptions$presentation) par(cex.lab=1.4,cex.axis=1.4,cex=1) else par(cex.lab=1,cex.axis=1,cex=1)
        output$mapPlot <- renderPlot({
          plotmap(metadata,mapOptions)
        },
        width = function() {
          width<-session$clientData$output_mapPlot_width
          if (width>720) width<-720
          return(width)
        },
        height = function() {
          # Dynamically calculate height based on the aspect ratio
          width<-session$clientData$output_mapPlot_width
          if (width>720) width<-720
          if (mapOptions$presentation){
            x_range <- diff(c(0, 121))  # xlim range
            y_range <- diff(c(0, 140))  # ylim range
          }
          else{
            x_range <- diff(c(-5, 125))  # xlim range
            y_range <- diff(c(-25, 150))  # ylim range
          }
          # Calculate height using the aspect ratio and width
          height <- (y_range / x_range) * width
          return(height)
        })
      })
    }
  })
  
  
  # Download  
  output$generate_and_download_map <- downloadHandler(
    filename = function() {
      paste0("SCCII_Map-",input$title,"_", Sys.Date(), ".", tolower(input$output_format))
    },
    content = function(file) {
      # Generate the map directly to the file path provided by `file`
      mapOptions <- activeGlobalMapOptions()
      
      tryCatch({
        if (input$output_format == "PDF") {
          if (mapOptions$presentation) {
            pdf(file,width =  8, height = 8*(141/121)-0.18)
            par(cex.lab=1.4,cex.axis=1.4,cex=1)
          }else {
            pdf(file, width = 8.27, height = 11.69)  # A4 dimensions  
          }
        } else if (input$output_format == "JPEG") {
          if (mapOptions$presentation) {
            jpeg(file, width =  8, height = 8*(141/121)-0.18, units = "in", quality = 95, res = 300)
            par(cex.lab=1.4,cex.axis=1.4,cex=1)
          }else {
            jpeg(file, width = 8.27, height = 11.69, units = "in", quality = 100, res = 1200)
          }
          #tiff(file, width = 8.27, height = 11.69, units = "in", res = 300)
        } else {
          stop("Unsupported output format")
        }
        
        plotmap(metadata, mapOptions)
        dev.off()  # Close the graphics device
      }, error = function(e) {
        dev.off()  # Ensure the device is closed in case of an error
        showModal(modalDialog(
          title = "Error",
          paste("Failed to generate the map:", e$message)
        ))
        stop(e$message)  # Rethrow the error to prevent download
      })
    }
  )
  
  output$save_config <- downloadHandler(
    filename = function() { paste0("map_config_", Sys.Date(), ".rds") },
    content = function(file) {
      config <- list(
        global_options = reactiveValuesToList(input),
        tree_configurations = trees$list
      )
      saveRDS(config, file)
    }
  )
  
  observeEvent(input$load_config, {
    req(input$load_config)
    # Attempt to load the configuration file
    loaded_config <- tryCatch({
      readRDS(input$load_config$datapath)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Invalid configuration file."
      ))
      return(NULL)
    })
    
    # Validate the loaded configuration file
    if (!is.null(loaded_config) && all(c("global_options", "tree_configurations") %in% names(loaded_config))) {
      # Update global options in the UI
      for (name in names(loaded_config$global_options)) {
        if (name %in% names(input)) {
          # Dynamically use the appropriate update function
          if (is.logical(loaded_config$global_options[[name]])) {
            updateCheckboxInput(session, name, value = loaded_config$global_options[[name]])
          } else if (is.character(loaded_config$global_options[[name]])) {
            updateTextInput(session, name, value = loaded_config$global_options[[name]])
          } else if (is.numeric(loaded_config$global_options[[name]])) {
            updateNumericInput(session, name, value = loaded_config$global_options[[name]])
          } else if (is.factor(loaded_config$global_options[[name]])) {
            updateSelectInput(session, name, selected = as.character(loaded_config$global_options[[name]]))
          }
        }
      }
      
      # Update tree configurations
      trees$list <- loaded_config$tree_configurations
      showModal(modalDialog(
        title = "Configuration Loaded",
        "Configuration has been successfully loaded."
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        "The configuration file does not match the expected format."
      ))
    }
  })
  
}

shinyApp(ui, server)
