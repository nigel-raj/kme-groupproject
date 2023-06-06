library(shiny)
library(DBI)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

# Database connection configuration
con <- dbConnect(
  drv = RMySQL::MySQL(),
  host = "localhost",
  port = 3306,
  user = "root",
  password = "kmeproject1122",
  dbname = "PCAdvisorySystem"
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "PC Advisory System"),
  dashboardSidebar(
    tags$head(
      tags$style(
        HTML("
          .sidebar {
            position: fixed !important;
            width: 230px
          }
          .main-header {
            position: fixed !important;
            width: 100%;
            z-index: 1000;
          }
          .content-wrapper {
            margin-top: 50px;
          }
        ")
      )
    ),
    div(class = "sidebar",
        sidebarMenu(
          menuItem(
            "Manual Build", tabName = "manualBuild", startExpanded=TRUE,
            menuSubItem("PC Parts", tabName = "pcParts", icon = icon("desktop")),
            menuSubItem("Selection", tabName = "selection", icon = icon("check")),
            menuSubItem("Benchmark", tabName = "benchmark", icon = icon("line-chart"))
          ),
          menuItem("Generate Build", tabName = "generateBuild"),
          menuItem("Parts Database", tabName = "partsDatabase")
        )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "pcParts",
        fluidRow(
          box(
            title = "CPU",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            height = "100%",
            uiOutput("cpuOutput")
          ),
          box(
            title = "GPU",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            height = "100%",
            uiOutput("gpuOutput")
          ),
          box(
            title = "RAM",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            height = "100%",
            uiOutput("ramOutput")
          ),
          box(
            title = "Storage",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            height = "100%",
            uiOutput("storageOutput")
          ),
          box(
            title = "Motherboard",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            height = "100%",
            uiOutput("motherboardOutput")
          ),
          box(
            title = "PSU",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            height = "100%",
            uiOutput("psuOutput")
          )
        )
      ),
      tabItem(
        tabName = "selection",
        fluidRow(
          box(
            title = "Selection",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            height = "500px",
            tableOutput("selectionTable"),
            actionButton("clearSelections", "Clear Selections"),
          )
        ),
        box(
          title = "Total Price ($)",
          width = 13,
          status = "primary",
          solidHeader = TRUE,
          height = "120px",
          tags$span(textOutput("totalPrice"))
        ),
        box(
          title = "Compatibility",
          width = 13,
          status = "primary",
          solidHeader = TRUE,
          height = "500px",
          tableOutput("compatibilityTable")
        ),
      ),
      tabItem(
        tabName = "generateBuild",
        fluidRow(
          box(
            title = "Generate Build",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            height = "500px",
          )
        )
      ),
      tabItem(
        tabName = "partsDatabase",
        fluidRow(
          box(
            title = "Parts Database",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            height = "500px",
          )
        )
      ),
      tabItem(
        tabName = "benchmark",
        fluidRow(
          box(
            title = "Benchmark",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            height = "500px",
            column(4, uiOutput("csgoBenchmark")),
            column(4, uiOutput("cinebenchBenchmark")),
            column(4, uiOutput("ramBenchmark"))
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Fetch CPU data from database
  cpuData <- reactive({
    query <- "SELECT * FROM CPU"
    dbGetQuery(con, query)
  })
  
  # Render CPU information
  output$cpuOutput <- renderUI({
    cpuBoxes <- lapply(1:nrow(cpuData()), function(i) {
      image <- tags$img(src = "https://www.computerhope.com/jargon/c/cpu.png", width = "50px", height = "50px")
      selectButton <- tags$button("Select", class = "btn btn-info btn-sm", onclick = paste0("Shiny.setInputValue('cpuButton', ", i, ")"))
      
      info <- tags$div(
        tags$p(tags$b("Name:"), cpuData()$Name[i]),
        tags$p(tags$b("Brand:"), cpuData()$Brand[i]),
        tags$p(tags$b("Socket:"), cpuData()$Socket[i]),
        tags$p(tags$b("Price: $"), cpuData()$Price[i]),
        tags$p(tags$b("Cores:"), cpuData()$Core_Count[i]),
        tags$p(tags$b("Threads:"), cpuData()$Thread_Count[i]),
        tags$p(tags$b("Clock Speed:"), cpuData()$Clock_Speed[i]),
        tags$p(tags$b("Max Clock Speed:"), cpuData()$Max_Clock_Speed[i])
      )
      boxContent <- tags$div(image, selectButton, info)
      box(
        title = NULL,
        width = 3,
        status = if (i %in% input$cpuButton) "success" else "primary",
        solidHeader = TRUE,
        height = "400px",
        boxContent
      )
    })
    fluidRow(cpuBoxes)
  })
  
  # Fetch GPU data from database
  gpuData <- reactive({
    query <- "SELECT * FROM GPU"
    dbGetQuery(con, query)
  })
  
  # Render GPU information
  output$gpuOutput <- renderUI({
    gpuBoxes <- lapply(1:nrow(gpuData()), function(i) {
      image <- tags$img(src = "https://www.pngall.com/wp-content/uploads/5/Graphic-Card-PNG-Image.png", width = "50px", height = "50px")
      selectButton <- tags$button("Select", class = "btn btn-info btn-sm", onclick = paste0("Shiny.setInputValue('gpuButton', ", i, ")"))
      info <- tags$div(
        tags$p(tags$b("Name:"), gpuData()$Name[i]),
        tags$p(tags$b("Brand:"), gpuData()$Brand[i]),
        tags$p(tags$b("PCIE Bus:"), gpuData()$PCIE_Bus_Interface[i]),
        tags$p(tags$b("Price: $"), gpuData()$Price[i]),
        tags$p(tags$b("Memory Size:"), gpuData()$Memory_Size[i]),
        tags$p(tags$b("Core Clock:"), gpuData()$Core_Clock[i])
      )
      boxContent <- tags$div(image, selectButton, info)
      box(
        title = NULL,
        width = 3,
        status = if (i %in% input$gpuButton) "success" else "primary",
        solidHeader = TRUE,
        height = "400px",
        boxContent
      )
    })
    fluidRow(gpuBoxes)
  })
  
  # Fetch RAM data from database
  ramData <- reactive({
    query <- "SELECT * FROM RAM"
    dbGetQuery(con, query)
  })
  
  # Render RAM information
  output$ramOutput <- renderUI({
    ramBoxes <- lapply(1:nrow(ramData()), function(i) {
      image <- tags$img(src = "https://www.computerhope.com/jargon/m/memory-ddr4.png", width = "50px", height = "50px")
      selectButton <- tags$button("Select", class = "btn btn-info btn-sm", onclick = paste0("Shiny.setInputValue('ramButton', ", i, ")"))
      info <- tags$div(
        tags$p(tags$b("Name:"), ramData()$Name[i]),
        tags$p(tags$b("Brand:"), ramData()$Brand[i]),
        tags$p(tags$b("Type:"), ramData()$Type[i]),
        tags$p(tags$b("Price: $"), ramData()$Price[i]),
        tags$p(tags$b("Channels:"), ramData()$Channels[i]),
        tags$p(tags$b("Capacity:"), ramData()$Total_Capacity[i]),
        tags$p(tags$b("Clock Speed:"), ramData()$Clock_Speed[i])
      )
      boxContent <- tags$div(image, selectButton, info)
      box(
        title = NULL,
        width = 3,
        status = if (i %in% input$ramButton) "success" else "primary",
        solidHeader = TRUE,
        height = "400px",
        boxContent
      )
    })
    fluidRow(ramBoxes)
  })
  
  # Fetch Storage data from database
  storageData <- reactive({
    query <- "SELECT * FROM Storage"
    dbGetQuery(con, query)
  })
  
  # Render Storage information
  output$storageOutput <- renderUI({
    storageBoxes <- lapply(1:nrow(storageData()), function(i) {
      image <- tags$img(src = "https://www.computerhope.com/issues/pictures/hard-drive-settings.png", width = "50px", height = "50px")
      selectButton <- tags$button("Select", class = "btn btn-info btn-sm", onclick = paste0("Shiny.setInputValue('storageButton', ", i, ")"))
      info <- tags$div(
        tags$p(tags$b("Name:"), storageData()$Name[i]),
        tags$p(tags$b("Brand:"), storageData()$Brand[i]),
        tags$p(tags$b("Type:"), storageData()$Type[i]),
        tags$p(tags$b("Price: $"), storageData()$Price[i]),
        tags$p(tags$b("Form Factor:"), storageData()$Form_Factor[i]),
        tags$p(tags$b("Capacity:"), storageData()$Capacity[i]),
      )
      boxContent <- tags$div(image, selectButton, info)
      box(
        title = NULL,
        width = 3,
        status = if (i %in% input$storageButton) "success" else "primary",
        solidHeader = TRUE,
        height = "400px",
        boxContent
      )
    })
    fluidRow(storageBoxes)
  })
  
  # Fetch Motherboard data from database
  motherboardData <- reactive({
    query <- "SELECT * FROM motherboard"
    dbGetQuery(con, query)
  })
  
  # Render motherboard information
  output$motherboardOutput <- renderUI({
    motherboardBoxes <- lapply(1:nrow(motherboardData()), function(i) {
      image <- tags$img(src = "https://www.pngall.com/wp-content/uploads/2016/04/Motherboard-Download-PNG.png", width = "50px", height = "50px")
      selectButton <- tags$button("Select", class = "btn btn-info btn-sm", onclick = paste0("Shiny.setInputValue('motherboardButton', ", i, ")"))
      
      info <- tags$div(
        tags$p(tags$b("Name:"), motherboardData()$Name[i]),
        tags$p(tags$b("Brand:"), motherboardData()$Brand[i]),
        tags$p(tags$b("Socket:"), motherboardData()$Socket[i]),
        tags$p(tags$b("Price: $"), motherboardData()$Price[i]),
        tags$p(tags$b("Chipset:"), motherboardData()$Chipset[i]),
        tags$p(tags$b("Form Factor:"), motherboardData()$Form_Factor[i])
      )
      boxContent <- tags$div(image, selectButton, info)
      box(
        title = NULL,
        width = 3,
        status = if (i %in% input$motherboardButton) "success" else "primary",
        solidHeader = TRUE,
        height = "400px",
        boxContent
      )
    })
    fluidRow(motherboardBoxes)
  })
  
  # Fetch PSU data from database
  psuData <- reactive({
    query <- "SELECT * FROM psu"
    dbGetQuery(con, query)
  })
  
  # Render PSU information
  output$psuOutput <- renderUI({
    psuBoxes <- lapply(1:nrow(psuData()), function(i) {
      image <- tags$img(src = "https://www.pngall.com/wp-content/uploads/12/UPS-PNG-Photos.png", width = "50px", height = "50px")
      selectButton <- tags$button("Select", class = "btn btn-info btn-sm", onclick = paste0("Shiny.setInputValue('psuButton', ", i, ")"))
      
      info <- tags$div(
        tags$p(tags$b("Name:"), psuData()$Name[i]),
        tags$p(tags$b("Brand:"), psuData()$Brand[i]),
        tags$p(tags$b("Price: $"), psuData()$Price[i]),
        tags$p(tags$b("Maximum Power Output:"), psuData()$Maximum_Power_Output[i]),
        tags$p(tags$b("Efficiency Certification:"), psuData()$Efficency_Certification[i])
      )
      boxContent <- tags$div(image, selectButton, info)
      box(
        title = NULL,
        width = 3,
        status = if (i %in% input$psuButton) "success" else "primary",
        solidHeader = TRUE,
        height = "400px",
        boxContent
      )
    })
    fluidRow(psuBoxes)
  })
  
  # Create a reactive value for selected parts
  selectedParts <- reactiveValues(
    cpu = character(),
    gpu = character(),
    ram = character(),
    storage = character(),
    motherboard = character(),
    psu = character()
  )
  
  # Update selected parts when checkbox is clicked
  # Update selected parts based on user input
  observeEvent(input$cpuButton, {
    selectedParts$cpu <- input$cpuButton
    updateRadioButtons(session, "cpuButton", selected = selectedParts$cpu)
  })
  
  
  observeEvent(input$gpuButton, {
    selectedParts$gpu <- input$gpuButton
    updateRadioButtons(session, "gpuButton", selected = selectedParts$gpu)
  })
  
  
  observeEvent(input$ramButton, {
    selectedParts$ram <- input$ramButton
    updateRadioButtons(session, "ramButton", selected = selectedParts$ram)
  })
  
  
  observeEvent(input$storageButton, {
    selectedParts$storage <- input$storageButton
    updateRadioButtons(session, "storageButton", selected = selectedParts$storage)
  })
  
  observeEvent(input$motherboardButton, {
    selectedParts$motherboard <- input$motherboardButton
    updateRadioButtons(session, "motherboardButton", selected = selectedParts$motherboard)
  })
  
  observeEvent(input$psuButton, {
    selectedParts$psu <- input$psuButton
    updateRadioButtons(session, "psuButton", selected = selectedParts$psu)
  })
  
  
  output$selectionTable <- renderTable({
    selectedCPU <- cpuData()[selectedParts$cpu, c("Name", "Brand", "Price")]
    selectedGPU <- gpuData()[selectedParts$gpu, c("Name", "Brand", "Price")]
    selectedRAM <- ramData()[selectedParts$ram, c("Name", "Brand", "Price")]
    selectedStorage <- storageData()[selectedParts$storage, c("Name", "Brand", "Price")]
    selectedMotherboard <- motherboardData()[selectedParts$motherboard, c("Name", "Brand", "Price")]
    selectedPSU <- psuData()[selectedParts$psu, c("Name", "Brand", "Price")]
    
    selectedParts <- rbind(selectedCPU, selectedGPU, selectedRAM, selectedStorage, selectedMotherboard, selectedPSU)
    selectedParts$Price <- as.numeric(selectedParts$Price)
    
    selectedParts
  })
  
  
  output$totalPrice <- renderText({
    # Calculate total price
    selectedCPU <- cpuData()[selectedParts$cpu, c("Name", "Brand", "Price")]
    selectedGPU <- gpuData()[selectedParts$gpu, c("Name", "Brand", "Price")]
    selectedRAM <- ramData()[selectedParts$ram, c("Name", "Brand", "Price")]
    selectedStorage <- storageData()[selectedParts$storage, c("Name", "Brand", "Price")]
    selectedMotherboard <- motherboardData()[selectedParts$motherboard, c("Name", "Brand", "Price")]
    selectedPSU <- psuData()[selectedParts$psu, c("Name", "Brand", "Price")]
    
    selectedParts <- rbind(selectedCPU, selectedGPU, selectedRAM, selectedStorage, selectedMotherboard, selectedPSU)
    selectedParts$Price <- as.numeric(selectedParts$Price)
    
    total <- sum(selectedParts$Price)
    
    paste0(total)
  })
  
  output$compatibilityTable <- renderTable({
    compatibilityTable <- data.frame(Part_Type = character(0),
                                     Compatibility = character(0),
                                     stringsAsFactors = FALSE)
    
    # Check if any part is selected
    if (!is.null(selectedParts$cpu) || !is.null(selectedParts$gpu) ||
        !is.null(selectedParts$ram) || !is.null(selectedParts$storage) ||
        !is.null(selectedParts$motherboard) || !is.null(selectedParts$psu)) {
      
      # Check if all part types are selected
      if (!is.null(selectedParts$cpu) && !is.null(selectedParts$gpu) &&
          !is.null(selectedParts$ram) && !is.null(selectedParts$storage) &&
          !is.null(selectedParts$motherboard) && !is.null(selectedParts$psu)) {
        
        selectedCPU <- cpuData()[selectedParts$cpu, ]
        selectedGPU <- gpuData()[selectedParts$gpu, ]
        selectedRAM <- ramData()[selectedParts$ram, ]
        selectedStorage <- storageData()[selectedParts$storage, ]
        selectedMotherboard <- motherboardData()[selectedParts$motherboard, ]
        selectedPSU <- psuData()[selectedParts$psu, ]
        
        # Split the Compatible_Chipsets values of CPU
        compatibleChipsets <- strsplit(as.character(selectedCPU$Compatible_Chipsets), ", ")[[1]]
        
        # Check if any of the compatibleChipsets is present in the Chipset of Motherboard
        isCPUChipcompatible <- any(compatibleChipsets %in% strsplit(as.character(selectedMotherboard$Chipset), ", ")[[1]])
        
        # Extract the individual expansion slots from the Expansion_Slots string of the Motherboard
        expansionSlots <- unlist(strsplit(trimws(as.character(selectedMotherboard$Expansion_Slots)), ", "))
        
        # Check if any of the expansion slots contain the PCIE_Bus_Interface of the GPU
        isGPUcompatible <- any(grepl(trimws(selectedGPU$PCIE_Bus_Interface), expansionSlots))
        
        isCPUcompatible <- selectedCPU$Socket == selectedMotherboard$Socket && isCPUChipcompatible
        isRAMcompatible <- selectedRAM$Type == selectedMotherboard$RAM_Type &&
          selectedRAM$Bus_Interface == selectedMotherboard$RAM_Interface &&
          selectedRAM$Total_Capacity <= selectedMotherboard$RAM_Max_Capacity &&
          selectedRAM$Clock_Speed <= selectedMotherboard$RAM_Max_Speed &&
          selectedRAM$Channels <= selectedMotherboard$RAM_Max_Channels
        # Extract the individual storage slots from the Storage_Slots string of the Motherboard
        storageSlots <- unlist(strsplit(trimws(as.character(selectedMotherboard$Storage_Slots)), ", "))
        
        # Check if any of the storage slots contain the Bus_Interface of the Storage Drive
        isStorageDriveCompatible <- any(grepl(trimws(selectedStorage$Bus_Interface), storageSlots))
        
        isPSUcompatible <- selectedPSU$Maximum_Power_Output >= (selectedCPU$TDP + selectedGPU$Max_TDP + selectedStorage$Max_TDP)
        
        # Build compatibility
        isBuildCompatible <- isCPUcompatible && isGPUcompatible && isRAMcompatible &&
          isStorageDriveCompatible && isPSUcompatible
        
        # Add compatibility status to the table
        compatibilityTable <- rbind(compatibilityTable,
                                    data.frame(Part_Type = "CPU", Compatibility = ifelse(isCPUcompatible, "Compatible", "Not Compatible")),
                                    data.frame(Part_Type = "GPU", Compatibility = ifelse(isGPUcompatible, "Compatible", "Not Compatible")),
                                    data.frame(Part_Type = "RAM", Compatibility = ifelse(isRAMcompatible, "Compatible", "Not Compatible")),
                                    data.frame(Part_Type = "Storage Drive", Compatibility = ifelse(isStorageDriveCompatible, "Compatible", "Not Compatible")),
                                    data.frame(Part_Type = "Motherboard", Compatibility = ifelse(isBuildCompatible, "Compatible", "Not Compatible")),
                                    data.frame(Part_Type = "PSU", Compatibility = ifelse(isPSUcompatible, "Compatible", "Not Compatible"))
        )
      } else {
        # At least one part type is missing
        compatibilityTable <- data.frame(Part_Type = character(),
                                         Compatibility = character(),
                                         stringsAsFactors = FALSE)
      }
    }
    
    compatibilityTable
  })
  
  observeEvent(input$clearSelections, {
    # Clear the selected parts
    selectedParts$cpu <- NULL
    selectedParts$gpu <- NULL
    selectedParts$ram <- NULL
    selectedParts$storage <- NULL
    selectedParts$motherboard <- NULL
    selectedParts$psu <- NULL
    
    # Reset the selected button inputs
    updateRadioButtons(session, "cpuButton", selected = NULL)
    updateRadioButtons(session, "gpuButton", selected = NULL)
    updateRadioButtons(session, "ramButton", selected = NULL)
    updateRadioButtons(session, "storageButton", selected = NULL)
    updateRadioButtons(session, "motherboardButton", selected = NULL)
    updateRadioButtons(session, "psuButton", selected = NULL)
  })
  
  # Calculate benchmark values
  observe({
    selectedCPUData <- cpuData()[selectedParts$cpu, ]
    selectedGPUData <- gpuData()[selectedParts$gpu, ]
    selectedRAMData <- ramData()[selectedParts$ram, ]
    
    maxGPUScore <- max(gpuData()$G3D_Average_Score)
    maxCPUClockSpeed <- max(cpuData()$Clock_Speed)
    maxCPUMaxClockSpeed <- max(cpuData()$Max_Clock_Speed)
    maxRAMClockSpeed <- max(ramData()$Clock_Speed)
    
    csgoBenchmark <- if (length(selectedParts$gpu) > 0) {
      gpuScore <- selectedGPUData$G3D_Average_Score / maxGPUScore * 100
      progressBar(id = "csgoBenchmark", value = gpuScore, display_pct = TRUE, title = "CSGO (High Preset: 1080p)")
    } else {
      progressBar(id = "csgoBenchmark", value = 0, display_pct = TRUE, title = "CSGO (High Preset: 1080p)")
    }
    
    cinebenchBenchmark <- if (length(selectedParts$cpu) > 0) {
      cpuClockSpeed <- selectedCPUData$Clock_Speed / maxCPUClockSpeed * 50
      cpuMaxClockSpeed <- selectedCPUData$Max_Clock_Speed / maxCPUMaxClockSpeed * 50
      progressBar(id = "cinebenchBenchmark", value = cpuClockSpeed + cpuMaxClockSpeed, display_pct = TRUE, title = "Cinebench (Multi-Core R23)")
    } else {
      progressBar(id = "cinebenchBenchmark", value = 0, display_pct = TRUE, title = "Cinebench (Multi-Core R23)")
    }
    
    ramBenchmark <- if (length(selectedParts$ram) > 0) {
      ramClockSpeed <- selectedRAMData$Clock_Speed / maxRAMClockSpeed * 100
      progressBar(id = "ramBenchmark", value = ramClockSpeed, display_pct = TRUE, title = "RAM Speed")
    } else {
      progressBar(id = "ramBenchmark", value = 0, display_pct = TRUE, title = "RAM Speed")
    }
    
    output$csgoBenchmark <- renderUI({
      csgoBenchmark
    })
    
    output$cinebenchBenchmark <- renderUI({
      cinebenchBenchmark
    })
    
    output$ramBenchmark <- renderUI({
      ramBenchmark
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
