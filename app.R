
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)

# -----------------------------
# SAMPLE SUSTAINABILITY DATA
# -----------------------------
sustainability_data <- data.frame(

  Building = c(
    "Science Center",
    "Library",
    "Student Center",
    "Engineering Hall",
    "Residence Hall",
    "Business Building"
  ),

  Energy_kWh = c(
    120000,
    95000,
    150000,
    175000,
    140000,
    88000
  ),

  Water_Gallons = c(
    50000,
    42000,
    61000,
    70000,
    65000,
    39000
  ),

  CO2_Emissions = c(
    45,
    35,
    60,
    72,
    55,
    30
  ),

  Waste_kg = c(
    1200,
    950,
    1600,
    1800,
    1450,
    800
  )
)
# -----------------------------
# UI
# -----------------------------

ui <- navbarPage(

  title = div(
    style = "font-weight:700; font-size:30px;",
    "University Sustainability Dashboard"
  ),

  inverse = TRUE,

  collapsible = TRUE,

  theme = bslib::bs_theme(
    version = 5,
    primary = "#008f39",
    bg = "#f5f7fa",
    fg = "#1a1a1a"
  ),

  # -----------------------------
  # OVERVIEW
  # -----------------------------

  tabPanel(

    "Overview",

    br(),

    fluidRow(

      column(
        4,

        div(
          style = "
            background: linear-gradient(135deg,#008f39,#00b050);
            color:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.15);
          ",

          h1(sum(sustainability_data$Energy_kWh)),
          h4("Total Energy Usage (kWh)"),
          icon("bolt", "fa-3x")
        )
      ),

      column(
        4,

        div(
          style = "
            background: linear-gradient(135deg,#2e8b57,#66cdaa);
            color:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.15);
          ",

          h1(sum(sustainability_data$CO2_Emissions)),
          h4("Total CO2 Emissions"),
          icon("cloud", "fa-3x")
        )
      ),

      column(
        4,

        div(
          style = "
            background: linear-gradient(135deg,#00bcd4,#26c6da);
            color:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.15);
          ",

          h1(sum(sustainability_data$Water_Gallons)),
          h4("Water Consumption"),
          icon("tint", "fa-3x")
        )
      )
    ),

    br(),

    fluidRow(

      column(
        12,

        div(
          style = "
            background:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.1);
          ",

          h2(
            style = "font-weight:700;",
            "University Sustainability Overview"
          ),

          plotlyOutput("overview_plot", height = 500)
        )
      )
    )
  ),

  # -----------------------------
  # ENERGY
  # -----------------------------

  tabPanel(

    "Energy Analytics",

    br(),

    fluidRow(

      column(
        12,

        div(
          style = "
            background:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.1);
          ",

          h2("Building-wise Energy Consumption"),

          plotOutput("energy_plot", height = 500)
        )
      )
    )
  ),

  # -----------------------------
  # CARBON
  # -----------------------------

  tabPanel(

    "Carbon Emissions",

    br(),

    fluidRow(

      column(
        12,

        div(
          style = "
            background:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.1);
          ",

          h2("Carbon Emissions by Building"),

          plotOutput("carbon_plot", height = 500)
        )
      )
    )
  ),

  # -----------------------------
  # UTILITIES
  # -----------------------------

  tabPanel(

    "Utilities",

    br(),

    fluidRow(

      column(
        6,

        div(
          style = "
            background:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.1);
          ",

          h3("Water Usage"),

          plotOutput("water_plot")
        )
      ),

      column(
        6,

        div(
          style = "
            background:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.1);
          ",

          h3("Waste Generation"),

          plotOutput("waste_plot")
        )
      )
    )
  ),

  # -----------------------------
  # DATA
  # -----------------------------

  tabPanel(

    "Data Explorer",

    br(),

    fluidRow(

      column(
        12,

        div(
          style = "
            background:white;
            padding:25px;
            border-radius:20px;
            box-shadow:0px 4px 15px rgba(0,0,0,0.1);
          ",

          h2("Sustainability Dataset"),

          DTOutput("data_table")
        )
      )
    )
  )
)
# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {

  # OVERVIEW PLOT
  output$overview_plot <- renderPlotly({

    p <- ggplot(
      sustainability_data,
      aes(x = Building, y = Energy_kWh)
    ) +

      geom_bar(
        stat = "identity",
        fill = "forestgreen"
      ) +

      theme_minimal() +

      labs(
        title = "Energy Consumption Overview",
        x = "Building",
        y = "Energy (kWh)"
      )

    ggplotly(p)
  })

  # ENERGY PLOT
  output$energy_plot <- renderPlot({

    ggplot(
      sustainability_data,
      aes(x = Building, y = Energy_kWh)
    ) +

      geom_bar(
        stat = "identity",
        fill = "darkgreen"
      ) +

      theme_minimal() +

      labs(
        title = "Energy Usage by Building",
        x = "Building",
        y = "Energy (kWh)"
      )
  })

  # CARBON PLOT
  output$carbon_plot <- renderPlot({

    ggplot(
      sustainability_data,
      aes(x = Building, y = CO2_Emissions)
    ) +

      geom_bar(
        stat = "identity",
        fill = "seagreen"
      ) +

      theme_minimal() +

      labs(
        title = "CO2 Emissions by Building",
        x = "Building",
        y = "CO2 Emissions"
      )
  })

  # WATER PLOT
  output$water_plot <- renderPlot({

    ggplot(
      sustainability_data,
      aes(x = Building, y = Water_Gallons)
    ) +

      geom_bar(
        stat = "identity",
        fill = "steelblue"
      ) +

      theme_minimal() +

      labs(
        title = "Water Consumption",
        x = "Building",
        y = "Gallons"
      )
  })

  # WASTE PLOT
  output$waste_plot <- renderPlot({

    ggplot(
      sustainability_data,
      aes(x = Building, y = Waste_kg)
    ) +

      geom_bar(
        stat = "identity",
        fill = "darkolivegreen"
      ) +

      theme_minimal() +

      labs(
        title = "Waste Generation",
        x = "Building",
        y = "Waste (kg)"
      )
  })

  # DATA TABLE
  output$data_table <- renderDT({

    datatable(
      sustainability_data,

      options = list(
        pageLength = 5
      ),

      rownames = FALSE
    )
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui, server)

