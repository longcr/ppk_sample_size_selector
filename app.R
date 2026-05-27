# app.R
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- DATA LOAD ----
table_long <- read_excel("SCMH.xlsx", sheet = "table_long")

# Load wide-format table
table_wide0 <- read_excel("SCMH.xlsx", sheet = "table_original")

table_wide <- table_wide0 %>% 
  rename_with(
    ~ sprintf("%.2f", as.numeric(.x)),
    .cols = -1
  ) %>% 
  mutate(`Sample Size` = as.integer(`Sample Size`))


ui <- fluidPage(
  titlePanel("Sample Size for Minimum Ppk (90% Lower Confidence Limit)"),
  
  tabsetPanel(
    
    # ---------------- TAB 1: Calculator ----------------
    tabPanel("Sample Size Calculator",
             sidebarLayout(
               sidebarPanel(
                 numericInput("target_ppk",
                              "Target minimum Ppk (lower confidence bound):",
                              value = 1.33, step = 0.01, min = 0),
                 numericInput("observed_ppk",
                              "Assumed / observed Ppk:",
                              value = 1.50, step = 0.01, min = 0),
                 selectInput("conf_level",
                             "Confidence level (fixed by table):",
                             choices = c("90%"), selected = "90%"),
                 actionButton("calc", "Calculate sample size")
               ),
               
               mainPanel(
                 h4("Recommended sample size"),
                 verbatimTextOutput("result_text"),
                 h4("Matching rows from table_long"),
                 tableOutput("result_table"),
                 h4("Required point estimate vs. sample size"),
                 plotOutput("result_plot")
               )
             )
    ),
    
    # ---------------- TAB 2: Wide Table ----------------
    tabPanel("Full Table (Wide Format)",
             h3("SCMH 90% Lower Confidence Limits Table"),
             p("This is the full wide-format table from the 'table_original' sheet."),
             tableOutput("wide_table")
    )
  )
)

server <- function(input, output, session) {
  
  # ---- CALCULATION LOGIC ----
  calc_result <- eventReactive(input$calc, {
    target <- input$target_ppk
    obs    <- input$observed_ppk
    
    candidates <- table_long %>%
      filter(lcl90_meets >= target,
             req_calc_pointest <= obs) %>%
      arrange(ssize, lcl90_meets, req_calc_pointest)
    
    if (nrow(candidates) == 0) {
      return(list(
        text = "No combination in the table achieves that target lower confidence bound with the given observed Ppk.",
        table = NULL
      ))
    }
    
    min_n <- min(candidates$ssize)
    best  <- candidates %>% filter(ssize == min_n)
    
    list(
      text = paste0(
        "Minimum sample size: ", min_n,
        "\nThis is the smallest n where the 90% lower confidence bound (lcl90_meets) ",
        "is at least ", round(target, 3),
        " and the required point estimate (req_calc_pointest) is ≤ ",
        round(obs, 3), "."
      ),
      table = best,
      all   = candidates
    )
  })
  
  output$result_text <- renderText({
    calc_result()$text
  })
  
  output$result_table <- renderTable({
    calc_result()$table
  })
  
  output$result_plot <- renderPlot({
    res <- calc_result()
    if (is.null(res$all)) return(NULL)
    
    ggplot(res$all, aes(x = ssize, y = req_calc_pointest,
                        color = factor(lcl90_meets))) +
      geom_point(size = 3) +
      geom_hline(yintercept = input$observed_ppk,
                 linetype = "dashed", color = "red") +
      labs(
        x = "Sample size (ssize)",
        y = "Required point estimate (req_calc_pointest)",
        color = "LCL 90% meets",
        title = "Required Ppk vs. sample size for given 90% lower confidence bounds"
      ) +
      theme_minimal()
  })
  
  # ---- WIDE TABLE OUTPUT ----
  output$wide_table <- renderTable({
    table_wide
  })
}

shinyApp(ui, server)