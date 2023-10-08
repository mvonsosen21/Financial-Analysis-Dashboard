# Dashboard updated: 10/5/2023

library(shinydashboard)
library(shiny)
library(formattable)
library(ggplot2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ui.R ##
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Company Financial Analysis", titleWidth = 300),
  dashboardSidebar(
    sidebarMenuOutput("menu"),
    dateRangeInput('dateRange',
                   label = 'Date range input: dd/mm/yyyy',
                   start = accts_total$Date[1], end = accts_total$Date[nrow(accts_total)],
                   format = "dd/mm/yyyy"),
    menuItem("Note: The initial date range which appears includes all data which is currently available to be visible in the dashboard"),
    menuItem("To view recurring expenses for an indicated month(s), the first of the month must be selected as the start date.")
  ),
  dashboardBody(
    tabsetPanel(
      id = "tab",
      type = "tabs",
      tabPanel("Total",
               h3("Account 1234 and Account 5678"),
               fluidRow(valueBoxOutput("total_balance_box", width = 5), valueBoxOutput("monthly_balance_box", width = 5)),
               plotOutput("plot1.1"),
               plotOutput("plot1.2"),
               plotOutput("plot1.3")
      ),
      tabPanel("Account 1234",
               h3("Account 1234"),
               fluidRow(valueBoxOutput("acc1234_balance_box", width = 5), valueBoxOutput("monthly_balance_box_1234", width = 5)),
               plotOutput("plot2.1"),
               plotOutput("plot2.2"),
               plotOutput("plot2.3")
      ),
      tabPanel("Account 5678",
               h3("Account 5678"),
               fluidRow(valueBoxOutput("acc5678_balance_box", width = 5), valueBoxOutput("monthly_balance_box_5678", width = 5)),
               plotOutput("plot3.1"),
               plotOutput("plot3.2"),
               plotOutput("plot3.3")
      ),
      tabPanel("Recurring Expenses",
               h3("Recurring Expenses"),
               fluidRow(valueBoxOutput("recur_monthly_balance_box", width = 5), valueBoxOutput("num_recur_balance_box", width = 5)),
               plotOutput("plot4.1"),
               plotOutput("plot4.2")
      )
    )
  )
)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## server.R ##

server <- function(input, output) {
  
  # Filter data based on date range selection
  # Filters are in the order of the analysis code
  filtered_data_accts_total <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    accts_total_filtered <- accts_total %>%
      filter(Date >= start_date, Date <= end_date)
    return(accts_total_filtered)
  })
  
  filtered_data_cat_1234 <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    acc1234_cat_filtered <- acc1234_cat %>%
      filter(Transaction.Date >= start_date, Transaction.Date <= end_date) %>%
      select(Expense.Category, Amount) %>%
      group_by(Expense.Category) %>%
      summarise(Amount_Sum = sum(Amount), .groups = 'drop') %>%
      as.data.frame()
    return(acc1234_cat_filtered)
  })
  
  filtered_data_cat_5678 <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    acc5678_cat_filtered <- acc5678_cat %>%
      filter(Transaction.Date >= start_date, Transaction.Date <= end_date) %>%
      select(Expense.Category, Amount) %>%
      group_by(Expense.Category) %>%
      summarise(Amount_Sum = sum(Amount), .groups = 'drop') %>%
      as.data.frame()
    return(acc5678_cat_filtered)
  })
  
  filtered_data_cat_both <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    both_accts = rbind(filtered_data_cat_1234(), filtered_data_cat_5678())
    bothaccts_cat_filtered <- both_accts %>%
      group_by(Expense.Category) %>%
      summarise(Amount_Sum = sum(Amount_Sum), .groups = 'drop') %>%
      as.data.frame()
    return(bothaccts_cat_filtered)
  })
  
  filtered_data_freq_1234 <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    freq_filtered_1234 <- acc1234_cat %>%
      filter(Transaction.Date >= start_date, Transaction.Date <= end_date) %>%
      filter(Transaction.Type != "Deposit") %>%
      select(Expense.Category) %>%
      count(Expense.Category) %>%
      mutate(Total = sum(n)) %>%
      mutate(prop = (n / Total) * 100) %>%
      mutate(prop = round(prop, digits = 2)) %>%
      select(Expense.Category, prop) %>%
      as.data.frame()
    return(freq_filtered_1234)
  })
  
  filtered_data_freq_5678 <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    freq_filtered_5678 <- acc5678_cat %>%
      filter(Transaction.Date >= start_date, Transaction.Date <= end_date) %>%
      filter(Transaction.Type != "Deposit") %>%
      filter(Transaction.Type != "Credit") %>%
      select(Expense.Category) %>%
      count(Expense.Category) %>%
      mutate(Total = sum(n)) %>%
      mutate(prop = (n / Total) * 100) %>%
      mutate(prop = round(prop, digits = 2)) %>%
      select(Expense.Category, prop) %>%
      as.data.frame()
    return(freq_filtered_5678)
  })
  
  filtered_data_freq_both <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    both_accts_freq = rbind(filtered_data_freq_1234(), filtered_data_freq_5678())
    bothaccts_freq_filtered <- both_accts_freq %>%
      select(Expense.Category) %>%
      count(Expense.Category) %>%
      mutate(Total = sum(n)) %>%
      mutate(prop = (n / Total)*100) %>%
      mutate(prop = round(prop, digits = 2)) %>%
      select(Expense.Category, prop)
    return(bothaccts_freq_filtered)
  })
  
  filtered_data_recur_cat <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    data_filtered_recur_cat <- acc5678_recurring %>%
      filter(Transaction.Date >= start_date, Transaction.Date <= end_date) %>%
      select(!Expense.Category) %>%
      group_by(Recurring.Category, month = lubridate::floor_date(Transaction.Date, "month")) %>%
      summarize(Monthly_Amount = sum(Amount), .groups = 'drop') %>%
      as.data.frame()
    return(data_filtered_recur_cat)
  })
  
  filtered_data_recur_total <- reactive({
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    data_filtered_recur_total <- acc5678_recurring %>%
      filter(Transaction.Date >= start_date, Transaction.Date <= end_date) %>%
      select(!Recurring.Category) %>%
      group_by(month = lubridate::floor_date(Transaction.Date, "month")) %>%
      summarize(Total_Recurring = sum(Amount)) %>%
      as.data.frame()
    return(data_filtered_recur_total)
  })
  
  ### Value Boxes
  
  # Total
  total_balance <- reactive({
    filtered_data = filtered_data_accts_total()
    total_balance = round(filtered_data$Total_Balance[nrow(filtered_data)], 2)
    total_balance = currency(total_balance, digits = 2L)
    return(total_balance)
  })
  
  avg_monthly_spend_total <- reactive({
    filtered_data = filtered_data_accts_total()
    monthly_total_balance = filtered_data %>%
      group_by(month = lubridate::floor_date(Date, "month")) %>%
      mutate(Total_Amount = Amount_1234 + Amount_5678) %>%
      summarize(Monthly_Total = sum(Total_Amount), .groups = 'drop') %>%
      as.data.frame()
    monthly_avg = round(mean(monthly_total_balance$Monthly_Total), 2)
    monthly_avg = currency(monthly_avg, digits = 2L)
    return(monthly_avg)
  })
  
  # Acc 1234
  acc1234_balance <- reactive({
    filtered_data = filtered_data_accts_total()
    acc1234_balance = round(filtered_data$Daily.Posted.Balance_1234[nrow(filtered_data)], 2)
    acc1234_balance = currency(acc1234_balance, digits = 2L)
    return(acc1234_balance)
  })
  
  avg_monthly_spend_1234 <- reactive({
    filtered_data = filtered_data_accts_total()
    monthly_total_balance = filtered_data %>%
      group_by(month = lubridate::floor_date(Date, "month")) %>%
      mutate(Total_Amount = Amount_1234 + Amount_5678) %>%
      summarize(Monthly_Total = sum(Amount_1234), .groups = 'drop') %>%
      as.data.frame()
    monthly_avg_1234 = round(mean(monthly_total_balance$Monthly_Total), 2)
    monthly_avg_1234 = currency(monthly_avg_1234, digits = 2L)
    return(monthly_avg_1234)
  })
  
  # Acc 5678
  acc5678_balance <- reactive({
    filtered_data = filtered_data_accts_total()
    acc5678_balance = round(filtered_data$Daily.Posted.Balance_5678[nrow(filtered_data)], 2)
    acc5678_balance = currency(acc5678_balance, digits = 2L)
    return(acc5678_balance)
  })
  
  avg_monthly_spend_5678 <- reactive({
    filtered_data = filtered_data_accts_total()
    monthly_total_balance = filtered_data %>%
      group_by(month = lubridate::floor_date(Date, "month")) %>%
      summarize(Monthly_Total = sum(Amount_5678), .groups = 'drop') %>%
      as.data.frame()
    monthly_avg_5678 = round(mean(monthly_total_balance$Monthly_Total), 2)
    monthly_avg_5678 = currency(monthly_avg_5678, digits = 2L)
    return(monthly_avg_5678)
  })
  
  # Recurring
  # Avg monthly recurring expense amount
  avg_recurring_monthly <- reactive({
    filtered_data = filtered_data_recur_total()
    monthly_recur_avg = round(mean(filtered_data$Total_Recurring), 2)
    monthly_recur_avg = currency(monthly_recur_avg, digits = 2L)
    return(monthly_recur_avg)
  })
  
  # Number of recurring expenses
  num_recurring <- reactive({
    filtered_data = filtered_data_recur_cat()
    num_cat = length(unique(filtered_data$Recurring.Category))
    return(num_cat)
  })
  
  
  ### Update ggplot graphs with filtered data
  
  # Total
  # What is the daily posted balance over time?
  output$plot1.1 <- renderPlot({
    p1_total_filtered <- ggplot(filtered_data_accts_total(), aes(x = Date, y = Total_Balance)) +
      geom_line(color = "#cc5500", size = 1) +
      geom_point(color = "darkgreen", size = 1) +
      labs(
        title = "Total Daily Posted Balance",
        subtitle = "Account 1234 and Account 5678",
        y = "Total Daily Posted Balance ($)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13)
      )
    print(p1_total_filtered)
  })
  # Where are we spending the most money in general?
  output$plot1.2 <- renderPlot({
    p2_both_filtered <- ggplot(filtered_data_cat_both(), aes(x=Expense.Category, y=Amount_Sum)) +
      geom_bar(stat = "identity", color = "darkgreen", fill = "#72BF6A") +
      geom_text(aes(label=Amount_Sum), vjust=-1, size=3) +
      labs(
        title = "Expenses by Category",
        subtitle = "Account 1234 and Account 5678",
        x = "Expense Category",
        y = "Amount Spent ($)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13))
    print(p2_both_filtered)
  })
  # Where are we spending money the most frequently?
  output$plot1.3 <- renderPlot({
    p3_both_filtered <- ggplot(filtered_data_freq_both(), aes(x=Expense.Category, y=prop)) +
      geom_bar(stat = "identity", color = "#7c4700", fill = "#D2B48C") +
      geom_text(aes(label=prop), vjust=-0.75, size=3) +
      labs(
        title = "Expense Frequency by Category",
        subtitle = "Account 1234 and Account 5678",
        x = "Expense Category",
        y = "Proportion (%)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13))
    print(p3_both_filtered)
  })
  
  # Value Boxes
  output$total_balance_box <- renderValueBox({
    valueBox(
      value = total_balance(),
      subtitle = "Current Posted Balance",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  output$monthly_balance_box <- renderValueBox({
    valueBox(
      value = avg_monthly_spend_total(),
      subtitle = "Average Monthly Spend",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  
  # 1234
  # What is the daily posted balance over time?
  output$plot2.1 <- renderPlot({
    p1_1234_filtered <- ggplot(filtered_data_accts_total(), aes(x = Date, y = Daily.Posted.Balance_1234)) +
      geom_line(color = "#cc5500", size = 1) +
      geom_point(color = "darkgreen", size = 1) +
      labs(
        title = "Daily Posted Balance",
        subtitle = "Account 1234",
        y = "Daily Posted Balance ($)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13)
      )
    print(p1_1234_filtered)
  })
  # Where are we spending the most money in general?
  output$plot2.2 <- renderPlot({
    p2_1234_filtered <- ggplot(filtered_data_cat_1234(), aes(x=Expense.Category, y=Amount_Sum)) +
      geom_bar(stat = "identity", color = "darkgreen", fill = "#72BF6A") +
      geom_text(aes(label=Amount_Sum), vjust=-1, size=3) +
      labs(
        title = "Expenses by Category",
        subtitle = "Account 1234",
        x = "Expense Category",
        y = "Amount Spent ($)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13))
    print(p2_1234_filtered)
  })
  # Where are we spending the most money the most frequently?
  output$plot2.3 <- renderPlot({
    p3_1234_filtered <- ggplot(filtered_data_freq_1234(), aes(x=Expense.Category, y=prop)) +
      geom_bar(stat = "identity", color = "#7c4700", fill = "#D2B48C") +
      geom_text(aes(label=prop), vjust=-0.75, size=3) +
      labs(
        title = "Expense Frequency by Category",
        subtitle = "Account 1234",
        x = "Expense Category",
        y = "Proportion (%)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13))
    print(p3_1234_filtered)
  })
  
  output$acc1234_balance_box <- renderValueBox({
    valueBox(
      value = acc1234_balance(),
      subtitle = "Current Posted Balance",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  output$monthly_balance_box_1234 <- renderValueBox({
    valueBox(
      value = avg_monthly_spend_1234(),
      subtitle = "Average Monthly Spend",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  
  
  # 5678
  # What is the daily posted balance over time?
  output$plot3.1 <- renderPlot({
    p1_5678_filtered <- ggplot(filtered_data_accts_total(), aes(x = Date, y = Daily.Posted.Balance_5678)) +
      geom_line(color = "#cc5500", size = 1) +
      geom_point(color = "darkgreen", size = 1) +
      labs(
        title = "Daily Posted Balance",
        subtitle = "Account 5678",
        y = "Daily Posted Balance ($)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13)
      )
    print(p1_5678_filtered)
  })
  # Where are we spending the most money in general?
  output$plot3.2 <- renderPlot({
    p2_5678_filtered <- ggplot(filtered_data_cat_5678(), aes(x=Expense.Category, y=Amount_Sum)) +
      geom_bar(stat = "identity", color = "darkgreen", fill = "#72BF6A") +
      geom_text(aes(label=Amount_Sum), vjust=-1, size=3) +
      labs(
        title = "Expenses by Category",
        subtitle = "Account 5678",
        x = "Expense Category",
        y = "Amount Spent ($)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13))
    print(p2_5678_filtered)
  })
  # Where are we spending money the most frequently?
  output$plot3.3 <- renderPlot({
    p3_5678_filtered <- ggplot(filtered_data_freq_5678(), aes(x=Expense.Category, y=prop)) +
      geom_bar(stat = "identity", color = "#7c4700", fill = "#D2B48C") +
      geom_text(aes(label=prop), vjust=-0.75, size=3) +
      labs(
        title = "Expense Frequency by Category",
        subtitle = "Account 5678",
        x = "Expense Category",
        y = "Proportion (%)"
      ) +
      theme(
        plot.title = element_text(color = "darkgreen", size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "bold"),
        plot.caption = element_text(face = "italic", hjust = 0),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13))
    print(p3_5678_filtered)
  })
  
  # Value box
  output$acc5678_balance_box <- renderValueBox({
    valueBox(
      value = acc5678_balance(),
      subtitle = "Current Posted Balance",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  output$monthly_balance_box_5678 <- renderValueBox({
    valueBox(
      value = avg_monthly_spend_5678(),
      subtitle = "Average Monthly Spend",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  
  
  #Recurring
  output$plot4.1 <- renderPlot({
    p1_recur_filtered <- ggplot(filtered_data_recur_cat(), aes(x=month, y=Monthly_Amount, fill=Recurring.Category)) +
      scale_x_date(date_labels="%b %y",date_breaks  ="1 month") +
      geom_col(position = position_dodge2(width = 1, preserve = "single")) +
      scale_fill_manual(values = c("#7c4700", "darkgreen", "#CC5500")) +
      labs(
        fill = "Recurring Expense Category",
        title = "Recurring Expenses by Category",
        x = "Month",
        y = "Monthly Amount ($)"
      )+
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "#7c4700", size = 20, face = "bold"),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13)
      )
    print(p1_recur_filtered)
  })
  
  output$plot4.2 <- renderPlot({
    p2_recur_filtered <- ggplot(filtered_data_recur_total(), aes(x=month, y=Total_Recurring)) +
      geom_line(size = 1, color = "#7c4700") +
      labs(
        title = "Total Recurring Expenses Over Time",
        x = "Transaction Date",
        y = "Amount ($)"
      ) +
      geom_point(size = 2.5, color = "#7c4700") +
      theme(
        plot.title = element_text(color = "#7c4700", size = 20, face = "bold"),
        axis.text.x=element_text(size=15),
        axis.text.y = element_text(size = 15),
        axis.title=element_text(size=13)
      ) +
      theme_hc()
    print(p2_recur_filtered)
  })
  
  # Value box
  output$recur_monthly_balance_box <- renderValueBox({
    valueBox(
      value = avg_recurring_monthly(),
      subtitle = "Average Monthy Recurring Spend",
      icon = icon("dollar-sign"),
      color = "orange"
    )
  })
  
  output$num_recur_balance_box <- renderValueBox({
    valueBox(
      value = num_recurring(),
      subtitle = "Number of Recurring Expenses",
      icon = icon("hashtag"),
      color = "orange"
    )
  })
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run the app!
shinyApp(ui, server)

