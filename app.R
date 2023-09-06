library(shiny)
library(shinydashboard)
library(tidyverse)
library(openxlsx)

ui <- dashboardPage(
  dashboardHeader(title = 'My expenditure'),
  dashboardSidebar(
    fluidRow(
      # add column for own file import
      fileInput("file1", "Choose CSV File",
                multiple = F,
                accept = ".csv"),
      
      # add toggle to reset data
      actionButton("reset", "Use default data"),

      # dynamic dateRangeInput
      uiOutput("date_range"),
      
      # dynamic category checkbox
      uiOutput("input_category")
    )
  ),
  
  dashboardBody(
    
    tabsetPanel(type = 'tabs',
                tabPanel("Main", 
                         fluidRow(
                           valueBoxOutput("valuebox_out_day"),
                           valueBoxOutput("valuebox_out_total"),
                           valueBoxOutput("valuebox_out_mean")),
                         fluidRow(
                           plotOutput("plot_out_cumsum")),
                         fluidRow(
                           splitLayout(style = 'border: 1px solid silver;',
                                       cellArgs = list(style = "padding: 6px"),
                                       cellWidths = c('50%', '50%'),
                                       plotOutput("plot_out_cat"),
                                       plotOutput("plot_out_top5")))
                         ),
                tabPanel("Data", tableOutput("table_out_df"))
                )
    )
)

# server is to designate how to build the objects.
# name in object should match the reactive element (ie functions in ui that ends with *output)

server <- function(input, output, session) {
  
  # set default data if no input file given
  default_data = 
    data.frame(date =  c(
      "1/8/2023", "1/8/2023", "1/8/2023", "2/8/2023", "2/8/2023", "2/8/2023", "2/8/2023", "2/8/2023", "3/8/2023","3/8/2023", "3/8/2023", "3/8/2023", "4/8/2023", "4/8/2023", "4/8/2023", "5/8/2023", "5/8/2023", "5/8/2023","5/8/2023", "5/8/2023", "6/8/2023", "6/8/2023", "6/8/2023", "6/8/2023", "6/8/2023", "6/8/2023", "6/8/2023",
      "6/8/2023", "7/8/2023", "7/8/2023", "7/8/2023", "7/8/2023", "7/8/2023", "7/8/2023", "8/8/2023", "9/8/2023","9/8/2023", "9/8/2023", "9/8/2023", "9/8/2023", "10/8/2023", "10/8/2023", "10/8/2023", "10/8/2023","11/8/2023", "11/8/2023", "11/8/2023", "12/8/2023", "12/8/2023", "12/8/2023", "12/8/2023", "12/8/2023","12/8/2023", "12/8/2023", "12/8/2023", "12/8/2023", "12/8/2023", "12/8/2023", "12/8/2023", "13/8/2023","13/8/2023", "14/8/2023", "14/8/2023", "14/8/2023", "14/8/2023", "15/8/2023", "15/8/2023", "15/8/2023","15/8/2023", "16/8/2023", "16/8/2023", "16/8/2023", "17/8/2023", "18/8/2023", "18/8/2023", "18/8/2023","18/8/2023", "18/8/2023", "20/8/2023", "20/8/2023", "20/8/2023", "20/8/2023", "21/8/2023", "21/8/2023","22/8/2023", "22/8/2023", "23/8/2023", "23/8/2023", "23/8/2023", "23/8/2023", "24/8/2023", "25/8/2023","25/8/2023", "25/8/2023", "25/8/2023", "25/8/2023", "27/8/2023", "28/8/2023", "28/8/2023", "28/8/2023"),category = c("outside meal", "dessert", "outside meal", "morning coffee", "outside meal", "outside meal", "outside meal","grocery", "morning coffee", "outside meal", "dessert", "outside meal", "morning coffee", "outside meal", "outside meal","outside meal", "transport", "household", "transport", "outside meal", "dessert","transport", "household", "outside meal","grocery", "transport", "household","household", "morning coffee", "outside meal", "dessert", "transport", "transport","transport", "grocery", "transport", "household", "outside meal", "transport","dessert", "outside meal", "household", "dessert", "transport", "transport","shopping_entertainment", "outside meal","transport", "shopping_entertainment","transport", "shopping_entertainment", "household", "dessert", "outside meal","morning coffee", "outside meal", "dessert", "transport", "morning coffee", "outside meal", "household", "household","morning coffee", "outside meal", "dessert","grocery", "morning coffee", "outside meal", "dessert", "transport", "morning coffee","outside meal", "household", "household", "morning coffee", "outside meal", "dessert","outside meal", "grocery","transport", "outside meal", "outside meal", "grocery", "transport", "outside meal", "dessert", "grocery", "transport","outside meal", "outside meal","dessert", "grocery", "outside meal", "dessert", "grocery", "transport", "outside meal","dessert","grocery", "outside meal"),item = c("lunch", "ice cream", "dinner", "coffee", "lunch", "lunch", "dinner", "grocery", "coffee", "lunch", "coffee","dinner", "coffee", "lunch", "dinner", "brunch", "grab", "kitchenware", "gocar", "dinner", "tea", "gocar","furnitures","dinner", "grocery", "gocar", "glasswares", "cookware", "coffee", "lunch", "dimsum", "gocar", "gocar", "gocar","grocery", "gocar", "electronics", "coffee", "lunch", "coffee", "ice cream", "coffee", "tea", "gocar", "coffee","lunch", "washware", "lighting", "coffee", "lunch", "chips", "grocery", "coffee", "coffee","lunch", "french fries","dinner", "gocar", "grocery", "cleaning", "ice cream", "grocery", "lunch", "coffee", "tea", "grocery", "lunch", "dimsum","ice cream", "dinner", "coffee", "lunch", "tea", "lunch", "dinner", "grocery", "gocar", "lunch", "lunch", "grocery","lunch", "grocery", "lunch", "transport",  "lunch", "ice cream", "dinner", "coffee", "lunch", "lunch", "dinner", "grocery", "coffee", "lunch", "coffee", "lunch", "ice cream", "dinner", "coffee", "lunch"),price = c(37, 14, 21, 10, 14, 30, 18, 263, 10, 14, 20, 13, 10, 31, 40, 12, 124, 442, 26, 20, 14.3, 67, 536, 42, 325, 34, 513,421, 10, 24, 23, 132, 68, 38, 395, 39, 26, 72, 528, 8, 8, 20, 17, 445, 21, 120, 5, 59, 455, 473, 45, 59, 151, 77,391, 460, 33, 30, 372, 143, 511, 34, 13, 40, 22, 8, 29, 420, 425, 8, 20, 5, 302, 24, 17, 22, 42, 30, 174, 402, 25.3,106, 26, 5, 5, 355, 44, 5, 7, 41, 5, 33, 47, 50, 31, 396, 13, 40, 22, 12)) %>% 
    mutate(date2 = date,
           date = lubridate::dmy(date),
           price = price*0.5) 
  
  # dynamic UI for dateRangeInput
  output$date_range = renderUI({
    
    date_range_min <- current_data() %>% pull(date) %>% min()
    date_range_max <- current_data() %>% pull(date) %>% max()
    
    dateRangeInput("var_input_date", "Dates",
                   start = date_range_min,
                   end = date_range_max)
  })
  
  # dynamic UI for input category
  output$input_category = renderUI({
    
    cat_all = current_data() %>% pull(category) %>% unique()
    
    checkboxGroupInput("var_input_category", "Expense categories",
                       choices = cat_all,
                       selected = cat_all)
  })
  
  # config for uploaded data
  uploaded_data = reactive({
    
    req(input$file1)
    df = read.csv(input$file1$datapath, header=T) %>% 
         mutate(date = lubridate::dmy(date)) 
    
  })
  
  # set default data when app starts
  current_data = reactiveVal(default_data)
  
  # if 'reset' button (defined in ui) clicked, dashboard will default to default data
  observeEvent(input$reset, {
    current_data(default_data)
  })
  
  # if a file is uploaded, then change current_data to the uploaded data
  observeEvent(input$file1, { 
    if (!is.null(input$file1$datapath))
      current_data(uploaded_data())
  })
  
  # render data.frame of default or uploaded data
  output$table_out_df = renderTable({
    
    current_data() %>%
      filter(category %in% input$var_input_category)
    
    })
  
  # generate temporary dataframe to ease code visibility
  df_filtered = reactive({
      current_data() %>%
      filter(category %in% input$var_input_category) %>%
      filter(date >=  input$var_input_date[1] & 
               date <= input$var_input_date[2]) 
  })
  tempdata = reactive({
    
    df_filtered() %>% 
      summarise(no_of_day = max(date)+1 - min(date),
      total_in_m = sum(price)/1000,
      total_in_m = round(total_in_m, digits=2),
      mean_daily = total_in_m*1000 / as.numeric(no_of_day),
      mean_daily = round(mean_daily, digits=2)) 
  })
  
  # value box no days
  output$valuebox_out_day = renderValueBox({
    valueBox(value = tempdata()$no_of_day,
             subtitle = '# Days',
             color='olive')
  })
  
  # value box total
  output$valuebox_out_total = renderValueBox({
    valueBox(value = paste0(tempdata()$total, 'M'),
             subtitle = 'Total spending',
             color='orange')
  })
  
  # value box mean
  output$valuebox_out_mean = renderValueBox({
    valueBox(value = paste0(tempdata()$mean_daily, 'K'),
             subtitle = 'Daily average spending',
             color='blue')
  })
  
  # plot cumulative spending
  output$plot_out_cumsum = renderPlot({
    
    df_filtered() %>%
      group_by(date) %>%
      summarise(sum_daily = sum(price)) %>%
      mutate(cum_sum = cumsum(sum_daily),
             col_color = ifelse(cum_sum <= 10000, "Below Limit", "Passed Limit")) %>%
      ggplot(aes_string(x='date', y='cum_sum')) +
      geom_line(aes(color = col_color), size=2, color = 'grey') + theme_bw() +
      geom_point(size = 3, aes(color = col_color)) +
      scale_x_date(breaks = 'day', date_labels = '%d-%b') +
      scale_color_manual(values = c("Below Limit" = '#0c343d', 
                                    "Passed Limit" = '#85200c')) +
      labs(x='', y='Rp (`000)',
           title= 'Cumulative spending') +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust=1),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12),
            legend.position = 'none',
            plot.title = element_text(size=15, face='bold'))
  })
  
  # plot for spending category
  output$plot_out_cat = renderPlot({
    df_filtered() %>%
      group_by(category) %>%
      summarise(total = sum(price)) %>%
      filter(category != 'Undefined') %>%
      ggplot(aes(x=total, y=reorder(category, total), fill=total)) +
      geom_segment(aes(x=0, xend = total,
                       y=reorder(category, total),
                       yend = reorder(category, total))) +
      geom_col(aes(x=total, y=reorder(category, total))) +
      geom_text(aes(label = total, hjust=1.5), color = 'white', size=4) +
      scale_fill_gradient(low = '#0c343d', high = '#85200c') +
      labs(x='Rp (`000)',
           y='', title = 'Top Spending categories') +
      theme_bw() +
      theme(plot.title = element_text(size=15, face='bold'),
            legend.position = 'none',
            axis.text.y = element_text(size=12),
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12))
  })
  
  # top 5 spending
  output$plot_out_top5 = renderPlot({
   df_filtered() %>%
      slice_max(order_by = price, n = 5) %>%
      ggplot(aes(x=price, y=reorder(item,price), fill=category)) +
      geom_segment(aes(x=0, xend=price,
                       y=reorder(item,price), yend=reorder(item,price),
                       color=price), size=1.5) +
      geom_point(size=1.5) +
      geom_text(aes(label = price, hjust='inward', vjust=-0.5), size=4) +
      scale_color_gradient(low = '#0c343d', high = '#85200c') +
      theme_bw() +
      labs(x='Rp (`000)', y='',
           title = 'Top 5 Item Spending') +
      theme(plot.title = element_text(size=15, face='bold'),
            legend.position = 'none',
            axis.text.y = element_text(size=12),
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12))
  })
  
}

shinyApp(ui, server)
