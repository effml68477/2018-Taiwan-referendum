server <- function(input, output) {
  output$supported_rate_formula <- renderUI({
    withMathJax('$$\\frac{the \\, number \\, of \\, people \\, who \\, participate \\, this \\, vote \\, and \\, supporte \\, it} {the \\, number \\, of \\, people \\, who \\, participate \\, this \\, vote}$$')
  })
  
  output$supported_rate_datatable <- DT::renderDataTable(
    DT::datatable({
      data_o <- referendum %>% round(., 4)
      if(input$which_vars_datatable!="all"){
        data <- data.frame(data_o[, input$which_vars_datatable])
        colnames(data) <- as.character(input$which_vars_datatable)
        row.names(data) <- row.names(referendum)
        data
      }else{
        data_o
      }
    })
  )

  output$correlation_plot <- renderPlot({
    ggplot(data = melt(corr_referendum_up[input$which_vars_Correlation, input$which_vars_Correlation], na.rm = T) , 
           aes(x = Var1, y = Var2, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "#FFD306", ##EAC100
                           high = "#ff60af", 
                           midpoint = 0, 
                           limit = c(-1, 1), 
                           space = "Lab", 
                           name = "Spearman\nCorrelation")+
      theme(axis.text.x = element_text(angle = 0, vjust = 1, size = 10, hjust = 1))+
      theme(axis.text.y = element_text(angle = 0, vjust = 1, size = 10, hjust = 1))+
      theme_minimal()+geom_text(aes(Var1, Var2, label = value), color = "black", size = 4)+
      labs(x = "cases", y = "cases")
  })
  cases_cluster <- reactive({
    set.seed(123)
    kmeans(x = var_position[, 1:2], centers = input$kmeans_clusters, iter.max = 100)
  })
  output$cases_cluster_plot <- renderPlot({
    ggplot()+
      geom_point(aes(x = var_position[, 1], y = var_position[, 2]), 
                 color = as.factor(cases_cluster()$cluster), size = 2)+
      theme(panel.background = element_rect(fill = "#ecf5ff", color = "white", size = 0.87))+
      labs(x = "pca_dim1", y = "pca_dim2", title = "visulaized plot for every cases")+
      geom_text(aes(x = var_position[, 1], y = var_position[, 2], label = var_position[, 3]), 
                color = as.factor(cases_cluster()$cluster), size = 5)+
      geom_point(aes(x = cases_cluster()$center[, 1], y = cases_cluster()$center[, 2]), 
                 shape = 23, fill = "orange", color = "orange", size = 5, alpha = 0.5)
  })
  
  
  selected_case <- reactive({
    if(length(input$which_vars_Distribution)==1){
      data.frame(values = referendum[, input$which_vars_Distribution], ind = rep("input$which_vars_Distribution", 368))
    }else{
      referendum[, input$which_vars_Distribution]
    }
  })
  ########## I must fix the variate color problem
  output$cases_distribution_plot <- renderPlot({
    ggplot()+
      geom_density(aes(x = values, fill = ind, color = ind), 
                   data = stack(selected_case()), size = 1.00, adjust = 0.9, alpha = 0.05)+
      labs(title = paste0("the distribtuion of supported rate over all sub administrative region ( each cases ) "), 
           x = "supported rate", y = "")
      #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#D9006C", "#FF00FF", "#DCB5FF", "#019858", "#D94600", "#8F4586", "#808040"))+
      #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#D9006C", "#FF00FF", "#DCB5FF", "#019858", "#D94600", "#8F4586", "#808040"))
  })
  
  output$cases_box_plot <- renderPlot({
    ggplot()+
      geom_boxplot(aes(x = ind, y = values, color = ind), 
                   data = stack(selected_case()))+
      labs(title = "the boxplot of supported rate over all sub administrative region ( each cases ) ", 
           x = "cases", y = "supported rate")
  })
  
  output$samples_biplot <- renderPlot({
    ggplot()+geom_point(aes(x = V1, y = V2), data = samples_position_with_vote,
                        color = "#d9b300", size = 3)+
      geom_text(aes(x = V1, y = V2), data = samples_position_with_vote,
                    label = row.names(referendum), color = "#009100", size = 3)+
      theme(panel.background = element_rect(fill = "#ecf5ff", color = "white", size = 0.87))+
      labs(x = "dim1", y = "dim2", title = "biplot")+
      geom_segment(aes(x = 0, y = 0, xend = var_position[, 1]*2, yend = var_position[, 2]*2), 
                   arrow = arrow(length = unit(0.5,"cm")), alpha = 0.6, color = "red", size = 1)+
      geom_text(aes(x = var_position[, 1]*2.2, y = var_position[, 2]*2.2, label = colnames(referendum)), 
                size = 5)
  })
  
  
  selected_range_biplot <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    actually_selected_biplot <- input$selected_area_biplot
    if(!is.null(actually_selected_biplot)){
      selected_range_biplot$x <- c(actually_selected_biplot$xmin, actually_selected_biplot$xmax)
      selected_range_biplot$y <- c(actually_selected_biplot$ymin, actually_selected_biplot$ymax)
    }else{
      selected_range_biplot$x <- NULL
      selected_range_biplot$y <- NULL
    }
  })
  
  output$amplified_samples_biplot <- renderPlot({
    ggplot()+geom_point(aes(x = V1, y = V2), data = samples_position_with_vote,
                        color = "#d9b300", size = 3)+
      geom_text(aes(x = V1, y = V2), data = samples_position_with_vote,
                label = row.names(referendum), color = "#009100", size = 3)+
      theme(panel.background = element_rect(fill = "#ecf5ff", color = "white", size = 0.87))+
      labs(x = "dim1", y = "dim2", title = "biplot")+
      geom_segment(aes(x = 0, y = 0, xend = var_position[, 1]*2, yend = var_position[, 2]*2), 
                   arrow = arrow(length = unit(0.5,"cm")), alpha = 0.6, color = "red", size = 1)+
      geom_text(aes(x = var_position[, 1]*2.2, y = var_position[, 2]*2.2, label = colnames(referendum)), 
                size = 5)+
      coord_cartesian(xlim = selected_range_biplot$x, ylim = selected_range_biplot$y, expand = F)
  })
  
  output$samples_detail <- renderPrint({
    brushedPoints(samples_position_with_vote, input$selected_area_biplot)
  })
  
  selected_vote_data <- reactive({
    data.frame(vote_map[, c("long", "lat", "group", input$which_votes_taiwanmap)])
  })
  
  output$taiwan_plot <- renderPlot({
    ggplot()+
      geom_polygon(aes(x = long, y = lat, group = group, fill = selected_vote_data()[, 4]), 
                   data = selected_vote_data())+
      scale_fill_gradient2(low = "#2894ff", high = "#ae0000", mid = "#e8ffc4", 
                           midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "level")+
      theme_minimal()+ coord_fixed(xlim = c(117, 123), ylim = c(21, 26))+
      labs(title = paste0("supported rate map for ", colnames(selected_vote_data())[4]))
      #lims(x = c(117, 123), y = c(21, 26))
      
       #coord_map(xlim = c(117, 123), ylim = c(21, 26))+
  })
  
  selected_range_taiwan_plot <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    actually_selected_taiwan_plot <- input$selected_area_taiwan_plot
    if(!is.null(actually_selected_taiwan_plot)){
      selected_range_taiwan_plot$x <- c(actually_selected_taiwan_plot$xmin, actually_selected_taiwan_plot$xmax)
      selected_range_taiwan_plot$y <- c(actually_selected_taiwan_plot$ymin, actually_selected_taiwan_plot$ymax)
    }else{
      selected_range_taiwan_plot$x <- NULL
      selected_range_taiwan_plot$y <- NULL
    }
  })
  
  output$amplified_taiwan_plot <- renderPlot({
    ggplot()+
      geom_polygon(aes(x = long, y = lat, group = group, fill = selected_vote_data()[, 4]), 
                   color = "black", size = 0.1, data = selected_vote_data())+
      scale_fill_gradient2(low = "#2894ff", high = "#ae0000", mid = "#e8ffc4", 
                           midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "level")+
      theme_minimal()+ coord_fixed(xlim = selected_range_taiwan_plot$x, ylim = selected_range_taiwan_plot$y)+
      labs(title = paste0("supported rate map for ", colnames(selected_vote_data())[4]))+
      geom_text(aes(x = centraltown$longxmean, y = centraltown$latymean), 
                data = centraltown, label = centraltown$towneng, size = 2.3)
  }) 
  
  
  
  output$each_distribution_plot <- renderPlot({
    ggplot()+
      geom_histogram(aes(x = referendum[, input$which_votes_taiwanmap]), binwidth = 0.003, fill = "#FF0080", alpha = 0.5)+
      labs(title = paste0("the distribution of supported rate for ", colnames(selected_vote_data())[4]), x = "supported rate")+
      geom_density(aes(x = referendum[, input$which_votes_taiwanmap]), color = "#FFD306", size = 1.2)+
      theme(panel.background = element_rect(fill = "white", color = "black", size = 0.87))
  }) ##FF60AF #FFD306
  
  

  
  
}