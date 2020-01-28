ui <- fluidPage(
  titlePanel("2018 Taiwan Referendum"),
  
  navlistPanel("Table of Content", well = F, fluid = F, selected = "Result",
               tabPanel( 
                 "Introduction", 
                 h3("TAIWAN"), 
                 h5("Taiwan, an island country, also known as Formosa, 
                    is located in North-East Asia with its subsidiary island. 
                    Citizens in Taiwan have the natural right to participate in their public affairs, 
                    such as reaching the consensus by voting, electing their president, 
                    legislator, councilor, etc. As of 31 December 2018, 
                    Taiwan consist of 368 sub administrative district. 
                    The referendum (2018) in Taiwan is around three major issues. 
                    energy issue (case7, 8, 9, 16), homosexual issue (case10, 11, 12, 14, 15), 
                    and the national name in the international occasion of Taiwan(case13). 
                    All cases are adopted except for case13, case14, and case15."), 
                 br(), 
                 br(),
                 h3("THE TEN QUESTIONS"),
                 h5('Case7: Do you agree "to reduce at least 1% on average per year" 
                    gradually of the power generation outputs of thermal power plant?'), 
                 br(), 
                 h5('Case8: Do you agree to establish an energy policy to 
                    "Stop the construction and expansion of any 
                    fossil fuel power station or generator units 
                    (including the Shen Ao Power Plant currently under expansion construction)"?'),
                 br(),
                 h5('Case9: Do you agree that the government should continue to prohibit 
                    the import of agricultural products and food from areas 
                    affected by the Fukushima March 11th Disaster? Specifically, 
                    those from Fukushima district and the 4 surrounding cities of 
                    Ibaraki, Tochigi, Gunma, and Chiba?'),
                 br(),
                 h5('Case10: Do you agree that marriage defined in the Civil Code 
                    should be restricted to the union between a man and a woman?'),
                 br(),
                 h5('Case11: Do you agree that the Ministry of Education and schools 
                    (elementary and junior high schools) should not implement the gender equality education 
                    defined by the Enforcement Rules for Gender Equity Education 
                    Act to students under primary and junior high school education?'),
                 br(),
                 h5("Case12: Do you agree to implement the protection of same-sex couples' rights of 
                    permanent cohabitation by ways other than amending the marriage definition 
                    in the Civil Code?"),
                 br(),
                 h5('Case13: Do you agree to use the name "Taiwan" when applying for attending in 
                    all international sport competitions, including the upcoming 2020 Tokyo Olympics?'),
                 br(),
                 h5('Case14: Do you agree to implement the protection of same-sex marital rights 
                    with the marriage definition in the Civil Code?'),
                 br(),
                 h5('Case15: Do you agree that the "Gender Equality Education Act" 
                    should stipulate the implementation of gender equality education 
                    in every stage of primary and junior high school education, 
                    including relationship education, sex education and Sexual Orientation 
                    & Gender Identity education?'),
                 br(),
                 h5('Case16: Do you agree to repeal the Article 95 paragraph 1 of The Electricity Act: 
                    "The nuclear-energy-based power-generating facilities shall wholly stop running by 2025"?'),
                 br(),
                 h3("DATA"),
                 h5("The data are collected from the Central Election Commission, Taiwan.
                    The supported rate is defined by "),
                 withMathJax(),
                 uiOutput('supported_rate_formula'),
                 br(),
                 h5("the data are shown below"),
                 br(),
                 fluidPage(
                   fluidRow(
                     column(
                       width = 6,
                       selectInput(
                         inputId = "which_vars_datatable",
                         label = "cases(votes)",
                         choices = c("all", colnames(referendum))
                       )
                     )
                   )
                 ),
                 dataTableOutput("supported_rate_datatable")
               ),
               tabPanel(
                 "Conclusion",
                 h5("The result shows that these 10 cases can adequately group into 4 clusters. 
                    And before concluding, there is some fact that needs to be clarified. 
                    Case 7 to case 9 are proposed by the KMT(a Chinese nationalist party). 
                    Case 10 to case 12 is submitted by an organization related to the Christian. 
                    Case13 is offered by Chi, who got the bronze medal in the 1968 Olympic game. 
                    case 14 case15 are proposed by Miao and Wang respectively, 
                    who make an effort to the equity of the marriage rights for LGBT. 
                    Case 16 is submitted by Huang, 
                    who wants to abolish the bill said that the nuclear-generating electricity 
                    must stop entirely before2015. This proposal is also endorsed by some people 
                    who rich in influence in KMT. 
                    Lanyu Township is the location for nuclear waste storage."),
                 br(),
                 h5("Conclusion1 : Case 7 to case 9 and case 16 seems to make a consensus around Taiwan 
                    no matter which district you live in except for location for nuclear waste storage, lanyu township."),
                 br(),
                 h5("Conclusion2 : Case 7 to case 9, case 16, and case 13 imply the confrontation between the two major political parties in Taiwan."),
                 br(),
                 h5("Conclusion3 : Case 10 to case 12 seems not to integrate well, although the same organization proposes these cases."),
                 br(),
                 h5("Conclusion4 : There is a high discrepancy in case 13 around Taiwan though this case tends to veto it. 
                    The difference related to the location you live in. 
                    Peoples who live in southern Taiwan are more support to this case 
                    which compared to the eastern Taiwan and the other regions."),
                 br(),
                 h5("Conclusion5 : Case 14 and case 15 show a slightly urban-rural gap. 
                    People who live in Northern and Eastern Taiwan with opinions closing to neutral."),
                 br(),
                 h5("We are going to learn more about these cases."),
                 h5("Let's go into detail."),
                 br(),
                 h5("For conclusion1 : The correlation plot shows that there are terribly strong relationships 
                    among case 7, case 8 and case9, and fairly strong with case 16, which are 
                    visualized in the cluster plot. According to the shape of distribution/box plot, the shape of these cases
                    are concentrated, and the color of the heat plot of Taiwan shows a uniform. It seems that
                    these caes are get a high consensus around Taiwan. In the biplot, Lanyu township is opposite to 
                    the arrow of case 7 to case 9 and case 16, which shows peoples in Lanyu township tend to object to these cases."),
                 br(),
                 h5("For conclusion2 : The conclusion is based on the correlation plot ,the biplot, and the heat plot of Taiwan. 
                    Case 13 has rather negative relationship to the case 7 to case 9, and case 16, 
                    the arrow of these two groups in biplot are on the opposite side, which shows the more support in case 7 to case 9 and case 16,
                    the less support in case 13. Case 13 get more supported rate in southern Taiwan; peoples who live in
                    southern Taiwan have the tendency of supporting DPP, which is one of the two major political parties in Taiwan."),
                 br(),
                 h5("For conclusion3 : Case 12 is literally 0.3764 with case 10. The color of case 12 is more light
                    than case 10 in heat plot, which shows that there is an existing discrepancy
                    between these two cases."),
                 br(),
                 h5("For conclusion4 : Case 13 shows quiet high variance in the distribution plot and box plot. According to the
                    heat plot of Taiwan, peoples live in the southern Taiwan are more supportive to this case compared to the
                    eastern Taiwan and the others regions."),
                 br(),
                 h5("For conclusion5 : Despite case 14 and case 15 are vetoed around Taiwan, we still can find some information
                    that may be useful. The color of case 14 and case 15 in urban region is more light than rural area in heat plot which
                    shows that there may exist an urban-rural gap.")
               ),
               tabPanel(
                 "Correlation Cluster Distribution Plot", 
                 mainPanel(
                   tabsetPanel(
                     #type = "tabs",
                     tabPanel(
                       "Correlation",
                       h3("Correlation for each cases"),
                       br(),
                       h4("A value of 1 and -1 imples the relationship between two variables perfectly, a value of -1 indicates
                          one varible increase as the other decrease. A value of 0 means that two variable have no relationship."),
                       br(),
                       br(),
                       conditionalPanel(
                         'input.data == corr_referendum_up',
                         checkboxGroupInput(
                           inputId = "which_vars_Correlation",
                           label = "cases(votes) to show",
                           choices = colnames(corr_referendum_up), 
                           selected = colnames(corr_referendum_up),
                           inline = T
                         )
                       ),
                       plotOutput(
                         outputId = "correlation_plot", 
                         width = 600, height = 500
                       )
                     ),
                     tabPanel(
                       "Cluster",
                       h3("Cluster for each cases"),
                       br(),
                       h4("The visualized plot is drawn on the basis of the correlation among each cases after conducting PCA"),
                       br(),
                       br(),
                       sidebarPanel(width = 6,
                         numericInput(inputId = "kmeans_clusters", 
                                      label = "number of cluster", 
                                      value = 4, min = 1, max = 10, step = 1)
                       ),
                       plotOutput(
                         outputId = "cases_cluster_plot", 
                         width = 600, height = 400
                       )
                     ),
                     tabPanel(
                       "Distribution",
                       h3("Distribution plot and Boxplot for each cases"),
                       br(),
                       h4("These two plots based on the distribution of supported rate in 
                          368 sub administrative districts in Taiwan."),
                       br(),
                       br(),
                       conditionalPanel(
                         'input.data == corr_referendum_up',
                         checkboxGroupInput(
                           inputId = "which_vars_Distribution",
                           label = "cases(votes) to show",
                           choices = colnames(corr_referendum_up),
                           selected = colnames(corr_referendum_up),
                           inline = T
                         )
                       ),
                       plotOutput(
                         outputId = "cases_distribution_plot", 
                         width = 600, height = 300
                       ),
                       plotOutput(
                         outputId = "cases_box_plot", 
                         width = 600, height = 300
                       )
                     )
                   )
                 )
               ),
               tabPanel(
                 "Biplot",
                 h3("Biplot"), #class = "well", for frame
                 br(),
                 h4("Biplot is the PCA of samples visualized plot, these two component contain 80% variance of data. 
                    The arrows on the biplot indicate where you can find districts with high values on a particular variable."),
                 br(),
                 h5("The chosen region is going to show an amplified image on the next plot and
                    the detail of the supported rate and the location in the plot is going to in the end of the page."),
                 br(),
                 br(),
                 plotOutput(
                   outputId = "samples_biplot", 
                   width = 600, height = 400,
                   brush = brushOpts(id = "selected_area_biplot", resetOnNew = TRUE)
                 ),
                 h3("Amplified_Biplot"),
                 plotOutput(
                   outputId = "amplified_samples_biplot", 
                   width = 600, height = 400
                 ),
                 fluidPage(
                   fluidRow(
                     column(
                       width = 12,
                       h3("Selected District"),
                       verbatimTextOutput(
                         outputId = "samples_detail", 
                         placeholder = T
                       )
                     )
                   )
                 )
                 
               ),
               tabPanel(
                 "Heat plot Taiwan",
                 br(),
                 h4("The supported rate of each cases are demonstrated as a heat plot."),
                 br(),
                 h5("The chosen region is going to show an amplified image on the right plot.
                    The district name also be demonstrated."),
                 br(),
                 br(),
                 fluidPage(
                   fluidRow(
                     column(
                       width = 6,
                       wellPanel( 
                         h4("cases(votes) selection"),
                         selectInput(
                           inputId = "which_votes_taiwanmap", 
                           label = "cases(votes)", 
                           choices = colnames(referendum)
                         )
                       )
                     )
                   )
                 ),
                 fluidPage(
                   fluidRow(
                     column(
                       width = 6, class = "well",
                       h4("supported rate on each district"),
                       plotOutput(
                         outputId = "taiwan_plot",
                         brush = brushOpts(id = "selected_area_taiwan_plot")
                       )
                     ),
                     column(
                       width = 6, class = "well",
                       h4("amplified supported rate on each district"),
                       plotOutput(
                         outputId = "amplified_taiwan_plot"
                       )
                     )
                   )
                 ),
                 fluidPage(
                   fluidRow(
                     h5(""),
                     column(
                       width = 6, 
                       class = "well",
                       plotOutput(
                         outputId = "each_distribution_plot"
                       )
                     )
                   )
                 )
               )
  )
)
