ui_minimal <- sidebarLayout(
  sidebarPanel(width = 3, 
               div(img(src = "mexca_logo_minimal.png",
                       width = "70px", height = "70px"), style="text-align: center;"),
               fileInput('your_csv',"Data", accept = c('.csv'), buttonLabel = 'Upload csv...'),
               fileInput('your_mp4',"", accept = c('.mp4'), buttonLabel = 'Upload mp4...'),
               uiOutput("slider"),
               downloadButton('download_tsv', 'CSV', class = 'btn-block'),
               downloadButton('download_mp4', 'MP4', class = 'btn-block')
  ), mainPanel(
    h3(textOutput('plot_name')),
    column(6, 
           plotOutput('frame_plot'),
           wellPanel(
             span(textOutput('text_output'),
                  style='color:black'
             )
           ),
    ),
    column(6, 
           plotOutput('au')
    )
    
  )
)

ui_display_data <- fluidRow(
  column(10, align="left", offset = 1,
    h3('Data'),
    DT::dataTableOutput('preview')
    
  )
)

