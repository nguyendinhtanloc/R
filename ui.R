ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Dự đoán Bệnh Tim",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Trực quan hóa dữ liệu", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Nhập liệu", tabName = "input", icon = icon("user-md")),
      menuItem("Kết quả", tabName = "result", icon = icon("chart-line")),
      menuItem("Giải thích", tabName = "explain", icon = icon("info-circle")),
      menuItem("Thông tin", tabName = "info", icon = icon("book"))
    )
  ),
  
  # Body
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      # Trực quan hóa dữ liệu
      tabItem(
        tabName = "visualization",
        h2("Trang Trực quan hóa Dữ liệu"),
        
        # Biểu đồ 3.2: Phân bố độ tuổi theo giới tính
        fluidRow(
          shinydashboard::box(
            title = "Phân bố Độ Tuổi theo Giới Tính",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("ageDistributionPlot", height = "400px")
          ),
          shinydashboard::box(
            title = "Phân tích Biểu đồ Độ Tuổi",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            htmlOutput("ageDistributionAnalysis")
          )
        ),
        
        # Biểu đồ 3.3: Cholesterol vs Huyết áp nghỉ theo bệnh tim
        fluidRow(
          shinydashboard::box(
            title = "Cholesterol vs Huyết Áp Nghỉ theo Bệnh Tim",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("cholVsTrestbpsPlot", height = "400px")
          ),
          shinydashboard::box(
            title = "Phân tích Biểu đồ Cholesterol vs Huyết Áp",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            htmlOutput("cholVsTrestbpsAnalysis")
          )
        ),
        
        # Biểu đồ 3.4: Boxplot độ giãn ST theo đau thắt ngực
        fluidRow(
          shinydashboard::box(
            title = "Độ Giãn ST theo Đau Thắt Ngực do Gắng Sức",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("oldpeakVsExangPlot", height = "400px")
          ),
          shinydashboard::box(
            title = "Phân tích Biểu đồ Độ Giãn ST",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            htmlOutput("oldpeakVsExangAnalysis")
          )
        ),
        
        # Biểu đồ 3.5: Số mạch máu bị hẹp theo bệnh tim
        fluidRow(
          shinydashboard::box(
            title = "Số Mạch Máu Bị Hẹp theo Bệnh Tim",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("caVsTargetPlot", height = "400px")
          ),
          shinydashboard::box(
            title = "Phân tích Biểu đồ Số Mạch Máu Bị Hẹp",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            htmlOutput("caVsTargetAnalysis")
          )
        ),
        
        # Biểu đồ 3.7: Phân bố theo loại đau ngực
        fluidRow(
          shinydashboard::box(
            title = "Phân bố Bệnh Nhân theo Loại Đau Ngực",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("cpVsTargetPlot", height = "400px")
          ),
          shinydashboard::box(
            title = "Phân tích Biểu đồ Loại Đau Ngực",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            htmlOutput("cpVsTargetAnalysis")
          )
        ),
        
        # Biểu đồ 3.8: Kết quả kiểm tra Thalium theo bệnh tim
        fluidRow(
          shinydashboard::box(
            title = "Kết quả Kiểm tra Thalium theo Bệnh Tim",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("thalVsTargetPlot", height = "400px")
          ),
          shinydashboard::box(
            title = "Phân tích Biểu đồ Kết quả Thalium",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            htmlOutput("thalVsTargetAnalysis")
          )
        ),
        
        # Biểu đồ 3.9: Nhịp tim tối đa vs Độ giãn ST
        fluidRow(
          shinydashboard::box(
            title = "Nhịp Tim Tối Đa vs Độ Giãn ST",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("thalachVsOldpeakPlot", height = "400px")
          ),
          shinydashboard::box(
            title = "Phân tích Biểu đồ Nhịp Tim vs Độ Giãn ST",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            htmlOutput("thalachVsOldpeakAnalysis")
          )
        ),
        
        # Bảng dữ liệu chi tiết
        fluidRow(
          shinydashboard::box(
            title = "Dữ liệu Chi tiết",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("dataTableVisualization")
          )
        ),
      ),
      
      # Tab nhập liệu
      tabItem(
        tabName = "input",
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Thông tin bệnh nhân",
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 6,
                numericInput("age", field_definitions$age$label, 
                             field_definitions$age$default,
                             field_definitions$age$min, 
                             field_definitions$age$max),
                radioButtons("sex", field_definitions$sex$label, 
                             field_definitions$sex$choices,
                             field_definitions$sex$default),
                selectInput("cp", field_definitions$cp$label, 
                            field_definitions$cp$choices,
                            field_definitions$cp$default),
                numericInput("trestbps", field_definitions$trestbps$label, 
                             field_definitions$trestbps$default,
                             field_definitions$trestbps$min, 
                             field_definitions$trestbps$max),
                numericInput("chol", field_definitions$chol$label, 
                             field_definitions$chol$default,
                             field_definitions$chol$min, 
                             field_definitions$chol$max),
                radioButtons("fbs", field_definitions$fbs$label, 
                             field_definitions$fbs$choices,
                             field_definitions$fbs$default)
              ),
              column(
                width = 6,
                selectInput("restecg", field_definitions$restecg$label, 
                            field_definitions$restecg$choices,
                            field_definitions$restecg$default),
                numericInput("thalach", field_definitions$thalach$label, 
                             field_definitions$thalach$default,
                             field_definitions$thalach$min, 
                             field_definitions$thalach$max),
                radioButtons("exang", field_definitions$exang$label, 
                             field_definitions$exang$choices,
                             field_definitions$exang$default),
                numericInput("oldpeak", field_definitions$oldpeak$label, 
                             field_definitions$oldpeak$default,
                             field_definitions$oldpeak$min, 
                             field_definitions$oldpeak$max,
                             field_definitions$oldpeak$step),
                selectInput("slope", field_definitions$slope$label, 
                            field_definitions$slope$choices,
                            field_definitions$slope$default),
                selectInput("ca", field_definitions$ca$label, 
                            field_definitions$ca$choices,
                            field_definitions$ca$default),
                selectInput("thal", field_definitions$thal$label, 
                            field_definitions$thal$choices,
                            field_definitions$thal$default)
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                actionButton("predict_btn", "Dự đoán", 
                             class = "btn-lg btn-success",
                             icon = icon("stethoscope")),
                actionButton("reset_btn", "Làm mới", 
                             class = "btn-lg",
                             icon = icon("sync"))
              )
            )
          )
        ),
        
        # Khu vực hiển thị thông tin trợ giúp
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Trợ giúp",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            
            # Hiển thị lịch sử hội thoại
            htmlOutput("chat_history"),
            
            # Ngăn cách giữa hội thoại và nhập liệu
            tags$hr(),
            
            # Nhập câu hỏi từ người dùng
            textInput(
              inputId = "user_question",
              label = "Nhập câu hỏi của bạn:",
              placeholder = "Ví dụ: thal là gì ?"
            ),
            
            # Nút gửi câu hỏi
            actionButton(
              inputId = "ask_button",
              label = "Gửi",
              icon = icon("paper-plane"),
              class = "btn-primary"
            )
          )
        )
      ),
      
      # Tab kết quả
      tabItem(
        tabName = "result",
        fluidRow(
          valueBoxOutput("risk_box", width = 12)
        ),
        
        fluidRow(
          box(
            width = 6,
            title = "Kết quả dự đoán",
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput("prediction_gauge", height = "300px"),
            
            h4("Dự đoán từ các mô hình:"),
            tableOutput("model_predictions")
          ),
          
          shinydashboard::box(
            width = 6,
            title = "Các yếu tố nguy cơ chính",
            status = "warning",
            solidHeader = TRUE,
            
            plotlyOutput("risk_factors", height = "300px"),
            htmlOutput("risk_text")
          )
        ),
        
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Đề xuất tiếp theo",
            status = "success",
            solidHeader = TRUE,
            
            htmlOutput("recommendations")
          )
        )
      ),
      
      # Tab giải thích
      tabItem(
        tabName = "explain",
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Giải thích kết quả",
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput("feature_importance", height = "400px"),
            htmlOutput("explanation_text")
          )
        ),
        
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "So sánh với dân số",
            status = "info",
            solidHeader = TRUE,
            
            plotlyOutput("population_comparison", height = "400px")
          )
        )
      ),
      
      # Tab thông tin
      tabItem(
        tabName = "info",
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Thông tin về ứng dụng",
            status = "primary",
            solidHeader = TRUE,
            
            includeMarkdown("www/about.md")
          )
        ),
        
        fluidRow(
          shinydashboard::box(
            width = 6,
            title = "Cách sử dụng ứng dụng",
            status = "info",
            solidHeader = TRUE,
            
            includeMarkdown("www/usage.md")
          ),
          
          shinydashboard::box(
            width = 6,
            title = "Thông tin về bệnh tim",
            status = "warning",
            solidHeader = TRUE,
            
            includeMarkdown("www/heart_disease_info.md")
          )
        )
      )
    )
  )
)
