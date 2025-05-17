message("========== Kiểm tra và cài đặt thư viện ==========\n")

# Danh sách tất cả các gói cần thiết
required_packages <- c(
  "shiny", "shinydashboard"
)

# Cài đặt gói chưa có
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
  message("Đã cài đặt các gói: ", paste(missing_packages, collapse = ", "))
} else {
  message("Tất cả các thư viện đã được cài đặt.")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# UI tối giản
ui <- dashboardPage(
  dashboardHeader(title = "Debug App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Debug", tabName = "debug")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "debug",
        fluidRow(
          # Box 1 - width chuẩn
          box(
            title = "Box 1",
            width = 6,  # Hợp lệ
            "Nội dung box 1"
          ),
          
          # Box 2 - width chuẩn
          box(
            title = "Box 2",
            width = 6,  # Hợp lệ
            "Nội dung box 2"
          )
        ),
        
        # In ra thông tin debug
        verbatimTextOutput("debug_info")
      )
    )
  )
)

# Server tối giản
server <- function(input, output, session) {
  output$debug_info <- renderPrint({
    cat("App khởi động thành công!\n")
    cat("Kiểm tra các file cấu hình:\n")
    
    # Kiểm tra ui.R
    if (file.exists("ui.R")) {
      ui_content <- readLines("ui.R")
      box_lines <- grep("box\\(", ui_content)
      cat("Số lượng hàm box() trong ui.R:", length(box_lines), "\n")
      
      for (i in box_lines) {
        width_found <- FALSE
        # Tìm tham số width trong 5 dòng tiếp theo
        for (j in i:(min(i+5, length(ui_content)))) {
          if (grepl("width\\s*=", ui_content[j])) {
            width_found <- TRUE
            cat("  Line", j, ":", trimws(ui_content[j]), "\n")
          }
        }
        if (!width_found) {
          cat("  Line", i, ":", trimws(ui_content[i]), " - KHÔNG TÌM THẤY WIDTH!\n")
        }
      }
    } else {
      cat("Không tìm thấy file ui.R\n")
    }
  })
}

# Chạy ứng dụng
shinyApp(ui, server)
