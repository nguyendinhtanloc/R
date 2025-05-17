message("========== Kiểm tra và cài đặt thư viện ==========\n")

# Danh sách tất cả các gói cần thiết
required_packages <- c(
  "shiny", "shinydashboard", "shinyjs", "plotly", "DT", "gbm", 
  "randomForest", "tidyverse", "caret", "pROC", "ggplot2", 
  "corrplot", "markdown", "shinyBS", "xfun", "dplyr", "tidyr", 
  "readr", "shinythemes", "lime", "e1071", "ROCR"
)

# Hàm cài đặt các gói còn thiếu
install_if_missing <- function(packages) {
  missing <- setdiff(packages, rownames(installed.packages()))
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org/")
    message("Đã cài đặt các gói: ", paste(missing, collapse = ", "))
  } else {
    message("Tất cả các gói đã được cài đặt.")
  }
}

# Cài đặt các gói còn thiếu
install_if_missing(required_packages)

# Tải tất cả các thư viện cần thiết
invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}))

# Nếu vẫn báo thiếu thư viện thì nên chạy lại thủ công
# Thư viện 1: shiny
# if (!require(shiny)) install.packages("shiny")
# library(shiny)

# Thư viện 2: shinydashboard
# if (!require(shinydashboard)) install.packages("shinydashboard")
# library(shinydashboard)

# Thư viện 3: shinyjs
# if (!require(shinyjs)) install.packages("shinyjs")
# library(shinyjs)

# Thư viện 4: plotly
# if (!require(plotly)) install.packages("plotly")
# library(plotly)

# Thư viện 5: DT
# if (!require(DT)) install.packages("DT")
# library(DT)

# Thư viện 6: gbm
# if (!require(gbm)) install.packages("gbm")
# library(gbm)

# Thư viện 7: randomForest
# if (!require(randomForest)) install.packages("randomForest")
# library(randomForest)

# Thư viện 8: tidyverse
# if (!require(tidyverse)) install.packages("tidyverse")
# library(tidyverse)

# Thư viện 9: caret
# if (!require(caret)) install.packages("caret")
# library(caret)

# Thư viện 10: pROC
# if (!require(pROC)) install.packages("pROC")
# library(pROC)

# Thư viện 11: ggplot2
# if (!require(ggplot2)) install.packages("ggplot2")
# library(ggplot2)

# Thư viện 12: corrplot
# if (!require(corrplot)) install.packages("corrplot")
# library(corrplot)

# Thư viện 13: markdown
# if (!require(markdown)) install.packages("markdown")
# library(markdown)

# Thư viện 14: shinyBS
# if (!require(shinyBS)) install.packages("shinyBS")
# library(shinyBS)

# Thư viện 15: xfun
# if (!require(xfun)) install.packages("xfun")
# library(xfun)

# Thư viện 16: dplyr
# if (!require(dplyr)) install.packages("dplyr")
# library(dplyr)

# Thư viện 17: tidyr
# if (!require(tidyr)) install.packages("tidyr")
# library(tidyr)

# Thư viện 18: readr
# if (!require(readr)) install.packages("readr")
# library(readr)

# Thư viện 19: shinythemes
# if (!require(shinythemes)) install.packages("shinythemes")
# library(shinythemes)

# Thư viện 20: lime
# if (!require(lime)) install.packages("lime")
# library(lime)

# Thư viện 21: e1071
# if (!require(e1071)) install.packages("e1071")
# library(e1071)

# Thư viện 22: ROCR
# if (!require(ROCR)) install.packages("ROCR")
# library(ROCR)

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)