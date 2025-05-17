required_packages <- c(
  "shiny", "shinydashboard", "shinyjs", "gbm", "randomForest", "caret",
  "DT", "plotly", "ggplot2", "dplyr", "tidyr", "readr",
  "shinythemes", "lime", "shinyBS", "corrplot"
)

invisible(lapply(required_packages, library, character.only = TRUE))

source("R/predict.R")
source("R/preprocess.R")
source("R/ui_helpers.R")
source("R/explanation.R")

heart_data <- read_csv("data/heart.csv")

gbm_model <- readRDS("models/gbm_model.rds")
rf_model  <- readRDS("models/rf_model.rds")
log_model <- readRDS("models/log_model.rds")

# Định nghĩa các biến toàn cục
field_definitions <- list(
  age = list(
    label = "Tuổi",
    min = 20,
    max = 100,
    default = 45,
    description = "Tuổi của bệnh nhân (năm)"
  ),
  sex = list(
    label = "Giới tính",
    choices = c("Nam" = 1, "Nữ" = 0),
    default = 1,
    description = "Giới tính của bệnh nhân"
  ),
  cp = list(
    label = "Loại đau ngực",
    choices = c(
      "Đau thắt ngực điển hình" = 0,
      "Đau thắt ngực không điển hình" = 1,
      "Đau không do tim" = 2,
      "Không triệu chứng" = 3
    ),
    default = 0,
    description = "Loại đau ngực mà bệnh nhân đang gặp phải"
  ),
  trestbps = list(
    label = "Huyết áp tâm thu khi nghỉ",
    min = 90,
    max = 200,
    default = 120,
    description = "Huyết áp tâm thu khi nghỉ (mm Hg)"
  ),
  chol = list(
    label = "Cholesterol huyết thanh",
    min = 100,
    max = 600,
    default = 200,
    description = "Cholesterol huyết thanh (mg/dl)"
  ),
  fbs = list(
    label = "Đường huyết lúc đói > 120 mg/dl",
    choices = c("Có" = 1, "Không" = 0),
    default = 0,
    description = "Đường huyết lúc đói có lớn hơn 120 mg/dl không"
  ),
  restecg = list(
    label = "Kết quả điện tâm đồ khi nghỉ",
    choices = c(
      "Bình thường" = 0,
      "Bất thường sóng ST-T" = 1,
      "Phì đại thất trái" = 2
    ),
    default = 0,
    description = "Kết quả điện tâm đồ trong trạng thái nghỉ"
  ),
  thalach = list(
    label = "Nhịp tim tối đa",
    min = 60,
    max = 220,
    default = 150,
    description = "Nhịp tim tối đa đạt được"
  ),
  exang = list(
    label = "Đau thắt ngực khi gắng sức",
    choices = c("Có" = 1, "Không" = 0),
    default = 0,
    description = "Có đau thắt ngực khi gắng sức không"
  ),
  oldpeak = list(
    label = "ST depression khi gắng sức",
    min = 0,
    max = 10,
    step = 0.1,
    default = 0,
    description = "Độ suy giảm ST khi gắng sức so với nghỉ ngơi"
  ),
  slope = list(
    label = "Độ dốc đoạn ST",
    choices = c(
      "Đi lên" = 0,
      "Phẳng" = 1,
      "Đi xuống" = 2
    ),
    default = 1,
    description = "Độ dốc của đoạn ST khi tập luyện"
  ),
  ca = list(
    label = "Số lượng mạch chính",
    choices = c("0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4),
    default = 0,
    description = "Số lượng mạch chính (0-4) được nhuộm màu bởi fluoroscopy"
  ),
  thal = list(
    label = "Thalassemia",
    choices = c(
      "Bình thường" = 1,
      "Khiếm khuyết cố định" = 2,
      "Khiếm khuyết có thể đảo ngược" = 3
    ),
    default = 1,
    description = "Tình trạng thalassemia (loại bệnh về máu)"
  )
)