# Tiền xử lý dữ liệu đầu vào
preprocess_data <- function(data) {
  # Đảm bảo dữ liệu là data frame
  if (!"data.frame" %in% class(data)) {
    data <- as.data.frame(data)
  }
  
  # Chuyển đổi các kiểu dữ liệu
  numeric_cols <- c("age", "trestbps", "chol", "thalach", "oldpeak")
  for (col in numeric_cols) {
    if (col %in% names(data)) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }
  
  factor_cols <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
  for (col in factor_cols) {
    if (col %in% names(data)) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  
  # Xử lý giá trị NA nếu có
  for (col in names(data)) {
    if (sum(is.na(data[[col]])) > 0) {
      if (col %in% numeric_cols) {
        # Thay thế bằng giá trị trung bình cho biến số
        data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
      } else {
        # Thay thế bằng giá trị phổ biến nhất cho biến phân loại
        data[[col]][is.na(data[[col]])] <- names(sort(table(data[[col]]), decreasing = TRUE)[1])
      }
    }
  }
  
  # Chuẩn hóa dữ liệu số nếu cần
  # for (col in numeric_cols) {
  #   if (col %in% names(data)) {
  #     data[[col]] <- scale(data[[col]])
  #   }
  # }
  
  return(data)
}

# Phân tích văn bản đầu vào của người dùng
analyze_text_input <- function(text) {
  # Trong tương lai có thể mở rộng với NLP để phân tích văn bản
  # Hiện tại chúng ta sẽ sử dụng phương pháp đơn giản với từ khóa
  
  # Danh sách từ khóa cho các trường dữ liệu
  keywords <- list(
    age = c("tuổi", "tuoi", "năm", "nam"),
    sex = c("nam", "nữ", "nu", "giới tính", "gioi tinh"),
    cp = c("đau ngực", "dau nguc", "thắt ngực", "that nguc", "tức ngực", "tuc nguc"),
    trestbps = c("huyết áp", "huyet ap", "HA", "cao huyết áp", "cao huyet ap", "tăng huyết áp", "tang huyet ap"),
    chol = c("cholesterol", "mỡ máu", "mo mau", "lipid"),
    fbs = c("đường", "duong", "glucose", "tiểu đường", "tieu duong", "đái tháo đường", "dai thao duong"),
    restecg = c("điện tâm đồ", "dien tam do", "ECG", "EKG"),
    thalach = c("nhịp tim", "nhip tim", "mạch", "mach"),
    exang = c("đau khi gắng sức", "dau khi gang suc", "đau khi vận động", "dau khi van dong")
  )
  
  # Chuyển văn bản thành chữ thường và loại bỏ dấu
  text_lower <- tolower(text)
  
  # Kết quả phân tích
  result <- list()
  
  # Tìm kiếm các từ khóa trong văn bản
  for (field in names(keywords)) {
    for (keyword in keywords[[field]]) {
      if (grepl(keyword, text_lower)) {
        # Tìm giá trị số gần từ khóa
        pattern <- paste0("\\b(\\d+)\\s*(?:", keyword, "|", paste0(keyword, "\\s*là\\s*(\\d+)"), ")")
        matches <- gregexpr(pattern, text_lower, perl = TRUE)
        
        if (length(matches) > 0 && matches[[1]][1] != -1) {
          # Trích xuất giá trị số
          start_pos <- matches[[1]][1]
          match_length <- attr(matches[[1]], "match.length")[1]
          value_str <- substr(text_lower, start_pos, start_pos + match_length - 1)
          
          # Lấy chỉ phần số
          value <- as.numeric(gsub("\\D", "", value_str))
          
          result[[field]] <- value
        } else {
          # Nếu là trường binary như giới tính
          if (field == "sex") {
            if (grepl("nam", text_lower)) {
              result[[field]] <- 1
            } else if (grepl("nữ|nu", text_lower)) {
              result[[field]] <- 0
            }
          }
        }
      }
    }
  }
  
  return(result)
}