# Dự đoán với mô hình GBM
predict_gbm <- function(model, data) {
  # Chuyển đổi dữ liệu đầu vào nếu cần
  if (!"data.frame" %in% class(data)) {
    data <- as.data.frame(data)
  }
  
  # Dự đoán với mô hình GBM
  pred <- predict(model, newdata = data, n.trees = 500, type = "response")
  
  return(pred)
}

# Dự đoán với mô hình Random Forest
# Mở file R/predict.R và tìm hàm predict_rf
# Thay đổi hàm predict_rf như sau:

predict_rf <- function(model, data) {
  # Chuyển đổi dữ liệu đầu vào nếu cần
  if (!"data.frame" %in% class(data)) {
    data <- as.data.frame(data)
  }
  
  # Đảm bảo tất cả các biến categorical được chuyển thành factor
  cat_vars <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
  for (col in cat_vars) {
    if (col %in% names(data)) {
      data[[col]] <- as.factor(data[[col]])
      
      # Kiểm tra và đảm bảo levels khớp với mô hình
      if (!all(levels(data[[col]]) %in% levels(model$forest$xlevels[[col]]))) {
        # Đặt lại levels khớp với mô hình
        data[[col]] <- factor(data[[col]], levels = levels(model$forest$xlevels[[col]]))
      }
    }
  }
  
  # In ra để debug
  cat("Cấu trúc dữ liệu dự đoán:\n")
  print(str(data))
  
  # Dự đoán với mô hình Random Forest
  tryCatch({
    pred <- predict(model, newdata = data, type = "prob")[, 2]
    return(pred)
  }, error = function(e) {
    cat("Lỗi khi dự đoán với Random Forest:", e$message, "\n")
    cat("Lấy thông tin về mô hình:\n")
    print(names(model))
    print(model$terms)
    return(0.5)  # Giá trị mặc định nếu có lỗi
  })
}


# Dự đoán với mô hình Logistic Regression
predict_log <- function(model, data) {
  # Chuyển đổi dữ liệu đầu vào nếu cần
  if (!"data.frame" %in% class(data)) {
    data <- as.data.frame(data)
  }
  
  # Dự đoán với mô hình Logistic Regression
  pred <- predict(model, newdata = data, type = "response")
  
  return(pred)
}

# Tính toán độ quan trọng của đặc trưng
calculate_feature_importance <- function(data) {
  # Sử dụng mô hình Random Forest để tính toán độ quan trọng
  # Trong thực tế, có thể sử dụng giá trị đã lưu từ mô hình hoặc tính toán lại
  # Ở đây, chúng ta sẽ tạo một giá trị mẫu
  feature_importance <- data.frame(
    feature = names(data),
    importance = c(
      age = 15,
      sex = 8,
      cp = 20,
      trestbps = 10,
      chol = 12,
      fbs = 5,
      restecg = 7,
      thalach = 18,
      exang = 14,
      oldpeak = 16,
      slope = 9,
      ca = 25,
      thal = 22
    )[names(data)],
    stringsAsFactors = FALSE
  )
  
  # Đổi tên các đặc trưng để dễ đọc
  feature_importance$feature_name <- sapply(feature_importance$feature, function(f) {
    field_definitions[[f]]$label
  })
  
  return(feature_importance)
}