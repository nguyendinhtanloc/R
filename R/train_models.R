# Cài đặt và tải các thư viện cần thiết cho Machine Learning
ml_packages <- c(
  "caret", 
  "randomForest", 
  "gbm", 
  "e1071", 
  "tidyverse", 
  "ROCR", 
  "pROC"
)

# Tải các thư viện
invisible(lapply(ml_packages, library, character.only = TRUE))

# Tải dữ liệu
heart_data <- read_csv("data/heart.csv")

# Khám phá dữ liệu
str(heart_data)
summary(heart_data)

# Tiền xử lý dữ liệu
# Chuyển đổi biến mục tiêu thành factor
heart_data$target <- as.factor(heart_data$target)

# Chuyển đổi các biến phân loại thành factor
cat_vars <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
heart_data[cat_vars] <- lapply(heart_data[cat_vars], as.factor)

# Chia dữ liệu thành tập huấn luyện và tập kiểm tra
set.seed(123)
train_index <- createDataPartition(heart_data$target, p = 0.7, list = FALSE)
train_data <- heart_data[train_index, ]
test_data <- heart_data[-train_index, ]

# Xác nhận tỷ lệ các lớp trong cả hai tập
table(train_data$target)
table(test_data$target)

# Huấn luyện mô hình Random Forest
set.seed(123)
rf_model <- randomForest(
  target ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Huấn luyện mô hình GBM
set.seed(123)
# Tạo biến mục tiêu dạng số cho GBM
train_data$target_numeric <- as.numeric(as.character(train_data$target))

gbm_model <- gbm(
  target_numeric ~ age + sex + cp + trestbps + chol + fbs + restecg + 
    thalach + exang + oldpeak + slope + ca + thal,
  data = train_data,
  distribution = "bernoulli",
  n.trees = 500,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = 1,
  verbose = FALSE
)

# Tìm số lượng cây tối ưu cho GBM
best_iter <- gbm.perf(gbm_model, method = "cv")
print(paste0("Số lượng cây tối ưu cho GBM: ", best_iter))

# Huấn luyện mô hình Logistic Regression
# SỬA: Chỉ định rõ các biến trong công thức thay vì sử dụng "."
set.seed(123)
log_model <- glm(
  target ~ age + sex + cp + trestbps + chol + fbs + restecg + 
    thalach + exang + oldpeak + slope + ca + thal,
  data = train_data,
  family = "binomial",
  control = glm.control(maxit = 50)  # Tăng số lần lặp để giải quyết vấn đề hội tụ
)

# Đánh giá mô hình Random Forest
rf_pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
rf_pred_class <- predict(rf_model, newdata = test_data, type = "class")
rf_conf_matrix <- confusionMatrix(rf_pred_class, test_data$target)
rf_roc <- roc(as.numeric(as.character(test_data$target)), rf_pred)
rf_auc <- auc(rf_roc)

# Đánh giá mô hình GBM
# Thêm biến target_numeric vào test_data để tránh lỗi
test_data$target_numeric <- as.numeric(as.character(test_data$target))

gbm_pred <- predict(gbm_model, newdata = test_data, n.trees = best_iter, type = "response")
gbm_pred_class <- ifelse(gbm_pred > 0.5, 1, 0)
gbm_conf_matrix <- confusionMatrix(as.factor(gbm_pred_class), test_data$target)
gbm_roc <- roc(as.numeric(as.character(test_data$target)), gbm_pred)
gbm_auc <- auc(gbm_roc)

# Đánh giá mô hình Logistic Regression
log_pred <- predict(log_model, newdata = test_data, type = "response")
log_pred_class <- ifelse(log_pred > 0.5, 1, 0)
log_conf_matrix <- confusionMatrix(as.factor(log_pred_class), test_data$target)
log_roc <- roc(as.numeric(as.character(test_data$target)), log_pred)
log_auc <- auc(log_roc)

# In kết quả đánh giá
cat("\n### Đánh giá mô hình Random Forest ###\n")
print(rf_conf_matrix)
cat("AUC:", rf_auc, "\n\n")

cat("### Đánh giá mô hình GBM ###\n")
print(gbm_conf_matrix)
cat("AUC:", gbm_auc, "\n\n")

cat("### Đánh giá mô hình Logistic Regression ###\n")
print(log_conf_matrix)
cat("AUC:", log_auc, "\n\n")

# Vẽ biểu đồ ROC so sánh các mô hình
roc_plot <- ggroc(list(RF = rf_roc, GBM = gbm_roc, LogReg = log_roc)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "So sánh đường cong ROC của các mô hình",
    subtitle = paste0(
      "AUC - RF: ", round(rf_auc, 3), 
      ", GBM: ", round(gbm_auc, 3), 
      ", LogReg: ", round(log_auc, 3)
    )
  ) +
  theme_minimal()
print(roc_plot)

# In ra độ quan trọng của các đặc trưng từ mô hình Random Forest
rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(
  Feature = rownames(rf_importance),
  MeanDecreaseGini = rf_importance[, "MeanDecreaseGini"]
)
rf_importance_df <- rf_importance_df[order(rf_importance_df$MeanDecreaseGini, decreasing = TRUE), ]
print(rf_importance_df)

# Vẽ biểu đồ độ quan trọng của đặc trưng
ggplot(rf_importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Độ quan trọng của các đặc trưng (Random Forest)",
    x = "Đặc trưng",
    y = "Mean Decrease Gini"
  ) +
  theme_minimal()

# In ra độ quan trọng của các đặc trưng từ mô hình GBM
gbm_importance <- summary(gbm_model, plotit = FALSE)
print(gbm_importance)

# Lưu các mô hình
# Đảm bảo thư mục models tồn tại
if (!dir.exists("models")) dir.create("models")

saveRDS(rf_model, "models/rf_model.rds")
saveRDS(gbm_model, "models/gbm_model.rds")
saveRDS(log_model, "models/log_model.rds")

cat("Đã lưu các mô hình thành công!\n")