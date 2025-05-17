# Tạo giải thích cho các yếu tố nguy cơ
generate_risk_explanations <- function(top_factors, input_data) {
  explanations <- character(nrow(top_factors))
  
  for (i in 1:nrow(top_factors)) {
    feature <- top_factors$feature[i]
    value <- input_data[[feature]]
    
    # Giải thích dựa trên từng đặc trưng
    switch(feature,
           "age" = {
             if (value > 60) {
               explanations[i] <- paste0("<strong>Tuổi (", value, " tuổi):</strong> Tuổi cao là một yếu tố nguy cơ quan trọng đối với bệnh tim. Nguy cơ tăng đáng kể sau 55 tuổi.")
             } else {
               explanations[i] <- paste0("<strong>Tuổi (", value, " tuổi):</strong> Mặc dù bạn chưa cao tuổi, nhưng tuổi vẫn là yếu tố nguy cơ cần theo dõi.")
             }
           },
           "sex" = {
             if (value == 1) {
               explanations[i] <- "<strong>Giới tính (Nam):</strong> Nam giới có nguy cơ mắc bệnh tim cao hơn nữ giới, đặc biệt ở độ tuổi trước 55."
             } else {
               explanations[i] <- "<strong>Giới tính (Nữ):</strong> Nữ giới thường có nguy cơ thấp hơn nam giới ở độ tuổi trước mãn kinh, nhưng nguy cơ tăng sau mãn kinh."
             }
           },
           "cp" = {
             cp_types <- c("Đau thắt ngực điển hình", "Đau thắt ngực không điển hình", "Đau không do tim", "Không triệu chứng")
             explanations[i] <- paste0("<strong>Loại đau ngực (", cp_types[as.numeric(value) + 1], "):</strong> Kiểu đau ngực là một chỉ báo quan trọng về tình trạng tim mạch.")
           },
           "trestbps" = {
             if (value >= 140) {
               explanations[i] <- paste0("<strong>Huyết áp tâm thu (", value, " mmHg):</strong> Huyết áp của bạn cao hơn mức bình thường (120 mmHg). Cao huyết áp làm tăng nguy cơ bệnh tim.")
             } else if (value >= 120 && value < 140) {
               explanations[i] <- paste0("<strong>Huyết áp tâm thu (", value, " mmHg):</strong> Huyết áp của bạn ở mức tiền cao huyết áp. Theo dõi và kiểm soát là cần thiết.")
             } else {
               explanations[i] <- paste0("<strong>Huyết áp tâm thu (", value, " mmHg):</strong> Huyết áp của bạn ở mức bình thường.")
             }
           },
           "chol" = {
             if (value >= 240) {
               explanations[i] <- paste0("<strong>Cholesterol (", value, " mg/dl):</strong> Cholesterol của bạn cao (>= 240 mg/dl). Cholesterol cao làm tăng nguy cơ bệnh tim mạch.")
             } else if (value >= 200 && value < 240) {
               explanations[i] <- paste0("<strong>Cholesterol (", value, " mg/dl):</strong> Cholesterol của bạn ở mức giới hạn cao (200-239 mg/dl). Cần theo dõi và kiểm soát.")
             } else {
               explanations[i] <- paste0("<strong>Cholesterol (", value, " mg/dl):</strong> Cholesterol của bạn ở mức bình thường (<200 mg/dl).")
             }
           },
           "fbs" = {
             if (value == 1) {
               explanations[i] <- "<strong>Đường huyết lúc đói:</strong> Đường huyết lúc đói của bạn cao (>120 mg/dl), tăng nguy cơ bệnh tim mạch."
             } else {
               explanations[i] <- "<strong>Đường huyết lúc đói:</strong> Đường huyết lúc đói của bạn ở mức bình thường."
             }
           },
           "thalach" = {
             max_hr_by_age <- 220 - input_data$age
             if (value < (max_hr_by_age * 0.7)) {
               explanations[i] <- paste0("<strong>Nhịp tim tối đa (", value, " bpm):</strong> Nhịp tim tối đa của bạn thấp hơn dự kiến theo độ tuổi. Điều này có thể là dấu hiệu của vấn đề tim mạch.")
             } else {
               explanations[i] <- paste0("<strong>Nhịp tim tối đa (", value, " bpm):</strong> Nhịp tim tối đa của bạn nằm trong phạm vi bình thường cho độ tuổi của bạn.")
             }
           },
           "exang" = {
             if (value == 1) {
               explanations[i] <- "<strong>Đau thắt ngực khi gắng sức:</strong> Bạn có đau thắt ngực khi gắng sức, đây là một dấu hiệu quan trọng của bệnh động mạch vành."
             } else {
               explanations[i] <- "<strong>Đau thắt ngực khi gắng sức:</strong> Bạn không có đau thắt ngực khi gắng sức, đây là một dấu hiệu tốt."
             }
           },
           "oldpeak" = {
             if (value >= 2) {
               explanations[i] <- paste0("<strong>ST depression (", value, "):</strong> Mức độ suy giảm ST cao khi gắng sức, là dấu hiệu của thiếu máu cơ tim.")
             } else {
               explanations[i] <- paste0("<strong>ST depression (", value, "):</strong> Mức độ suy giảm ST khi gắng sức ở mức trung bình hoặc thấp.")
             }
           },
           "ca" = {
             if (as.numeric(value) > 0) {
               explanations[i] <- paste0("<strong>Số lượng mạch chính (", value, "):</strong> Bạn có ", value, " mạch chính bị tổn thương. Số lượng mạch bị tổn thương tỷ lệ thuận với mức độ nghiêm trọng của bệnh.")
             } else {
               explanations[i] <- "<strong>Số lượng mạch chính (0):</strong> Không phát hiện mạch chính bị tổn thương, đây là dấu hiệu tốt."
             }
           },
           "thal" = {
             thal_types <- c("", "Bình thường", "Khiếm khuyết cố định", "Khiếm khuyết có thể đảo ngược")
             explanations[i] <- paste0("<strong>Thalassemia (", thal_types[as.numeric(value) + 1], "):</strong> Kết quả xét nghiệm thallium cho thấy ", tolower(thal_types[as.numeric(value) + 1]), ".")
           },
           # Mặc định nếu không có giải thích cụ thể
           {
             explanations[i] <- paste0("<strong>", field_definitions[[feature]]$label, " (", value, "):</strong> Đây là một yếu tố ảnh hưởng đến dự đoán của mô hình.")
           }
    )
  }
  
  return(explanations)
}

# Tạo đề xuất dựa trên kết quả dự đoán
generate_recommendations <- function(risk_level, input_data) {
  recommendations <- "<h4>Đề xuất:</h4><ul>"
  
  # Đề xuất chung dựa trên mức độ nguy cơ
  if (risk_level > 0.7) {
    recommendations <- paste0(recommendations, 
                              "<li><strong>Tham vấn bác sĩ tim mạch ngay:</strong> Nguy cơ cao mắc bệnh tim. Cần đánh giá chi tiết bởi chuyên gia.</li>",
                              "<li><strong>Xét nghiệm bổ sung:</strong> Cân nhắc làm các xét nghiệm chẩn đoán hình ảnh như siêu âm tim, chụp mạch vành.</li>"
    )
  } else if (risk_level > 0.3) {
    recommendations <- paste0(recommendations, 
                              "<li><strong>Theo dõi với bác sĩ:</strong> Nguy cơ trung bình mắc bệnh tim. Nên đặt lịch khám với bác sĩ tim mạch.</li>",
                              "<li><strong>Xét nghiệm định kỳ:</strong> Cân nhắc kiểm tra huyết áp, cholesterol và đường huyết thường xuyên.</li>"
    )
  } else {
    recommendations <- paste0(recommendations, 
                              "<li><strong>Tiếp tục theo dõi:</strong> Nguy cơ thấp mắc bệnh tim. Duy trì kiểm tra sức khỏe định kỳ.</li>"
    )
  }
  
  # Đề xuất cụ thể dựa trên các yếu tố nguy cơ
  
  # Kiểm tra tuổi
  if (input_data$age > 60) {
    recommendations <- paste0(recommendations, 
                              "<li><strong>Kiểm tra sức khỏe thường xuyên hơn:</strong> Ở độ tuổi của bạn, nên kiểm tra sức khỏe tim mạch ít nhất mỗi 6 tháng.</li>"
    )
  }
  
  # Kiểm tra huyết áp
  if (input_data$trestbps >= 140) {
    recommendations <- paste0(recommendations, 
                              "<li><strong>Kiểm soát huyết áp:</strong> Huyết áp của bạn cao hơn mức bình thường. Cân nhắc thay đổi chế độ ăn uống, tập thể dục và có thể cần dùng thuốc theo chỉ định của bác sĩ.</li>"
    )
  }
  
  # Kiểm tra cholesterol
  if (input_data$chol >= 200) {
    recommendations <- paste0(recommendations, 
                              "<li><strong>Giảm cholesterol:</strong> Cholesterol của bạn cao. Nên giảm thực phẩm giàu chất béo bão hòa, tăng cường rau xanh và cân nhắc dùng thuốc nếu được chỉ định.</li>"
    )
  }
  
  # Kiểm tra đường huyết
  if (input_data$fbs == 1) {
    recommendations <- paste0(recommendations, 
                              "<li><strong>Kiểm soát đường huyết:</strong> Đường huyết lúc đói của bạn cao. Cần kiểm soát chặt chẽ, giảm thực phẩm giàu đường và tinh bột, tăng hoạt động thể chất.</li>"
    )
  }
  
  # Kiểm tra đau thắt ngực
  if (input_data$cp == 0 || input_data$cp == 1 || input_data$exang == 1) {
    recommendations <- paste0(recommendations, 
                              "<li><strong>Đánh giá triệu chứng đau ngực:</strong> Bạn có triệu chứng đau ngực. Cần được đánh giá chi tiết bởi bác sĩ chuyên khoa tim mạch.</li>"
    )
  }
  
  # Đề xuất về lối sống
  recommendations <- paste0(recommendations, 
                            "<li><strong>Lối sống lành mạnh:</strong> Tập thể dục đều đặn (150 phút/tuần), ăn uống cân bằng, hạn chế muối và chất béo bão hòa, không hút thuốc, hạn chế rượu bia.</li>"
  )
  
  recommendations <- paste0(recommendations, "</ul>")
  
  return(recommendations)
}

# Tạo giải thích chi tiết
generate_detailed_explanation <- function(input_data, risk_level, feature_importance) {
  explanation <- "<h3>Phân tích chi tiết kết quả dự đoán</h3>"
  
  # Giải thích mức độ nguy cơ
  if (risk_level > 0.7) {
    explanation <- paste0(explanation, 
                          "<p>Dự đoán cho thấy bạn có <strong>nguy cơ cao</strong> mắc bệnh tim với xác suất ", 
                          round(risk_level * 100, 1), 
                          "%. Kết quả này dựa trên sự kết hợp của các yếu tố nguy cơ đã được xác định trong nghiên cứu y khoa.</p>"
    )
  } else if (risk_level > 0.3) {
    explanation <- paste0(explanation, 
                          "<p>Dự đoán cho thấy bạn có <strong>nguy cơ trung bình</strong> mắc bệnh tim với xác suất ", 
                          round(risk_level * 100, 1), 
                          "%. Điều này có nghĩa là bạn có một số yếu tố nguy cơ cần được theo dõi.</p>"
    )
  } else {
    explanation <- paste0(explanation, 
                          "<p>Dự đoán cho thấy bạn có <strong>nguy cơ thấp</strong> mắc bệnh tim với xác suất ", 
                          round(risk_level * 100, 1), 
                          "%. Mặc dù vậy, việc duy trì lối sống lành mạnh vẫn rất quan trọng để giữ nguy cơ ở mức thấp.</p>"
    )
  }
  
  # Giải thích các yếu tố nguy cơ chính
  explanation <- paste0(explanation, "<h4>Các yếu tố ảnh hưởng đến kết quả dự đoán:</h4><ul>")
  
  # Lấy top 5 yếu tố quan trọng nhất
  top_factors <- head(feature_importance[order(feature_importance$importance, decreasing = TRUE), ], 5)
  
  for (i in 1:nrow(top_factors)) {
    feature <- top_factors$feature[i]
    value <- input_data[[feature]]
    
    # Giải thích chi tiết về từng yếu tố và tác động của nó
    switch(feature,
           "age" = {
             explanation <- paste0(explanation, 
                                   "<li><strong>Tuổi (", value, " tuổi):</strong> Nguy cơ mắc bệnh tim tăng theo tuổi, đặc biệt sau 55 tuổi đối với nam và 65 tuổi đối với nữ. ",
                                   "Tuổi ảnh hưởng đến quá trình lão hóa tự nhiên của hệ thống tim mạch, bao gồm cả sự cứng lại của mạch máu và tích tụ mảng bám.</li>"
             )
           },
           "sex" = {
             if (value == 1) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Giới tính (Nam):</strong> Nam giới thường có nguy cơ mắc bệnh tim cao hơn nữ giới ở cùng độ tuổi, đặc biệt trước 55 tuổi. ",
                                     "Điều này một phần do sự bảo vệ của hormone estrogen ở nữ giới trước mãn kinh.</li>"
               )
             } else {
               explanation <- paste0(explanation, 
                                     "<li><strong>Giới tính (Nữ):</strong> Trước mãn kinh, nữ giới thường có nguy cơ thấp hơn nam giới nhờ tác dụng bảo vệ của estrogen. ",
                                     "Tuy nhiên, sau mãn kinh, nguy cơ tăng lên và gần bằng nam giới ở cùng độ tuổi.</li>"
               )
             }
           },
           "cp" = {
             cp_types <- c("Đau thắt ngực điển hình", "Đau thắt ngực không điển hình", "Đau không do tim", "Không triệu chứng")
             cp_explanations <- c(
               "Đau thắt ngực điển hình thường là dấu hiệu của bệnh động mạch vành nghiêm trọng. Đặc điểm là cảm giác nặng, tức hoặc bóp nghẹt ở ngực, thường xuất hiện khi gắng sức và giảm khi nghỉ ngơi.",
               "Đau thắt ngực không điển hình vẫn có thể liên quan đến bệnh tim nhưng không đáp ứng đầy đủ tiêu chuẩn của đau thắt ngực điển hình. Triệu chứng có thể không rõ ràng hoặc xuất hiện không theo quy luật.",
               "Đau không do tim thường không liên quan đến bệnh tim mạch, có thể do nguyên nhân khác như vấn đề về cơ xương khớp, tiêu hóa hoặc lo âu.",
               "Không có triệu chứng đau ngực, tuy nhiên điều này không loại trừ khả năng bệnh tim. Nhiều bệnh nhân mắc bệnh động mạch vành không có triệu chứng đau ngực."
             )
             explanation <- paste0(explanation, 
                                   "<li><strong>Loại đau ngực (", cp_types[as.numeric(value) + 1], "):</strong> ", 
                                   cp_explanations[as.numeric(value) + 1], "</li>"
             )
           },
           "trestbps" = {
             if (value >= 140) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Huyết áp tâm thu (", value, " mmHg):</strong> Huyết áp của bạn cao hơn mức bình thường (120 mmHg). ",
                                     "Cao huyết áp làm tăng áp lực lên tim và mạch máu, dẫn đến tổn thương mạch máu, tăng nguy cơ xơ vữa động mạch, đau tim và đột quỵ. Mỗi tăng 20 mmHg huyết áp tâm thu làm tăng gấp đôi nguy cơ tử vong do bệnh tim mạch.</li>"
               )
             } else if (value >= 120 && value < 140) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Huyết áp tâm thu (", value, " mmHg):</strong> Huyết áp của bạn ở mức tiền cao huyết áp. ",
                                     "Đây là dấu hiệu cảnh báo sớm và cần điều chỉnh lối sống để ngăn ngừa tiến triển thành cao huyết áp. Nghiên cứu cho thấy ngay cả mức tiền cao huyết áp cũng làm tăng nguy cơ bệnh tim.</li>"
               )
             } else {
               explanation <- paste0(explanation, 
                                     "<li><strong>Huyết áp tâm thu (", value, " mmHg):</strong> Huyết áp của bạn ở mức bình thường. ",
                                     "Duy trì huyết áp ở mức này là rất tốt cho sức khỏe tim mạch và giảm nguy cơ bệnh tim.</li>"
               )
             }
           },
           "chol" = {
             if (value >= 240) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Cholesterol (", value, " mg/dl):</strong> Cholesterol của bạn cao (≥ 240 mg/dl). ",
                                     "Cholesterol cao làm tăng nguy cơ xơ vữa động mạch do tích tụ mảng bám trong thành mạch máu, dẫn đến thu hẹp lòng mạch và giảm lưu lượng máu đến tim. Cứ tăng 10% cholesterol máu làm tăng 20-30% nguy cơ bệnh tim mạch.</li>"
               )
             } else if (value >= 200 && value < 240) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Cholesterol (", value, " mg/dl):</strong> Cholesterol của bạn ở mức giới hạn cao (200-239 mg/dl). ",
                                     "Mức này làm tăng nguy cơ bệnh tim so với mức dưới 200 mg/dl. Nên xem xét tỷ lệ giữa các loại cholesterol (HDL, LDL) để đánh giá chính xác hơn.</li>"
               )
             } else {
               explanation <- paste0(explanation, 
                                     "<li><strong>Cholesterol (", value, " mg/dl):</strong> Cholesterol của bạn ở mức bình thường (<200 mg/dl). ",
                                     "Mức này giúp giảm nguy cơ tích tụ mảng bám trong động mạch. Tuy nhiên, cân bằng giữa cholesterol tốt (HDL) và xấu (LDL) cũng rất quan trọng.</li>"
               )
             }
           },
           "fbs" = {
             if (value == 1) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Đường huyết lúc đói:</strong> Đường huyết lúc đói của bạn cao (>120 mg/dl). ",
                                     "Đường huyết cao mạn tính làm tổn thương thành mạch máu, tăng viêm và oxy hóa, dẫn đến xơ vữa động mạch nhanh hơn. Đối với người mắc đái tháo đường, nguy cơ bệnh tim tăng gấp 2-4 lần.</li>"
               )
             } else {
               explanation <- paste0(explanation, 
                                     "<li><strong>Đường huyết lúc đói:</strong> Đường huyết lúc đói của bạn ở mức bình thường. ",
                                     "Điều này giúp giảm nguy cơ tổn thương mạch máu và các biến chứng tim mạch. Kiểm soát đường huyết là yếu tố quan trọng trong phòng ngừa bệnh tim.</li>"
               )
             }
           },
           "thalach" = {
             max_hr_by_age <- 220 - input_data$age
             if (value < (max_hr_by_age * 0.7)) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Nhịp tim tối đa (", value, " bpm):</strong> Nhịp tim tối đa của bạn thấp hơn dự kiến theo độ tuổi. ",
                                     "Điều này có thể là dấu hiệu của suy giảm chức năng tim hoặc thiếu máu cơ tim khi gắng sức. Giảm khả năng tăng nhịp tim khi gắng sức liên quan đến tăng nguy cơ các biến cố tim mạch.</li>"
               )
             } else {
               explanation <- paste0(explanation, 
                                     "<li><strong>Nhịp tim tối đa (", value, " bpm):</strong> Nhịp tim tối đa của bạn nằm trong phạm vi bình thường cho độ tuổi. ",
                                     "Khả năng đạt nhịp tim cao khi gắng sức thường là dấu hiệu của tim khỏe mạnh và hệ tuần hoàn hoạt động tốt.</li>"
               )
             }
           },
           "exang" = {
             if (value == 1) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Đau thắt ngực khi gắng sức:</strong> Bạn có đau thắt ngực khi gắng sức. ",
                                     "Đây là dấu hiệu quan trọng của bệnh động mạch vành, cho thấy cơ tim không nhận đủ máu khi nhu cầu oxy tăng lên. Đau thắt ngực khi gắng sức tăng khả năng có stenosis (hẹp) động mạch vành đáng kể.</li>"
               )
             } else {
               explanation <- paste0(explanation, 
                                     "<li><strong>Đau thắt ngực khi gắng sức:</strong> Bạn không có đau thắt ngực khi gắng sức. ",
                                     "Đây là dấu hiệu tốt, tuy nhiên không loại trừ hoàn toàn khả năng có bệnh động mạch vành, đặc biệt ở những người có thể bị thiếu máu cơ tim thầm lặng.</li>"
               )
             }
           },
           "oldpeak" = {
             if (value >= 2) {
               explanation <- paste0(explanation, 
                                     "<li><strong>ST depression (", value, "):</strong> Mức độ suy giảm ST cao khi gắng sức. ",
                                     "Sự suy giảm đoạn ST trên điện tâm đồ là dấu hiệu quan trọng của thiếu máu cơ tim. Suy giảm >2mm liên quan chặt chẽ đến tổn thương mạch vành nặng và tăng nguy cơ biến cố tim mạch.</li>"
               )
             } else if (value > 0 && value < 2) {
               explanation <- paste0(explanation, 
                                     "<li><strong>ST depression (", value, "):</strong> Mức độ suy giảm ST vừa phải khi gắng sức. ",
                                     "Cho thấy có thể có thiếu máu cơ tim ở mức độ vừa phải. Cần đánh giá thêm để xác định mức độ tổn thương mạch vành.</li>"
               )
             } else {
               explanation <- paste0(explanation, 
                                     "<li><strong>ST depression (", value, "):</strong> Không có suy giảm ST khi gắng sức. ",
                                     "Đây là dấu hiệu tốt, gợi ý không có thiếu máu cơ tim đáng kể khi gắng sức.</li>"
               )
             }
           },
           "ca" = {
             if (as.numeric(value) > 0) {
               explanation <- paste0(explanation, 
                                     "<li><strong>Số lượng mạch chính (", value, "):</strong> Bạn có ", value, " mạch chính bị tổn thương. ",
                                     "Số lượng mạch vành chính bị tổn thương là yếu tố dự báo mạnh mẽ về nguy cơ bệnh tim và tiên lượng. Bệnh nhiều mạch làm tăng đáng kể nguy cơ biến cố tim mạch và tử vong.</li>"
               )
             } else {
               explanation <- paste0(explanation, 
                                     "<li><strong>Số lượng mạch chính (0):</strong> Không phát hiện mạch chính bị tổn thương. ",
                                     "Đây là dấu hiệu tốt, gợi ý không có bệnh động mạch vành đáng kể ở các nhánh chính.</li>"
               )
             }
           },
           "thal" = {
             thal_types <- c("", "Bình thường", "Khiếm khuyết cố định", "Khiếm khuyết có thể đảo ngược")
             thal_explanations <- c(
               "",
               "Tưới máu cơ tim bình thường trên xét nghiệm thallium, cho thấy không có vấn đề về lưu lượng máu đến tim.",
               "Khiếm khuyết cố định trên xét nghiệm thallium, thường là dấu hiệu của sẹo cơ tim do nhồi máu cơ tim trước đó.",
               "Khiếm khuyết có thể đảo ngược trên xét nghiệm thallium, thường là dấu hiệu của thiếu máu cơ tim có thể hồi phục, gợi ý bệnh động mạch vành đang hoạt động."
             )
             explanation <- paste0(explanation, 
                                   "<li><strong>Thalassemia (", thal_types[as.numeric(value) + 1], "):</strong> ", 
                                   thal_explanations[as.numeric(value) + 1], "</li>"
             )
           },
           # Mặc định nếu không có giải thích cụ thể
           {
             explanation <- paste0(explanation, 
                                   "<li><strong>", field_definitions[[feature]]$label, " (", value, "):</strong> ",
                                   "Đây là một yếu tố ảnh hưởng đến dự đoán của mô hình. Tham khảo ý kiến bác sĩ để hiểu rõ hơn về ý nghĩa của chỉ số này trong trường hợp cụ thể của bạn.</li>"
             )
           }
    )
  }
  
  explanation <- paste0(explanation, "</ul>")
  
  # Thêm giải thích về tương tác giữa các yếu tố
  explanation <- paste0(explanation, 
                        "<h4>Tương tác giữa các yếu tố nguy cơ:</h4>",
                        "<p>Nguy cơ bệnh tim không chỉ phụ thuộc vào từng yếu tố riêng lẻ mà còn vào sự kết hợp giữa các yếu tố. Ví dụ, "
  )
  
  # Tạo các giải thích về tương tác dựa trên dữ liệu
  interactions <- c()
  
  # Tương tác giữa tuổi và giới tính
  if (input_data$age > 55 && input_data$sex == 1) {
    interactions <- c(interactions, "nam giới trên 55 tuổi có nguy cơ cao hơn đáng kể")
  } else if (input_data$age > 65 && input_data$sex == 0) {
    interactions <- c(interactions, "nữ giới trên 65 tuổi (sau mãn kinh) có nguy cơ tăng nhanh")
  }
  
  # Tương tác giữa cao huyết áp và cholesterol
  if (input_data$trestbps >= 140 && input_data$chol >= 240) {
    interactions <- c(interactions, "sự kết hợp giữa cao huyết áp và cholesterol cao làm tăng nguy cơ xơ vữa động mạch nhanh hơn")
  }
  
  # Tương tác giữa đường huyết và các yếu tố khác
  if (input_data$fbs == 1 && (input_data$trestbps >= 140 || input_data$chol >= 240)) {
    interactions <- c(interactions, "đường huyết cao kết hợp với rối loạn huyết áp hoặc lipid máu tạo ra hiệu ứng cộng hưởng làm tăng tổn thương mạch máu")
  }
  
  # Tương tác giữa đau thắt ngực và ST depression
  if ((input_data$cp == 0 || input_data$exang == 1) && input_data$oldpeak >= 2) {
    interactions <- c(interactions, "đau thắt ngực kết hợp với suy giảm đoạn ST đáng kể là dấu hiệu mạnh của bệnh động mạch vành")
  }
  
  if (length(interactions) > 0) {
    explanation <- paste0(explanation, paste(interactions, collapse = "; "), ".")
  } else {
    explanation <- paste0(explanation, "các yếu tố nguy cơ cùng tồn tại thường làm tăng nguy cơ tổng thể nhiều hơn so với tổng các nguy cơ riêng lẻ.")
  }
  
  explanation <- paste0(explanation, "</p>")
  
  # Thêm đoạn kết về ý nghĩa của kết quả
  explanation <- paste0(explanation, 
                        "<h4>Lưu ý quan trọng:</h4>",
                        "<p>Kết quả dự đoán này chỉ mang tính tham khảo và không thay thế cho chẩn đoán y khoa chuyên nghiệp. ",
                        "Mô hình máy học dựa trên dữ liệu thống kê và có thể không tính đến tất cả các yếu tố cá nhân ảnh hưởng đến sức khỏe tim mạch của bạn. ",
                        "Vui lòng tham khảo ý kiến bác sĩ để được đánh giá toàn diện và tư vấn phù hợp.</p>"
  )
  
  return(explanation)
}
             