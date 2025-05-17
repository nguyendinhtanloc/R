server <- function(input, output, session) {
  # Biểu đồ 3.2: Phân bố độ tuổi theo giới tính
  output$ageDistributionPlot <- renderPlotly({
    p <- ggplot(data = heart_data, aes(x = age, fill = factor(sex))) +
      geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c("blue", "red"), labels = c("Female", "Male")) +
      labs(title = "Age Distribution by Sex",
           x = "Age",
           y = "Count",
           fill = "Sex") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$ageDistributionAnalysis <- renderUI({
    HTML("
    <h4>Mô tả</h4>
    <p>Biểu đồ histogram này hiển thị phân bố độ tuổi của các bệnh nhân trong tập dữ liệu, được chia thành hai nhóm theo giới tính (Nam và Nữ). Trục x biểu diễn độ tuổi (age), trục y biểu diễn số lượng bệnh nhân (count). Mỗi cột được tô màu khác nhau: màu đỏ cho Nam (sex = 1) và màu xanh lam cho Nữ (sex = 0).</p>
    <h4>Quan sát</h4>
    <ul>
      <li>Độ tuổi của bệnh nhân dao động từ khoảng 29 đến 77 tuổi.</li>
      <li>Nhóm tuổi phổ biến nhất cho cả Nam và Nữ nằm trong khoảng 50-60 tuổi, với số lượng Nam thường cao hơn Nữ.</li>
      <li>Có một số lượng lớn Nam trong nhóm tuổi 55-60, trong khi Nữ có số lượng ít hơn ở cùng nhóm tuổi này.</li>
      <li>Ở nhóm tuổi trẻ hơn (dưới 40 tuổi) và lớn hơn (trên 70 tuổi), số lượng bệnh nhân giảm đáng kể.</li>
    </ul>
    <h4>Kết luận</h4>
    <p>Nam giới có xu hướng chiếm đa số trong tập dữ liệu này, đặc biệt ở các nhóm tuổi trung niên (40-60 tuổi), có thể cho thấy Nam giới ở độ tuổi này dễ gặp các vấn đề về tim mạch hơn Nữ giới. Độ tuổi trung niên (50-60) là nhóm tuổi có nguy cơ cao nhất về các vấn đề tim mạch trong tập dữ liệu này.</p>
  ")
  })
  
  # Biểu đồ 3.3: Cholesterol vs Huyết áp nghỉ
  output$cholVsTrestbpsPlot <- renderPlotly({
    p <- ggplot(data = heart_data, aes(x = trestbps, y = chol, color = factor(target))) +
      geom_point(size = 3, alpha = 0.6) +
      scale_color_manual(values = c("blue", "red"), labels = c("No Heart Disease", "Heart Disease")) +
      labs(title = "Cholesterol vs Resting Blood Pressure by Target",
           x = "Resting Blood Pressure (mm Hg)",
           y = "Cholesterol (mg/dl)",
           color = "Target") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$cholVsTrestbpsAnalysis <- renderUI({
    HTML("
    <h4>Mô tả</h4>
    <p>Biểu đồ phân tán này hiển thị mối quan hệ giữa huyết áp nghỉ (trestbps) và mức cholesterol (chol). Mỗi điểm đại diện cho một bệnh nhân, màu đỏ cho bệnh nhân có bệnh tim (target = 1) và màu xanh lam cho bệnh nhân không có bệnh tim (target = 0).</p>
    <h4>Quan sát</h4>
    <ul>
      <li>Huyết áp nghỉ dao động từ khoảng 90 đến 200 mm Hg, cholesterol từ 100 đến 600 mg/dl.</li>
      <li>Không có sự phân cụm rõ rệt giữa hai nhóm (có bệnh và không có bệnh).</li>
      <li>Một số bệnh nhân có cholesterol rất cao (trên 400 mg/dl), thuộc cả hai nhóm.</li>
      <li>Có một số điểm bất thường, ví dụ, cholesterol 564 mg/dl ở nhóm không có bệnh tim.</li>
    </ul>
    <h4>Kết luận</h4>
    <p>Mối quan hệ giữa huyết áp nghỉ và cholesterol không cho thấy mô hình rõ ràng liên quan đến bệnh tim. Cần phân tích sâu hơn với các biến số khác hoặc sử dụng mô hình dự đoán.</p>
  ")
  })
  
  # Biểu đồ 3.4: Boxplot độ giãn ST theo đau thắt ngực
  output$oldpeakVsExangPlot <- renderPlotly({
    p <- ggplot(data = heart_data, aes(x = factor(exang), y = oldpeak, fill = factor(exang))) +
      geom_boxplot() +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Angina", "Angina")) +
      labs(title = "ST Depression (Oldpeak) by Exercise-Induced Angina (Exang)",
           x = "Exercise-Induced Angina (0 = No, 1 = Yes)",
           y = "ST Depression (oldpeak)",
           fill = "Exang") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$oldpeakVsExangAnalysis <- renderUI({
    HTML("
    <h4>Quan sát</h4>
    <ul>
      <li>Nhóm không đau thắt ngực (exang = 0): Trung vị độ giãn ST thấp (~0.5), phạm vi 0-2.5, nhiều ngoại lai (3.0-5.0).</li>
      <li>Nhóm có đau thắt ngực (exang = 1): Trung vị cao hơn (~1.5), phạm vi 0-4.0, ít ngoại lai nhưng giá trị cao (lên đến 6.2).</li>
      <li>Nhóm có đau thắt ngực có độ giãn ST trung bình cao hơn rõ rệt.</li>
    </ul>
    <h4>So sánh giữa hai nhóm</h4>
    <ul>
      <li>Nhóm có đau thắt ngực (exang = 1) có độ giãn ST trung bình cao hơn rõ rệt so với nhóm không có đau thắt ngực (exang = 0).</li>
      <li>Phân vị thứ ba (Q3) của nhóm có đau thắt ngực cao hơn nhiều (khoảng 2.5) so với nhóm không có đau thắt ngực (khoảng 1.0), cho thấy sự khác biệt rõ rệt trong mức độ giãn ST.</li>
      <li>Nhóm không có đau thắt ngực có nhiều giá trị ngoại lai hơn, đặc biệt ở mức 3.0-5.0, trong khi nhóm có đau thắt ngực có ít ngoại lai hơn nhưng giá trị ngoại lai cao nhất (5.8) lại thuộc về nhóm này.</li>
    </ul>
    <h4>Kết luận</h4>
    <ul>
      <li>Đau thắt ngực do gắng sức liên quan mạnh đến độ giãn ST cao hơn, phù hợp với thiếu máu cơ tim. Bệnh nhân có exang = 1 và oldpeak cao cần kiểm tra thêm (như chụp mạch vành).</li>
      <li>Nhóm không có đau thắt ngực (exang = 0) có độ giãn ST trung bình thấp hơn, phản ánh tình trạng tim mạch ổn định hơn trong lúc gắng sức. Tuy nhiên, sự hiện diện của nhiều giá trị ngoại lai (lên đến 5.0 hoặc 6.2) trong nhóm này cho thấy một số bệnh nhân không có triệu chứng đau thắt ngực vẫn có thể có vấn đề tim mạch tiềm ẩn.</li>
    </ul>
  ")
  })
  
  # Biểu đồ 3.5: Số mạch máu bị hẹp
  output$caVsTargetPlot <- renderPlotly({
    p <- ggplot(data = heart_data, aes(x = factor(target), y = ca, fill = factor(target))) +
      geom_boxplot() +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Heart Disease", "Heart Disease")) +
      labs(title = "Number of Narrowed Blood Vessels (ca) by Heart Disease Status",
           x = "Heart Disease (0 = No, 1 = Yes)",
           y = "Number of Narrowed Blood Vessels (ca)",
           fill = "Target") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$caVsTargetAnalysis <- renderUI({
    HTML("
    <h4>Quan sát</h4>
    <ul>
      <li>Nhóm không bệnh tim (target = 0): Trung vị ~1, phạm vi 0-4, nhiều ngoại lai ở 3-4.</li>
      <li>Nhóm có bệnh tim (target = 1): Trung vị ~0, phạm vi 0-3, ít ngoại lai hơn.</li>
    </ul>
    <h4>Kết luận</h4>
    <ul>
      <li>Dựa trên biểu đồ này, có vẻ như mối quan hệ giữa số lượng mạch máu bị hẹp (ca) và tình trạng bệnh tim không tuân theo giả định ban đầu rằng số mạch máu bị hẹp càng nhiều, tỷ lệ mắc bệnh tim càng cao. Thay vào đó, nhóm không có bệnh tim (target = 0) có trung vị và phân vị cao hơn so với nhóm có bệnh tim (target = 1), điều này có thể phản ánh một số bất thường trong dữ liệu hoặc cách phân loại bệnh tim trong tập dữ liệu.</li>
      <li>Tuy nhiên, điều này không nhất quán với thực tế y khoa, vì hẹp mạch máu (đặc biệt là hẹp nhiều mạch) thường liên quan đến bệnh tim. Có thể tập dữ liệu heart.csv có cách ghi nhận target hoặc ca không hoàn toàn phản ánh chính xác mối quan hệ này, hoặc có các yếu tố khác (như mức độ hẹp, triệu chứng lâm sàng) chưa được xem xét.</li>
    </ul>  
  ")
  })
  
  
  # Biểu đồ 3.7: Phân bố theo loại đau ngực
  output$cpVsTargetPlot <- renderPlotly({
    p <- ggplot(data = heart_data, aes(x = factor(cp), fill = factor(target))) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Heart Disease", "Heart Disease")) +
      labs(title = "Distribution of Patients by Chest Pain Type (cp) and Heart Disease Status",
           x = "Chest Pain Type (cp: 0-3)",
           y = "Count",
           fill = "Target") +
      theme_minimal() +
      scale_x_discrete(labels = c("0" = "Typical Angina", "1" = "Atypical Angina", "2" = "Non-Anginal Pain", "3" = "Asymptomatic"))
    ggplotly(p)
  })
  
  output$cpVsTargetAnalysis <- renderUI({
    HTML("
    <h4>Quan sát</h4>
    <ul>
      <li>Typical Angina (cp = 0): Đông nhất, tỷ lệ không bệnh tim cao hơn.</li>
      <li>Atypical Angina (cp = 1): Tỷ lệ bệnh tim cao hơn.</li>
      <li>Non-Anginal Pain (cp = 2): Tỷ lệ bệnh tim vượt trội.</li>
      <li>Asymptomatic (cp = 3): Tỷ lệ bệnh tim cao dù ít bệnh nhân.</li>
    </ul>
    <h4>Kết luận</h4>
    <p>Loại đau ngực ảnh hưởng rõ đến nguy cơ bệnh tim.</p>
    <p>Typical Angina (cp = 0): Mặc dù đây là nhóm đông nhất, nhưng tỷ lệ bệnh nhân không có bệnh tim cao hơn nhiều so với có bệnh tim. Điều này có thể cho thấy rằng đau ngực đặc trưng không phải lúc nào cũng là dấu hiệu của bệnh tim, mà có thể liên quan đến các nguyên nhân khác (như đau cơ hoặc vấn đề tiêu hóa).</p>
    <p>Atypical Angina (cp = 1) và Non-Anginal Pain (cp = 2): Các nhóm này có tỷ lệ bệnh tim cao hơn, đặc biệt là cp = 2 (Non-Anginal Pain) với tỷ lệ bệnh tim vượt trội. Điều này cho thấy đau không đặc trưng hoặc không liên quan đến tim có thể là dấu hiệu quan trọng của bệnh tim tiềm ẩn.</p>
    <p>Asymptomatic (cp = 3): Dù số lượng ít, nhưng tỷ lệ bệnh tim cao hơn, cho thấy nhiều bệnh nhân không có triệu chứng đau ngực vẫn có nguy cơ mắc bệnh tim, nhấn mạnh tầm quan trọng của việc kiểm tra định kỳ ngay cả khi không có triệu chứng.</p>
  ")
  })
  
  # Biểu đồ 3.8: Kết quả kiểm tra Thalium
  output$thalVsTargetPlot <- renderPlotly({
    p <- ggplot(data = heart_data, aes(x = factor(thal), fill = factor(target))) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("blue", "red"), labels = c("No Heart Disease", "Heart Disease")) +
      scale_x_discrete(labels = c("1" = "Normal", "2" = "Fixed Defect", "3" = "Reversible Defect")) +
      labs(title = "Thalium Test Result (thal) by Heart Disease Status",
           x = "Thalium Result (thal)",
           y = "Count") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$thalVsTargetAnalysis <- renderUI({
    HTML("
    <h4>Quan sát</h4>
    <ul>
      <li>Normal (thal = 1): Tỷ lệ không bệnh tim cao (~60%).</li>
      <li>Fixed Defect (thal = 2): Tỷ lệ bệnh tim vượt trội.</li>
      <li>Reversible Defect (thal = 3): Tỷ lệ không bệnh tim chiếm ưu thế.</li>
    </ul>
    <h4>Kết luận</h4>
    <p>Kết quả Thalium ảnh hưởng mạnh đến nguy cơ bệnh tim. Bệnh nhân có Fixed Defect hoặc Reversible Defect cần can thiệp y tế sớm.</p>
    <p>Khi thal = 1 (Normal), tỷ lệ không có bệnh tim rất cao (khoảng 60% là không có bệnh tim), cho thấy kết quả bình thường thường liên quan đến sức khỏe tim mạch tốt.</p>
    <p>Khi thal = 2 (Fixed Defect) hoặc thal = 3 (Reversible Defect), tỷ lệ bệnh tim tăng mạnh (khoảng 70-80% là có bệnh tim), cho thấy các khuyết tật trong kiểm tra Thalium là dấu hiệu quan trọng của bệnh tim.</p>
    <h4>Ứng dụng</h4>
    <p>Biểu đồ này có thể giúp bác sĩ đưa ra quyết định lâm sàng nhanh chóng. Ví dụ, bệnh nhân có kết quả thal = 2 hoặc thal = 3 nên được ưu tiên can thiệp y tế (như đặt stent hoặc phẫu thuật) để giảm nguy cơ biến chứng tim mạch.</p>
    <p>Đối với bệnh nhân có thal = 1, cần kết hợp với các yếu tố khác (như cp, oldpeak, hoặc ca) để đánh giá toàn diện nguy cơ bệnh tim, vì vẫn có khả năng mắc bệnh dù kết quả kiểm tra Thalium bình thường.</p>
  ")
  })
  
  # Biểu đồ 3.9: Nhịp tim tối đa vs Độ giãn ST
  output$thalachVsOldpeakPlot <- renderPlotly({
    p <- ggplot(data = heart_data, aes(x = thalach, y = oldpeak, color = factor(target))) +
      geom_point(alpha = 0.6, size = 2) +
      scale_color_manual(values = c("blue", "red"), labels = c("No Heart Disease", "Heart Disease")) +
      labs(title = "Maximum Heart Rate (thalach) vs ST Depression (oldpeak) by Heart Disease Status",
           x = "Maximum Heart Rate (thalach)",
           y = "ST Depression (oldpeak)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$thalachVsOldpeakAnalysis <- renderUI({
    HTML("
    <h4>Phân bố tổng thể</h4>
    <ul>
      <li>Điểm dữ liệu chủ yếu trong khoảng thalach 120-180 bpm, oldpeak 0-4.</li>
      <li>Nhóm không bệnh tim: Nhiều điểm, tập trung thalach cao, oldpeak thấp.</li>
      <li>Nhóm có bệnh tim: Ít điểm, tập trung thalach thấp, oldpeak cao.</li>
    </ul>
    <h4>Nhóm không có bệnh tim</h4>
    <ul>
      <li>Tập trung chủ yếu ở thalach từ 120 đến 180 bpm, với phần lớn oldpeak nằm trong khoảng 0 đến 2.</li>
      <li>Một số ít điểm có oldpeak cao (lên đến 4-6), nhưng thường đi kèm với thalach cao (trên 150 bpm).</li>
      <li>Phân bố dàn trải, cho thấy sự biến thiên lớn trong nhịp tim tối đa và độ giãn ST.</li>
    </ul>
    <h4>Nhóm có bệnh tim</h4>
    <ul>
      <li>Tập trung ở thalach cao hơn, chủ yếu từ 150 đến 190 bpm, với phần lớn oldpeak từ 0 đến 2.</li>
      <li>Có một số điểm ngoại lệ với oldpeak rất cao, thường đi kèm với thalach thấp (dưới 140 bpm).</li>
      <li>Phân bố tập trung hơn ở khu vực thalach cao và oldpeak thấp, cho thấy xu hướng rõ rệt.</li>
    </ul>
    <h4>Kết luận</h4>
    <p>Nếu thalach > 150 bpm và oldpeak < 1, khả năng không bệnh tim cao.</p>
    <p>Nếu thalach < 150 bpm và oldpeak > 1, khả năng có bệnh tim cao.</p>
  ")
  })
  
  # Bảng dữ liệu chi tiết
  output$dataTableVisualization <- DT::renderDataTable({
    DT::datatable(heart_data, options = list(
      pageLength = 10,
      autoWidth = TRUE
    ))
  })
  
  # Reactive value để lưu trữ kết quả dự đoán
  prediction_results <- reactiveVal(NULL)
  source("chatbot_logic.R")
  chat_history <- reactiveVal(character())
  
  observeEvent(input$ask_button, {
    req(input$user_question)
    
    user_q <- input$user_question
    current_history <- chat_history()
    
    # Gọi hàm trả lời từ file chatbot_logic.R
    answer <- chatbot_reply(user_q)
    
    # Cập nhật hội thoại
    new_history <- c(current_history, 
                     paste0("<b>Bạn:</b> ", user_q), 
                     paste0("<b>Bot:</b> ", answer))
    chat_history(new_history)
  })
  
  output$chat_history <- renderUI({
    HTML(paste(chat_history(), collapse = "<br><br>"))
  })
  
  # Hiển thị trợ giúp khi người dùng hover lên trường dữ liệu
  output$help_text <- renderUI({
    trigger <- sapply(names(field_definitions), function(id) {
      input[[id]]
    })
    
    # Tìm trường đang được tương tác
    active_field <- names(which(sapply(names(input), function(x) {
      x == session$input$...client_data_...["input_binding"] &&
        !is.null(field_definitions[[x]])
    })))
    
    if(length(active_field) > 0) {
      field <- active_field[1]
      HTML(paste0(
        "<strong>", field_definitions[[field]]$label, ":</strong> ",
        field_definitions[[field]]$description
      ))
    } else {
      HTML("<p>Di chuyển chuột đến các trường dữ liệu để xem mô tả chi tiết.</p>")
    }
  })
  
  # Observer cho nút làm mới
  observeEvent(input$reset_btn, {
    lapply(names(field_definitions), function(id) {
      updateNumericInput(session, id, value = field_definitions[[id]]$default)
      updateSelectInput(session, id, selected = field_definitions[[id]]$default)
      updateRadioButtons(session, id, selected = field_definitions[[id]]$default)
    })
    prediction_results(NULL)
  })
  
  # Observer để phát hiện thiếu dữ liệu và đề xuất thông tin bổ sung
  observe({
    # Kiểm tra các giá trị quan trọng
    if (!is.null(input$age) && !is.null(input$trestbps) && !is.null(input$chol)) {
      # Kiểm tra mối tương quan và hiển thị thông báo nếu cần
      if (input$age > 60 && input$trestbps < 100) {
        showNotification(
          "Huyết áp thấp ở người cao tuổi có thể cần kiểm tra thêm. Bạn có dùng thuốc hạ huyết áp không?",
          type = "warning",
          duration = 10
        )
      }
      
      if (input$chol > 240 && input$fbs == 1) {
        showNotification(
          "Cholesterol cao kết hợp với đường huyết cao là yếu tố nguy cơ. Bạn có đang điều trị rối loạn lipid máu không?",
          type = "warning",
          duration = 10
        )
      }
    }
  })
  
  # Kiểm tra tính hợp lệ của đầu vào
  check_input_validity <- reactive({
    # Kiểm tra các điều kiện
    if (input$age < 20 || input$age > 100) {
      return(list(valid = FALSE, message = "Tuổi nên nằm trong khoảng từ 20 đến 100"))
    }
    
    if (input$trestbps < 90 || input$trestbps > 200) {
      return(list(valid = FALSE, message = "Huyết áp tâm thu nên nằm trong khoảng từ 90 đến 200 mmHg"))
    }
    
    if (input$thalach < 60 || input$thalach > 220) {
      return(list(valid = FALSE, message = "Nhịp tim tối đa nên nằm trong khoảng từ 60 đến 220"))
    }
    
    # Kiểm tra nếu nhịp tim tối đa quá cao so với tuổi
    max_hr_by_age <- 220 - input$age
    if (input$thalach > max_hr_by_age + 10) {
      return(list(valid = FALSE, message = paste0("Nhịp tim tối đa (", input$thalach, ") cao bất thường so với độ tuổi của bạn. Giá trị thông thường là khoảng ", max_hr_by_age)))
    }
    
    # Kiểm tra kết hợp các thông số
    if (input$exang == 1 && input$cp == 0) {
      return(list(valid = TRUE, warning = "Bạn đã chọn đau thắt ngực điển hình và có đau thắt ngực khi gắng sức. Hãy đảm bảo thông tin này chính xác."))
    }
    
    # Nếu tất cả đều hợp lệ
    return(list(valid = TRUE))
  })
  
  # Observer cho nút dự đoán
  observeEvent(input$predict_btn, {
    # Kiểm tra tính hợp lệ của đầu vào
    validity <- check_input_validity()
    
    if (!validity$valid) {
      showNotification(validity$message, type = "error", duration = 5)
      return()
    }
    
    if (!is.null(validity$warning)) {
      showNotification(validity$warning, type = "warning", duration = 5)
    }
    
    # Thu thập dữ liệu đầu vào
    input_data <- data.frame(
      age = as.numeric(input$age),
      sex = as.numeric(input$sex),
      cp = as.numeric(input$cp),
      trestbps = as.numeric(input$trestbps),
      chol = as.numeric(input$chol),
      fbs = as.numeric(input$fbs),
      restecg = as.numeric(input$restecg),
      thalach = as.numeric(input$thalach),
      exang = as.numeric(input$exang),
      oldpeak = as.numeric(input$oldpeak),
      slope = as.numeric(input$slope),
      ca = as.numeric(input$ca),
      thal = as.numeric(input$thal)
    )
    
    # Tiền xử lý dữ liệu
    processed_data <- preprocess_data(input_data)
    
    # Dự đoán với các mô hình
    gbm_pred <- predict_gbm(gbm_model, processed_data)
    rf_pred <- predict_rf(rf_model, processed_data)
    log_pred <- predict_log(log_model, processed_data)
    
    # Tính toán tổng hợp dự đoán
    ensemble_pred <- (gbm_pred + rf_pred + log_pred) / 3
    
    # Tính toán độ quan trọng của các đặc trưng
    feature_imp <- calculate_feature_importance(processed_data)
    
    # Lưu kết quả dự đoán
    prediction_results(list(
      input_data = input_data,
      processed_data = processed_data,
      gbm_pred = gbm_pred,
      rf_pred = rf_pred,
      log_pred = log_pred,
      ensemble_pred = ensemble_pred,
      feature_importance = feature_imp
    ))
    
    # Chuyển đến tab kết quả
    updateTabItems(session, "sidebarMenu", "result")
  })
  
  # Hiển thị value box nguy cơ
  output$risk_box <- renderValueBox({
    results <- prediction_results()
    
    if (is.null(results)) {
      valueBox(
        "Chưa có dự đoán",
        "Hãy nhập thông tin bệnh nhân và nhấp 'Dự đoán'",
        icon = icon("question-circle"),
        color = "gray"
      )
    } else {
      risk_level <- results$ensemble_pred
      
      if (risk_level > 0.7) {
        valueBox(
          paste0(round(risk_level * 100), "%"),
          "Nguy cơ cao mắc bệnh tim",
          icon = icon("heart-broken"),
          color = "red"
        )
      } else if (risk_level > 0.3) {
        valueBox(
          paste0(round(risk_level * 100), "%"),
          "Nguy cơ trung bình mắc bệnh tim",
          icon = icon("heartbeat"),
          color = "yellow"
        )
      } else {
        valueBox(
          paste0(round(risk_level * 100), "%"),
          "Nguy cơ thấp mắc bệnh tim",
          icon = icon("heart"),
          color = "green"
        )
      }
    }
  })
  
  # Hiển thị biểu đồ đồng hồ dự đoán
  output$prediction_gauge <- renderPlotly({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    risk_level <- results$ensemble_pred
    
    # Tạo biểu đồ đồng hồ
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = risk_level * 100,
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "darkblue"),
        steps = list(
          list(range = c(0, 30), color = "green"),
          list(range = c(30, 70), color = "yellow"),
          list(range = c(70, 100), color = "red")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = risk_level * 100
        )
      )
    ) %>% 
    layout(margin = list(l = 20, r = 30))
  })
  
  # Hiển thị bảng kết quả dự đoán từ các mô hình
  output$model_predictions <- renderTable({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    data.frame(
      Mô_hình = c("Gradient Boosting", "Random Forest", "Logistic Regression", "Ensemble"),
      Xác_suất = c(
        paste0(round(results$gbm_pred * 100, 1), "%"),
        paste0(round(results$rf_pred * 100, 1), "%"),
        paste0(round(results$log_pred * 100, 1), "%"),
        paste0(round(results$ensemble_pred * 100, 1), "%")
      ),
      Dự_đoán = c(
        ifelse(results$gbm_pred > 0.5, "Có nguy cơ", "Không có nguy cơ"),
        ifelse(results$rf_pred > 0.5, "Có nguy cơ", "Không có nguy cơ"),
        ifelse(results$log_pred > 0.5, "Có nguy cơ", "Không có nguy cơ"),
        ifelse(results$ensemble_pred > 0.5, "Có nguy cơ", "Không có nguy cơ")
      )
    )
  })
  
  # Hiển thị biểu đồ các yếu tố nguy cơ
  output$risk_factors <- renderPlotly({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    # Lấy top 5 yếu tố nguy cơ
    top_factors <- head(results$feature_importance[order(results$feature_importance$importance, decreasing = TRUE), ], 5)
    
    # Tạo biểu đồ
    plot_ly(
      top_factors,
      x = ~importance,
      y = ~reorder(feature, importance),
      type = "bar",
      orientation = "h",
      marker = list(color = "steelblue")
    ) %>%
    layout(
      title = "Top 5 yếu tố nguy cơ",
      xaxis = list(title = "Mức độ ảnh hưởng"),
      yaxis = list(title = ""),
      margin = list(l = 150)
    )
  })
  
  # Hiển thị văn bản giải thích yếu tố nguy cơ
  output$risk_text <- renderUI({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    # Lấy top 3 yếu tố nguy cơ
    top_factors <- head(results$feature_importance[order(results$feature_importance$importance, decreasing = TRUE), ], 3)
    
    # Tạo giải thích
    explanations <- generate_risk_explanations(top_factors, results$input_data)
    
    HTML(paste0(
      "<div class='risk-explanation'>",
      "<h4>Giải thích các yếu tố nguy cơ:</h4>",
      "<ul>",
      paste0("<li>", explanations, "</li>", collapse = ""),
      "</ul>",
      "</div>"
    ))
  })
  
  # Hiển thị đề xuất tiếp theo
  output$recommendations <- renderUI({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    # Tạo đề xuất dựa trên kết quả dự đoán và dữ liệu đầu vào
    recommendations <- generate_recommendations(results$ensemble_pred, results$input_data)
    
    HTML(paste0(
      "<div class='recommendations'>",
      recommendations,
      "</div>"
    ))
  })
  
  # Hiển thị biểu đồ độ quan trọng của đặc trưng
  output$feature_importance <- renderPlotly({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    # Tạo biểu đồ độ quan trọng của đặc trưng
    plot_ly(
      results$feature_importance,
      x = ~reorder(feature, importance),
      y = ~importance,
      type = "bar",
      marker = list(color = ~importance, colorscale = "Viridis")
    ) %>%
    layout(
      title = "Mức độ ảnh hưởng của các yếu tố",
      xaxis = list(title = ""),
      yaxis = list(title = "Mức độ ảnh hưởng")
    )
  })
  
  # Hiển thị văn bản giải thích
  output$explanation_text <- renderUI({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    # Tạo giải thích chi tiết
    explanation <- generate_detailed_explanation(results$input_data, results$ensemble_pred, results$feature_importance)
    
    HTML(paste0(
      "<div class='detailed-explanation'>",
      explanation,
      "</div>"
    ))
  })
  
  # Hiển thị biểu đồ so sánh với dân số
  output$population_comparison <- renderPlotly({
    results <- prediction_results()
    
    if (is.null(results)) {
      return(NULL)
    }
    
    # So sánh dữ liệu của người dùng với phân phối trong bộ dữ liệu
    # Chọn các biến số quan trọng để so sánh
    important_vars <- c("age", "trestbps", "chol", "thalach")
    
    # Tạo data frame để chứa kết quả so sánh
    comparison_data <- data.frame(
      variable = character(),
      value = numeric(),
      mean = numeric(),
      min = numeric(),
      max = numeric(),
      percentile = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Tính toán thống kê so sánh cho từng biến
    for (var in important_vars) {
      # Lấy giá trị của người dùng
      user_value <- results$input_data[[var]]
      
      # Tính toán thống kê từ bộ dữ liệu
      var_stats <- heart_data %>%
        summarise(
          mean = mean(!!sym(var), na.rm = TRUE),
          min = min(!!sym(var), na.rm = TRUE),
          max = max(!!sym(var), na.rm = TRUE)
        )
      
      # Tính toán phân vị (percentile) của giá trị người dùng
      percentile <- ecdf(heart_data[[var]])(user_value) * 100
      
      # Thêm vào data frame so sánh
      comparison_data <- rbind(
        comparison_data,
        data.frame(
          variable = var,
          value = user_value,
          mean = var_stats$mean,
          min = var_stats$min,
          max = var_stats$max,
          percentile = percentile,
          stringsAsFactors = FALSE
        )
      )
    }
    
    # Đổi tên biến để hiển thị thân thiện hơn
    comparison_data$variable_name <- sapply(comparison_data$variable, function(v) {
      field_definitions[[v]]$label
    })
    
    # Tạo biểu đồ so sánh
    plot_ly() %>%
      add_trace(
        data = comparison_data,
        x = ~variable_name,
        y = ~value,
        type = "scatter",
        mode = "markers",
        name = "Giá trị của bạn",
        marker = list(size = 12, color = "red")
      ) %>%
      add_trace(
        data = comparison_data,
        x = ~variable_name,
        y = ~mean,
        type = "scatter",
        mode = "markers",
        name = "Trung bình dân số",
        marker = list(size = 12, color = "blue")
      ) %>%
      add_segments(
        data = comparison_data,
        x = ~variable_name,
        xend = ~variable_name,
        y = ~min,
        yend = ~max,
        line = list(color = "gray"),
        name = "Phạm vi trong dân số"
      ) %>%
      layout(
        title = "So sánh với dân số trong bộ dữ liệu",
        xaxis = list(title = ""),
        yaxis = list(title = "Giá trị"),
        legend = list(x = 0.7, y = 0.9)
      )
  })
}