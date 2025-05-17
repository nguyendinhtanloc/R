chatbot_reply <- function(user_input) {
  user_input <- tolower(user_input)
  
  if (grepl("helo|hello|chào|hi", user_input)) {
    return("Chào bạn tôi có thể giúp gì cho bạn")
    
  } else if (grepl("phát triển|tạo ra|tác giả", user_input)) {
    return("Phần mềm này do nhóm sinh viên Mai Quốc Thái,Nguyễn Vũ Minh, Nguyễn Vũ Minh, Đinh Hoàng Phương dưới sự hướng dẫn của TS.Phan Thị Thể phát triển.")
    
  } else if (grepl("tuổi|age", user_input)) {
    return("Tuổi là một yếu tố nguy cơ tim mạch quan trọng. Người trên 45 tuổi có nguy cơ cao hơn.")
    
  } else if (grepl("giới tính|gt|sex", user_input)) {
    return("Giới tính ảnh hưởng đến nguy cơ bệnh tim. Nam thường có nguy cơ cao hơn nữ ở cùng độ tuổi.")
    
  } else if (grepl("đau ngực|cp|chest pain", user_input)) {
    return("Loại đau ngực giúp chẩn đoán bệnh tim. Đau thắt ngực điển hình là dấu hiệu rõ ràng hơn.")
    
  } else if (grepl("huyết áp|bp|resting bp|áp lực máu", user_input)) {
    return("Huyết áp tâm thu là áp lực máu khi nghỉ. Giá trị bình thường là dưới 120 mmHg.")
    
  } else if (grepl("cholesterol|chol|mỡ máu", user_input)) {
    return("Cholesterol huyết thanh cao làm tăng nguy cơ xơ vữa động mạch và bệnh tim.Cholesterol huyết thanh là lượng chất béo trong máu, 
           bao gồm cholesterol xấu (LDL) và tốt (HDL). Mức LDL cao làm tăng nguy cơ xơ vữa động mạch và bệnh tim. HDL thấp cũng là yếu tố nguy cơ. Giá trị cholesterol cao (trên 240 mg/dl) có thể cảnh báo bệnh tim mạch tiềm ẩn. Kiểm soát mỡ máu là rất quan trọng trong phòng ngừa bệnh tim.")
  
  } else if (grepl("đường huyết|glucose|fbs|đường máu", user_input)) {
    return("Đường huyết lúc đói trên 120 mg/dl có thể là dấu hiệu tiền tiểu đường hoặc tiểu đường.")
    
  } else if (grepl("điện tâm đồ|ecg|restecg", user_input)) {
    return("Kết quả điện tâm đồ giúp phát hiện rối loạn nhịp tim hoặc nhồi máu cơ tim.")
    
  } else if (grepl("nhịp tim|max heart rate|thalach", user_input)) {
    return("Nhịp tim tối đa phản ánh khả năng gắng sức. Giá trị quá thấp hoặc cao có thể bất thường.")
    
  } else if (grepl("gắng sức|exercise|exang", user_input)) {
    return("Đau ngực khi gắng sức là dấu hiệu bệnh tim thiếu máu cục bộ.")
    
  } else if (grepl("st depression|oldpeak|độ trũng st", user_input)) {
    return("ST depression cho thấy khả năng thiếu máu cơ tim khi gắng sức.
            Bạn cần đo điện tâm đồ để xác định giá trị này
            Giá trị cao hơn (ví dụ: 3.1, 2.6, 4.4, 3.2, 4.2): Có thể gợi ý về tiền sử thiếu máu cơ tim cục bộ đáng kể hoặc tổn thương cơ tim trước đó. Mức độ chênh xuống càng lớn có thể liên quan đến nguy cơ tim mạch cao hơn.
            Giá trị thấp hơn (ví dụ: 1, 0, 1.9, 0.8, 0.7): Có thể cho thấy mức độ thiếu máu cơ tim trong quá khứ ít nghiêm trọng hơn hoặc không có biểu hiện rõ ràng trên điện tâm đồ nghỉ ngơi.
            Giá trị 0: Có chỉ ra rằng không có chênh xuống đoạn ST đáng kể nào được ghi nhận trước đây.")
  } else if (grepl("độ dốc st|slope|st slope", user_input)) {
    return("Độ dốc ST là thông số điện tâm đồ phản ánh tình trạng tim khi gắng sức.Độ dốc đoạn ST (ST slope) là hình dạng 
    nghiêng của đoạn ST trên điện tâm đồ (ECG) khi đo lúc gắng sức. Chỉ số này giúp bác sĩ đánh giá xem tim có bị thiếu máu khi 
    làm việc nhiều hay không. Có ba dạng chính: nếu đoạn ST dốc lên (up-sloping), thường là bình thường và ít nguy cơ bệnh tim; 
    nếu ST phẳng (flat), có thể gợi ý thiếu máu cơ tim nhẹ hoặc sớm; còn nếu đoạn ST dốc xuống (down-sloping), đây là dấu hiệu nghiêm 
    trọng hơn, cảnh báo nguy cơ cao bị bệnh động mạch vành hoặc nhồi máu cơ tim. Vì vậy, độ dốc ST là một thông số rất quan trọng trong việc chẩn đoán bệnh tim mạch.

")
  } else if (grepl("mạch chính|ca|number of vessels", user_input)) {
    return("Số mạch chính bị hẹp cho biết mức độ nghiêm trọng của bệnh mạch vành.
           0: Có thể biểu thị rằng không có mạch chính nào bị hẹp, tức là tình trạng bệnh mạch vành có thể chưa xuất hiện hoặc không đáng kể.
1, 2, 3, 4: Tương ứng với số lượng mạch chính bị hẹp. Số lượng càng cao có thể cho thấy mức độ nghiêm trọng của bệnh mạch vành càng lớn, vì có nhiều mạch máu quan trọng bị ảnh hưởng, làm giảm lưu lượng máu đến tim.")
  
  } else if (grepl("thalassemia|thal|tan máu", user_input)) {
    return("Thalassemia là bệnh di truyền liên quan đến bất thường cấu trúc hồng cầu, ảnh hưởng tới chẩn đoán tim mạch.
           Khiếm khuyết cố định: Lựa chọn này có thể được hiểu là Fixed defect hoặc Permanent deficiency. Nó có thể đề cập đến một dạng Thalassemia mà sự thiếu hụt các chuỗi globin (thành phần của hemoglobin) là nghiêm trọng và không thay đổi.
           Khiếm khuyết có thể đảo ngược: Lựa chọn này có nghĩa là Reversible defect hoặc Deficiency that can be reversed. Nó có thể đề cập đến một tình trạng thiếu hụt trong Thalassemia mà có khả năng cải thiện hoặc được điều trị để trở về gần với trạng thái bình thường.")
  
  } else if (grepl("hướng dẫn|cách dùng|sử dụng|", user_input)) {
    return("Bạn có thể đọc ở phần thông tin để xem hướng dẫn sử dụng")
 
  } else {
    return("Xin lỗi, tôi chưa hiểu câu hỏi. Bạn có thể hỏi về các thông số như: tuổi, bp, cholesterol, thalassemia, v.v.")
  }
}
