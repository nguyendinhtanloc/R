# Tạo nút trợ giúp
create_help_button <- function(id, content) {
  actionButton(
    inputId = paste0(id, "_help"),
    label = "",
    icon = icon("question-circle"),
    class = "btn-xs btn-info help-button",
    title = content
  )
}

# Tạo trường nhập liệu với trợ giúp
create_input_with_help <- function(input_func, input_args, help_text) {
  div(
    class = "input-with-help",
    do.call(input_func, input_args),
    create_help_button(input_args$inputId, help_text)
  )
}

# Tạo hộp thoại modal với thông tin giải thích
create_explanation_modal <- function(title, content) {
  modalDialog(
    title = title,
    HTML(content),
    easyClose = TRUE,
    footer = modalButton("Đóng")
  )
}

# Tạo biểu đồ đồng hồ
create_gauge_chart <- function(value, min = 0, max = 100, title = "", levels = list(
  low = list(min = 0, max = 30, color = "green", label = "Thấp"),
  medium = list(min = 30, max = 70, color = "yellow", label = "Trung bình"),
  high = list(min = 70, max = 100, color = "red", label = "Cao")
)) {
  # Xác định mức độ dựa trên giá trị
  current_level <- NULL
  for (level_name in names(levels)) {
    level <- levels[[level_name]]
    if (value >= level$min && value <= level$max) {
      current_level <- level
      current_level$name <- level_name
      break
    }
  }
  
  if (is.null(current_level)) {
    current_level <- list(
      min = min, 
      max = max, 
      color = "gray", 
      label = "Không xác định",
      name = "unknown"
    )
  }
  
  # Tạo dữ liệu cho biểu đồ
  gauge_data <- data.frame(
    value = value,
    min = min,
    max = max,
    level = current_level$name,
    level_label = current_level$label,
    level_color = current_level$color
  )
  
  # Tạo biểu đồ với plotly
  plot_ly(
    gauge_data,
    type = "indicator",
    mode = "gauge+number",
    value = ~value,
    title = list(text = title),
    gauge = list(
      axis = list(range = list(min, max)),
      bar = list(color = current_level$color),
      steps = lapply(names(levels), function(level_name) {
        level <- levels[[level_name]]
        list(
          range = c(level$min, level$max),
          color = level$color
        )
      }),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 0.75,
        value = value
      )
    )
  ) %>% 
    layout(margin = list(l = 30, r = 30, t = 70, b = 30))
}

# Tạo biểu đồ thanh ngang
create_horizontal_bar_chart <- function(data, x_col, y_col, title = "", x_label = "", y_label = "", color = "steelblue") {
  plot_ly(
    data,
    x = ~data[[x_col]],
    y = ~data[[y_col]],
    type = "bar",
    orientation = "h",
    marker = list(color = color)
  ) %>%
    layout(
      title = title,
      xaxis = list(title = x_label),
      yaxis = list(title = y_label, autorange = "reversed"),
      margin = list(l = 150)
    )
}

# Tạo hộp giá trị
create_value_box <- function(value, subtitle, icon, color = "blue") {
  div(
    class = paste0("value-box bg-", color),
    div(class = "inner",
        h3(value),
        p(subtitle)
    ),
    div(class = "icon",
        icon(icon)
    )
  )
}

# Tạo biểu đồ radar
create_radar_chart <- function(data, categories, values, title = "") {
  # Chuẩn bị dữ liệu
  plot_data <- data.frame(
    category = categories,
    value = values
  )
  
  # Tính toán vị trí các điểm trên biểu đồ radar
  n <- length(categories)
  angles <- seq(0, 2 * pi, length.out = n + 1)
  
  # Tạo tọa độ x, y cho các điểm
  plot_data$x <- values * sin(angles[1:n])
  plot_data$y <- values * cos(angles[1:n])
  
  # Tạo dữ liệu cho đường viền
  line_data <- rbind(plot_data, plot_data[1, ])
  
  # Tạo dữ liệu cho các trục
  axis_data <- data.frame(
    category = rep(categories, each = 2),
    x = c(rbind(0, sin(angles[1:n]) * max(values, na.rm = TRUE) * 1.1)),
    y = c(rbind(0, cos(angles[1:n]) * max(values, na.rm = TRUE) * 1.1))
  )
  
  # Tạo biểu đồ
  plot_ly() %>%
    # Thêm khu vực điền màu
    add_polygon(
      x = ~x,
      y = ~y,
      data = plot_data,
      fill = "toself",
      fillcolor = "rgba(31, 119, 180, 0.3)",
      line = list(color = "rgb(31, 119, 180)")
    ) %>%
    # Thêm các điểm dữ liệu
    add_markers(
      x = ~x,
      y = ~y,
      data = plot_data,
      marker = list(color = "rgb(31, 119, 180)", size = 10)
    ) %>%
    # Thêm các trục
    add_segments(
      x = ~x[seq(1, length(x), 2)],
      y = ~y[seq(1, length(y), 2)],
      xend = ~x[seq(2, length(x), 2)],
      yend = ~y[seq(2, length(y), 2)],
      data = axis_data,
      line = list(color = "rgba(150, 150, 150, 0.6)", dash = "dash")
    ) %>%
    # Thêm nhãn cho các trục
    add_annotations(
      x = sin(angles[1:n]) * max(values, na.rm = TRUE) * 1.2,
      y = cos(angles[1:n]) * max(values, na.rm = TRUE) * 1.2,
      text = categories,
      showarrow = FALSE,
      data = data.frame(angles = angles[1:n])
    ) %>%
    # Cấu hình layout
    layout(
      title = title,
      xaxis = list(
        zeroline = FALSE,
        showgrid = FALSE,
        showticklabels = FALSE,
        range = c(-max(values, na.rm = TRUE) * 1.3, max(values, na.rm = TRUE) * 1.3)
      ),
      yaxis = list(
        zeroline = FALSE,
        showgrid = FALSE,
        showticklabels = FALSE,
        range = c(-max(values, na.rm = TRUE) * 1.3, max(values, na.rm = TRUE) * 1.3)
      ),
      showlegend = FALSE
    )
}