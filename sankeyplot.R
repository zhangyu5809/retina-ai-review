library(tidyverse)
library(ggsankey)
# 读取CSV文件
clinical_data <- read.csv("C:/Users/zy/Desktop/notebookLM/original data/clinicaltask.csv")
# 查看数据结构
str(clinical_data)

# 数据预处理
clinical_data_long <- clinical_data %>%
  pivot_wider(names_from = Task, values_from = Volume) %>%
  pivot_longer(cols = c(screening, grading, oculomic, prognosis, others),
               names_to = "Task",
               values_to = "Volume")

# 创建过渡数据集
clinical_data_transition <- clinical_data_long %>%
  group_by(Task) %>%
  arrange(Year) %>%
  reframe(Year_prev = lag(Year), Volume_prev = lag(Volume), Year, Volume, Task) %>%
  mutate(from = ifelse(is.na(Volume_prev), Task, paste0(Task, " ", Year_prev)),
    to = paste0(Task, " ", Year), value = Volume) %>%
  select(from, to, value,Year, Task)
# 添加 x 和 next_x 列
clinical_data_transition <- clinical_data_transition %>%
  mutate(
    x = as.numeric(as.factor(Year)),
    next_x = lead(x)
  )

# Create sankeyplot
ggplot(clinical_data_transition, 
       aes(x = as.factor(Year), 
           next_x=next_x,
           node = from, 
           next_node = to,
           fill = Task,
           value = value)) +
  geom_sankey(flow.alpha = 0.7, node.color = "black") +
  scale_fill_viridis_d(option = "plasma", alpha = .8) +
  theme_minimal() +
  labs(title = "Literature Volume of Clinical Tasks Over Time(2020-2025)",
       subtitle = "Actual Data from CSV File",
       x = NULL,
       y = NULL,
       fill = "Clinical Task",
       caption = "Data source: CSV file.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Create the Sankey bump plot
ggplot(clinical_data_long, 
       aes(x = Year,
           node = Task,
           fill = Task,
           value = Volume)) +
  geom_sankey_bump(space = 0, 
                   type = "alluvial", 
                   color = "transparent", 
                   smooth = 6) +
  scale_fill_viridis_d(option = "inferno", alpha = .8)  +
  scale_x_continuous(breaks = scales::pretty_breaks(), expand = c(0.05,0.05)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  theme_sankey_bump(base_size = 16) +
  labs(x = NULL,
       y = "Volume of literature",
       fill = "Task",
       title = "Literature Volume of Clinical Tasks Over Time",
       subtitle = "Period: 2020-2025",
       caption = "Data sources: private data") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"))