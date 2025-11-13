#========================================================#
#      Yorkshire Industry Employment Analysis (2015–2024)#
#========================================================#

#-------------------------#
#       Libraries         #
#-------------------------#
library(readxl)
library(tidyverse)
library(stringr)
library(ggpubr)
library(ggrepel)
library(scales)

#-------------------------#
#       Loading Data      #
#-------------------------#
bres <- read_excel("bres_yorkshire_industry.xlsx") %>% 
  select(-contains("Flags")) %>%  # Removes unnecessary columns
  mutate(Industry = str_replace(Industry, "^\\d+\\s*:\\s*", "") %>%
           str_replace("\\s*\\([^)]*\\)$", ""))  # Clean names

bres_long <- bres %>% 
  pivot_longer(cols = `2015`:`2024`, names_to = "Year", values_to = "Employees") %>%
  mutate(Year = as.numeric(Year))

#-------------------------#
#   Exploratory Analysis  #
#-------------------------#
summary(bres_long$Employees)
length(unique(bres_long$Industry))

regional_trend <- bres_long %>%
  group_by(Year) %>%
  summarise(TotalEmployment = sum(Employees, na.rm = TRUE))

#-------------------------#
#   Employment Trends     #
#-------------------------#
employment_trends_all_industries <- ggplot(bres_long, aes(Year, Employees, colour = Industry)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(title = "Employment Trends by Industry (2015–2024)",
       x = "Year", y = "Number of Employees", colour = "Industry") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 20),
        legend.key.height = unit(1.2, "cm"),
        legend.key.width = unit(0.25, "cm"))

ggsave("employment_trends_all_industries.png", plot = employment_trends_all_industries, width = 16, height = 12, dpi = 300)

#-------------------------#
#     Top 5 Industries    #
#-------------------------#
top_industries <- bres_long %>%
  group_by(Industry) %>%
  summarise(TotalEmployees = sum(Employees, na.rm = TRUE)) %>%
  arrange(desc(TotalEmployees)) %>%
  slice_head(n = 5)

bres_top <- bres_long %>% filter(Industry %in% top_industries$Industry)

ggplot(bres_top, aes(Year, Employees, colour = Industry)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(title = "Employment Trends for the Top 5 Industries (2015–2024)",
       x = "Year", y = "Number of Employees", colour = "Industry") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 20),
        legend.key.height = unit(1.2, "cm"),
        legend.key.width = unit(0.25, "cm"))

ggsave("employment_trends_top5_industries.png", width = 16, height = 12, dpi = 300)

#========================================================#
#                      RQ1                               #
#   Industries with the most consistent growth           #
#========================================================#
industry_growth <- bres_long %>% 
  group_by(Industry) %>% 
  summarise(GrowthPercent = ((last(Employees) - first(Employees)) / first(Employees)) * 100) %>%
  arrange(desc(GrowthPercent))

ggplot(industry_growth, aes(reorder(Industry, GrowthPercent), GrowthPercent, fill = GrowthPercent)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(GrowthPercent, 1)), hjust = -0.1, size = 5) +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Percentage Employment Growth by Industry (2015–2024)",
       x = "Industry", y = "Growth (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 24))

ggsave("percentage_growth_by_industry.png", width = 15, height = 12, dpi = 300)

#========================================================#
#                      RQ2                               #
#   COVID-19 impact on employment (2019–2021)            #
#========================================================#
covid_change <- bres_long %>%
  filter(Year %in% c(2019, 2021)) %>%
  group_by(Industry) %>%
  summarise(ChangeDuringCovid = ((last(Employees) - first(Employees)) / first(Employees)) * 100) %>%
  arrange(desc(ChangeDuringCovid))

ggplot(covid_change, aes(reorder(Industry, ChangeDuringCovid), ChangeDuringCovid, fill = ChangeDuringCovid > 0)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(ChangeDuringCovid, 1)), 
            hjust = ifelse(covid_change$ChangeDuringCovid > 0, -0.1, 1.1), size = 5) +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick")) +
  coord_flip() +
  labs(title = "Employment Change During COVID-19 (2019–2021)",
       x = "Industry", y = "Change (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 24))

ggsave("covid_employment_change.png", width = 15, height = 12, dpi = 300)

#========================================================#
#                      RQ3                               #
#   Predicting industry growth/decline by 2030           #
#========================================================#
fit_predict_industry <- function(df, year_pred = 2030) {
  model <- lm(Employees ~ Year, data = df)
  predicted <- predict(model, newdata = data.frame(Year = year_pred))
  tibble(
    Industry = unique(df$Industry),
    Employees_2024 = df %>% filter(Year == 2024) %>% pull(Employees),
    Predicted_2030 = round(predicted),
    Projected_Growth_Percent = ((predicted - Employees_2024) / Employees_2024) * 100
  )
}

predictions_2030 <- bres_long %>%
  group_by(Industry) %>%
  group_modify(~fit_predict_industry(.x)) %>%
  ungroup() %>%
  arrange(desc(Projected_Growth_Percent))

# Bar chart of projected growth
ggplot(predictions_2030, aes(reorder(Industry, Projected_Growth_Percent), Projected_Growth_Percent, fill = Projected_Growth_Percent > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "firebrick")) +
  coord_flip() +
  labs(title = "Predicted Employment Growth by Industry (2024–2030)",
       x = "Industry", y = "Projected Growth (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 19, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 24))

ggsave("predicted_employment_change.png", width = 15, height = 12, dpi = 300)

# Historical and 2030 line plot
bres_long_2030 <- bres_long %>%
  bind_rows(predictions_2030 %>% transmute(Industry, Year = 2030, Employees = Predicted_2030))

ggplot(bres_long_2030, aes(Year, Employees, colour = Industry)) +
  geom_line(size = 1.2) +
  geom_point(data = bres_long_2030 %>% filter(Year == 2030), size = 3, shape = 17) +
  scale_y_continuous(labels = comma) +
  labs(title = "Historical Employment Trends and 2030 Predictions",
       x = "Year", y = "Number of Employees") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 17),
        legend.key.height = unit(1.2, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 3))

ggsave("2030_predictions_line_graph.png", width = 16, height = 12, dpi = 300)

# Pie chart of predicted 2030 employment
predictions_2030_pie <- predictions_2030 %>%
  mutate(Share = Predicted_2030 / sum(Predicted_2030) * 100,
         Label = paste0(round(Share, 1), "%"))

ggplot(predictions_2030_pie, aes(x = 1, y = Share, fill = Industry)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5, colour = "black") +
  labs(title = "Predicted Employment Distribution by Industry (2030)", fill = "Industry") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
        legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 16),
        legend.position = "right")

ggsave("predicted_2030_employment_piechart.png", width = 14, height = 12, dpi = 300)

