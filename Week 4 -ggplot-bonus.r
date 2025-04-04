# load library, including ggplot、 dplyr
library(tidyverse)

# ! set the working directory
PATH <- "/Users/gaoshaoyun/Documents/study/big data/r-ds/insurance"
FILE_NAME <- "Insurance_Scrubbed.csv"
INFILE <- paste(PATH, FILE_NAME, sep = "/")
setwd(PATH)


# ! read the data
insurance_data <- read.csv(FILE_NAME)
str(insurance_data)
summary(insurance_data)
head(insurance_data)


# create age groups with 5 year intervals, e.g., 0-5, 5-10, ..., 95-100
insurance_data <- insurance_data %>%
    mutate(age_group = cut(IMP_AGE,
        breaks = seq(0, 100, by = 5),
        labels = paste(seq(0, 95, by = 5), seq(5, 100, by = 5), sep = "-"),
        right = FALSE
    ))

# filter the data to include only those with accidents
accident_cost <- insurance_data %>%
    filter(TARGET_CLM_FLAG == 1) %>% # 只选择有车祸的记录
    group_by(age_group) %>%
    summarise(
        avg_cost = mean(TARGET_CLM_AMT, na.rm = TRUE),
        n = n()
    ) %>%
    filter(!is.na(age_group)) # 移除NA的年龄组

# plot the average cost of accidents by age group
# using ggplot to plot the bar chart
ggplot(accident_cost, aes(x = age_group, y = avg_cost)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    geom_text(
        aes(label = paste0("$", round(avg_cost))), # 修正：闭合括号
        vjust = -0.5,
        size = 3
    ) +
    labs(
        title = "Average Car Accident Cost by Age Group",
        x = "Age Group (years)",
        y = "Average Accident Cost ($)",
        caption = "Data shows average repair costs for drivers involved in accidents"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    scale_y_continuous(labels = scales::dollar)
