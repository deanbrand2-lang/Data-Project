# חבילות
library(readr)
library(dplyr)
library(ggplot2)
library(gt)          

# נתונים + שכר חודשי
df <- read_csv("Employers_data.csv", show_col_types = FALSE) %>%
  mutate(Monthly_Salary = Salary / 12)

# סדר רמות השכלה לפי חציון שכר
if ("Education_Level" %in% names(df)) {
  ord_levels <- df %>%
    group_by(Education_Level) %>%
    summarise(med = median(Monthly_Salary), .groups = "drop") %>%
    arrange(med) %>%
    pull(Education_Level)
  df <- df %>%
    mutate(Education_Level = factor(Education_Level, levels = ord_levels))
}

base_sz <- 14

# 1) התפלגות שכר חודשי 
p_salary <- ggplot(df, aes(x = Monthly_Salary)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  geom_vline(xintercept = mean(df$Monthly_Salary, na.rm = TRUE),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median(df$Monthly_Salary, na.rm = TRUE),
             color = "darkgreen", linetype = "dotted", linewidth = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "משכורת חודשית",
       subtitle = "קו אדום = ממוצע | קו ירוק = חציון",
       x = "שכר חודשי (דולר)", y = "צפיפות") +
  theme_minimal(base_size = base_sz)
print(p_salary)

# 2) פער מגדרי 
p_gender <- ggplot(df, aes(x = Gender, y = Monthly_Salary, fill = Gender)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA, fill = "white") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "התפלגות שכר חודשי לפי מגדר",
       x = "מגדר", y = "שכר חודשי (דולר)") +
  theme_minimal(base_size = base_sz) +
  theme(legend.position = "none")
print(p_gender)

# 3) השכלה ושכר 
p_edu <- ggplot(df, aes(x = Education_Level, y = Monthly_Salary, fill = Education_Level)) +
  geom_jitter(aes(color = Education_Level), width = 0.15, alpha = 0.15,
              size = 1.2, show.legend = FALSE) +
  geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.75, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5,
               fill = "white", color = "black") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "שכר חודשי לפי רמת השכלה",
       x = "רמת השכלה", y = "שכר חודשי (דולר)") +
  theme_minimal(base_size = base_sz) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 8)),
        axis.title.y = element_text(margin = margin(r = 8)),
        legend.position = "none")
print(p_edu)

# 4) גיל מול שכר 
p_age <- ggplot(df, aes(x = Age, y = Monthly_Salary)) +
  geom_point(aes(color = Age), size = 1.8, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 1.2) +
  scale_color_gradient(low = "skyblue", high = "darkblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "שכר חודשי לפי גיל",
       x = "גיל (שנים)", y = "שכר חודשי (דולר)") +
  theme_minimal(base_size = base_sz) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none",
        panel.grid.minor = element_blank())
print(p_age)

# 5) מיקום ושכר
ci_df <- df %>%
  group_by(Location) %>%
  summarise(
    N      = n(),
    mean   = mean(Monthly_Salary),
    sd     = sd(Monthly_Salary),
    se     = sd / sqrt(N),
    tcrit  = qt(0.975, df = N - 1),
    ci_low = mean - tcrit * se,
    ci_high= mean + tcrit * se,
    .groups = "drop"
  ) %>%
  arrange(desc(mean)) %>%
  mutate(Location = factor(Location, levels = Location))

p_loc <- ggplot(ci_df, aes(x = Location, y = mean)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, linewidth = 0.8) +
  geom_point(size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "קשר בין מיקום לשכר חודשי",
       x = "מיקום", y = "שכר חודשי (דולר)") +
  theme_minimal(base_size = base_sz) +
  theme(panel.grid.minor = element_blank())
print(p_loc)




