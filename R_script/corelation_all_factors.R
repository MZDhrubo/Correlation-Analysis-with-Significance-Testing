library(ggplot2)
library(ggpubr)
library(ggthemes)# for stat_cor()
install.packages('ggthemes')
library(hrbrthemes)
library(dplyr)

#load data
ffa_data <- readxl::read_excel('data/FFA_&_Lipid.xlsx')

#corelation between lipid and ffa

ggplot(ffa_data, aes(x = Lipid, y = FFA)) +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  stat_cor(
    method  = "pearson",
    label.x = min(ffa_data$Lipid),
    label.y = max(ffa_data$FFA) * 0.9
  ) +
  labs(x = "Lipid (avg.)", y = "FFA (avg.)") +
  theme_tufte()

#corelation between ffa & total_wt_vicera

data_muscle2 <- data_muscle %>%
  rename(
    Length_cm         = `Length(cm)`,
    Viscera_wt_g      = `Total_Wt_of_viscera(g)`
  )
# Pearson test
cor.test(data_muscle2$Length_cm,
         data_muscle2$Viscera_wt_g,
         method = "pearson")
# scatter + lm + 95% CI + r & p
p3 <- ggplot(data_muscle2,
             aes(x = Length_cm,
                 y = Viscera_wt_g)) +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm",
              color  = "red",
              fill   = "#69b3a2",
              se     = TRUE) +
  stat_cor(method    = "pearson",
           label.x   = min(data_muscle2$Length_cm),
           label.y   = max(data_muscle2$Viscera_wt_g) * 0.9) +
  labs(x = "Length (cm)",
       y = "Total Viscera Weight (g)") +
  theme_minimal()

print(p3)

##corelation between ffa & total_wt_vicera

# scatter + lm + 95% CI + r & p
p4 <- ggplot(data_muscle2,
             aes(x = FFA,
                 y = Viscera_wt_g)) +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm",
              color  = "red",
              fill   = "#69b3a2",
              se     = TRUE) +
  stat_cor(method    = "pearson",
           label.x   = min(data_muscle2$FFA),
           label.y   = max(data_muscle2$Viscera_wt_g) * 0.9) +
  labs(x = "FFA",
       y = "Total Viscera Weight (g)") +
  theme_minimal()

print(p4)

##corelation between ffa & moisture

# scatter + lm + 95% CI + r & p
p5 <- ggplot(data_muscle2,
             aes(x = FFA,
                 y = moisture)) +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm",
              color  = "red",
              fill   = "#69b3a2",
              se     = TRUE) +
  stat_cor(method    = "pearson",
           label.x   = min(data_muscle2$FFA),
           label.y   = max(data_muscle2$moisture) * 0.9) +
  labs(x = "FFA",
       y = "Moisture") +
  theme_minimal()

print(p5)

##corelation between ffa & length

# scatter + lm + 95% CI + r & p
p6 <- ggplot(data_muscle2,
             aes(x = FFA,
                 y = Length_cm)) +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm",
              color  = "red",
              fill   = "#69b3a2",
              se     = TRUE) +
  stat_cor(method    = "pearson",
           label.x   = min(data_muscle2$FFA),
           label.y   = max(data_muscle2$Length_cm) * 0.9) +
  labs(x = "FFA",
       y = "Length (cm)") +
  theme_minimal()

print(p6)


##corelation between lipid & moisture
# scatter + lm + 95% CI + r & p
p7 <- ggplot(data_muscle2,
             aes(x = lipid,
                 y = moisture)) +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm",
              color  = "red",
              fill   = "#69b3a2",
              se     = TRUE) +
  stat_cor(method    = "pearson",
           label.x   = min(data_muscle2$lipid),
           label.y   = max(data_muscle2$moisture) * 0.9) +
  labs(x = "Lipid",
       y = "Moisture") +
  theme_minimal()

print(p7)






