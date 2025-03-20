#------------- Loading packages -------------
install.packages("MASS")
pacman::p_load("tidyverse", "dplyr", "ggplot2", "gridExtra", "purr", "psych", "MASS")


#------------- Setting working directory -------------

getwd()
setwd("/Users/justina/Desktop/internship")


#------------- Loading and cleaning data -------------

data <- read.csv("Internship Data.csv", sep = ";", na.strings = c("", " "), stringsAsFactors = FALSE)
head(data)

data_c1 <- data[1, ]                       # Saving the first row just in case 

data <- data[-1, ]                         # Removing the first row - more convenient

text_data <- data[, 7]                     # Saving text data
clean_text <- text_data[!is.na(text_data)]   # Cleaning up text data for analysis
class(clean_text)
#clean_text[1]
clean_text_df <- data.frame(text = clean_text)

write_csv(clean_text_df, "text-analysis.csv")

data <- data[, -7]                         # Removing the 7th column from the dataset (text column)

data <- data[-c(263, 285, 298, 322, 353, 377, 462, 485, 490, 511, 525), ] # Removing rows that contain NAs in all answers

#------------- Adjusting data -------------

data <- data %>% mutate(across(everything(), as.factor))

#data <- data %>% mutate(across(c(ID, Gender, Country, IsManager, Management.Unit.Name, Group.Band), as.factor))
#data <- data %>% mutate(across(c(Q3, Q5, Q9, Q13, Q5.1, Q7, Q9.1, Q11, Q12, Q14, Q15.1, Q16, Q18, Q19, Q21, Q22, Q24, Q25, Q27, Q28, Q29, Q30, Q31), as.numeric))

# Saving only likert-scale data

likert_data <- data[, 3:25]
likert_data <- likert_data %>% mutate(across(everything(), as.numeric))
full_likert_data <- likert_data # Back-up

summary(likert_data) # Quick overview

likert_data <- likert_data[, -c(21, 23)]
summary(likert_data)

# Removing rows with NAs:
likert_data_clean <- likert_data[complete.cases(likert_data), ]
summary(likert_data_clean)

data_c1 <- data_c1[, -c(1, 2, 24, 26:31)]

data_c1 <- data_c1 %>%
  pivot_longer(cols = everything(), names_to = "Q", values_to = "t")

#------------- Plotting simple histograms for inspection -------------

p1 <- ggplot(likert_data_clean, aes(x = Q3)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "Core EX Metric: Experience New Hire Week 1", x = "Value [Q3]", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p2 <- ggplot(likert_data_clean, aes(x = Q5)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "KPI: Belonging - I feel that I belong at Grundfos", x = "Value [Q5]", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p2_1 <- ggplot(likert_data_clean, aes(x = Q5.1)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have a clear understanding of my onboarding activities for the next month", x = "Value [Q5.1]", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p3 <- ggplot(likert_data_clean, aes(x = Q7)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I feel that I had a good start at Grundfos", x = "Value [Q7]", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p4 <- ggplot(likert_data_clean, aes(x = Q9)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "Driver: Manager support - Actively supporting onboarding", x = "Value [Q9]", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p4_2 <- ggplot(likert_data_clean, aes(x = Q9.1)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "My leader reached out to me before my start date", x = "Value [Q9.1]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p5 <- ggplot(likert_data_clean, aes(x = Q11)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have my access cards to Grundfos physical locations", x = "Value [Q11]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p6 <- ggplot(likert_data_clean, aes(x = Q12)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "My workstation is set up", x = "Value [Q12]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p7 <- ggplot(likert_data_clean, aes(x = Q13)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I would recommend Grundfos to people I know as a great place to work", x = "Value [Q13]", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p8 <- ggplot(likert_data_clean, aes(x = Q14)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have the IS equipment that I need e.g. PC, phone", x = "Value [Q14]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p9 <- ggplot(likert_data_clean, aes(x = Q15.1)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have my email account", x = "Value [Q15.1]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p10 <- ggplot(likert_data_clean, aes(x = Q16)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have the system accesses that I need", x = "Value [Q16]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p11 <- ggplot(likert_data_clean, aes(x = Q18)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "My credit card was ordered", x = "Value [Q18]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p12 <- ggplot(likert_data_clean, aes(x = Q19)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "My business cards were ordered", x = "Value [Q19]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p13 <- ggplot(likert_data_clean, aes(x = Q21)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have received local HR orientation", x = "Value [Q21]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p14 <- ggplot(likert_data_clean, aes(x = Q22)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have received local Health & Safety training", x = "Value [Q22]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p15 <- ggplot(likert_data_clean, aes(x = Q24)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I received an onboarding plan", x = "Value [Q24]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p16 <- ggplot(likert_data_clean, aes(x = Q25)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I met my team, onsite or virtual.  (If you do not have a team, please choose N/A)", x = "Value [Q25]", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p17 <- ggplot(likert_data_clean, aes(x = Q27)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I understand my role and what I am hired to do", x = "Value [Q27]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p18 <- ggplot(likert_data_clean, aes(x = Q28)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have a Buddy assigned", x = "Value [Q28]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p19 <- ggplot(full_likert_data, aes(x = Q29)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "My Buddy is active and helpful in my onboarding activities", x = "Value [Q29]", y = "Frequency") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p20 <- ggplot(likert_data_clean, aes(x = Q30)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "I have a Host Manager / location host assigned", x = "Value [Q30]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

p21 <- ggplot(full_likert_data, aes(x = Q31)) +
  geom_histogram(binwidth = 0.5, fill = "royalblue1", alpha = 0.5) +
  labs(title = "My Host Manager is active and helpful in my onboarding activities", x = "Value [Q31]", y = "Frequency") +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        panel.grid.major.y = element_line(color = "gray", size = 0.1))

#------------- Plotting the overview -------------

ggplot(data, aes(x = Management.Unit.Name, fill = Gender)) + 
  geom_bar(stat = "count", position = "dodge", width = 0.6) + 
  labs(title = "Distribution of gender across management units",
       x = "Management Unit",
       y = "Count",
       fill = "Gender") + 
  scale_fill_brewer(palette = "Blues") + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        panel.grid.major.y = element_line(color = "gray", size = 0.1),
        axis.text.x = element_text(angle = 60, hjust = 1)) +  # Rotate x-axis labels
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.8), vjust = -0.5, size = 3)


top_10_countries <- data %>%
  count(Country, sort = TRUE) %>%
  top_n(10, n)

filtered_data <- data %>% 
  filter(Country %in% top_10_countries$Country)

ggplot(filtered_data, aes(x = fct_infreq(Country))) + 
  geom_bar(stat = "count", fill = "#99CCFF") +
  labs(title = "Top 10 countries by survey respondents",
       x = "Country", 
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        panel.grid.major.y = element_line(color = "gray", size = 0.1)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3)

#------------- Likert scale plotting -------------

# Factor analysis
df_cor <- cor(likert_data_clean)
heatmap(df_cor)

fa.parallel(df_cor, fa = "fa") # 3 factors

fa_max <- fa(likert_data_clean, 3)
fa_max

print(fa_max$loadings, cutoff = 0.2)

# Plotting plots ~based on factor analysis

grid.arrange(p1, p2, p2_1, p3, p4, p7, ncol = 3)
grid.arrange(p4_2, p13, p14, p15, p17, p18, p20, ncol = 4)
grid.arrange(p5, p6, p8, p9, p10, p11, p12, ncol = 4)
grid.arrange(p19, p21, ncol = 2)

#------------- Ordinal Logistic Regression M1 -------------
likert_data_clean <- likert_data_clean %>% mutate(across(everything(), as.factor))

m1 <- polr(Q3 ~ Q11 + Q12 + Q14 + Q15.1 + Q16 + Q18 + Q19, data = likert_data_clean, method = "logistic")

summary(m1)

coefs <- summary(m1)$coefficients[, "Value"]
se <- summary(m1)$coefficients[, "Std. Error"]

# Calculate 95% confidence intervals
lower <- coefs - 1.96 * se
upper <- coefs + 1.96 * se

# Create a data frame for plotting
coef_df_m1 <- data.frame(
  Predictor = names(coefs),
  Coefficient = coefs,
  Lower = lower,
  Upper = upper
)

coef_df_m1 <- coef_df_m1[-c(1, 2, 5, 6, 7, 8, 10, 11, 12, 14, 15, 16, 17), ] 

ggplot(coef_df_m1, aes(x = Predictor, y = Coefficient, ymin = Lower, ymax = Upper)) +
  geom_pointrange() +
  theme_classic() +
  labs(title = "Predictors influencing 1st week's experience",
       x = "Predictor (yes, maybe, no)",
       y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(color = "gray", size = 0.1),
        plot.title = element_text(hjust = 0.5, size = 12))

#------------- Ordinal Logistic Regression M2 -------------
m2 <- polr(Q3 ~ Q9.1 + Q21 + Q22 + Q24 + Q27 + Q28 + Q30, data = likert_data_clean, method = "logistic")

summary(m2)

coefs_m2 <- summary(m2)$coefficients[, "Value"]
se_m2 <- summary(m2)$coefficients[, "Std. Error"]

# Calculate 95% confidence intervals
lower <- coefs_m2 - 1.96 * se_m2
upper <- coefs_m2 + 1.96 * se_m2

# Create a data frame for plotting
coef_df_m2 <- data.frame(
  Predictor = names(coefs_m2),
  Coefficient = coefs_m2,
  Lower = lower,
  Upper = upper
)

coef_df_m2 <- coef_df_m2[-c(1, 2, 3, 8, 9, 10), ] 

ggplot(coef_df_m2, aes(x = Predictor, y = Coefficient, ymin = Lower, ymax = Upper)) +
  geom_pointrange() +
  theme_classic() +
  labs(title = "Predictors influencing 1st week's experience",
       x = "Predictor (yes, no)",
       y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(color = "gray", size = 0.1),
        plot.title = element_text(hjust = 0.5, size = 12))

#------------- Ordinal Logistic Regression M3 -------------

full_likert_data <- full_likert_data[,-c(2:20, 22)]
full_likert_data <- full_likert_data[complete.cases(full_likert_data), ]

full_likert_data <- full_likert_data %>% mutate(across(everything(), as.factor))

m3 <- polr(Q3 ~ Q29 + Q31, data = full_likert_data, method = "logistic")

summary(m3)

coefs_m3 <- summary(m3)$coefficients[, "Value"]
se_m3 <- summary(m3)$coefficients[, "Std. Error"]

# Calculate 95% confidence intervals
lower <- coefs_m3 - 1.96 * se_m3
upper <- coefs_m3 + 1.96 * se_m3

# Create a data frame for plotting
coef_df_m3 <- data.frame(
  Predictor = names(coefs_m3),
  Coefficient = coefs_m3,
  Lower = lower,
  Upper = upper
)

coef_df_m3 <- coef_df_m3[-c(1, 2, 3, 6, 7, 8), ] 

ggplot(coef_df_m3, aes(x = Predictor, y = Coefficient, ymin = Lower, ymax = Upper)) +
  geom_pointrange() +
  theme_classic() +
  labs(title = "Predictors influencing 1st week's experience",
       x = "Predictor (1 to 5)",
       y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(color = "gray", size = 0.1),
        plot.title = element_text(hjust = 0.5, size = 12))
