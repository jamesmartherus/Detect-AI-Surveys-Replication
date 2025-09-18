library(dplyr)
library(ggplot2)

data <- readRDS("data/replication_data_clean.rds") %>%
  mutate(date = lubridate::as_date(lubridate::mdy_hm(StartDate)),
         prompt = case_when(date < as.Date("2025-04-02") ~ "Initial Prompt",
                            date >= as.Date("2025-04-02") & date <= as.Date("2025-04-03") ~ "Profile 63 YO 3 Children Inc 150k",
                            date >= as.Date("2025-04-04") & date <= as.Date("2025-04-08") ~ "Profile 29 YO 2 Children Inc 30k",
                            date >= as.Date("2025-04-09") & date <= as.Date("2025-04-11") ~ "Profile 45 YO 1 Children Inc 12k"),
         age = Q48,
         derived_age = 2025 - Q49,
         age_consistent = case_when(abs(age - derived_age) <= 1 ~ TRUE,
                                    TRUE ~ FALSE),
         num_kids = Q33,
         kids_yn = Q50,
         kids_consistent = case_when(num_kids == 0 & kids_yn == 2 ~ TRUE,
                                     num_kids > 0 & kids_yn == 1 ~ TRUE,
                                     TRUE ~ FALSE),
         income_raw = Q51,
         income_bin = Q37,
         income_consistent = case_when(income_raw %in% 0:24999 & income_bin == 1 ~ TRUE,
                                       income_raw %in% 25000:49999 & income_bin == 2 ~ TRUE,
                                       income_raw %in% 50000:74999 & income_bin == 3 ~ TRUE,
                                       income_raw %in% 75000:99999 & income_bin == 4 ~ TRUE,
                                       income_raw %in% 100000:149999 & income_bin == 5 ~ TRUE,
                                       income_raw %in% 150000:1500000 & income_bin == 6 ~ TRUE,
                                       is.na(income_raw) | income_bin == 7 ~ NA,
                                       TRUE ~ FALSE))
  

### Image Recognition

# Fanta (multiple choice)
prop.table(table(data$Q16))

# Target (text entry)
table(data$Q17)

### Honeypot (response 5 was hidden)
table(data$Q28)

### Attention Check
# item 15 was an extremely unlikely event like "ride a dinosaur to work"
# Respondents should answer they did this activity "0 times" (category 1) in the past year
table(data$SM2_15)

### Consistency

# age vs birth year
prop.table(table(data$prompt, data$age_consistent), 1)

# Children under 18
prop.table(table(data$prompt, data$kids_consistent), 1)

# HH Income
prop.table(table(data$prompt == "Initial Prompt", data$income_consistent), 1)


### Directly asking if respondent is human or AI

data %>%
mutate(are_you_ai = case_when(Q43 == 1 ~ "I am an AI",
                              Q43 == 2 ~ "I am a real, live human"),
       are_you_ai = factor(are_you_ai, levels = c("I am an AI", "I am a real, live human"))) %>%
  filter(!is.na(are_you_ai)) %>%
  count(are_you_ai) %>%
  tidyr::complete(are_you_ai, fill = list(n=0)) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x = are_you_ai, y = prop, fill = are_you_ai)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("#4657ce", "#0dd6cc")) +
  labs(x = "", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none")
  
ggsave("figures/figure-2.png", width = 6, height = 4)

#### Reverse Shibboleth Questions
table(data$Q8) # Fortran "Hello World" program
table(data$Q10) # correct answer is 2
table(data$Q12) # correct answer is 3

### Copy-paste detection

# column with pasted text
data$pasted_text

### Proportion of responses with ZIP codes 90210, 90001, and 10001
prop.table(table(data$Q30 %in% c("90210", "90001", "10001")))

# Proportion claiming vanilla or chocolate are respondent's
# favorite flavor of ice cream
data %>%
  mutate(vanilla = stringr::str_detect(Q27, stringr::regex("vanilla|chocolate", ignore_case = TRUE))) %>%
  summarise(proportion = mean(vanilla))


### Paradata

# IP
data$IPAddress

# User Agent
data$user_agent

# Time zone
data$timezone

# Headless browser
data$headless_browser




