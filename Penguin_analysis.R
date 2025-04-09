---
title: "Penguins Behavioral Health Insights"
output: html_notebook
---

This is a practice project using the Palmer Perguins dataset.

Installing the package, library and dataset
```{r}

install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyr)
library(tidyverse)
  
data("penguins")

```

The questions I want to answer about this dataset:
-What species has the highest body mass?
-Are those species women or men?
-Are there any interesting trends by sex?

```{r}
summary(penguins)
```


Step 1: Data cleaning and transformation
```{r}
#Removing NAs
#In the summary function we see there are NA values in columns: bill_length, bill_depth, flipper_length body_mass, and sex

clean_penguins <-  penguins %>% drop_na(body_mass_g,sex)

is.na(clean_penguins)

#Finding the average based on species and sex
penguins_summary <- clean_penguins %>% 
   group_by(species, sex) %>%
  summarise(avg_body_mass = mean(body_mass_g),
            avg_flipper_length = mean(flipper_length_mm),
            .groups = "drop"
  )

#Re-arranging by average body mass 
penguins_summary %>% arrange(desc(avg_body_mass))


print(penguins_summary)

#Labeling penguins based on their body mass and its comparison to the median

clean_penguins <- clean_penguins %>% 
  group_by(species, sex) %>%
  mutate(body_mass_label = ifelse(body_mass_g <= median(body_mass_g), "Light", "Heavy")) %>%
  ungroup()

#Renaming a species  
clean_penguins <- clean_penguins %>% 
mutate(species = case_when(
  species == "Gentoo" ~ "Gentoo Penguin",
  TRUE ~ species
))
    
print(clean_penguins)
print(clean_penguins %>% distinct(species))
         
         
```

Step 2: Visualizing 

```{r}
#Bar chart - counting penguins per species filled by sex

bar_plot <- ggplot(
  clean_penguins, aes(x = species, fill = sex)) +
         geom_bar() +
  labs(
    title = "Penguin Count by Species and Sex",
    x = "Species",
    y = "Count",
    fill = "Sex"
  ) +
  theme_minimal()

#Boxplot - Compare body mass across species and sex

box_plot <-ggplot(
  clean_penguins, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
    labs(
    title = "Body Mass by Species and Sex",
    x = "Species",
    y = "Body Mass (g)",
    fill = "Sex"
  ) +
  theme_minimal()

#Scatterplot - show relationship between flipper and body mass, circles are enlarged by sex

scatter_plot <- ggplot(
  clean_penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species, size = sex )) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Relationship between flipper length and body mass",
    x = "Flipper Length",
    y = "Body Mass (g)",
    color = "Species",
    size = "Sex"
  ) +
  theme_minimal()



#Trying other visualizations

bar_plot_flipper_species <- ggplot(
  clean_penguins, aes(x = flipper_length_mm, fill = species)) +
  geom_bar() +
  labs(
    title = "Relationship between flipper length and body mass",
    x = "Flipper Length",
    y = "Body Mass (g)",
    color = "Species",
    size = "Sex"
  ) +
  theme_minimal()


scatter_plot_sizecolor <- ggplot(
  clean_penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
     title = "Relationship between Flipper Length and Body Mass",
     x = "Flipper Length (mm)",
     y = "Body Mass (g)",
     color = "Species"
   ) +
   theme_minimal()


 ggsave("bar_plot.png", plot = bar_plot, width = 8, height = 5)
 ggsave("box_plot.png", plot = box_plot, width = 8, height = 5)
 ggsave("scatter_plot.png", plot = scatter_plot, width = 8, height = 5)
 ggsave("bar_plot_flipper_species.png", plot = bar_plot, width = 8, height = 5)
 ggsave("scatter_plot_sizecolor.png", plot = scatter_plot, width = 8, height = 5)
 
```

Step 3: Anaylsis

```{r}
#1. What species has the highest body mass?
#Below we observe that Gentoo Penguin has the highest average body mass across all species, at 5092 grams. Gentoo Penguin have the highest median body mass as well at 5050 grams. 

summary_stats <- clean_penguins %>%
  group_by(species) %>%
  summarize(avg_mass = mean(body_mass_g, na.rm = TRUE)) %>%
  arrange(desc(avg_mass))
print(summary_stats)

clean_penguins %>%
  group_by(species) %>%
  summarize(median_mass = median(body_mass_g, na.rm = TRUE)) %>%
  arrange(desc(median_mass))



#2. Are those species women or men?
#We see Gentoo Penguin have the highest body mass no matter what the sex is. The next is Adelie males then Chinstrap males. So we can see males usually have the highest body mass in all species. 

summary_stats_sex <- clean_penguins %>%
  group_by(species, sex) %>%
  summarize(avg_mass = mean(body_mass_g, na.rm = TRUE)) %>%
  arrange(desc(avg_mass))
print(summary_stats_sex)



#3. Are there any interesting trends by sex?
#An interesting trend is that among the penguins labeled 'heavy' the male Chinestrap Penguins have the largest bill length.

bill_length_summary <- clean_penguins %>%
    filter(body_mass_label == "Heavy") %>%
  group_by(species, sex) %>%
  summarize(avg_bill_length = mean(bill_length_mm),
            .groups = "drop") %>%
  arrange(desc(avg_bill_length))

print(bill_length_summary)


#Lets look at all penguins - with the body mass label of 'Heavy' and 'Light'. 
#We observe that Chinestrap males have the largest bill length no matter their body mass. 

bill_trend <- clean_penguins %>%
  group_by(species, sex, body_mass_label) %>%
  summarize(avg_bill_length = mean(bill_length_mm), 
            .groups = "drop") %>%
  arrange(desc(avg_bill_length))

print(bill_trend)


```
