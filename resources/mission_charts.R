library(tidyverse)
library(openxlsx)
library(devtools)
library(gemini.R)
library(glue)
library(logger)
library(httr)
library(patchwork)
library(treemapify)

###
# Charts for Mission Dataset
###
df <- read.csv("./data/mission_dataset.csv")

# Chart #1 Missions by Region
df %>%
  count(Continent) %>%
  mutate(proportion = round(n/sum(n),2)*100) %>%
  arrange(proportion) %>%
  mutate(Continent = factor(Continent, levels = Continent)) %>%
  ggplot(aes(x=Continent, y=proportion)) +
  geom_bar(stat = "identity", fill="#097969") +
  geom_text(data = . %>%
              mutate(
                percent_lbl = as.character(round(proportion)),
                percent_lbl = paste0(percent_lbl, '%'),
              ),
            aes(x=Continent, y=proportion,label=percent_lbl),
            family = "Cairo",
            fontface='bold',
            size = 5,
            nudge_y=1.5,
            show.legend = FALSE)+
  coord_flip()+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = 0, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "italic"),
        plot.caption = element_text(hjust = 1, size = 8, face = "italic"))+
  scale_y_discrete(expand = expansion(mult = c(0.01,0.2))) +
  labs(caption = "Source: Wikipedia @mormon_metrics",
       subtitle = "Proportion of LDS Missions by Region Including 36 New Missions Announced Nov 2023",
       title = "Most Mission Calls Are to The Americas")

#Missions Inside the US
round(sum(df$inside_US == "Yes")/nrow(df),2)

# Chart 2: Us and UTAH vs outside

(p1 <- data.frame(
  region = c("In the US", "Outside the US"),
  proportion = c(0.27, 0.73)
) %>% 
    ggplot(aes(x =region, y = proportion, fill = region)) +
    geom_bar(stat = "identity", position = "stack", width = 1) +
    geom_text(aes(label = paste0(as.character(proportion*100),"%")),
              family = "Cairo", nudge_y = 0.03, size = 5, fontface = "bold") +
    theme(axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_blank(),
          text = element_text(face = "bold", family = "Cairo"),
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          legend.title = element_blank(),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          legend.position = "top",
          plot.caption = element_text(hjust = 1, size = 8, face = "italic"))+
    scale_fill_manual(values = c("dodgerblue3", "grey75")) + 
    ylim(0,1)+
    labs(
      subtitle = "Specifically, 120 out of the 450 missions are in the US",
      title = "Almost 3 in 10 go to the US"))

(p2 <- data.frame(
  region = c("In Utah", "Outside the Utah"),
  proportion = c(0.03, 0.97)
) %>% 
    ggplot(aes(x =region, y = proportion, fill = region)) +
    geom_bar(stat = "identity", position = "stack", width = 1) +
    geom_text(aes(label = paste0(as.character(proportion*100),"%")),
              family = "Cairo", nudge_y = 0.03, size = 5, fontface = "bold") +
    labs(title = "Simple Proportion Pie Chart") +
    ylim(0,1)+
    theme(axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_blank(),
          text = element_text(face = "bold", family = "Cairo"),
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          legend.title = element_blank(),
          plot.subtitle = element_text(hjust = 0.5, face = "italic"),
          legend.position = "top",
          plot.caption = element_text(hjust = 1, size = 8, face = "italic"))+
    scale_fill_manual(values = c("darkred", "grey75")) + 
    labs(caption = "Source: Wikipedia @mormon_metrics",
         subtitle = "Specifically, 13 out of the 450 missions are in Utah",
         title = "About 1 in 33 go to Utah"))

(p3 <-  p1+plot_spacer()+p2 + plot_layout(widths = c(8,1,8)))

#Missions Inside Utah
round(sum(df$inside_utah == "Yes")/nrow(df),2)
# 1 in 33 missionaries will go to Utah

# Chart 3
# Most likely mission languages
p4 <- df %>%
  mutate(language = ifelse(!language %in% c("English", "Spanish", "Portuguese", "Filipino", "French", "Japanese", "Swahili"), "Other", language)) %>% 
  count(language) %>%
  mutate(proportion = round(n/sum(n),2)*100) %>%
  arrange(proportion) %>%
  mutate(language = factor(language, levels = c("Other",language[language != "Other"]))) %>%
  ggplot(aes(x=language, y=proportion)) +
  geom_bar(stat = "identity", fill="#097969") +
  geom_text(data = . %>%
              mutate(
                percent_lbl = as.character(round(proportion)),
                percent_lbl = paste0(percent_lbl, '%'),
              ),
            aes(x=language, y=proportion,label=percent_lbl),
            family = "Cairo",
            fontface='bold',
            size = 5,
            nudge_y=4,
            show.legend = FALSE)+
  coord_flip()+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = 0, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "italic"),
        plot.caption = element_text(hjust = 1, size = 8, face = "italic"))+
  scale_y_discrete(expand = expansion(mult = c(0.01,0.2))) +
  ylim(0,100)+
  labs(
    subtitle = "Proportion of Most Common Languages by Mission Including 36 New Missions",
    title = "Top Mission Primary Language is English or Spanish")

# Chart #4 Religions by mission
p5 <- df %>%
  count(religion) %>%
  mutate(proportion = round(n/sum(n),2)*100) %>%
  arrange(proportion) %>%
  mutate(religion = factor(religion, levels = religion)) %>%
  ggplot(aes(x=religion, y=proportion)) +
  geom_bar(stat = "identity", fill="#097969") +
  geom_text(data = . %>%
              mutate(
                percent_lbl = as.character(round(proportion)),
                percent_lbl = paste0(percent_lbl, '%'),
              ),
            aes(x=religion, y=proportion,label=percent_lbl),
            family = "Cairo",
            fontface='bold',
            size = 5,
            nudge_y=4,
            show.legend = FALSE)+
  coord_flip()+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = 0, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "italic"),
        plot.caption = element_text(hjust = 1, size = 8, face = "italic"))+
  scale_y_discrete(expand = expansion(mult = c(0.01,0.2))) +
  labs(caption = "Source: Wikipedia @mormon_metrics",
       subtitle = "Proportion of LDS Missions by Most Popular Religion Including 36 New Missions",
       title = "The Majority Religion for Most Missions is Christianity")

(p6 <-  p4/p5)

# chart 5: persons per missionary

# I should also do when showing proportion regional population vs proportion regional missions
non_china_asia <- df %>% 
  filter(Continent == "Asia") %>% 
  group_by(Continent) %>% 
  reframe('mission_count'=n(), 'regional_population'=regional_population) %>% 
  filter(!duplicated(.)) %>% 
  mutate(
    regional_population = regional_population - 1400000000, #subtracting china
    missionaries = mission_count*160, #missionaries per mission
    persons_per_missionary = regional_population/missionaries,
    Continent = "Asia Without China"
  )

df %>% 
  group_by(Continent) %>% 
  reframe('mission_count'=n(), 'regional_population'=regional_population) %>% 
  filter(!duplicated(.)) %>% 
  mutate(
    missionaries = mission_count*200,
    persons_per_missionary = round(regional_population/missionaries)
  ) %>% 
  rows_append(non_china_asia) %>% 
  filter(Continent != "Asia") %>% 
  mutate(persons_per_missionary = round(persons_per_missionary),
         persons_per_missionary_char = format(persons_per_missionary, big.mark = ",", scientific = FALSE),
         mylabel = paste(Continent, persons_per_missionary_char, sep = "
")) %>% 
  ggplot(aes(area = as.numeric(persons_per_missionary), label = mylabel)) +
  geom_treemap(fill = "#097969") +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15,
                    family = "Cairo", 
                    fontface = "bold") +
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = 0, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "italic"),
        plot.caption = element_text(hjust = 1, size = 8, face = "italic"))+
  labs(caption = "Source: Wikipedia @mormon_metrics",
       subtitle = "Shows NUMBER OF PERSONS PER MISSIONARY by Region; Assuming 160 persons per mission",
       title = "Asia Undersaturated With Missionaries; North Am. Oversaturated")
