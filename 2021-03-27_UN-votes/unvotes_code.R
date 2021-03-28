library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(patchwork)
library(maps)
library(ggtext)

#---------------------------- Get data ---------------------------------------#

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

# Development index data downloaded from here: https://worldpopulationreview.com/country-rankings/developed-countries

developmentindex <- read.csv("2021-03-27_UN-votes/developmentindex.csv") %>%
  rename(country = `ï..country`) %>%
  mutate(country = str_replace(country, " and (the )?", " & ")) %>%
  mutate(country = str_replace(country, "Saint", "St.")) %>%
  mutate(country = case_when(country == "Micronesia" ~ "Micronesia (Federated States of)",
                             country == "Ivory Coast" ~ "Côte d’Ivoire",
                             country == "Sao Tome & Principe" ~ "São Tomé & Príncipe",
                             country == "Czech Republic" ~ "Czechia",
                             country == "Republic of the Congo" ~ "Congo - Brazzaville",
                             country == "Myanmar" ~ "Myanmar (Burma)",
                             country == "DR Congo" ~ "Congo - Kinshasa",
                             country == "Macedonia" ~ "North Macedonia",
                             TRUE ~ as.character(country)))

#------------------------- Binding datasets -----------------------------------#

colonialism <- issues %>%
  left_join(roll_calls, by = "rcid") %>%
  left_join(unvotes, by = "rcid") %>%
  left_join(developmentindex, by = "country") %>%
  filter(issue == "Colonialism")

#----------------------- Wrangling for plots ----------------------------------#

colonialism_2000s  <- colonialism %>%
  filter(str_detect(date, "201[0-9]")) %>%
  mutate("developed" = ifelse(humanDevelopmentIndex >= 0.8, "Developed \n(HDI > 0.8)", "Developing \n(HDI < 0.8)"),
         "vote" = factor(vote, levels = c("yes", "abstain", "no"))) %>%
  mutate(country = forcats::fct_reorder(country, as.numeric(vote), .fun = mean)) %>%
  filter(! is.na(developed))

data_for_map <- colonialism_2000s %>%
  group_by(country_code, country) %>%
  summarise("num_yes" = sum(vote == "yes"),
            "total_votes" = n(),
            "devindex" = mean(humanDevelopmentIndex)) %>%
  mutate("prop_yes" = num_yes/total_votes)


votes_world <- map_data("world") %>%
  rename(mapname = region) %>%
  left_join(iso3166, by = "mapname") %>%
  rename(country_code = a2) %>%
  left_join(data_for_map, by = "country_code")

#--------------------------- Individual plots ---------------------------------#

plot_bars_dev <- colonialism_2000s %>%
  filter(developed  == "Developed \n(HDI > 0.8)") %>%
  ggplot(aes(x = country, colour = vote, fill = vote)) +
  geom_bar(position="fill") +
  coord_flip() +
  scale_colour_manual(values = c("#5b92e5", "lightgray", "#DB7E12"),
                      aesthetics = c("fill", "colour")) +
  labs(y = "Proportion of votes",
       x = "Country",
       title = "Developed countries",
       subtitle = "(HDI* > 0.8)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 5, hjust = 0.5),
        axis.title = element_text(size = 8),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        legend.position = "none")

plot_bars_mlic <- colonialism_2000s %>%
  filter(developed  == "Developing \n(HDI < 0.8)") %>%
  ggplot(aes(x = country, colour = vote, fill = vote)) +
  geom_bar(position="fill") +
  coord_flip() +
  scale_colour_manual(aesthetics = c("fill", "colour"),
                      breaks = c("yes", "no", "abstain"),
                      labels = c("Yes", "No", "Abstain"),
                      values = c("#5b92e5", "#DB7E12", "lightgray")) +
  labs(y = "Proportion of votes",
       x = "Country",
       title = "Developing countries",
       subtitle = "(HDI* < 0.8)",
       fill = "", color = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 5, hjust = 0.5),
        axis.title = element_text(size = 8),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        legend.position = "bottom",
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 8))

votes_map <- ggplot(votes_world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = prop_yes), colour = "#e9e9e9", size = 0.01) +
  scale_fill_gradient(low = "#db7e12", high = "#5b92e5", na.value = "#e9e9e9") +
  labs(fill = "Proportion of \nsupported motions**") +
  theme_void() +
  theme(legend.position = c(0.5,0),
        legend.direction = "horizontal",
        legend.key.size = unit(0.7, "cm"),
        legend.background = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size =8),
        plot.margin = margin(0,0,0,0),
        plot.caption = element_text(face = "italic"))
votes_map
#---------------------------- Multiplot ---------------------------------------#

layout <- c(
  area(t = 1, l = 1, b = 3, r = 1.3),
  area(t = 4, l = 1, b = 7.5, r = 1.5),
  area(t = 1, l = 2, b = 7.5, r = 5.5)
)


(plot_bars_dev + plot_bars_mlic + votes_map) +
  plot_layout(design = layout) +
  plot_annotation(title = "<strong>Between 2000-2019 developed countries supported <span style='color:#db7e12'>less</span> <span style='color:#5b92e5'>United Nations</span> motions <br>related to colonialism.</strong>",
                  subtitle = "This is especially true for Israel, USA and Canada.",
                  caption = "* HDI = Human development Index. \n** Countries with missing values were excluded from the map.\nData: Haravard's dataverse. Citation: Voeten et al, 2013. Visualization: @alehsegura13",
                  theme = theme(
                    plot.title = element_markdown(size = 12),
                    plot.background = element_rect(fill = "#e9e9e9"),
                    plot.caption = element_text(face = "italic", size = 9)
                    )
                  )

ggsave("2021-03-27_UN-votes/2021-03-2_UN-votes.jpeg",
       dpi = 300, height = 4.5, width = 7.1)
