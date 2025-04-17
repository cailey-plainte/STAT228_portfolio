---
title: 'The Buzz on Colony Loss: What Colony Numbers Tell Us About Loss Percentages'
author: "Cailey Plainte"
date: "March 11th, 2025"
output:
  html_document:
    df_print: paged
---

```{r message=FALSE, warning=FALSE, include=FALSE}
library(tidyverse)
library(ggplot2)
library(ggthemes)
```

```{r message=FALSE, warning=FALSE, include=FALSE}
colonies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-01-11/colony.csv')
```

## **Introduction:**

Honeybees are essential to the health of ecosystems and global agriculture. In the United States agricultural system, they are critical in sustaining biodiversity and ensuring food security around the world. Unfortunately, in recent years, honeybee populations have been steadily declining, with increasing losses in colony numbers becoming a growing concern. As the environment continues to change, honeybees are increasingly vulnerable, and understanding the patterns behind their population declines has never been more urgent.

As climate change continues to reshape the environment, studying trends in honeybee colony numbers and the percentage of colonies lost has become more critical than ever. Understanding the relationship between honeybee colony numbers and the percentage of colonies lost is crucial for identifying the causes behind these declines and developing effective strategies for their conservation. By analyzing these trends, we can pinpoint the colony management factors that contribute to higher percentages of colony loss and, hopefully, identify best practices for colony management.

The dataset used for this report comes from the USDA. It provides information on the number of colonies, the maximum number of colonies, the number lost, the percent lost, the number added, the number renovated, and the percent renovated. Observations were separated by state, year, and season, giving a comprehensive view of how trends have potentially changed over time and how states implement different colony management practices. The dataset also includes observations recorded in other, unspecified areas, indicated as “United States” or “Other State”. For the purpose of this report, which will focus on comparing different states over the years, these data points have been disregarded.

```{r message=FALSE, warning=FALSE, include=FALSE}
colonies_single_states <- colonies |> 
  filter(!grepl("United States", state) &
         !grepl("Other States", state))

percent_loss_by_colony_n <- colonies_single_states |>
  group_by(state, year) |>
  mutate(avg_colony_n = mean(colony_n, na.rm = TRUE)) |>
  mutate(avg_loss_pct_state = mean(colony_lost_pct, na.rm = TRUE)) 
```

## **Figure 1:**

### Number of Colonies vs Percentage of Colonies Lost

```{r echo=TRUE, message=FALSE, warning=FALSE}
ggplot(
  data = percent_loss_by_colony_n,
  mapping = aes(x = avg_colony_n, y = avg_loss_pct_state)) +
  geom_point(size = .6, alpha = 0.2,
             mapping = aes(colour = year)) +
  geom_smooth(method = lm) +
  labs(
    x = "Average Number of Colonies",
    y = "Average Percentage of Colonies Lost",
    title = "Number of Colonies vs Percentage of Colonies Lost",
    subtitle = "Yearly Averages from Each State",
    color = "Year"
  )
```

The above plot aims to observe how the percentage of colonies lost changes as the number of colonies increases, hoping to potentially give insight into how increasing or decreasing colony numbers in an area impacts honeybee colony mortality rates. I chose to use a scatterplot to display this data as it had the greatest capacity to show a trend in how the average percentage of colonies lost changes as the number of colonies increases.

This plot uses yearly averages from different states in order to narrow down the colony numbers to a specific geographical area and possibly note any yearly patterns. By using averages to visually represent the data, we can mitigate the effect of potential outliers on the overall trend of the graph and increase the simplicity and readability of the plot by aggregating the data. The trendline on this figure shows that there is an overall decrease in the percentage of colonies lost as the number of colonies increases, indicating that increasing colony populations in a certain area can decrease the overall mortality rates of the honeybees. The colors on this plot, which indicate year, show no observable pattern in how this trends one way or another as time passes.

When looking at this plot, it is clear that there is a group of data points that are much higher than the rest and potentially skew the observed trend, leading me to my next figure where I attempt to uncover what those outlying points may be caused by.

```{r message=FALSE, warning=FALSE, include=FALSE}
colonies_n_by_state <- colonies_single_states |>
  group_by(state) |>
  summarize(colony_n_avg = mean(colony_n, na.rm = TRUE)) |>
  arrange(desc(colony_n_avg)) |>
  filter(row_number() <= 10)
```

## **Figure 2:**

### Top 10 States with The Most Honeybee Colonies

```{r echo=TRUE, message=FALSE, warning=FALSE}

ggplot( 
  data = colonies_n_by_state,
  mapping = aes(x = reorder(state, -colony_n_avg), y = colony_n_avg, fill = state)) +
  geom_bar(stat = "Identity") +
  labs(
    x = "State",
    y = "Total Number of Colonies",
    title = "Top 10 States with The Most Honeybee Colonies",
    subtitle = "Data from 2015-2021"
  ) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  theme(axis.title.x = element_text(hjust = .5)) +
  theme(legend.position = "none") +
  scale_fill_manual(values = scales::brewer_pal(palette = "YlGnBu")(length(unique(colonies_n_by_state$state))))
```

The plot above aims to identify the potential outlying factor that influences the results of Figure 1. To do this, I created a bar graph of the 10 states with the highest average colony numbers in order to see if these incredibly high numbers came from one source or potentially multiple sources of the highest honeybee yielding states.

I chose to use a bar graph to display this data as it is able to easily compare different groups side by side, clearly highlighting the significant difference in the average number of bee colonies recorded in California compared to the states with the nine next highest average recorded colony numbers. In this bar graph, each column is represented by the overall average number of honeybee colonies over the period that this data set encompasses. This provides a clear comparison between the states represented and mitigates yearly variations caused by potential stressors such as weather that we cannot identify from this dataset alone. The color in this graph is for aesthetic purposes only to better distinguish between the different states and does not aim to provide further information.

From this graph, we can see that California is likely the cause for these outliers, possibly due to its large size and agricultural presence. From the conclusions drawn in this figure, I then decided to revisit Figure 1 by removing the data points from California to see if these outliers had any significant impact on what we saw from the trend line in the first figure.

```{r message=FALSE, warning=FALSE, include=FALSE}
percent_loss_by_colony_n_no_california <- colonies_single_states |>
  filter(!grepl("California", state)) |>
  group_by(state, year) |>
  mutate(avg_colony_n = mean(colony_n, na.rm = TRUE)) |>
  mutate(avg_loss_pct_state = mean(colony_lost_pct, na.rm = TRUE)) 
```

## **Figure 3:**

### Number of Colonies vs Percentage of Colonies Lost Excluding California

```{r echo=TRUE, message=FALSE, warning=FALSE}
ggplot(
  data = percent_loss_by_colony_n_no_california,
  mapping = aes(x = avg_colony_n, y = avg_loss_pct_state)) +
  geom_point(size = .7, alpha = 0.2,
             mapping = aes(color = year)) +
  geom_smooth(method = lm) +
  labs(
    x = "Average Number of Colonies",
    y = "Average Percentage of Colonies Lost",
    title = "Number of Colonies vs Percentage of Colonies Lost",
    subtitle = "Yearly State Averages Excluding California",
    color = "Year"
  )
```

The plot above is virtually identical to Figure 1, yet it has one key difference: California is not included. I chose to use the same format with the scatterplot to see if the conclusions drawn from Figure 2 had any significant impact on what we saw previously. When revisiting this plot, now excluding California, we can see that while California had significantly larger colony numbers than any other state, the trend seems to remain the same when these points are removed. From this, we can conclude that increasing colony numbers has an overall positive impact on honeybee colony mortality by decreasing the percentage of colonies lost.

## **Discussion:**

The analysis of honeybee colony data reveals interesting insights into the relationship between colony numbers and the percentage of colonies lost. The initial scatterplot in Figure 1 suggests a negative correlation, meaning that as the number of colonies increases, the percentage of colonies lost tends to decrease. This is significant because it hints that areas with larger colony populations might be seeing better survival rates, possibly due to improved management practices or more favorable environmental conditions that support larger populations.

However, Figure 2 brings up an interesting point: California's much larger average colony numbers compared to the other states. This could have had a disproportionate influence on the overall results, so I took another look in Figure 3. After removing California's data, the trend in Figure 1 stayed pretty much the same, which suggests that, overall, more colonies are linked to lower mortality rates, regardless of California's large numbers.

This finding suggests that fostering larger, healthier colony populations could be an important strategy in reducing honeybee mortality. By focusing on strategies that promote larger, more stable colony populations, we may be able to better support our honeybee populations. However, further research and analysis are needed to explore the specific variables at play in different states and to ensure that effective conservation practices are applied across a variety of regions. It is possible that this data set was not able to capture the variability within large states such as California, disregarding potentially important regional differences. A potential future direction for analysis could involve conducting a more in-depth investigation specifically focused on California, given its significant contribution to the overall honeybee colony numbers in the United States.

## **Sources:**

"Honey Bee Colonies." USDA Economics, Statistics, and Market Information System. Retrieved from <https://usda.library.cornell.edu/concern/publications/rn301137d?locale=en>
