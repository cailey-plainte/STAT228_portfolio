---
title: "Fueling the Footprint: Exploring How Parent Entity and Commoditiy Types Impact Carbon Emissions"
author: "Cailey Plainte"
output:
  html_document:
    df_print: paged
    toc: true
    toc_float: true
    theme: cerulean
---

```{r message=FALSE, warning=FALSE, include=FALSE}
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
```

```{r message=FALSE, warning=FALSE, include=FALSE}

emissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv')

```

## Introduction

The emission of greenhouse gases (GHG) is the driving force of climate change. As climate change continues to worsen, exacerbated by the growing energy requirements of our expanding population, it’s more important than ever to understand the factors that drive carbon emissions. While a lot of attention is focused on what companies or sectors produce the most carbon emissions, not as much focus is put on exploring the factors that contribute to these large numbers. This project aims to use historical carbon emissions data from the production of fossil fuels to explore whether a company’s parent entity type and the type of commodity it produces can be used to predict whether its annual carbon emissions will fall above or below the industry average for that year. Looking at the causative factors behind larger carbon emissions can lead to a better understanding of what contributes to increased GHG emissions, potentially informing mitigation strategies and regulatory policy.

The dataset I used, which can be found [here](https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv), was generated using data from the group Climate Majors. This group was able to compile a dataset of historical carbon emissions data spanning from 1854 to 2022 and including 180 of the world's largest fossil fuel producers. It provides information on year, parent entity, parent entity type, commodity produced, quantity of production, the units that production is measured in for the specified commodity, and the total amount of carbon emissions resulting from that commodity production, measured in metric tons. While this data does not cover all emissions globally or all sectors, Climate Majors claims that it covers 72% of all carbon emissions since the start of the Industrial Revolution, making it a relatively comprehensive dataset. 

## Wrangling The Data

To reach my goal of determining the potential predictive factors of high carbon emissions, I had to begin by simplifying the dataset to meet my needs. This began with me selecting the variables that I wanted to focus on, which were year, parent type, commodity, and total emissions. By using the select function, I was able to keep these variables in my dataset while excluding any others that would not be useful to my goal. 

Next, I grouped the dataset by year and used the mutate function to calculate the average total emissions for all producers in each year. I decided to compare each observation to the average carbon emissions for that particular year to gain more informative results. Since this dataset spans such a large period, it is important to consider the drastic jumps in carbon emission measurements as technology advances. With the data still grouped by year, I used the newly calculated yearly average and implemented ifelse statements to classify each observation as either above or below the average emissions for that year. This was saved into another variable called “above_average”.
This allowed me to explore whether certain features, specifically parent entity type and commodity produced, can predict whether a producer's emissions are above or below average in a given year.

```{r message=FALSE, warning=FALSE, include=FALSE}
emissions <- emissions |>
  select(year, parent_type, total_emissions_MtCO2e, commodity) |>
  group_by(year) |>
  mutate(
    year_avg = mean(total_emissions_MtCO2e, na.rm = TRUE),
    above_average = ifelse( total_emissions_MtCO2e > year_avg, "Above", "Below")) |>
  ungroup()
```

Following data tidying, the commodity type, parent entity type, and relationship to the average were all converted to factors for ease of use in visualizations and future modeling. 

```{r message=FALSE, warning=FALSE, include=FALSE}
emissions <- emissions |>
  mutate(commodity = factor(commodity, levels = c("Oil & NGL", "Bituminous Coal", "Natural Gas", "Metallurgical Coal", "Anthracite Coal", "Sub-Bituminous Coal", "Thermal Coal", "Lignite Coal", "Cement"))) |>
  mutate(parent_type = factor(parent_type, levels = c("State-owned Entity", "Investor-owned Company", "Nation State" ))) |>
  mutate(above_average = factor(above_average, levels = c("Above", "Below")))

```


## Visualization 

Using the wrangled data, I created visualizations to further my understanding of the predictive variables and see any patterns that may be reflected when modeling the data. I created two bar graphs that display the proportion of total emissions reports above or below the yearly average by parent entity type and commodity produced.

### Proportion of Emission Reports Above or Below\nYearly Average by Parent Entity Type

The first visualization, which explores parent entity type, shows that fossil fuel producers owned or controlled by national governments, referred to as nation-state parent entities, show a significantly higher proportion of emissions reports that are above the yearly average. Companies owned by state governments or private investors, while still having reports above average, have much lower proportions of emissions reports exceeding the average. This suggests that ownership of fossil fuel companies at a national level can potentially be associated with less environmentally friendly practices, making it an important factor to observe when thinking about GHG mitigation.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(emissions, aes(x = fct_reorder(parent_type, above_average == "Above", .fun = mean, .desc = TRUE), fill = above_average)) +
  geom_bar(position = "fill", stat = "count") +
  scale_fill_manual(values = c("Above" = "tomato3", "Below" = "palegreen4")) +
  labs(
    x = "Parent Entity Type",
    y = "Proportion of Yearly Reports Above or Below Average",
    fill = "Relationship to Average",
    title = "Proportion of Emission Reports Above or Below\nYearly Average by Parent Entity Type"
  ) +
   theme(
    axis.text.x = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.5, "cm")
  )
```

### Proportion of Emission Reports Above or Below\nYearly Average by Commodity Produced

The second visualization shows that the production of commodities such as bituminous coal, oil and NGL, and anthracite coal is associated with higher proportions of emissions reports that are above the yearly average than other commodities reported in this dataset. This pattern suggests that the type of commodity produced could act as a strong predictive variable when assessing whether a company is likely to report emissions above or below the average for a given year. Additionally, these insights can offer guidance for regulatory policies that target specific commodities, potentially enabling more effective emissions control strategies.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(emissions, aes(x = fct_reorder(commodity, above_average == "Above", .fun = mean, .desc = TRUE), fill = above_average)) +
  geom_bar(position = "fill", stat = "count") +
  scale_fill_manual(values = c("Above" = "tomato3", "Below" = "palegreen4")) +
  labs(
    x = "Commodity Produced",
    y = "Proportion of Yearly Reports Above or Below Average",
    fill = "Relationship to Average",
    title = "Proportion of Emission Reports Above or Below\nYearly Average by Commodity Produced"
  ) +
   theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.5, "cm")
  )

```

## Modeling The Data

To begin modeling this data to determine whether or not these explored variables could act as strong predictors for a company's yearly relationship to average emissions, I decided to produce a decision tree. When training the dataset, 80% of the data was used for training, leaving 20% available to test the decision tree. Since the decision tree is being used to predict if a certain report will be above or below average for that year, I used a classification tree, meaning that the parent_type and commodity variables were used to predict if a point was above or below average, rather than predicting the exact emission amount. Entities classified as having Nation State parent types, along with the production of commodities such as oil & NGL and bituminous coal, were strong indicators of reports falling into the above-average emissions category, reflecting what we saw with our earlier visualizations. However, only 69% of the data classified as above average was correctly predicted, making it important to evaluate the strength of the model.

```{r message=FALSE, warning=FALSE, include=FALSE}
set.seed(12345)

train_indexes <- createDataPartition(emissions$above_average, p = 0.80, list = FALSE )

train_emissions <- slice(emissions, train_indexes)
test_emissions <- slice(emissions, -train_indexes)

decision_tree <- rpart(above_average ~ parent_type + commodity, data = train_emissions, method = "class")
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
rpart.plot(
  decision_tree,
  type = 5,
  extra = 104,
 box.palette = "auto",
 cex = 0.4
)
```

## Evaluating The Model

To evaluate the model, I created an ROC curve to compare the ability of my model to predict true positives and true negatives. I then calculated the area under the ROC curve (AUC) to determine if my model was better than random at predicting if a report would be above or below average. The resulting AUC was 0.661, indicating that the model performs better than random guessing. While this performance is acceptable, there is substantial room for improvement. Adding more predictive variables, such as the quantity of production, could increase the ability of this model to accurately classify observations.

```{r echo=FALSE, message=FALSE, warning=FALSE}

emissions_prediction <- predict(decision_tree, newdata = test_emissions, type = "prob") [, "Above"]

roc_emissions <- roc(test_emissions$above_average, emissions_prediction)

auc_emissions <- auc(roc_emissions)

plot(roc_emissions, main = "ROC Curve for Decision Tree", print.auc = TRUE)

```

## Conclusion

This project aimed to explore whether certain company factors, specifically the parent entity type and commodities produced, can help predict whether a fossil fuel producer’s annual carbon emissions will fall above or below the industry average for that year. By using a relatively comprehensive dataset compiled by Climate Majors, which accounts for 72% of all carbon emissions since the Industrial Revolution, the analysis was able to provide meaningful insights into emission patterns and their potential drivers. The visualizations produced from this wrangled data revealed strong associations between above average emissions and certain company traits. Nation State companies, in particular, demonstrated a higher proportion above average emissions compared to those owned by state governments or private investors. Similarly, commodities such as oil & NGL, bituminous coal, and anthracite coal were seen to have these above average emission reports.

The decision tree model developed to predict above or below average emissions confirmed these trends. However, its performance, while better than random, has room for improvement. This suggests that parent entity type and commodity can not be reliably used as predictors on their own. 

One variable that could be added to the model is production quantity, which was not included in the initial model. Larger production quantities are likely linked to higher emissions, and incorporating this variable may help the model of emission behaviors improve in accuracy. Improving the accuracy of this model could potentially inform more targeted mitigation strategies for GHG emissions from fossil fuel production by identifying the largest contributing factors to emissions production rather than putting widespread, unspecific regulations on the entire industry.
