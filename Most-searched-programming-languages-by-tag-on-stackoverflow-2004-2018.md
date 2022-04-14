Most searched programming languages by tag on stackoverflow 2004-2018
================

### **Quick introduction**

------------------------------------------------------------------------

Anyone who has worked with programming languages knows how helpful
platforms such as Stack overflow are.By using appropriate tags, we gain
access to a treasure trove of knowledge which are answers of other users
who struggled with similar problems.

I want to find out how often R and Python and their libraries have been
searched over time. I also want to compare R and Python to other
programs that enable data analysis. Also to see which languages were
most searched in the period 2008 - 2018. I hope you enjoy this simple
analysis.

### **Data**

------------------------------------------------------------------------

The following analysis and visualizations are based on data downloaded
from DataCamp [link](https://projects.datacamp.com/projects/435). They
represent the number of searches for particular tags on the Stack
Overflow platform between 2008 and 2018.

### **Analysis**

------------------------------------------------------------------------

###### Data loading

For this analysis I use the R with the packages ‘tidyverse’,
‘lubridate’, ‘ggrepel’, ‘ggthemr’.

``` r
# Load libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemr)
ggthemr("fresh")
library(knitr)
```

``` r
# Load data
tag_by_year <- read_csv('by_tag_year.csv')
```

###### Checking data

A quick look at the data before analysis. Let’s see what the table looks
like and what the variable type is.

``` r
# check the data
glimpse(tag_by_year)
```

    ## Rows: 40,518
    ## Columns: 4
    ## $ year       <dbl> 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008,~
    ## $ tag        <chr> ".htaccess", ".net", ".net-2.0", ".net-3.5", ".net-4.0", ".~
    ## $ number     <dbl> 54, 5910, 289, 319, 6, 3, 1, 42, 19, 4, 73, 149, 10, 1, 5, ~
    ## $ year_total <dbl> 58390, 58390, 58390, 58390, 58390, 58390, 58390, 58390, 583~

``` r
summary(tag_by_year)
```

    ##       year          tag                number         year_total     
    ##  Min.   :2008   Length:40518       Min.   :     1   Min.   :  58390  
    ##  1st Qu.:2011   Class :character   1st Qu.:    99   1st Qu.:1085170  
    ##  Median :2013   Mode  :character   Median :   227   Median :1645404  
    ##  Mean   :2013                      Mean   :  1050   Mean   :1539065  
    ##  3rd Qu.:2016                      3rd Qu.:   540   3rd Qu.:2219527  
    ##  Max.   :2018                      Max.   :266762   Max.   :2305207

###### Change the data type

``` r
# Change the data type of the 'year' column
class(tag_by_year$year)
```

    ## [1] "numeric"

``` r
tag_by_year$year <- ymd(tag_by_year$year, truncated = 2L)
class(tag_by_year$year)
```

    ## [1] "Date"

###### Let’s look for missing values

``` r
# missing values 
print(sum(is.na(tag_by_year)))
```

    ## [1] 0

#### **1. Total searches over time**

------------------------------------------------------------------------

Before we look at the results of individual tag searches, let’s analyse
the number of total searches over the years.

``` r
total_searches_per_year <- tag_by_year %>% 
  group_by(year, year_total) %>% 
  summarise()

as.data.frame(total_searches_per_year)
```

    ##          year year_total
    ## 1  2008-01-01      58390
    ## 2  2009-01-01     343868
    ## 3  2010-01-01     694391
    ## 4  2011-01-01    1200551
    ## 5  2012-01-01    1645404
    ## 6  2013-01-01    2060473
    ## 7  2014-01-01    2164701
    ## 8  2015-01-01    2219527
    ## 9  2016-01-01    2226072
    ## 10 2017-01-01    2305207
    ## 11 2018-01-01    1085170

``` r
kable(total_searches_per_year, col.names = c("Year", "Total"))
```

| Year       | Total   |
|:-----------|--------:|
| 2008-01-01 |   58390 |
| 2009-01-01 |  343868 |
| 2010-01-01 |  694391 |
| 2011-01-01 | 1200551 |
| 2012-01-01 | 1645404 |
| 2013-01-01 | 2060473 |
| 2014-01-01 | 2164701 |
| 2015-01-01 | 2219527 |
| 2016-01-01 | 2226072 |
| 2017-01-01 | 2305207 |
| 2018-01-01 | 1085170 |

###### Let’s visualise it

``` r
ggplot(total_searches_per_year, aes(year, year_total)) +
  geom_line(color = "darkslategray", size = 1, linejoin = "round") +
  geom_point(color = 'chocolate3', size = 2) +
  ylim(c(0, 2500000)) +
  ggtitle("Total searches per year") +
  labs(x = "Year", y = "Number of searches" ) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, color = 'gray32'),
        axis.title.x = element_text(margin = margin(t = 10), size = 15, color = 'gray32'),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, color = 'gray32'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, color = "burlywood4"),
        axis.text = element_text(color = "burlywood4", size = 12)) +
  annotate(geom ="text", x=as.Date("2017-01-01"), y = 950000, 
           label="Sudden collapse. \n I suspect the data is incomplete.", size = 2, color = "burlywood4" )+
  annotate(geom ="point", x=as.Date("2018-01-01"), y = 1085170, size = 15, shape = 1, fill = "transparent")
```
![Total searches per year11](https://user-images.githubusercontent.com/101715443/163364469-aa1eeec3-8f60-477f-a31b-5af0cc95043f.png)

``` r
ggsave('Total searches per year', device = "png",
         width = 10, height = 4, units = "in", dpi = 600, type = "cairo-png")
```

#### **2. R vs Python**

------------------------------------------------------------------------

Let’s see how the percentage of searches for R and Python breaks down
compared to all languages.

###### First, add a column with percentages

``` r
# Add percentage column
tag_by_year_percentage <- tag_by_year %>%
  mutate(fraction = (number/year_total)*100)

# Print the new table
as.data.frame(head(tag_by_year_percentage))
```

    ##         year           tag number year_total     fraction
    ## 1 2008-01-01     .htaccess     54      58390  0.092481589
    ## 2 2008-01-01          .net   5910      58390 10.121596164
    ## 3 2008-01-01      .net-2.0    289      58390  0.494947765
    ## 4 2008-01-01      .net-3.5    319      58390  0.546326426
    ## 5 2008-01-01      .net-4.0      6      58390  0.010275732
    ## 6 2008-01-01 .net-assembly      3      58390  0.005137866

###### Then, filter out R and Python

``` r
# Filter for R and Python tags
r_python_over_time <- tag_by_year_percentage %>%
  filter(tag %in% c("r", "python"))
```

###### Visualize it

``` r
# Create a line plot of percentage over time
ggplot(r_python_over_time, aes(year, fraction, color = tag, size = 1)) +
  geom_line(lwd = 1, linejoin = "round") +
  geom_point(color = 'darkkhaki', size = 2) +
  ggtitle("Percentage searches for R and Python tags") +
  xlab("Year") +
  ylab("Percentage searches") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, color = 'gray32'),
        axis.title.x = element_text(margin = margin(t = 10), size = 15, color = 'gray32'),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, color = 'gray32'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, color = "burlywood4"),
        axis.text = element_text(color = "burlywood4", size = 12),
        legend.title = element_text(face = "bold"),
        legend.title.align=0.5) +
  labs(color = 'Tags')
```
![Percentage searches for R and Python tags](https://user-images.githubusercontent.com/101715443/163364984-d2168c05-8870-438a-a118-dae8441f791d.png)

``` r
ggsave('Percentage searches for R and Python tags', device = "png",
       width = 10, height = 4, units = "in", dpi = 600, type = "cairo")
```

#### **3. Let’s find out how often R/Python libraries were searched**

For this visualisation I’ve chosen basic packages useful for data
analysis.

``` r
# A vector of selected tags 
selected_tags <- c('dplyr', 'ggplot2', "tidyverse", 'matplotlib', 'pandas', 'numpy')

# Filter for those tags
selected_tags_over_time <- tag_by_year_percentage %>%
  filter(tag %in% selected_tags)
```

###### Visualize it

``` r
# Prepare a column to make the legend looks better
selected_tags_over_time$label <- NA
selected_tags_over_time$label[which(selected_tags_over_time$year == max(selected_tags_over_time$year))] <- selected_tags_over_time$tag[which(selected_tags_over_time$year == max(selected_tags_over_time$year))]

# Plot tags over time on a line plot using color to represent tag
ggplot(selected_tags_over_time, aes(year, fraction, color = tag)) +  
  geom_line(size = 1, linejoin = "round") +
  labs(title = 'How often were R/Python libraries searched?',
       x = 'Year',
       y = 'Percentage of searches') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, color = 'grey32'),
        axis.title.x = element_text(margin = margin(t = 10), size = 15, color = 'gray32'),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, color = 'gray32'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, color = "burlywood4"),
        axis.text = element_text(color = "burlywood4", size = 12),
        legend.title = element_text(face = "bold"),
        legend.title.align=0.5) +
  ylim(c(0, 1.5)) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  theme(legend.position = 'none')
```
![Python libraries searched](https://user-images.githubusercontent.com/101715443/163365020-0fe38e45-2e9f-4b20-9685-442a2f239c08.png)

``` r
ggsave('How often were R/Python libraries searched', device = "png",
       width = 10, height = 6, units = "in", dpi = 300, type = "cairo")
```

#### **4. Excel and more…**

Let’s see how often other data analysis programs such as ‘spss’,
‘excel’, ‘sas’, ‘tableau’, ‘sql’ were searched.

###### Prepare subset

``` r
# Get tags of interest
analytics_tags <- c("r", "python", "spss", 'excel', 'sas', 'sql')

# Filter for those tags
by_tag_subset <- tag_by_year_percentage %>%
  filter(tag %in% analytics_tags)

# Prepare a column to make the legend looks better
by_tag_subset$label <- NA
by_tag_subset$label[which(by_tag_subset$year == max(by_tag_subset$year))] <- by_tag_subset$tag[which(by_tag_subset$year == max(by_tag_subset$year))]
```

###### Visualize it

``` r
# Plot tags over time on a line plot using color to represent tag
ggplot(by_tag_subset, aes(year, fraction, color = tag)) +
  geom_line(size = 1, linejoin = "round") +
  labs(title = "How often were data analysis programmes searched?",
       x = 'Year',
       y = 'Percentage of searches') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, color = 'grey32'),
        axis.title.x = element_text(margin = margin(t = 10), size = 15, color = 'gray32'),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, color = 'gray32'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, color = "burlywood4"),
        axis.text = element_text(color = "burlywood4", size = 12),
        legend.title = element_text(face = "bold"),
        legend.title.align=0.5) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  theme(legend.position = 'none')
```
![How often were data analysis programmes searched](https://user-images.githubusercontent.com/101715443/163364817-e9bb3695-298c-4633-8c8f-1683039dba84.png)

``` r
ggsave("How often were data analysis programmes searched", device = "png",
       width = 12, height = 6, units = "in", dpi = 300, type = "cairo-png")
```

#### **5. What we were looking for most**

Finally, let’s find out what were the most searched tags on stack
overflow between 2008 and 2018

###### 10 first places

``` r
# Find total number of questions for each tag
sorted_tags <- tag_by_year %>%
  group_by(tag) %>%
  summarize(tag_total = sum(number)) %>%
  arrange(desc(tag_total))

# Print table with 10 first places
as.data.frame(head(sorted_tags, 10))
```

    ##           tag tag_total
    ## 1  javascript   1632049
    ## 2        java   1425961
    ## 3          c#   1217450
    ## 4         php   1204291
    ## 5     android   1110261
    ## 6      python    970768
    ## 7      jquery    915159
    ## 8        html    755341
    ## 9         c++    574263
    ## 10        ios    566075

Taking into account the number of all searches, ‘javascript’, ‘java’ and
‘c\#’ are on the podium. Let’s see how the race has looked over these 10
years.

``` r
# Get the 10 largest tags
highest_tags <- head(sorted_tags$tag, 10)

# Filter for the 10 largest tags
highest_tags <- tag_by_year_percentage %>%
  filter(tag %in% highest_tags)

# Prepare a column to make the legend looks better
highest_tags$label <- NA
highest_tags$label[which(highest_tags$year == max(highest_tags$year))] <- 
  highest_tags$tag[which(highest_tags$year == max(highest_tags$year))]
```

###### Visualize it

``` r
# Plot tags over time on a line plot using color to represent tag
ggplot(highest_tags, aes(year, fraction, color = tag)) +
geom_line(size = 1, linejoin = "round") +
  labs(title = "10 Most searched programming languages between 2008 - 2018",
       x = 'Year',
       y = 'Percentage of searches') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, color = 'grey32'),
        axis.title.x = element_text(margin = margin(t = 10), size = 15, color = 'gray32'),
        axis.title.y = element_text(margin = margin(r = 10), size = 15, color = 'gray32'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, color = "burlywood4"),
        axis.text = element_text(color = "burlywood4", size = 12),
        legend.title = element_text(face = "bold"),
        legend.title.align=0.5) +
  geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  theme(legend.position = 'none')
```
![10 Most searched programming languages between 2008 - 2018](https://user-images.githubusercontent.com/101715443/163365129-8fa96cb7-ed11-4659-9db3-4444dcb7d84d.png)

``` r
ggsave("10 Most searched programming languages between 2008 - 2018", device = "png",
       width = 12, height = 6, units = "in", dpi = 300, type = "cairo-png")
```
