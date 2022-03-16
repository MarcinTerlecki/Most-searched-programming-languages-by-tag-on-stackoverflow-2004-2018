Most searched programming languages by tag on stackoverflow 2004-2018
================

#### **Quick introduction**

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

#### **Data**

------------------------------------------------------------------------

The following analysis and visualizations are based on data downloaded
from DataCamp [link](https://projects.datacamp.com/projects/435). They
represent the number of searches for particular tags on the Stack
Overflow platform between 2008 and 2018.

#### **Analysis**

------------------------------------------------------------------------

###### Data loading

For this analysis I use the R with the packages ‘tidyverse’,
‘lubridate’, ‘ggthemr’.

``` r
# Load libraries
library(tidyverse)
library(lubridate)
library(ggthemr)
ggthemr("dust")
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
print(tag_by_year)
```

    ## # A tibble: 40,518 x 4
    ##     year tag           number year_total
    ##    <dbl> <chr>          <dbl>      <dbl>
    ##  1  2008 .htaccess         54      58390
    ##  2  2008 .net            5910      58390
    ##  3  2008 .net-2.0         289      58390
    ##  4  2008 .net-3.5         319      58390
    ##  5  2008 .net-4.0           6      58390
    ##  6  2008 .net-assembly      3      58390
    ##  7  2008 .net-core          1      58390
    ##  8  2008 2d                42      58390
    ##  9  2008 32-bit            19      58390
    ## 10  2008 32bit-64bit        4      58390
    ## # ... with 40,508 more rows

``` r
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

The column ‘year’ is a ‘numeric’ type. Let’s change it to “date” type.

``` r
class(tag_by_year$year)
```

    ## [1] "numeric"

``` r
tag_by_year$year <- ymd(tag_by_year$year, truncated = 2L)
class(tag_by_year$year)
```

    ## [1] "Date"

Let’s look for missing and unique values

``` r
# missing values 
sum(is.na(tag_by_year))
```

    ## [1] 0

``` r
# unique values in columns tag, year_total
unique_tags <- tag_by_year %>% 
  select(tag) %>% 
  unique()

unique_tags
```

    ## # A tibble: 4,080 x 1
    ##    tag          
    ##    <chr>        
    ##  1 .htaccess    
    ##  2 .net         
    ##  3 .net-2.0     
    ##  4 .net-3.5     
    ##  5 .net-4.0     
    ##  6 .net-assembly
    ##  7 .net-core    
    ##  8 2d           
    ##  9 32-bit       
    ## 10 32bit-64bit  
    ## # ... with 4,070 more rows

##### **Total searches over time**

------------------------------------------------------------------------

Before we look at the results of individual tag searches, let’s analyse
the number of total searches over the years.

``` r
total_searches_per_year <- tag_by_year %>% 
  group_by(year, year_total) %>% 
  summarise()

total_searches_per_year
```

    ## # A tibble: 11 x 2
    ## # Groups:   year [11]
    ##    year       year_total
    ##    <date>          <dbl>
    ##  1 2008-01-01      58390
    ##  2 2009-01-01     343868
    ##  3 2010-01-01     694391
    ##  4 2011-01-01    1200551
    ##  5 2012-01-01    1645404
    ##  6 2013-01-01    2060473
    ##  7 2014-01-01    2164701
    ##  8 2015-01-01    2219527
    ##  9 2016-01-01    2226072
    ## 10 2017-01-01    2305207
    ## 11 2018-01-01    1085170

###### Let’s visualise it

``` r
ggplot(total_searches_per_year, aes(year, year_total)) +
  geom_line(lwd = 1, linejoin = "round") +
  ggtitle("Total searches per year") +
  xlab("Year") +
  ylab("Number of searches") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")
```

![](Most-searched-programming-languages-by-tag-on-stackoverflow-2004-2018_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("myplot.png", device = "png",
       width = 10, height = 4, units = "in", dpi = 600, type = "cairo-png")
```

    ## Warning: Using ragg device as default. Ignoring `type` and `antialias` arguments

##### **R vs Python**

------------------------------------------------------------------------

Let’s see how the percentage of searches for R and Python breaks down
compared to all languages.

###### First, add a column with percentages

``` r
# Add percentage column
tag_by_year_percentage <- tag_by_year %>%
  mutate(fraction = (number/year_total)*100)

# Print the new table
print(tag_by_year_percentage)
```

    ## # A tibble: 40,518 x 5
    ##    year       tag           number year_total fraction
    ##    <date>     <chr>          <dbl>      <dbl>    <dbl>
    ##  1 2008-01-01 .htaccess         54      58390  0.0925 
    ##  2 2008-01-01 .net            5910      58390 10.1    
    ##  3 2008-01-01 .net-2.0         289      58390  0.495  
    ##  4 2008-01-01 .net-3.5         319      58390  0.546  
    ##  5 2008-01-01 .net-4.0           6      58390  0.0103 
    ##  6 2008-01-01 .net-assembly      3      58390  0.00514
    ##  7 2008-01-01 .net-core          1      58390  0.00171
    ##  8 2008-01-01 2d                42      58390  0.0719 
    ##  9 2008-01-01 32-bit            19      58390  0.0325 
    ## 10 2008-01-01 32bit-64bit        4      58390  0.00685
    ## # ... with 40,508 more rows

###### Then, filter out R and Python

``` r
# Filter for R and python tags
r_python_over_time <- tag_by_year_percentage %>%
  filter(tag %in% c("r", "python"))

# Print the new table
print(r_python_over_time)
```

    ## # A tibble: 22 x 5
    ##    year       tag    number year_total fraction
    ##    <date>     <chr>   <dbl>      <dbl>    <dbl>
    ##  1 2008-01-01 python   2080      58390   3.56  
    ##  2 2008-01-01 r           8      58390   0.0137
    ##  3 2009-01-01 python  12906     343868   3.75  
    ##  4 2009-01-01 r         524     343868   0.152 
    ##  5 2010-01-01 python  27098     694391   3.90  
    ##  6 2010-01-01 r        2270     694391   0.327 
    ##  7 2011-01-01 python  42313    1200551   3.52  
    ##  8 2011-01-01 r        5845    1200551   0.487 
    ##  9 2012-01-01 python  64456    1645404   3.92  
    ## 10 2012-01-01 r       12221    1645404   0.743 
    ## # ... with 12 more rows

###### Visualize it

``` r
# Create a line plot of fraction over time
ggplot(r_python_over_time, aes(year, fraction, color = tag)) +
  geom_line(lwd = 1)+
  ggtitle("How often was the 'R' and 'python' tag searched for?") +
  xlab("Year") +
  ylab("Percentage of all searches by year") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(.20, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.margin = margin(4, 4, 4, 4),
        legend.title.align=0.5)+
  labs(color = 'Tags')
```

![](Most-searched-programming-languages-by-tag-on-stackoverflow-2004-2018_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### **Let’s find out how often R/Python libraries were searched**

``` r
# A vector of selected tags 
selected_tags <- c('dplyr', 'ggplot2', "tidyverse", 'matplotlib', 'pandas', 'numpy')

# Filter for those tags
selected_tags_over_time <- tag_by_year_percentage %>%
  filter(tag %in% selected_tags)

head(selected_tags_over_time, 50)
```

    ## # A tibble: 50 x 5
    ##    year       tag        number year_total fraction
    ##    <date>     <chr>       <dbl>      <dbl>    <dbl>
    ##  1 2008-01-01 matplotlib      4      58390  0.00685
    ##  2 2008-01-01 numpy          12      58390  0.0206 
    ##  3 2009-01-01 ggplot2        33     343868  0.00960
    ##  4 2009-01-01 matplotlib     86     343868  0.0250 
    ##  5 2009-01-01 numpy         159     343868  0.0462 
    ##  6 2010-01-01 ggplot2       245     694391  0.0353 
    ##  7 2010-01-01 matplotlib    331     694391  0.0477 
    ##  8 2010-01-01 numpy         524     694391  0.0755 
    ##  9 2011-01-01 ggplot2       547    1200551  0.0456 
    ## 10 2011-01-01 matplotlib    811    1200551  0.0676 
    ## # ... with 40 more rows

###### Visualize it

``` r
# Plot tags over time on a line plot using color to represent tag
ggplot(selected_tags_over_time, aes(year, fraction, color = tag)) +
  geom_line(size = 1, alpha = 0.5) +
  ggtitle("How often were R libraries searched?") +
  xlab("Year") +
  ylab("Percentage of searches") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = c(.25, 1),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.title.align=0.5)+
  labs(color = 'Tags')
```

![](Most-searched-programming-languages-by-tag-on-stackoverflow-2004-2018_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

#### **Excel and more…**

Let’s see how often other data analysis programs such as ‘spss’,
‘excel’, ‘sas’, ‘tableau’, ‘sql’ were searched.

``` r
analytisc_tags <- c("r", "python", "spss", 'excel', 'sas', 'tableau', 'sql')

# Filter for those tags
by_tag_subset <- tag_by_year_percentage %>%
  filter(tag %in% analytisc_tags)

by_tag_subset
```

    ## # A tibble: 75 x 5
    ##    year       tag    number year_total fraction
    ##    <date>     <chr>   <dbl>      <dbl>    <dbl>
    ##  1 2008-01-01 excel     399      58390  0.683  
    ##  2 2008-01-01 python   2080      58390  3.56   
    ##  3 2008-01-01 r           8      58390  0.0137 
    ##  4 2008-01-01 sas         5      58390  0.00856
    ##  5 2008-01-01 spss        2      58390  0.00343
    ##  6 2008-01-01 sql      2032      58390  3.48   
    ##  7 2009-01-01 excel    1871     343868  0.544  
    ##  8 2009-01-01 python  12906     343868  3.75   
    ##  9 2009-01-01 r         524     343868  0.152  
    ## 10 2009-01-01 sas       112     343868  0.0326 
    ## # ... with 65 more rows

###### Visualize it

``` r
# Plot tags over time on a line plot using color to represent tag
ggplot(by_tag_subset, aes(year, fraction, color = tag)) +
  geom_line(size = 1, alpha = 0.5) +
  ggtitle("How often were data analysis programmes searched?") +
  xlab("Year") +
  ylab("Percentage of searches") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title.align=0.5,
        legend.title = element_text(face = "bold"))+
  labs(color = 'Tags')
```

![](Most-searched-programming-languages-by-tag-on-stackoverflow-2004-2018_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

#### **What we were looking for most**

Finally, let’s find out what were the most searched tags on stack
overflow between 2008 and 2018

``` r
sorted_tags <- tag_by_year %>%
  group_by(tag) %>%
  summarize(tag_total = sum(number)) %>%
  arrange(desc(tag_total))

# Print the new table
print(sorted_tags)
```

    ## # A tibble: 4,080 x 2
    ##    tag        tag_total
    ##    <chr>          <dbl>
    ##  1 javascript   1632049
    ##  2 java         1425961
    ##  3 c#           1217450
    ##  4 php          1204291
    ##  5 android      1110261
    ##  6 python        970768
    ##  7 jquery        915159
    ##  8 html          755341
    ##  9 c++           574263
    ## 10 ios           566075
    ## # ... with 4,070 more rows

On the search podium were ‘javascript’, ‘java’ and ‘c\#’. Let’s see how
the race has looked over these 10 years

``` r
# Get the six largest tags
highest_tags <- head(sorted_tags$tag)

# Filter for the six largest tags
by_tag_subset <- tag_by_year_percentage %>%
  filter(tag %in% highest_tags)

# Plot tags over time on a line plot using color to represent tag

ggplot(by_tag_subset, aes(year, fraction, color = tag)) +
  geom_line(size = 1, alpha = 0.5) +
  ggtitle("Most searched programming languages between 2008 - 2018") +
  xlab("Year") +
  ylab("Number of searches") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title.align=0.5,
        legend.title = element_text(face = "bold"))+
  labs(color = 'Tags')
```

![](Most-searched-programming-languages-by-tag-on-stackoverflow-2004-2018_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
