Code
================

# Net-Migration Data, cell level

## Niva (2023) - World’s human migration patterns in 2000–2019 unveiled by high-resolution data

Source: <https://doi.org/10.1038/s41562-023-01689-4>

Code References:

- (Context) Spatial Data Science: With Applications in R:
  <https://doi.org/10.1201/9780429459016>
- (Code) R as GIS for Economists:
  <https://tmieno2.github.io/R-as-GIS-for-Economists-Quarto/>

Let’s import the dataset produced by Niva (2023):

- Data: <https://zenodo.org/records/7997134>
- Gridded global Net-Migration (per 1000 people)
- Yearly 2000-2019 (20 years)
- High-resolution, 5 arc-min / 0.083°
  <https://www.inchcalculator.com/convert/degree-to-arcminute/>

**RESAMPLING**! Climate data have resolution of 6 arcmin (0.1°).
Therefore I need to resample migration data from 5 arcmin (0.083°) to 6
arcmin!

``` r
library(terra)
```

    ## Warning: package 'terra' was built under R version 4.4.3

    ## terra 1.8.29

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.4.3

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:terra':
    ## 
    ##     extract

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.4.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.4.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:terra':
    ## 
    ##     intersect, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Import Net-Migration data, 2000-2019 yearly (20 layers)
filepath_raster_netMgr_2000_2019_annual <- "../data/raster_netMgr_2000_2019_annual.tif"
r_original <- rast(filepath_raster_netMgr_2000_2019_annual)

r_original # 2160 * 4320 * 20 years
```

    ## class       : SpatRaster 
    ## dimensions  : 2160, 4320, 20  (nrow, ncol, nlyr)
    ## resolution  : 0.08333333, 0.08333333  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : lon/lat WGS 84 (EPSG:4326) 
    ## source      : raster_netMgr_2000_2019_annual.tif 
    ## names       :      2000,      2001,      2002,      2003,      2004,      2005, ... 
    ## min values  : -245496.2, -137345.0, -130722.1, -106122.1, -114304.3, -235654.8, ... 
    ## max values  :  114237.0,  124515.2,  122075.5,  147669.6,  100850.3,  175273.8, ...

**RESAMPLING!**

``` r
# Template at 0.1° (6 arcmin) resolution
r_template <- rast(ext(r_original), resolution = 0.1, crs = crs(r_original))

# Resample to 0.1°
r_resampled <- resample(r_original, r_template, method = "bilinear")
```

    ## |---------|---------|---------|---------|=========================================                                          

``` r
# Convert to data.frame
df_netMgr_2000_2019_annual <- as.data.frame(r_resampled, xy = TRUE, na.rm = TRUE)
```

ORIGINAL raster

``` r
ext(r_original)
```

    ## SpatExtent : -180, 180, -90, 90 (xmin, xmax, ymin, ymax)

``` r
res(r_original)
```

    ## [1] 0.08333333 0.08333333

RESAMPLED raster

``` r
ext(r_resampled)
```

    ## SpatExtent : -180, 180, -90, 90 (xmin, xmax, ymin, ymax)

``` r
res(r_resampled)
```

    ## [1] 0.1 0.1

Now we can start working on the dataset.

``` r
# From wide (22 columns) to long
df_netMgr_2000_2019_annual_long <- pivot_longer(df_netMgr_2000_2019_annual, -c(x, y), names_to = "layer", values_to = "value") # 31.375.980 obs
colnames(df_netMgr_2000_2019_annual_long) <- c("x", "y", "year", "netmgr")

summary(df_netMgr_2000_2019_annual_long$netmgr)
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -122533.31      -1.46       0.00       0.34       1.05  308255.47

``` r
df_netMgr_2000_2019_annual_long[df_netMgr_2000_2019_annual_long$netmgr == 0, ]
```

    ## # A tibble: 4,259,374 × 4
    ##        x     y year  netmgr
    ##    <dbl> <dbl> <chr>  <dbl>
    ##  1 -34.8  83.6 2000       0
    ##  2 -34.8  83.6 2001       0
    ##  3 -34.8  83.6 2002       0
    ##  4 -34.8  83.6 2003       0
    ##  5 -34.8  83.6 2004       0
    ##  6 -34.8  83.6 2005       0
    ##  7 -34.8  83.6 2006       0
    ##  8 -34.8  83.6 2007       0
    ##  9 -34.8  83.6 2008       0
    ## 10 -34.8  83.6 2009       0
    ## # ℹ 4,259,364 more rows

Many cells (4.2 mln) have value 0. This would imply that natural birth
and death rate perfectly match. Leaving these 0 in the dataset alters
the analysis, and as a perfect match of the demographic rates seems
rather a residual case (total observations: 31 mln).

I’ve decided to remove the observations with value 0. Here I plot these
observations, they mostly cover Greenland.

``` r
# Zeros
df_zeros <- df_netMgr_2000_2019_annual_long[df_netMgr_2000_2019_annual_long$netmgr == 0,] # Length 6,7mln observations

avg_zeros <- df_zeros %>%
  group_by(x, y) %>%
  summarise(m = mean(netmgr, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'x'. You can override using the `.groups`
    ## argument.

``` r
ggplot() +
  geom_tile(data = avg_zeros, aes(x, y), fill = "red") +
  coord_fixed()
```

![](migration-data_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Therefore, let’s remove observations with 0.

``` r
# Remove 0s
df_netMgr_2000_2019_annual_long <- df_netMgr_2000_2019_annual_long[df_netMgr_2000_2019_annual_long$netmgr != 0,] # From 45mln to 38 mln observation

summary(df_netMgr_2000_2019_annual_long$netmgr)
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -122533.31      -3.17       0.00       0.40       2.47  308255.47

Let’s proceed cleaning our dataset, it’s time for outliers. I remove
outliers at 0.01 and 0.99 percentiles.

``` r
p <- quantile(df_netMgr_2000_2019_annual_long$netmgr, probs = c(0.01, 0.99))

# Filter out outliers (1st, 99th percentile)
df_netMgr_2000_2019_annual_long <- df_netMgr_2000_2019_annual_long[
  df_netMgr_2000_2019_annual_long$netmgr >= p[1] &
  df_netMgr_2000_2019_annual_long$netmgr <= p[2], ] # From 38mln to 37mln

summary(df_netMgr_2000_2019_annual_long$netmgr)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -794.9889   -2.8854   -0.0013   -2.0704    2.2263  775.1365

Our Net-Migration dataset is ready. Let’s plot the distribution.

Most observations have values very close to 0. For instance, a value of
0.02, considering values are the ratio between net migration and 1,000
people, means that for every 1,000 residents, there was a net gain of
only 0.02 people, equivalent to just 2 individuals per 100,000 people.
Instead, a value of 1 means that for every 1,000 residents, there was a
net gain of 1 person.

These are RATIOs. In a region with a population of 100,000: - A value of
0.2 would imply a net migration of 20 people (0.2/1000 \* 100k) - A
value of 1 would imply a net migration of 100 people (1/1000 \* 100k)

``` r
pp <- quantile(df_netMgr_2000_2019_annual_long$netmgr, probs = c(0.25, 0.5, 0.75))

ggplot(data = df_netMgr_2000_2019_annual_long, aes(netmgr)) +
  geom_histogram(binwidth = 0.01) +
  xlim(-3.5, 2.5) +
    geom_vline(xintercept = pp[1], color="red", alpha=0.5, linetype="dotted") +
  geom_vline(xintercept = pp[2], color="red", alpha=0.5, linetype="dotted") +
  geom_vline(xintercept = pp[3], color="red", alpha=0.5, linetype="dotted") +
  annotate("text", x = pp[1], y = 10000, label = "25th", vjust = -2, color = "red") +
  annotate("text", x = pp[2], y = 10000, label = "50th", vjust = -2, color = "red") +
  annotate("text", x = pp[3], y = 10000, label = "75th", vjust = -2, color = "red")
```

    ## Warning: Removed 12859872 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_bar()`).

![](migration-data_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Single observations (cell, year) with POSITIVE Net-Migration = 12 mln

``` r
df_netMgr_2000_2019_annual_long[df_netMgr_2000_2019_annual_long$netmgr > 0, ]
```

    ## # A tibble: 12,756,618 × 4
    ##        x     y year      netmgr
    ##    <dbl> <dbl> <chr>      <dbl>
    ##  1 -74.6  83.2 2003  0.0000302 
    ##  2 -74.6  83.2 2005  0.00000336
    ##  3 -74.6  83.2 2006  0.0000148 
    ##  4 -74.6  83.2 2011  0.00000146
    ##  5 -74.6  83.2 2014  0.0000202 
    ##  6 -74.6  83.2 2016  0.0000228 
    ##  7 -74.6  83.2 2018  0.000183  
    ##  8 -74.6  83.2 2003  0.0000347 
    ##  9 -74.6  83.2 2005  0.0000211 
    ## 10 -74.6  83.2 2006  0.0000291 
    ## # ℹ 12,756,608 more rows

Single observations (cell, year) with NEGATIVE Net-Migration = 13 mln

``` r
df_netMgr_2000_2019_annual_long[df_netMgr_2000_2019_annual_long$netmgr < 0, ]
```

    ## # A tibble: 13,817,654 × 4
    ##        x     y year       netmgr
    ##    <dbl> <dbl> <chr>       <dbl>
    ##  1 -74.6  83.2 2000  -0.0000322 
    ##  2 -74.6  83.2 2001  -0.0000570 
    ##  3 -74.6  83.2 2002  -0.0000180 
    ##  4 -74.6  83.2 2004  -0.00000878
    ##  5 -74.6  83.2 2007  -0.0000323 
    ##  6 -74.6  83.2 2008  -0.00000540
    ##  7 -74.6  83.2 2009  -0.0000146 
    ##  8 -74.6  83.2 2010  -0.00000341
    ##  9 -74.6  83.2 2012  -0.0000158 
    ## 10 -74.6  83.2 2013  -0.0000185 
    ## # ℹ 13,817,644 more rows

It’s time to merge Income and Net-Migration data.

1.  Both datasets use the same coordinate/projection system: EPSG:4326
    (WGS84) <https://epsg.io/4326>. This is reassuring, as allows us to
    spatially merge the features of the two datasets without having to
    resample.

2.  After RESAMPLING, both datasets have the same resolution: 0.1° / 6
    arc-min
    <https://www.inchcalculator.com/convert/degree-to-arcminute/>

Let’s plot them together.

``` r
# Import clean version of income data
gdp <- read.csv("../data/income-gdp-quarter.csv")

# Net-Migration data for 1 year, to be able to plot
netmgr_1y <- df_netMgr_2000_2019_annual_long[df_netMgr_2000_2019_annual_long$year == 2000,]

library(patchwork)
```

    ## Warning: package 'patchwork' was built under R version 4.4.3

    ## 
    ## Attaching package: 'patchwork'

    ## The following object is masked from 'package:terra':
    ## 
    ##     area

``` r
p1 <- ggplot() +
  geom_raster(data = gdp, aes(x = x, y = y, fill = mean_gdp)) +
  coord_fixed() +
  scale_fill_viridis_c(name = "GDP per capita")


p2 <- ggplot() + 
  geom_raster(data = netmgr_1y, aes(x = x, y = y, fill = netmgr)) +
  coord_fixed() +
  scale_fill_viridis_b(name = "Net-Migr/1000pp", limits = c(-50, 50), breaks = seq(-50, 50, 25))

p1 + p2 + plot_layout(ncol = 1)
```

    ## Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
    ## ℹ Consider using `geom_tile()` instead.

    ## Warning: Raster pixels are placed at uneven horizontal intervals and will be shifted
    ## ℹ Consider using `geom_tile()` instead.

![](migration-data_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# Function to filter
filter_continent <- function(grid, lon_min, lon_max, lat_min, lat_max) {
  subset(grid, x >= lon_min & x <= lon_max & y >= lat_min & y <= lat_max)
}
```

Let’s overlap them to check whether they’re actually using the same grid
after resampling!

First, let’s subset our global dataset and select a small area,
north-east Italy.

``` r
# Subset the area of interest
gdp_subset <- filter_continent(gdp, lon_min = 10, lon_max = 15, lat_min = 44, lat_max = 49)
netmgr_subset <- filter_continent(df_netMgr_2000_2019_annual_long, lon_min = 10, lon_max = 15, lat_min = 44, lat_max = 49)

p3 <- ggplot() +
  geom_tile(data = gdp_subset, aes(x = x, y = y, fill = mean_gdp)) +
  coord_fixed() +
  scale_fill_viridis_c(name = "Avg GDP", option = "magma", alpha = 1)

p4 <- ggplot() +
  geom_tile(data = netmgr_subset, aes(x = x, y = y, fill = netmgr)) +
  coord_fixed() +
  scale_fill_viridis_c(name = "Net-Migration/1000pp", option = "viridis")

p3 + p4 + plot_layout(ncol = 1)
```

![](migration-data_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Let’s overlap the grid of the two areas. They perfectly match! We can
now merge the variables of the two datasets.

``` r
library(ggnewscale) # Library to have multiple legends/scales
```

    ## Warning: package 'ggnewscale' was built under R version 4.4.3

``` r
ggplot() +
  # 1st layer: avg GDP
  geom_tile(data = gdp_subset, aes(x = x, y = y), fill = "white", alpha = 0, color="black", size = 1) +
  coord_fixed() +
  scale_fill_viridis_c(name = "Avg GDP", option = "magma") +
  
  new_scale_fill() + # New scale layer
  
  # 2nd layer: Net-Migration/1000pp
  geom_tile(data = netmgr_subset, aes(x = x, y = y), fill = "white", alpha = 0, color = "red", size = 0.1) +
  coord_fixed() +
  scale_fill_viridis_c(name = "Net-Migration/1000pp", option = "viridis") +
  
  # Legends position
  theme(
    legend.position = "bottom",  # Place both legends below
    legend.box = "horizontal"
  )
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Coordinate system already present. Adding new coordinate system, which will
    ## replace the existing one.

![](migration-data_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Spatial join with FNN library:
<https://rdrr.io/cran/FNN/man/get.knn.html>

``` r
# Fast Nearest Neighbor
library(FNN)
```

    ## Warning: package 'FNN' was built under R version 4.4.3

``` r
# Extract coordinates
netmgr_coords <- as.matrix(df_netMgr_2000_2019_annual_long[, c("x", "y")])
gdp_coords <- as.matrix(gdp[, c("x", "y")])

# Find nearest neighbor indices
nn_idx <- get.knnx(gdp_coords, netmgr_coords, k = 1)$nn.index[, 1]

# Assign quartiles
df_netMgr_2000_2019_annual_long$gdp_quartile <- gdp$gdp_quartile[nn_idx]
df_netMgr_2000_2019_annual_long$mean_gdp <- gdp$mean_gdp[nn_idx]
```

``` r
# Subset the area of interest
netmgr_subset1 <- filter_continent(df_netMgr_2000_2019_annual_long, lon_min = 10, lon_max = 15, lat_min = 44, lat_max = 49)

p5 <- ggplot() +
  geom_tile(data = gdp_subset, aes(x = x, y = y, fill = gdp_quartile)) +
  scale_fill_discrete(name = "GDP from original") +
  coord_fixed()

p6 <- ggplot() +
  geom_tile(data = netmgr_subset1, aes(x = x, y = y, fill = gdp_quartile)) +
  scale_fill_discrete(name = "GDP after merge") +
  coord_fixed()

p5 + p6 + plot_layout(ncol = 1)
```

![](migration-data_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Fancy table with gt: <https://gt.rstudio.com/index.html>

``` r
library(gt)
```

    ## Warning: package 'gt' was built under R version 4.4.3

``` r
# Summary Statistics
summary_stats <- function(data, variable) {
  q <- quantile(data[[deparse(substitute(variable))]], probs = c(0.25, 0.75), na.rm = TRUE)
  data %>%
    summarize(
      Obs. = n(),
      Mean = mean({{ variable }}, na.rm = TRUE),
      `St. dev.` = sd({{ variable }}, na.rm = TRUE),
      `25th` = q[1],
      `75th` = q[2]
    )
}

# Summary for full sample
summary_migr_full_sample <- summary_stats(df_netMgr_2000_2019_annual_long, netmgr) %>%
  mutate(Group = "Full sample")

# Summary by GDP group
summary_migr_by_group <- df_netMgr_2000_2019_annual_long %>%
  group_by(gdp_quartile) %>%
  # Apply function to each group
  group_modify(~summary_stats(.x, netmgr)) %>%
  rename(Group = gdp_quartile)

# Combine in one table
summary_table_migr <- bind_rows(summary_migr_full_sample, summary_migr_by_group) %>%
  select(Group, Obs., Mean, `St. dev.`, `25th`, `75th`)

# Print nice table
gt(summary_table_migr) %>% 
  fmt_number(columns = "Obs.", use_seps = TRUE, decimals = 0) %>% 
  fmt_number(columns = c(3,4,5,6), decimals = 2) %>% 
  tab_options(table.width = pct(80)) %>% 
  tab_header(title = "Net migration rates")
```

<div id="gretswgajw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gretswgajw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#gretswgajw thead, #gretswgajw tbody, #gretswgajw tfoot, #gretswgajw tr, #gretswgajw td, #gretswgajw th {
  border-style: none;
}
&#10;#gretswgajw p {
  margin: 0;
  padding: 0;
}
&#10;#gretswgajw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 80%;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#gretswgajw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#gretswgajw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#gretswgajw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#gretswgajw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#gretswgajw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gretswgajw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#gretswgajw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#gretswgajw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#gretswgajw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#gretswgajw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#gretswgajw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#gretswgajw .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#gretswgajw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#gretswgajw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#gretswgajw .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#gretswgajw .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#gretswgajw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#gretswgajw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gretswgajw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#gretswgajw .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#gretswgajw .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#gretswgajw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gretswgajw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#gretswgajw .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#gretswgajw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gretswgajw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gretswgajw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#gretswgajw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#gretswgajw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#gretswgajw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gretswgajw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#gretswgajw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gretswgajw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#gretswgajw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gretswgajw .gt_left {
  text-align: left;
}
&#10;#gretswgajw .gt_center {
  text-align: center;
}
&#10;#gretswgajw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#gretswgajw .gt_font_normal {
  font-weight: normal;
}
&#10;#gretswgajw .gt_font_bold {
  font-weight: bold;
}
&#10;#gretswgajw .gt_font_italic {
  font-style: italic;
}
&#10;#gretswgajw .gt_super {
  font-size: 65%;
}
&#10;#gretswgajw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#gretswgajw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#gretswgajw .gt_indent_1 {
  text-indent: 5px;
}
&#10;#gretswgajw .gt_indent_2 {
  text-indent: 10px;
}
&#10;#gretswgajw .gt_indent_3 {
  text-indent: 15px;
}
&#10;#gretswgajw .gt_indent_4 {
  text-indent: 20px;
}
&#10;#gretswgajw .gt_indent_5 {
  text-indent: 25px;
}
&#10;#gretswgajw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#gretswgajw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="6" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Net migration rates</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Group">Group</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Obs.">Obs.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Mean">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="St.-dev.">St. dev.</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a25th">25th</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a75th">75th</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Group" class="gt_row gt_left">Full sample</td>
<td headers="Obs." class="gt_row gt_right">26,574,272</td>
<td headers="Mean" class="gt_row gt_right">−2.07</td>
<td headers="St. dev." class="gt_row gt_right">109.66</td>
<td headers="25th" class="gt_row gt_right">−2.89</td>
<td headers="75th" class="gt_row gt_right">2.23</td></tr>
    <tr><td headers="Group" class="gt_row gt_left">Q1</td>
<td headers="Obs." class="gt_row gt_right">7,212,840</td>
<td headers="Mean" class="gt_row gt_right">−8.28</td>
<td headers="St. dev." class="gt_row gt_right">160.32</td>
<td headers="25th" class="gt_row gt_right">−26.77</td>
<td headers="75th" class="gt_row gt_right">16.52</td></tr>
    <tr><td headers="Group" class="gt_row gt_left">Q2</td>
<td headers="Obs." class="gt_row gt_right">7,181,746</td>
<td headers="Mean" class="gt_row gt_right">−3.69</td>
<td headers="St. dev." class="gt_row gt_right">113.13</td>
<td headers="25th" class="gt_row gt_right">−6.58</td>
<td headers="75th" class="gt_row gt_right">4.65</td></tr>
    <tr><td headers="Group" class="gt_row gt_left">Q3</td>
<td headers="Obs." class="gt_row gt_right">5,635,337</td>
<td headers="Mean" class="gt_row gt_right">1.67</td>
<td headers="St. dev." class="gt_row gt_right">66.07</td>
<td headers="25th" class="gt_row gt_right">−0.36</td>
<td headers="75th" class="gt_row gt_right">0.36</td></tr>
    <tr><td headers="Group" class="gt_row gt_left">Q4</td>
<td headers="Obs." class="gt_row gt_right">6,544,349</td>
<td headers="Mean" class="gt_row gt_right">3.32</td>
<td headers="St. dev." class="gt_row gt_right">51.11</td>
<td headers="25th" class="gt_row gt_right">−0.15</td>
<td headers="75th" class="gt_row gt_right">0.10</td></tr>
  </tbody>
  &#10;  
</table>
</div>
