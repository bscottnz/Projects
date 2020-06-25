---
title: "Data Cleaning and Visualisation in R"
output:
 prettydoc::html_pretty:
    theme: cayman
    keep_md: true
    df_print: paged
---

In this notebook I tidy and analyse the World Bank's data on [World Development Indicators.](https://datacatalog.worldbank.org/dataset/world-development-indicators) This is a widely used dataset of economic, health, education, and societal indicators. Additional [sub region](https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv) and [coordinate information](https://gist.githubusercontent.com/tadast/8827699/raw/3cd639fa34eec5067080a61c69e3ae25e3076abb/countries_codes_and_coordinates.csv) data were downloaded from Github. 


```r
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(scales)
library(plotly)
library(maps)
library(knitr)
```


```r
wdi_data = read.csv("Data/WDI/WDIData.csv")
wdi_country = read.csv("Data/WDI/WDICountry.csv")
region = read.csv("Data/WDI/regions.csv")
long_lat = read.csv("Data/WDI/country_coord.csv")
```


```r
head(as_tibble(wdi_data), 5)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["ï..Country.Name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Country.Code"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Indicator.Name"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Indicator.Code"],"name":[4],"type":["chr"],"align":["left"]},{"label":["X1960"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["X1961"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["X1962"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["X1963"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["X1964"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["X1965"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["X1966"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["X1967"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["X1968"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["X1969"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["X1970"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["X1971"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["X1972"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["X1973"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["X1974"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["X1975"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["X1976"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["X1977"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["X1978"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["X1979"],"name":[24],"type":["dbl"],"align":["right"]},{"label":["X1980"],"name":[25],"type":["dbl"],"align":["right"]},{"label":["X1981"],"name":[26],"type":["dbl"],"align":["right"]},{"label":["X1982"],"name":[27],"type":["dbl"],"align":["right"]},{"label":["X1983"],"name":[28],"type":["dbl"],"align":["right"]},{"label":["X1984"],"name":[29],"type":["dbl"],"align":["right"]},{"label":["X1985"],"name":[30],"type":["dbl"],"align":["right"]},{"label":["X1986"],"name":[31],"type":["dbl"],"align":["right"]},{"label":["X1987"],"name":[32],"type":["dbl"],"align":["right"]},{"label":["X1988"],"name":[33],"type":["dbl"],"align":["right"]},{"label":["X1989"],"name":[34],"type":["dbl"],"align":["right"]},{"label":["X1990"],"name":[35],"type":["dbl"],"align":["right"]},{"label":["X1991"],"name":[36],"type":["dbl"],"align":["right"]},{"label":["X1992"],"name":[37],"type":["dbl"],"align":["right"]},{"label":["X1993"],"name":[38],"type":["dbl"],"align":["right"]},{"label":["X1994"],"name":[39],"type":["dbl"],"align":["right"]},{"label":["X1995"],"name":[40],"type":["dbl"],"align":["right"]},{"label":["X1996"],"name":[41],"type":["dbl"],"align":["right"]},{"label":["X1997"],"name":[42],"type":["dbl"],"align":["right"]},{"label":["X1998"],"name":[43],"type":["dbl"],"align":["right"]},{"label":["X1999"],"name":[44],"type":["dbl"],"align":["right"]},{"label":["X2000"],"name":[45],"type":["dbl"],"align":["right"]},{"label":["X2001"],"name":[46],"type":["dbl"],"align":["right"]},{"label":["X2002"],"name":[47],"type":["dbl"],"align":["right"]},{"label":["X2003"],"name":[48],"type":["dbl"],"align":["right"]},{"label":["X2004"],"name":[49],"type":["dbl"],"align":["right"]},{"label":["X2005"],"name":[50],"type":["dbl"],"align":["right"]},{"label":["X2006"],"name":[51],"type":["dbl"],"align":["right"]},{"label":["X2007"],"name":[52],"type":["dbl"],"align":["right"]},{"label":["X2008"],"name":[53],"type":["dbl"],"align":["right"]},{"label":["X2009"],"name":[54],"type":["dbl"],"align":["right"]},{"label":["X2010"],"name":[55],"type":["dbl"],"align":["right"]},{"label":["X2011"],"name":[56],"type":["dbl"],"align":["right"]},{"label":["X2012"],"name":[57],"type":["dbl"],"align":["right"]},{"label":["X2013"],"name":[58],"type":["dbl"],"align":["right"]},{"label":["X2014"],"name":[59],"type":["dbl"],"align":["right"]},{"label":["X2015"],"name":[60],"type":["dbl"],"align":["right"]},{"label":["X2016"],"name":[61],"type":["dbl"],"align":["right"]},{"label":["X2017"],"name":[62],"type":["dbl"],"align":["right"]},{"label":["X2018"],"name":[63],"type":["dbl"],"align":["right"]},{"label":["X2019"],"name":[64],"type":["dbl"],"align":["right"]},{"label":["X"],"name":[65],"type":["lgl"],"align":["right"]}],"data":[{"1":"Arab World","2":"ARB","3":"Access to clean fuels and technologies for cooking (% of population)","4":"EG.CFT.ACCS.ZS","5":"NA","6":"NA","7":"NA","8":"NA","9":"NA","10":"NA","11":"NA","12":"NA","13":"NA","14":"NA","15":"NA","16":"NA","17":"NA","18":"NA","19":"NA","20":"NA","21":"NA","22":"NA","23":"NA","24":"NA","25":"NA","26":"NA","27":"NA","28":"NA","29":"NA","30":"NA","31":"NA","32":"NA","33":"NA","34":"NA","35":"NA","36":"NA","37":"NA","38":"NA","39":"NA","40":"NA","41":"NA","42":"NA","43":"NA","44":"NA","45":"73.70250","46":"74.94486","47":"76.25807","48":"77.12046","49":"78.09444","50":"79.06200","51":"79.85168","52":"80.57054","53":"81.21089","54":"81.80963","55":"82.36810","56":"82.78329","57":"83.12030","58":"83.53346","59":"83.89760","60":"84.17160","61":"84.51017","62":"NA","63":"NA","64":"NA","65":"NA"},{"1":"Arab World","2":"ARB","3":"Access to electricity (% of population)","4":"EG.ELC.ACCS.ZS","5":"NA","6":"NA","7":"NA","8":"NA","9":"NA","10":"NA","11":"NA","12":"NA","13":"NA","14":"NA","15":"NA","16":"NA","17":"NA","18":"NA","19":"NA","20":"NA","21":"NA","22":"NA","23":"NA","24":"NA","25":"NA","26":"NA","27":"NA","28":"NA","29":"NA","30":"NA","31":"NA","32":"NA","33":"NA","34":"NA","35":"NA","36":"NA","37":"NA","38":"NA","39":"NA","40":"NA","41":"NA","42":"77.29785","43":"78.30278","44":"78.98086","45":"78.47936","46":"80.02305","47":"77.99911","48":"79.30631","49":"79.53718","50":"80.57473","51":"84.62121","52":"82.92688","53":"83.64499","54":"84.27092","55":"86.95999","56":"87.19947","57":"87.51226","58":"88.12988","59":"87.27532","60":"88.72010","61":"89.30860","62":"90.28364","63":"89.28686","64":"NA","65":"NA"},{"1":"Arab World","2":"ARB","3":"Access to electricity, rural (% of rural population)","4":"EG.ELC.ACCS.RU.ZS","5":"NA","6":"NA","7":"NA","8":"NA","9":"NA","10":"NA","11":"NA","12":"NA","13":"NA","14":"NA","15":"NA","16":"NA","17":"NA","18":"NA","19":"NA","20":"NA","21":"NA","22":"NA","23":"NA","24":"NA","25":"NA","26":"NA","27":"NA","28":"NA","29":"NA","30":"NA","31":"NA","32":"NA","33":"NA","34":"NA","35":"NA","36":"NA","37":"54.1733","38":"56.10343","39":"57.35019","40":"57.3533","41":"59.52834","42":"60.38373","43":"62.11201","44":"63.26170","45":"61.62634","46":"64.90893","47":"66.24645","48":"64.85015","49":"64.47130","50":"66.61201","51":"72.36730","52":"68.80600","53":"72.13583","54":"70.50646","55":"75.81616","56":"75.95888","57":"77.25171","58":"78.16571","59":"75.51215","60":"78.21100","61":"79.06551","62":"81.10213","63":"79.24810","64":"NA","65":"NA"},{"1":"Arab World","2":"ARB","3":"Access to electricity, urban (% of urban population)","4":"EG.ELC.ACCS.UR.ZS","5":"NA","6":"NA","7":"NA","8":"NA","9":"NA","10":"NA","11":"NA","12":"NA","13":"NA","14":"NA","15":"NA","16":"NA","17":"NA","18":"NA","19":"NA","20":"NA","21":"NA","22":"NA","23":"NA","24":"NA","25":"NA","26":"NA","27":"NA","28":"NA","29":"NA","30":"NA","31":"NA","32":"NA","33":"NA","34":"NA","35":"NA","36":"NA","37":"NA","38":"NA","39":"NA","40":"NA","41":"NA","42":"NA","43":"NA","44":"NA","45":"NA","46":"94.57443","47":"93.02283","48":"93.32199","49":"93.60129","50":"93.55632","51":"95.14171","52":"94.93974","53":"95.71297","54":"95.06544","55":"96.29087","56":"96.46642","57":"96.43596","58":"96.77285","59":"96.46671","60":"96.93632","61":"97.29008","62":"97.46791","63":"97.06396","64":"NA","65":"NA"},{"1":"Arab World","2":"ARB","3":"Account ownership at a financial institution or with a mobile-money-service provider (% of population ages 15+)","4":"FX.OWN.TOTL.ZS","5":"NA","6":"NA","7":"NA","8":"NA","9":"NA","10":"NA","11":"NA","12":"NA","13":"NA","14":"NA","15":"NA","16":"NA","17":"NA","18":"NA","19":"NA","20":"NA","21":"NA","22":"NA","23":"NA","24":"NA","25":"NA","26":"NA","27":"NA","28":"NA","29":"NA","30":"NA","31":"NA","32":"NA","33":"NA","34":"NA","35":"NA","36":"NA","37":"NA","38":"NA","39":"NA","40":"NA","41":"NA","42":"NA","43":"NA","44":"NA","45":"NA","46":"NA","47":"NA","48":"NA","49":"NA","50":"NA","51":"NA","52":"NA","53":"NA","54":"NA","55":"NA","56":"22.26054","57":"NA","58":"NA","59":"30.27713","60":"NA","61":"NA","62":"37.16521","63":"NA","64":"NA","65":"NA"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


```r
n_distinct(wdi_data$Indicator.Name)
```

```
## [1] 1431
```

There are 1431 indicators for each country in the main data file. We first scan through these and make a note of potential indicators of particular interest for later analysis.    


```r
variables = c("GDP per capita (current US$)",
              "GDP (current US$)",
              "Life expectancy at birth, total (years)",
              "Net migration",
              "Population, total",
              "Refugee population by country or territory of origin",
              "Refugee population by country or territory of asylum")
```

To tidy this data the following steps are taken.

*   Select the indicators we wish to analyse.

*   Transform the multiple year columns into a single year variable.

*   Remove redundant columns.

*   Transform the single indicator column into multiple variable columns.

*   Reformat columns to be of appropriate name and type.

Once we have our tidied data we can then join with the relevant data from the supplementary files.  


```r
wdi_data_tidy = wdi_data %>%
  filter(Indicator.Name %in% variables) %>%
  pivot_longer(X1960:X2019, names_to = "Year", values_to = "Value") %>%
  select(-Indicator.Code, -X) %>%
  pivot_wider(names_from = Indicator.Name, values_from = Value) %<>%
  mutate(Year = as.numeric(gsub("X", "", Year))) %>%
  rename(Country = ï..Country.Name, 'Country Code' = Country.Code)
```


```r
wdi_country_2 = wdi_country %>%
  select(ï..Country.Code, Income.Group) %>%
  rename('Country Code' = ï..Country.Code, 'Income Group' = Income.Group)
```
 

```r
region_2 = region %>%
  select('alpha.3', region, 'sub.region') %>%
  rename("Region" = region, "Sub Region" = 'sub.region')
```


```r
long_lat_2 = long_lat %>%
  select(Alpha.3.code, Latitude..average., Longitude..average.) %>%
  rename("Country Code" = Alpha.3.code, Latitude = Latitude..average., Longitude = Longitude..average.) %>%
  mutate("Country Code" = gsub(" ", "", `Country Code`), Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))
```
 

```r
wdi = wdi_data_tidy %>%
  left_join(wdi_country_2, by = "Country Code") %>%
  left_join(region_2, by = c("Country Code" = "alpha.3")) %>%
  left_join(long_lat_2, by = "Country Code") %>%
  select("Country", "Country Code", "Region", "Sub Region", Latitude, Longitude, Year, everything())
```


```r
head(filter(wdi, Country == "New Zealand"), 5)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Country"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Country Code"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Region"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Sub Region"],"name":[4],"type":["chr"],"align":["left"]},{"label":["Latitude"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Longitude"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["Year"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["GDP (current US$)"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["GDP per capita (current US$)"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Life expectancy at birth, total (years)"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["Net migration"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["Population, total"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["Refugee population by country or territory of asylum"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["Refugee population by country or territory of origin"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["Income Group"],"name":[15],"type":["chr"],"align":["left"]}],"data":[{"1":"New Zealand","2":"NZL","3":"Oceania","4":"Australia and New Zealand","5":"-41","6":"174","7":"1960","8":"5485854792","9":"2312.950","10":"71.23659","11":"NA","12":"2371800","13":"NA","14":"NA","15":"High income"},{"1":"New Zealand","2":"NZL","3":"Oceania","4":"Australia and New Zealand","5":"-41","6":"174","7":"1961","8":"5670064168","9":"2343.292","10":"70.98537","11":"NA","12":"2419700","13":"NA","14":"NA","15":"High income"},{"1":"New Zealand","2":"NZL","3":"Oceania","4":"Australia and New Zealand","5":"-41","6":"174","7":"1962","8":"6077496268","9":"2448.629","10":"71.23171","11":"54391","12":"2482000","13":"NA","14":"NA","15":"High income"},{"1":"New Zealand","2":"NZL","3":"Oceania","4":"Australia and New Zealand","5":"-41","6":"174","7":"1963","8":"6638937283","9":"2622.220","10":"71.28049","11":"NA","12":"2531800","13":"NA","14":"NA","15":"High income"},{"1":"New Zealand","2":"NZL","3":"Oceania","4":"Australia and New Zealand","5":"-41","6":"174","7":"1964","8":"7274144351","9":"2813.547","10":"71.32927","11":"NA","12":"2585400","13":"NA","14":"NA","15":"High income"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

We now have all the data of interest in a workable format ready for analysis. 

We will begin by visualising the change in GDP v life expectancy between years. 


```r
bb = c(1000000000, 100000000, 10000000, 1000000)
g = wdi %>%
  filter(!is.na(Region), Year < 2019) %>%
  ggplot(aes(`GDP per capita (current US$)`, 
             `Life expectancy at birth, total (years)`, 
             size = `Population, total`, 
             color = Region)) +
  geom_point( alpha = 0.7) +
  scale_size_continuous(name = "Population",
                        breaks = bb,
                        limits = c(1000000, 1000000000),
                        range = c(1, 8)) +
  scale_x_log10(labels = comma) +
  theme_bw(base_size = 8) +
  labs(title = "{round(frame_time)}", 
       x = 'GDP per capita (USD)',
       y = 'Life expectancy', 
       caption = "") +
  transition_time(Year) +
  ease_aes('linear') + 
  guides(colour = guide_legend(override.aes = list(size=2)),
         (shape = guide_legend(override.aes = list(size = 0.5)))) +
  theme(plot.title = element_text(color = "#007020",
                                  size = 16, 
                                  face = "bold"),
        axis.text=element_text(size = 8), 
        legend.title=element_text(size = 7), 
        legend.text=element_text(size = 6))

options(gganimate.dev_args = list(width = 4,
                                  height = 3, 
                                  units = 'in', 
                                  res = 300))
#animate(g,
       # fps = 20, 
       # nframes = 200,
       # renderer=gifski_renderer())

#anim_save("life_ex_v_gdp6.gif")
```


```r
knitr::include_graphics("life_ex_v_gdp6.gif")
```

![](life_ex_v_gdp6.gif)<!-- -->

On average, both GDP and life expectancy are increasing each year. The gap between the highest and lowest life expectancy countries also seems to be decreasing. 

We will now visualise the top GDP countries for each year, emphasising the change in rankings with an animated bar chart.


```r
top_gdp = wdi %>%
  filter(!is.na(Region)) %>%
  select(Country, `GDP (current US$)`, Year, Region) %>%
  rename(g = `GDP (current US$)`) %>%
  mutate(g = g / 1e12) %>%
  filter(!is.na(g)) %>%
  group_by(Year) %>%
  mutate(rank = min_rank(-g) * 1, 
         g_lbl = str_c("  ", Country, " ",(round(g,2)), "  ")) %>%
  filter(rank <= 10) %>%
  ungroup()


p = ggplot(top_gdp, aes(x = rank, 
                        y = Country,
                        label = Country,
                        group = Country,
                        fill = as.factor(Region))) +
  geom_tile(aes(y = g / 2, 
                height = g,
                width = 0.9,
                fill = as.factor(Country)), 
            alpha = 0.8, 
            show.legend = FALSE) +
  geom_text(aes(y = g,  
                label = g_lbl,
                hjust = ifelse(g > 15, 1, 0))) + 
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  theme_minimal() + 
  theme(plot.title = element_text(color = "#007020", 
                                  face = "bold", 
                                  hjust = 0, 
                                  size = 30),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.justification = "left") +
  labs(title = "{closest_state}",
       subtitle = "Ten Highest GDP Countries 1960 to 2018",
       y = "",
       x = "",
       caption = "Source: worldbank.org | Dollar amount in Trillions (USD)") +
  transition_states(Year, 
                    transition_length = 4, 
                    state_length = 1, 
                    wrap = TRUE) + 
  ease_aes("cubic-in-out") 
  
#animate(p, 
        #nframes = 450, 
       # fps = 15,
       # width = 8,
       # height = 6, 
       # units = 'in',
       # res=150,
       # renderer=gifski_renderer())

#anim_save("bar4.gif")
```


```r
knitr::include_graphics("bar4.gif")
```

![](bar4.gif)<!-- -->

The United States has long been the dominating economic force, however, China is quickly rising and is challenging the United States for the leading position. 

We now explore the extent of migration for each country by interactively plotting a world map, displaying net migration between 1960 and 2018.


```r
migration = wdi %>%
  group_by(Country) %>%
  mutate(net_migration_to_date = sum(`Net migration`, na.rm = TRUE)) %>%
  filter(!is.na(Region)) %>%
  select(Country, `Country Code`, Latitude, Longitude, net_migration_to_date) %>%
  mutate(positive_migration = ifelse(net_migration_to_date > 0, net_migration_to_date, NA)) %>%
  mutate(negative_migration = ifelse(net_migration_to_date < 0, net_migration_to_date, NA)) %>%
  distinct()

# To set white in the colour scale to 0, we need to define a custom colour scale.
colorlength = 100

zero_normal = (0 - min(migration$net_migration_to_date)) / 
  (max(migration$net_migration_to_date) - min(migration$net_migration_to_date))

border = as.integer(zero_normal * colorlength)

colorscale = as.list(1:colorlength)

s = scales::seq_gradient_pal("#FF0000", "#ffe6e6", "Lab")(seq(0,1,length.out=border))
for (i in 1:border) {
  colorscale[[i]] = c((i - 1) / colorlength, s[i])
}

s = scales::seq_gradient_pal("#edf7f0", "#007020", "Lab")(seq(0,1,length.out=colorlength - border))
for (i in 1:(colorlength - border)) {
  colorscale[[i + border]] = c((i + border) / colorlength, s[i])
}

l = list(color = toRGB("grey"), width = 0.5)

g = list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator'))

fig = plot_geo(migration)
fig = fig %>% add_trace(z = ~net_migration_to_date,
                        color = ~net_migration_to_date,
                        colorscale = colorscale,
                        text = ~Country,
                        locations = ~`Country Code`,
                        marker = list(line = l))

fig = fig %>% colorbar(title = list( text = '<b>Net Migration<b>',
                                     family = "Arial" ))
fig = fig %>% layout(
  title = list(text = '<b>Net Migration by Country<br>1960 to 2018<b>',
               y = 0.95, 
               font = list(color = '#007020', 
                           size = 30,
                           family = "Arial" )), 
  geo = g) 

fig = fig %>% layout( 
                     width = 900)

fig
```

<!--html_preserve--><div id="htmlwidget-f2033e1fc63bf0898678" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-f2033e1fc63bf0898678">{"x":{"visdat":{"cbfc6d4c78fa":["function () ","plotlyVisDat"]},"cur_data":"cbfc6d4c78fa","attrs":{"cbfc6d4c78fa":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"z":{},"color":{},"colorscale":[["0","#FF0000"],["0.01","#FF2410"],["0.02","#FF361D"],["0.03","#FF4428"],["0.04","#FF5032"],["0.05","#FF5B3C"],["0.06","#FF6546"],["0.07","#FF6E4F"],["0.08","#FF7759"],["0.09","#FF8063"],["0.1","#FF886C"],["0.11","#FF9176"],["0.12","#FF9980"],["0.13","#FFA18A"],["0.14","#FFA994"],["0.15","#FFB19E"],["0.16","#FFB8A8"],["0.17","#FFC0B2"],["0.18","#FFC8BC"],["0.19","#FFCFC7"],["0.2","#FFD7D1"],["0.21","#FFDEDB"],["0.22","#FFE6E6"],["0.24","#EDF7F0"],["0.25","#EAF5ED"],["0.26","#E7F3EA"],["0.27","#E5F2E7"],["0.28","#E2F0E4"],["0.29","#DFEEE1"],["0.3","#DCECDE"],["0.31","#D9EADB"],["0.32","#D7E8D9"],["0.33","#D4E7D6"],["0.34","#D1E5D3"],["0.35","#CEE3D0"],["0.36","#CCE1CD"],["0.37","#C9DFCA"],["0.38","#C6DEC7"],["0.39","#C3DCC4"],["0.4","#C1DAC2"],["0.41","#BED8BF"],["0.42","#BBD6BC"],["0.43","#B8D5B9"],["0.44","#B6D3B6"],["0.45","#B3D1B3"],["0.46","#B0CFB0"],["0.47","#ADCDAE"],["0.48","#ABCCAB"],["0.49","#A8CAA8"],["0.5","#A5C8A5"],["0.51","#A2C6A2"],["0.52","#A0C4A0"],["0.53","#9DC39D"],["0.54","#9AC19A"],["0.55","#97BF97"],["0.56","#95BD95"],["0.57","#92BC92"],["0.58","#8FBA8F"],["0.59","#8DB88C"],["0.6","#8AB68A"],["0.61","#87B487"],["0.62","#84B384"],["0.63","#82B182"],["0.64","#7FAF7F"],["0.65","#7CAD7C"],["0.66","#79AC7A"],["0.67","#77AA77"],["0.68","#74A874"],["0.69","#71A671"],["0.7","#6EA46F"],["0.71","#6CA36C"],["0.72","#69A16A"],["0.73","#669F67"],["0.74","#639D64"],["0.75","#609C62"],["0.76","#5E9A5F"],["0.77","#5B985C"],["0.78","#58965A"],["0.79","#559557"],["0.8","#529355"],["0.81","#4F9152"],["0.82","#4C8F4F"],["0.83","#498E4D"],["0.84","#468C4A"],["0.85","#438A48"],["0.86","#408845"],["0.87","#3D8742"],["0.88","#3A8540"],["0.89","#37833D"],["0.9","#33813B"],["0.91","#308038"],["0.92","#2C7E35"],["0.93","#297C33"],["0.94","#257A30"],["0.95","#20792E"],["0.96","#1C772B"],["0.97","#177528"],["0.98","#117326"],["0.99","#097223"],["1","#007020"]],"text":{},"locations":{},"marker":{"line":{"color":"rgba(190,190,190,1)","width":0.5}},"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"mapType":"geo","scene":{"zaxis":{"title":"net_migration_to_date"}},"geo":{"domain":{"x":[0,1],"y":[0,1]},"showframe":false,"showcoastlines":false,"projection":{"type":"Mercator"}},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5},"title":{"text":"<b>Net Migration by Country<br>1960 to 2018<b>","y":0.95,"font":{"color":"#007020","size":30,"family":"Arial"}},"width":900},"source":"A","config":{"showSendToCloud":false},"data":[{"colorbar":{"title":{"text":"<b>Net Migration<b>","family":"Arial"},"ticklen":2,"len":0.5,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[["0","#FF0000"],["0.01","#FF2410"],["0.02","#FF361D"],["0.03","#FF4428"],["0.04","#FF5032"],["0.05","#FF5B3C"],["0.06","#FF6546"],["0.07","#FF6E4F"],["0.08","#FF7759"],["0.09","#FF8063"],["0.1","#FF886C"],["0.11","#FF9176"],["0.12","#FF9980"],["0.13","#FFA18A"],["0.14","#FFA994"],["0.15","#FFB19E"],["0.16","#FFB8A8"],["0.17","#FFC0B2"],["0.18","#FFC8BC"],["0.19","#FFCFC7"],["0.2","#FFD7D1"],["0.21","#FFDEDB"],["0.22","#FFE6E6"],["0.24","#EDF7F0"],["0.25","#EAF5ED"],["0.26","#E7F3EA"],["0.27","#E5F2E7"],["0.28","#E2F0E4"],["0.29","#DFEEE1"],["0.3","#DCECDE"],["0.31","#D9EADB"],["0.32","#D7E8D9"],["0.33","#D4E7D6"],["0.34","#D1E5D3"],["0.35","#CEE3D0"],["0.36","#CCE1CD"],["0.37","#C9DFCA"],["0.38","#C6DEC7"],["0.39","#C3DCC4"],["0.4","#C1DAC2"],["0.41","#BED8BF"],["0.42","#BBD6BC"],["0.43","#B8D5B9"],["0.44","#B6D3B6"],["0.45","#B3D1B3"],["0.46","#B0CFB0"],["0.47","#ADCDAE"],["0.48","#ABCCAB"],["0.49","#A8CAA8"],["0.5","#A5C8A5"],["0.51","#A2C6A2"],["0.52","#A0C4A0"],["0.53","#9DC39D"],["0.54","#9AC19A"],["0.55","#97BF97"],["0.56","#95BD95"],["0.57","#92BC92"],["0.58","#8FBA8F"],["0.59","#8DB88C"],["0.6","#8AB68A"],["0.61","#87B487"],["0.62","#84B384"],["0.63","#82B182"],["0.64","#7FAF7F"],["0.65","#7CAD7C"],["0.66","#79AC7A"],["0.67","#77AA77"],["0.68","#74A874"],["0.69","#71A671"],["0.7","#6EA46F"],["0.71","#6CA36C"],["0.72","#69A16A"],["0.73","#669F67"],["0.74","#639D64"],["0.75","#609C62"],["0.76","#5E9A5F"],["0.77","#5B985C"],["0.78","#58965A"],["0.79","#559557"],["0.8","#529355"],["0.81","#4F9152"],["0.82","#4C8F4F"],["0.83","#498E4D"],["0.84","#468C4A"],["0.85","#438A48"],["0.86","#408845"],["0.87","#3D8742"],["0.88","#3A8540"],["0.89","#37833D"],["0.9","#33813B"],["0.91","#308038"],["0.92","#2C7E35"],["0.93","#297C33"],["0.94","#257A30"],["0.95","#20792E"],["0.96","#1C772B"],["0.97","#177528"],["0.98","#117326"],["0.99","#097223"],["1","#007020"]],"showscale":true,"z":[-3970563,-1224132,-2105797,0,0,1097582,-13399,40000,-960973,4503,7113777,1499874,-526430,64655,825150,-15935370,-71553,206146,1461448,-9307,-122477,0,-23913,-1230300,-1799624,83582,121924,0,99944,-1060616,-1865708,-1708845,-155611,-498525,-303500,9505810,0,-732073,62709,18588,-11671012,-2282291,-67491,296076,131779,353606,3758816,-341863,-1932939,-63451,200655,412969,561629,223460,0,-1413782,-168199,-3549835,-2445549,348004,-740783,49329,-267691,432621,0,-372524,191317,5573807,-2399,263295,76407,-1737812,13024587,-953308,0,411077,0,-77132,-39616,-2291891,-1618832,-310203,-686760,-1444001,-576992,1798676,143992,7077,-7379958,-906239,1510307,-1110264,220567,0,1721343,4928717,-1197241,2446623,2359211,-3425222,-115555,-26431,-109829,705566,1626244,-698042,-873078,-165572,1238476,-540622,-22234,-347128,0,-546086,251098,295994,-125000,-323835,2751936,142748,-2136928,4238,0,-114952,-216713,-14114100,-52998,-260174,0,-175418,-153173,-5127536,-1163193,-10792306,-130781,0,-4419864,1329429,33814,564265,-1224550,-229063,-1289405,-458171,0,800345,1695658,-2030768,0,61135,-85336,-691650,-3563404,-7758297,-1982229,-727234,-1437190,2212567,-3519634,24106418,-614768,-170386,0,-65557,7444175,-1117133,-663882,-12450,-19255,2163534,0,-186263,181533,-60753,-524042,5455569,-670696,5835135,-3666346,0,-50966,0,-242727,-1703707,-199692,1556128,1972734,-8242525,-846397,-809364,2332709,-253872,-159050,-92920,-367163,-526719,1872691,-284935,0,0,-477638,1122622,7283992,6708924,50624424,-541000,-849278,-20940,-7801326,-8759978,-16613,-398549,-776802,-712370,-3312103],"text":["Afghanistan","Albania","Algeria","American Samoa","Andorra","Angola","Antigua and Barbuda","Argentina","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas, The","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bermuda","Bhutan","Bolivia","Bosnia and Herzegovina","Botswana","Brazil","British Virgin Islands","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cabo Verde","Cambodia","Cameroon","Canada","Cayman Islands","Central African Republic","Chad","Chile","China","Colombia","Comoros","Congo, Dem. Rep.","Congo, Rep.","Costa Rica","Cote d'Ivoire","Croatia","Cuba","Curacao","Cyprus","Czech Republic","Denmark","Djibouti","Dominica","Dominican Republic","Ecuador","Egypt, Arab Rep.","El Salvador","Equatorial Guinea","Eritrea","Estonia","Eswatini","Ethiopia","Faroe Islands","Fiji","Finland","France","French Polynesia","Gabon","Gambia, The","Georgia","Germany","Ghana","Gibraltar","Greece","Greenland","Grenada","Guam","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras","Hong Kong SAR, China","Hungary","Iceland","India","Indonesia","Iran, Islamic Rep.","Iraq","Ireland","Isle of Man","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Korea, Dem. Peopleâ€™s Rep.","Korea, Rep.","Kuwait","Kyrgyz Republic","Lao PDR","Latvia","Lebanon","Lesotho","Liberia","Libya","Liechtenstein","Lithuania","Luxembourg","Macao SAR, China","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Mauritania","Mauritius","Mexico","Micronesia, Fed. Sts.","Moldova","Monaco","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Caledonia","New Zealand","Nicaragua","Niger","Nigeria","North Macedonia","Northern Mariana Islands","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Puerto Rico","Qatar","Romania","Russian Federation","Rwanda","Samoa","San Marino","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Sint Maarten (Dutch part)","Slovak Republic","Slovenia","Solomon Islands","Somalia","South Africa","South Sudan","Spain","Sri Lanka","St. Kitts and Nevis","St. Lucia","St. Martin (French part)","St. Vincent and the Grenadines","Sudan","Suriname","Sweden","Switzerland","Syrian Arab Republic","Tajikistan","Tanzania","Thailand","Timor-Leste","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Turks and Caicos Islands","Tuvalu","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Vanuatu","Venezuela, RB","Vietnam","Virgin Islands (U.S.)","West Bank and Gaza","Yemen, Rep.","Zambia","Zimbabwe"],"locations":["AFG","ALB","DZA","ASM","AND","AGO","ATG","ARG","ARM","ABW","AUS","AUT","AZE","BHS","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BMU","BTN","BOL","BIH","BWA","BRA","VGB","BRN","BGR","BFA","BDI","CPV","KHM","CMR","CAN","CYM","CAF","TCD","CHL","CHN","COL","COM","COD","COG","CRI","CIV","HRV","CUB","CUW","CYP","CZE","DNK","DJI","DMA","DOM","ECU","EGY","SLV","GNQ","ERI","EST","SWZ","ETH","FRO","FJI","FIN","FRA","PYF","GAB","GMB","GEO","DEU","GHA","GIB","GRC","GRL","GRD","GUM","GTM","GIN","GNB","GUY","HTI","HND","HKG","HUN","ISL","IND","IDN","IRN","IRQ","IRL","IMN","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KIR","PRK","KOR","KWT","KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LIE","LTU","LUX","MAC","MDG","MWI","MYS","MDV","MLI","MLT","MHL","MRT","MUS","MEX","FSM","MDA","MCO","MNG","MNE","MAR","MOZ","MMR","NAM","NRU","NPL","NLD","NCL","NZL","NIC","NER","NGA","MKD","MNP","NOR","OMN","PAK","PLW","PAN","PNG","PRY","PER","PHL","POL","PRT","PRI","QAT","ROU","RUS","RWA","WSM","SMR","STP","SAU","SEN","SRB","SYC","SLE","SGP","SXM","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","KNA","LCA","MAF","VCT","SDN","SUR","SWE","CHE","SYR","TJK","TZA","THA","TLS","TGO","TON","TTO","TUN","TUR","TKM","TCA","TUV","UGA","UKR","ARE","GBR","USA","URY","UZB","VUT","VEN","VNM","VIR","PSE","YEM","ZMB","ZWE"],"marker":{"line":{"colorbar":{"title":"","ticklen":2},"cmin":-15935370,"cmax":50624424,"colorscale":[["0","rgba(68,1,84,1)"],["0.0416666666666667","rgba(70,19,97,1)"],["0.0833333333333333","rgba(72,32,111,1)"],["0.125","rgba(71,45,122,1)"],["0.166666666666667","rgba(68,58,128,1)"],["0.208333333333333","rgba(64,70,135,1)"],["0.25","rgba(60,82,138,1)"],["0.291666666666667","rgba(56,93,140,1)"],["0.333333333333333","rgba(49,104,142,1)"],["0.375","rgba(46,114,142,1)"],["0.416666666666667","rgba(42,123,142,1)"],["0.458333333333333","rgba(38,133,141,1)"],["0.5","rgba(37,144,140,1)"],["0.541666666666667","rgba(33,154,138,1)"],["0.583333333333333","rgba(39,164,133,1)"],["0.625","rgba(47,174,127,1)"],["0.666666666666667","rgba(53,183,121,1)"],["0.708333333333333","rgba(79,191,110,1)"],["0.75","rgba(98,199,98,1)"],["0.791666666666667","rgba(119,207,85,1)"],["0.833333333333333","rgba(147,214,70,1)"],["0.875","rgba(172,220,52,1)"],["0.916666666666667","rgba(199,225,42,1)"],["0.958333333333333","rgba(226,228,40,1)"],["1","rgba(253,231,37,1)"]],"showscale":false,"color":"rgba(190,190,190,1)","width":0.5}},"type":"choropleth","geo":"geo","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
The United States has experienced mass positive migration between 1960 and 2018, with neighbouring Mexico among the highest countries experiencing negative migration. 

We now determine the countries that have produced the highest number of refugees by 2018, and plot the trend in refugees from these countries between 2012 and 2018.


```r
refugees = wdi %>%
  filter(Year == 2018, !is.na(Region)) %>%
  distinct() %>%
  arrange(desc(`Refugee population by country or territory of origin`)) %>%
  head(7)

refugees_7_yr = wdi %>%
  filter(Country %in% refugees$Country, between(Year, 2012, 2018)) %>%
  select(Country, Year, `Refugee population by country or territory of origin`)

ref_fig = plot_ly(refugees_7_yr,
                  x = ~Year, 
                  y = ~`Refugee population by country or territory of origin`, 
                  type = "scatter", 
                  mode = "lines+markers", 
                  name = ~Country) %>%
  layout(title = list(text = '<b>Refugee Population by Country of Origin 2012 to 2018<b>',
                      y = 0.99, 
                      font = list(color = '#007020', 
                                  size = 20,
                                  family = "Arial" )),
         yaxis = list(title = 'Refugee Population by Country of Origin'),
         xaxis = list(showgrid = FALSE),
         width = 900)
ref_fig
```

<!--html_preserve--><div id="htmlwidget-59d54750c7ace646e0be" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-59d54750c7ace646e0be">{"x":{"visdat":{"cbfc1c1f7692":["function () ","plotlyVisDat"]},"cur_data":"cbfc1c1f7692","attrs":{"cbfc1c1f7692":{"x":{},"y":{},"mode":"lines+markers","name":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"width":900,"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"<b>Refugee Population by Country of Origin 2012 to 2018<b>","y":0.99,"font":{"color":"#007020","size":20,"family":"Arial"}},"yaxis":{"domain":[0,1],"automargin":true,"title":"Refugee Population by Country of Origin"},"xaxis":{"domain":[0,1],"automargin":true,"showgrid":false,"title":"Year"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":[2012,2013,2014,2015,2016,2017,2018],"y":[2586152,2556502,2596270,2666305,2501445,2624225,2681269],"mode":"lines+markers","name":"Afghanistan","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2012,2013,2014,2015,2016,2017,2018],"y":[509290,499563,516771,541496,537473,620775,720307],"mode":"lines+markers","name":"Congo, Dem. Rep.","type":"scatter","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2012,2012,2013,2013,2014,2014,2015,2015,2016,2016,2017,2017,2018,2018],"y":[415371,415371,479606,479606,479006,479006,451805,451805,490289,490289,1156732,1156732,1145154,1145154],"mode":"lines+markers","name":"Myanmar","type":"scatter","marker":{"color":"rgba(44,160,44,1)","line":{"color":"rgba(44,160,44,1)"}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"line":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2012,2013,2014,2015,2016,2017,2018],"y":[1136719,1121770,1106434,1123156,1012323,986356,949652],"mode":"lines+markers","name":"Somalia","type":"scatter","marker":{"color":"rgba(214,39,40,1)","line":{"color":"rgba(214,39,40,1)"}},"error_y":{"color":"rgba(214,39,40,1)"},"error_x":{"color":"rgba(214,39,40,1)"},"line":{"color":"rgba(214,39,40,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2012,2013,2014,2015,2016,2017,2018],"y":[87009,114470,616211,778718,1436719,2439868,2285316],"mode":"lines+markers","name":"South Sudan","type":"scatter","marker":{"color":"rgba(148,103,189,1)","line":{"color":"rgba(148,103,189,1)"}},"error_y":{"color":"rgba(148,103,189,1)"},"error_x":{"color":"rgba(148,103,189,1)"},"line":{"color":"rgba(148,103,189,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2012,2013,2014,2015,2016,2017,2018],"y":[568943,648942,665967,627087,650640,694506,724791],"mode":"lines+markers","name":"Sudan","type":"scatter","marker":{"color":"rgba(140,86,75,1)","line":{"color":"rgba(140,86,75,1)"}},"error_y":{"color":"rgba(140,86,75,1)"},"error_x":{"color":"rgba(140,86,75,1)"},"line":{"color":"rgba(140,86,75,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2012,2013,2014,2015,2016,2017,2018],"y":[729022,2468332,3887491,4873243,5524377,6308619,6654386],"mode":"lines+markers","name":"Syrian Arab Republic","type":"scatter","marker":{"color":"rgba(227,119,194,1)","line":{"color":"rgba(227,119,194,1)"}},"error_y":{"color":"rgba(227,119,194,1)"},"error_x":{"color":"rgba(227,119,194,1)"},"line":{"color":"rgba(227,119,194,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Syria has sadly seen a dramatic increase in refugees leaving the country as a result of the Syrian civil war, which has been on going since March 2011. 

We now analyse which of the high income category countries are accepting the greatest number of refugees. We display the amount of refugees taken, and also express this amount as a percentage of the countries total population. 


```r
top_asylum_countries = wdi %>%
  filter(Year == 2018, !is.na(Region), `Income Group` == 'High income') %>%
  distinct() %>%
  arrange(desc(`Refugee population by country or territory of asylum`)) %>%
  mutate(pop_percent_refugee = signif((`Refugee population by country or territory of asylum`
         / `Population, total` * 100), 2)) %>%
  select(Country, `Refugee population by country or territory of asylum`,
         pop_percent_refugee) %>%
  head(8)

fig1 = plot_ly(data = top_asylum_countries, 
               x = ~`Refugee population by country or territory of asylum`,
               y = ~reorder(Country, `Refugee population by country or territory of asylum`),
               name = 'Total Refugee Population',
               type = 'bar', orientation = 'h',
               marker = list(color = 'rgba(50, 171, 96, 0.6)',
                              line = list(color = 'rgba(50, 171, 96, 1.0)',
                                          width = 1.2))) %>% 
  layout(yaxis = list(showgrid = FALSE,
                      showline = FALSE, 
                      showticklabels = TRUE,
                      domain= c(0, 0.85)),
                      xaxis = list(zeroline = FALSE,
                                   showline = FALSE, 
                                   showticklabels = TRUE, 
                                   showgrid = TRUE)) %>% 
  add_annotations(xref = 'x1',
                  yref = 'y',
                  x = top_asylum_countries$'Refugee population by country or territory of asylum' * 1.0,
                  y = top_asylum_countries$Country,
                  text = paste("                 ", 
                               round(top_asylum_countries$'Refugee population by country or territory of asylum', 2)),
                  font = list(family = 'Arial', 
                              size = 12,
                              color = 'rgb(50, 171, 96)'),
                   showarrow = FALSE)

fig2 = plot_ly(x = ~top_asylum_countries$pop_percent_refugee, 
               y = ~reorder(top_asylum_countries$Country, 
                            top_asylum_countries$`Refugee population by country or territory of asylum`),
               name = 'Refugee Percentage of Total Polulation',
               type = 'bar', orientation = 'h',
               marker = list(color = 'rgb(184, 94, 184)',
                              line = list(color = 'rgb(163, 0, 128)',
                                          width = 1.2))) %>% 
  layout(yaxis = list(showgrid = FALSE,
                      showline = TRUE,
                      showticklabels = FALSE,
                      linecolor = 'rgba(102, 102, 102, 0.8)',
                      linewidth = 2,
                      domain = c(0, 0.85)),
                      xaxis = list(zeroline = FALSE, 
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   showgrid = TRUE,
                                   side = 'top',
                                   dtick = 0.5)) %>% 
  add_annotations(xref = 'x2', 
                  yref = 'y',
                  x = top_asylum_countries$pop_percent_refugee, 
                  y = top_asylum_countries$Country,
                  text = paste("               ", top_asylum_countries$pop_percent_refugee, '%'),
                  font = list(family = 'Arial',
                              size = 12, 
                              color = 'rgb(128, 0, 128)'),
                  showarrow = FALSE)

fig = subplot(fig1, fig2) %>% 
  layout(title = 'High Income Countries With Highest Refugee Population 2018',
         legend = list(x = 0.029, 
                       y = 1.038,
                       font = list(size = 10)),
         margin = list(l = 100,
                       r = 20, 
                       t = 70, 
                       b = 70),
         paper_bgcolor = 'rgb(248, 248, 255)',
         plot_bgcolor = 'rgb(248, 248, 255)',
         width = 900) %>% 
  add_annotations(xref = 'paper',
                  yref = 'paper',
                  x = -0.14,
                  y = -0.15,
                  text = paste(''),
                  font = list(family = 'Arial', 
                              size = 10, 
                              color = 'rgb(150,150,150)'),
                  showarrow = FALSE)

fig
```

<!--html_preserve--><div id="htmlwidget-474f08afdbb4f5f4225c" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-474f08afdbb4f5f4225c">{"x":{"data":[{"x":[1063837,368352,313241,248226,189243,128769,126720,114109],"y":["Germany","France","United States","Sweden","Italy","Austria","United Kingdom","Canada"],"orientation":"h","marker":{"color":"rgba(50, 171, 96, 0.6)","line":{"color":"rgba(50, 171, 96, 1.0)","width":1.2}},"name":"Total Refugee Population","type":"bar","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1.3,0.55,0.096,2.4,0.31,1.5,0.19,0.31],"y":["Germany","France","United States","Sweden","Italy","Austria","United Kingdom","Canada"],"orientation":"h","marker":{"color":"rgb(184, 94, 184)","line":{"color":"rgb(163, 0, 128)","width":1.2}},"name":"Refugee Percentage of Total Polulation","type":"bar","error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x2","yaxis":"y2","frame":null}],"layout":{"xaxis":{"domain":[0,0.48],"automargin":true,"zeroline":false,"showline":false,"showticklabels":true,"showgrid":true,"anchor":"y"},"xaxis2":{"domain":[0.52,1],"automargin":true,"zeroline":false,"showline":false,"showticklabels":true,"showgrid":true,"side":"top","dtick":0.5,"anchor":"y2"},"yaxis2":{"domain":[0,0.85],"automargin":true,"showgrid":false,"showline":true,"showticklabels":false,"linecolor":"rgba(102, 102, 102, 0.8)","linewidth":2,"type":"category","categoryorder":"array","categoryarray":["Canada","United Kingdom","Austria","Italy","Sweden","United States","France","Germany"],"anchor":"x2"},"yaxis":{"domain":[0,0.85],"automargin":true,"showgrid":false,"showline":false,"showticklabels":true,"type":"category","categoryorder":"array","categoryarray":["Canada","United Kingdom","Austria","Italy","Sweden","United States","France","Germany"],"anchor":"x"},"annotations":[{"text":"                  1063837","xref":"x1","yref":"y","x":1063837,"y":"Germany","font":{"family":"Arial","size":12,"color":"rgb(50, 171, 96)"},"showarrow":false},{"text":"                  368352","xref":"x1","yref":"y","x":368352,"y":"France","font":{"family":"Arial","size":12,"color":"rgb(50, 171, 96)"},"showarrow":false},{"text":"                  313241","xref":"x1","yref":"y","x":313241,"y":"United States","font":{"family":"Arial","size":12,"color":"rgb(50, 171, 96)"},"showarrow":false},{"text":"                  248226","xref":"x1","yref":"y","x":248226,"y":"Sweden","font":{"family":"Arial","size":12,"color":"rgb(50, 171, 96)"},"showarrow":false},{"text":"                  189243","xref":"x1","yref":"y","x":189243,"y":"Italy","font":{"family":"Arial","size":12,"color":"rgb(50, 171, 96)"},"showarrow":false},{"text":"                  128769","xref":"x1","yref":"y","x":128769,"y":"Austria","font":{"family":"Arial","size":12,"color":"rgb(50, 171, 96)"},"showarrow":false},{"text":"                  126720","xref":"x1","yref":"y","x":126720,"y":"United Kingdom","font":{"family":"Arial","size":12,"color":"rgb(50, 171, 96)"},"showarrow":false},{"text":"                  114109","xref":"x1","yref":"y","x":114109,"y":"Canada","font":{"family":"Arial","size":12,"color":"rgb(50, 171, 96)"},"showarrow":false},{"text":"                1.3 %","xref":"x2","yref":"y2","x":1.3,"y":"Germany","font":{"family":"Arial","size":12,"color":"rgb(128, 0, 128)"},"showarrow":false},{"text":"                0.55 %","xref":"x2","yref":"y2","x":0.55,"y":"France","font":{"family":"Arial","size":12,"color":"rgb(128, 0, 128)"},"showarrow":false},{"text":"                0.096 %","xref":"x2","yref":"y2","x":0.096,"y":"United States","font":{"family":"Arial","size":12,"color":"rgb(128, 0, 128)"},"showarrow":false},{"text":"                2.4 %","xref":"x2","yref":"y2","x":2.4,"y":"Sweden","font":{"family":"Arial","size":12,"color":"rgb(128, 0, 128)"},"showarrow":false},{"text":"                0.31 %","xref":"x2","yref":"y2","x":0.31,"y":"Italy","font":{"family":"Arial","size":12,"color":"rgb(128, 0, 128)"},"showarrow":false},{"text":"                1.5 %","xref":"x2","yref":"y2","x":1.5,"y":"Austria","font":{"family":"Arial","size":12,"color":"rgb(128, 0, 128)"},"showarrow":false},{"text":"                0.19 %","xref":"x2","yref":"y2","x":0.19,"y":"United Kingdom","font":{"family":"Arial","size":12,"color":"rgb(128, 0, 128)"},"showarrow":false},{"text":"                0.31 %","xref":"x2","yref":"y2","x":0.31,"y":"Canada","font":{"family":"Arial","size":12,"color":"rgb(128, 0, 128)"},"showarrow":false},{"text":"","xref":"paper","yref":"paper","x":-0.14,"y":-0.15,"font":{"family":"Arial","size":10,"color":"rgb(150,150,150)"},"showarrow":false},{"text":"","xref":"paper","yref":"paper","x":-0.14,"y":-0.15,"font":{"family":"Arial","size":10,"color":"rgb(150,150,150)"},"showarrow":false},{"text":"","xref":"paper","yref":"paper","x":-0.14,"y":-0.15,"font":{"family":"Arial","size":10,"color":"rgb(150,150,150)"},"showarrow":false}],"shapes":[],"images":[],"margin":{"b":70,"l":100,"t":70,"r":20},"hovermode":"closest","showlegend":true,"title":"High Income Countries With Highest Refugee Population 2018","legend":{"x":0.029,"y":1.038,"font":{"size":10}},"paper_bgcolor":"rgb(248, 248, 255)","plot_bgcolor":"rgb(248, 248, 255)","width":900},"attrs":{"cbfc1bc84836":{"x":{},"y":{},"orientation":"h","marker":{"color":"rgba(50, 171, 96, 0.6)","line":{"color":"rgba(50, 171, 96, 1.0)","width":1.2}},"name":"Total Refugee Population","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"cbfc544347cd":{"x":{},"y":{},"orientation":"h","marker":{"color":"rgb(184, 94, 184)","line":{"color":"rgb(163, 0, 128)","width":1.2}},"name":"Refugee Percentage of Total Polulation","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"source":"A","config":{"showSendToCloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"subplot":true,"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

While Germany has accepted the greatest number of refugees of any high income country, Sweden has accepted the most when expressed as a percentage of their total population.  
