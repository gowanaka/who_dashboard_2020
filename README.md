# WHO Interactive Dashboard: Explore the Disease Death Probability Dataset

### Go directly to the dashboard: -> [Dashboard Link](https://reminho.shinyapps.io/who_2020_shinydashboard/) <-

To explore the WHO Dataset on cumulative death probabilities of CDCC diseases (cancer, diabetes, cardiovascular and chronic respiratory diseases) 
across different countries and split up by gender, I built an interactive dashboard including inputs, infoboxes, 
plots and a chloropleth map using the packages *shiny*, *shinydashboard*, and *leaflet* in R. This is the first version of the dashboard.
A comprehensive notebook describing the dashboard can be found on my [Kaggle](https://www.kaggle.com/reminho/who-dashboard-eda).
<br>

![who_dash_foto](https://i.ibb.co/BLSWhgn/reminho-shinyapps-io-who-2020-shinydashboard.png)

# 1. Short description: What does it do?

So far the dashboard is divided into four tabs: **summary**, **inputs**, **plots** and **maps**.

* **Summary** summarizes the main findings of the data exploration with infoboxes, plots, and a map on one page.
    + All infoboxes except the one in the top right corner are responsive to the year input.
    + The left plot shows the CDCC death probability for a given country over time (from 2000 to 2016) and split up by sex. 
    Users can select the country they want to look at in the input from the sidebar.
        + Update (06/01/2021): It is now possible to compare two countries in the same plot by clicking "Compare to another country"!
    * The map on the right shows the whole world with the countries depicted as interactive polygons. 
    Each polygon is colored by the country's CDCC death probability in a given year. Red signals higher and green signals lower probability. 
    Users can select the year.
* **Inputs** shows an interactive datatable of the dataset used to build the plots and map(s).
* **Plots** is populated with plots.
    + Update (10/01/2021): Added "compare to another country" function to the second plot. It will now show a comparison of two countries should the user select that.
    + Update (09/01/2020): Added the summary plot again. Added a second plot showing the percent difference in death probability of a given period compared to the previous period. The dashed line represents no change (0%). Everything below is a reduction, Everything above is an increase. The year 2000 is not shown, because it is the earliest period in the dataset.
* **Maps** will include a bigger map.
