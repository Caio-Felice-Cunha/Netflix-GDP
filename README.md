# Netflix and GDP

Analyzing in R how a country's GDP relates to Netflix subscriptions, revenue, catalog size, and plan prices.

[Executive Report](https://github.com/Caio-Felice-Cunha/Netflix-GDP/blob/main/Executive%20Report.pdf) (the short read) and [Full Report](https://github.com/Caio-Felice-Cunha/Netflix-GDP/blob/main/Full%20Report.pdf) (the detailed walk-through).

![Netflix](https://user-images.githubusercontent.com/111542025/225651819-cc89de1e-39fb-486b-a105-b348d290c634.png)

## What this is

A data analysis project that joins six public datasets to ask one business question: how can Netflix grow in countries with a smaller GDP? The output is a set of plotly charts (scatter plots, a billing vs subscriptions view, two choropleth maps) and two written reports. The final combined dataset covers 38 countries with 16 variables (catalog size, the three plan prices, GDP, the gini disposable-income index, Q4-2021 subscribers and revenue, and the ISO code).

## Business problem

Netflix is an American streaming service (the streaming service launched in 2007 in the US, and international expansion began in 2010 with Canada). It now operates in over 190 countries. GDP measures the monetary value of final goods and services produced in a country in a given period. The question this project explores: where the GDP is lower, what should Netflix change to win more subscribers?

The working assumption is that Netflix wants more subscribers to grow profitability and influence in the markets where it operates.

## Data sources

- IMDB title data: https://datasets.imdbws.com/
- Netflix plan prices and catalog by country: https://www.comparitech.com/blog/vpn-privacy/countries-netflix-cost/
- GDP (World Bank): https://data.worldbank.org/indicator/
- Wage inequality (gini): https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LM4OWF
- Top 10 Netflix shows: https://top10.netflix.com/
- Netflix subscribers and revenue by country: https://www.comparitech.com/tv-streaming/netflix-subscribers/
- ISO country codes: https://www.kaggle.com/datasets/andradaolteanu/iso-country-codes-global

Most inputs are committed under `Datasets/raw/`. The one exception is the IMDB file (see below).

## How to run

1. Install R (4.x) and these packages:

   ```r
   install.packages(c("dplyr", "tidyr", "readxl", "readr", "plotly"))
   ```

2. Get the IMDB title basics file (it is large, about 800MB, so it is not committed). Download `title.basics.tsv.gz` from https://datasets.imdbws.com/, extract it, and save it as `Datasets/raw/data.tsv`. Stages 3 to 7 (the genre pipeline) need this file. The first two stages and all the country-level charts run without it because the cleaned outputs are committed under `Datasets/clean/`.

3. Open `GDP x Netflix Analysis.R` from the repository root and run it. In RStudio use Session > Set Working Directory > To Source File Location first, so the relative `Datasets/...` paths resolve. The script reads from `Datasets/raw/`, writes cleaned tables to `Datasets/clean/`, and renders the plotly figures.

## Solution strategy

- Import and inspect the data.
- Clean and combine the six datasets into one country-level table (`Datasets/clean/complete.csv`).
- Build the IMDB genre pipeline (sunburst, country tree) for the top-10 genre views.
- Visualize: GDP against revenue, subscribers, catalog size, and the three plan prices, plus a billing vs subscriptions scatter and two world maps.

## Results

These are the findings from the committed reports and datasets. Numbers are quoted as they appear in the reports.

- GDP and revenue track together only partially. Up to Canada there is a clear positive relationship between revenue and GDP. Italy breaks the pattern, with lower Netflix revenue than Spain despite a much higher GDP. The United States is excluded from the analysis as an extreme outlier. (Executive Report, p.1)
- Monetization differs sharply by market. The UK produces almost 500 million in revenue with 13 million subscribers, while Brazil needs 19 million subscribers to return 420 million. (Executive Report, p.2)
- There is a subscriber inflection point around 6.7M subscribers, where the scenario can shift, often in developing countries such as Brazil and Mexico. (Executive Report, p.3)
- A catalog-size paradox: several low-revenue countries (Hungary, Bulgaria, Lithuania) carry the largest catalogs. The report recommends reducing catalog size and price in developing countries and considering a cheaper "basic+" tier. (Executive Report, p.3)
- Scale of the genre input: the IMDB title file read for the genre pipeline had 9,512,109 rows and 9 columns. (Full Report, p.2)
- Top-10 genre analysis covered 134,791 total genre appearances across 28 genres (for example Action 18,782, Adventure 10,834, Western 453). (`Datasets/clean/sunburst.csv`)

## Conclusion

Higher-GDP countries tend to have more subscribers, but the country's development stage (developing vs developed) matters as much as raw GDP. For less-developed markets, one path is to trim the very large catalog so plan prices can come down, possibly alongside a simpler, cheaper plan tier.

## Data notes and known issues

These are documented so the numbers can be read honestly.

- GDP year: the column in `Datasets/clean/complete.csv` was originally labeled "2020 GDP" but the values are 2019 World Bank GDP. In the raw World Bank file the year header maps column V64 to 2019 and V65 to 2020, and the pipeline kept V64. As proof, Brazil's stored value (1,877,824,273,720.78) is the 2019 figure, while the real 2020 value (with the COVID drop) is 1,444,733,258,971.65. The column is now labeled "2019 GDP (World Bank)" in the CSV and in the script. The two committed PDFs still say "2020" on their charts. To switch to true 2020 GDP, keep V65 in the script and regenerate the cleaned data.
- Genre sunburst slice: the script previously removed 28 header rows where it should have removed 29 (1 grand-total row plus 28 genre aggregates), so the "Western" aggregate (n=453) leaked into the country-level data in `Datasets/clean/top10sunburst.csv` and inflated the grand total. The script has been fixed to filter by the parent and label fields instead of a fixed row count. The committed `top10sunburst.csv` was produced by the older version and will be corrected on the next run with R installed.
- Inequality angle: gini disposable income is merged into `complete.csv`, but the committed charts and reports are GDP-focused. A gini vs subscriptions chart is a natural next addition since the data is already in place.

## Next steps

- Build a Power BI dashboard.
- Add a gini vs subscriptions chart to use the inequality data already merged in.
- Quantify the GDP-to-subscribers correlation rather than reading it from the scatter plots.

## Disclaimer

A good part of this project was done as part of the Data Science Academy "Big Data Analytics with R and Microsoft Azure Machine Learning" course (part of the Data Scientist training).
