# Data Challenge 1: Which companies has survived a financial crisis?
Data challenge 1 at [Propulsion Academy](https://www.linkedin.com/school/propulsion-academy/?originalSubdomain=ch)

- Web scraping of financial reports in Python and statistical analysis of companies current ratios to see if it can be used to predict the probability of surviving a financial crisis (2008 crisis).
- The scraped financial reports can later be used for sentiment/theme analysis using NLP.
- [Google Slides presentation](https://docs.google.com/presentation/d/1wXfmQQvGGECkWRxRInyPSN39wJe4Ahna1tl07QzykXA/edit?usp=sharing) of this project (preliminary results).
- [Google Doc notes/brainstorming](https://docs.google.com/document/d/1AniEjj6gImymUvTHtJh18frXXIqXaIMA6B2j4qhAVQU/edit) for this project.

### Project contributors
- Matthias Galipaud
- Peerawan Wiwattananon
- Mevluet Polat (mpolat@mpol.ch)
- Anselme Borgeaud (aborgeaud@gmail.com)

### Content
- Python data scraping and basic analysis scripts are in the notebooks folder
- R data analysis (incl. survival analysis) are in the R analysis folder

### Data files
- [Companies from 2005 with their most recent SEC filling dates (6409 companies) [CSV] (companies_filling_minimal.csv)](https://www.dropbox.com/s/ukvmv87cm88iz7e/companies_filling_minimal.csv?dl=0)
- [Stock closing price data [Parquet] (stock_closes.pq)](https://www.dropbox.com/s/uqhczpn4fxak8w9/stock_closes.pq?dl=0)
- [stock_close.pq only for companies in mevluet_data_merged.csv [CSV] (prices_mevluet_data.csv)](https://www.dropbox.com/s/tp1on9hx1iqvp7i/prices_mevluet_data.csv?dl=0)
- [Bulk financial report data (from 2005, but data mostly from 2008) [CSV] (mevluet_data_merged.csv)](https://www.dropbox.com/s/z119lsqk57g09v4/mevluet_data_merged.csv?dl=0)
- [current assets and liabilities data for companies since 1993 scraped from the SEC website [CSV] (scraped_companies.csv)](https://www.dropbox.com/s/pvsqovoflkmaxpg/scraped_companies.csv?dl=0)
- [scraped_companies.csv merged with companies_fillinf_minimal.csv [CSV] (scraped_companies_merged_survival.csv)](https://www.dropbox.com/s/svk4l9qzkp9vj0u/scraped_companies_merged_survival.csv?dl=0)
- [scraped_companies_merged_survival.csv filtered for continuous records that includes fiscal year 2005 [CSV] (scraped_companies_merged_survival_continuous_from2005.csv)](https://www.dropbox.com/s/at23ufz0ts03enf/scraped_companies_merged_survival_continuous_from2005.csv?dl=0)

