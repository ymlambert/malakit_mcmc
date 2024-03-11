# malakit_mcmc
Malakit modeling study - Data and MCMC scripts

## Start
Launch script.R
The MCMC runs performed in the Malakit modeling study are listed in "runs.csv"

## Calibration datasets
### Surveillance data
_data/data_surv_monthly.csv_

Monthly cases of malaria notified in Brazil and Suriname, between April 2014 and March 2020, with a likely place of contamination in French Guiana. 
* **month:** month of notification
* **obs_pf:** cases positive for P. falciparum
* **obs_pv:** cases positive for P. vivax

### Temperature data
_data/data_meteo_daily.csv_

Average of maximum daily temperature measured accross four Meteo France weather stations in French Guiana
* **day:** date of measurement
* **TX:** average maximum daily temperature

## MCMC outputs
MCMC outputs are saved in the _output_ subfolder in rds format.

Use _loadTraces()_ from _mcmc/post_mcmc.R_ to load traces from one MCMC run.
