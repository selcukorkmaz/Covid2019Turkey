
underReporting <- function(){
  
  # Code to estimate reporting
  
  
  # Set up paths and parameters ---------------------------------------------
  
  # Load libraries
  library(tidyverse)
  library(padr)
  
  # setting functions for the delay distribution
  muTransform <- function(zMedian){
    mu <- log(zMedian) 
  }
  
  sigmaTransform <- function(zMean, mu){
    sigma <- sqrt(2*(log(zMean) - mu))
  }
  
  # Hospitalisation to death distribution
  hospitalisation_to_death_truncated <- function(x, mu, sigma) {
    plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
  }
  
  
  hospitalisation_to_death_truncated_low <- function(x){
    hospitalisation_to_death_truncated(x, muLow, sigmaLow)
  }
  
  hospitalisation_to_death_truncated_mid <- function(x){
    hospitalisation_to_death_truncated(x, muMid, sigmaMid)
  }
  
  hospitalisation_to_death_truncated_high <- function(x){
    hospitalisation_to_death_truncated(x, muHigh, sigmaHigh)
  }
  
  scale_cfr <- function(data_1_in, delay_fun){
    case_incidence <- data_1_in$new_cases
    death_incidence <- data_1_in$new_deaths
    cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
    # Sum over cases up to time tt
    for(ii in 1:nrow(data_1_in)){
      known_i <- 0 # number of cases with known outcome at time ii
      for(jj in 0:(ii - 1)){
        known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
        known_i <- known_i + known_jj
      }
      cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
    }
    # naive CFR value
    b_tt <- sum(death_incidence)/sum(case_incidence) 
    # corrected CFR estimator
    p_tt <- sum(death_incidence)/cumulative_known_t
    data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
               cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
  }
  
  # working out under-reporting estimate and CIs
  
  
  
  
  # setting the baseline CFR
  cCFRBaseline <- 1.4
  cCFREstimateRange <- c(1.2, 1.7)
  
  # set parameters of delay distribution -----------------------------------
  
  # lower end of the range
  zmeanLow <- 8.7
  zmedianLow <- 6.7
  muLow <- muTransform(zmedianLow)
  sigmaLow <- sigmaTransform(zmeanLow, muLow)
  
  
  # middle of the range
  zmeanMid <- 13
  zmedianMid <- 9.1
  muMid <- muTransform(zmedianMid)
  sigmaMid <- sigmaTransform(zmeanMid, muMid)
  
  # upper end of the range
  zmeanHigh <- 20.9
  zmedianHigh <- 13.7
  muHigh <- muTransform(zmedianHigh)
  sigmaHigh <- sigmaTransform(zmeanHigh, muHigh)
  
  # Load data -----------------------------------------------------
  
  httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".csv")))
  allDat <- readr::read_csv(tf)
  
  
  # munge data, pad data and select only those with greater than 10 deaths
  allTogetherClean <- allDat %>% 
    dplyr::arrange(countriesAndTerritories, dateRep) %>% 
    dplyr::mutate(dateRep = lubridate::dmy(dateRep))%>% 
    dplyr::rename(date = dateRep, new_cases = cases, new_deaths = deaths, country = countriesAndTerritories) %>%
    dplyr::select(date, country, new_cases, new_deaths) %>%
    dplyr::filter(country != "CANADA", 
                  country != "Cases_on_an_international_conveyance_Japan") %>%
    dplyr::group_by(country) %>%
    padr::pad() %>%
    dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                  new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
    dplyr::filter(cum_deaths > 0) %>%
    dplyr::select(-cum_deaths)
  
  underReportingEstimates <- function(data, delay_fun){ 
    dplyr::group_by(data, country) %>%
      dplyr::do(scale_cfr(., delay_fun)) %>%
      dplyr::filter(cum_known_t > 0 & cum_known_t >= total_deaths)  %>%
      dplyr::mutate(nCFR_UQ = binom.test(total_deaths, total_cases)$conf.int[2],
                    nCFR_LQ = binom.test(total_deaths, total_cases)$conf.int[1],
                    cCFR_UQ = binom.test(total_deaths, cum_known_t)$conf.int[2],
                    cCFR_LQ = binom.test(total_deaths, cum_known_t)$conf.int[1],
                    underreporting_estimate = cCFRBaseline / (100*cCFR),
                    lower = cCFREstimateRange[1] / (100 * cCFR_UQ),
                    upper = cCFREstimateRange[2] / (100 * cCFR_LQ),
                    quantile25 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[1],
                    quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2]) %>% 
      dplyr::filter(total_deaths > 10)}
  
  # calculate table of estimates using three delay distributions (both ends of the reported ranges and the mean)
  allTogetherLow <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_low) 
  allTogetherMid <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_mid) 
  allTogetherHigh <- underReportingEstimates(allTogetherClean, hospitalisation_to_death_truncated_high)
  
  
  # choosing CIs such that they include all uncertainty from delay distribution
  finalRes <- dplyr::tibble(
    country = allTogetherMid$country,
    total_cases = allTogetherMid$total_cases,
    total_deaths = allTogetherMid$total_deaths,
    underreporting_estimate  = pmin(allTogetherLow$underreporting_estimate, allTogetherMid$underreporting_estimate, allTogetherHigh$underreporting_estimate),
    lower = pmin(allTogetherLow$lower, allTogetherMid$lower, allTogetherHigh$lower),
    upper = pmax(allTogetherLow$upper, allTogetherMid$upper, allTogetherHigh$upper))
  
  
  # putting all of the data together in a readable format for the Rmd file
  reportDataFinal <- finalRes %>%
    dplyr::select(country, total_cases, total_deaths, underreporting_estimate, lower,
                  upper) %>%
    dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
    dplyr::mutate(upper = ifelse(upper <= 1, upper, 1)) %>%
    dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
    dplyr::mutate(lower = signif(lower, 2)) %>%
    dplyr::mutate(upper = signif(upper, 2)) %>%
    dplyr::ungroup(country) %>%
    dplyr::mutate(country = country %>% stringr::str_replace_all("_", " ")) %>% 
    dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                         "% (",lower*100,"% - ",upper*100,"%)"))
  
  
reportDataFinal

}
