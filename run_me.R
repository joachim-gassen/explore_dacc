# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2019, gassen@wiwi.hu-berlin.de
# See LICENSE file for details 
#
# Prepares the "Explore Discretionary Accruals" display 
# ------------------------------------------------------------------------------

# --- Setup --------------------------------------------------------------------
# Start this with a virgin R session
# You will need the packages in the install.packages() call below to run 
# the code. Install them by uncommenting the code below if need be.
#
#install.packages(c("rstudioapi", "RPostgres", "DBI",
#                   "tidyverse", "broom", "lubridate", "ExPanDaR", "zoo",
#                   "modelr"))

# --- Configuration ------------------------------------------------------------
# Set the below to TRUE if you want repull Data from WRDS 
# This needs to be done at least once.
#
# You will be asked for your WRDS username/password.
# Your password will not be stored.
# Pulling data will take a while.

pull_wrds_data <- TRUE

# --- End of Configuration - no editing needed below this line -----------------

# --- Check for R environment --------------------------------------------------

if (getRversion() < "3.3") stop(paste("You are running a too old R version.",
                                      "At least version 3.3 is required."))


library(rstudioapi)

if (versionInfo()$version <= "1.1.67") 
  stop(paste("You are running a too old RStudio version.",
             "At least version 1.1.67+ is required."))


# --- Generate samples ---------------------------------------------------------

if (pull_wrds_data) source("code/pull_wrds_data.R", local = new.env())

# --- Setup libraries and define helper functions ------------------------------

library(tidyverse)
library(zoo)
library(ExPanDaR)
library(modelr)
library(broom)
library(lubridate)

accrual_model_mjones <- function(df) {
  lm(tacc ~ inverse_a + drev + ppe, data = df)
}

accrual_model_dd <- function(df) {
  lm(dwc ~ lagcfo + cfo + leadcfo, data = df)
}

tcoef <- function(m) {as_tibble(t(coef(m)))}

mleadlag <- function(x, n, ts_id) {
  pos <- match(as.numeric(ts_id) + n, as.numeric(ts_id))
  x[pos]
}

winsorize <- function(df, percentile = 0.01, 
                      include=NULL, exclude=NULL, byval=NULL) {
  if (!is.null(exclude) & !is.null(include)) 
    stop("You can only set exclude or include, not both.")
  if (!is.null(exclude)) vars <- !(names(df) %in% exclude)
  else if (!is.null(include)) {
    if (!is.null(byval)) include <- c(byval, include) 
    vars <- names(df) %in% include
  }
  else vars <- names(df)
  ret <- df
  ret[vars] <- treat_outliers(ret[vars], percentile, by = byval)
  return(ret)
}


# --- Prepare base sample ------------------------------------------------------

ff12 <- readRDS("data/ff_12_ind.RDS")
ff48 <- readRDS("data/ff_48_ind.RDS")

rest_data <- readRDS("data/rest_data.RDS")

us_base_sample <- readRDS("data/cstat_us_sample.RDS") %>%
  filter(indfmt == "INDL",
         fic == "USA",
         !is.na(at),
         at > 0,
         !is.na(sale),
         sale > 0) %>%
  mutate(sic = ifelse(!is.na(sich), sprintf("%04d", sich), sic)) %>%
  filter(!is.na(sic),
         as.numeric(sic) < 6000 | as.numeric(sic) > 6999) %>%
  left_join(ff48, by = "sic") %>%
  left_join(ff12, by = "sic") %>%
  filter(!is.na(ff48_ind) & !is.na(ff12_ind)) %>%
  distinct(gvkey, fyear, .keep_all = TRUE) %>%
  droplevels()

rd <- us_base_sample %>%
  select(gvkey, fyear, cik, datadate) %>%
  rename(fye = datadate) %>%
  mutate(fys = fye - years(1)) %>%
  full_join(rest_data, by = c("cik" = "company_fkey")) %>%
  filter(!is.na(res_notif_key),
         !(fys >  res_end_date),
         !(fye < res_begin_date)) %>%
  group_by(gvkey, fyear) %>% 
  summarise(res_accounting = sum(res_accounting) > 0,
            res_fraud = sum(res_fraud) > 0,
            res_sec_invest = sum(res_sec_invest) > 0,
            res_adverse = sum(res_adverse) > sum(res_improves)) %>%
  filter(res_accounting | res_fraud | res_sec_invest) 


# --- Calculate modified Jones model accruals and statistics -------------------

# Methodology is somewhat loosely based on Hribar and Nichols (JAR, 2007)
# https://doi.org/10.1111/j.1475-679X.2007.00259.x

mj <-   us_base_sample %>%
  group_by(gvkey) %>%
  mutate(lagta = mleadlag(at, -1, fyear),
         tacc = (ibc - oancf)/lagta,
         drev = (sale - mleadlag(sale, -1, fyear) + recch)/lagta,
         inverse_a = 1/lagta,
         ppe = ppegt/lagta) %>%
  filter(!is.na(tacc),
         !is.na(drev),
         !is.na(ppe)) %>%
  group_by(ff48_ind, fyear) %>%
  filter(n() >= 10) %>%
  winsorize(include = c("tacc", "drev", "inverse_a", "ppe")) %>%
  nest() %>%
  mutate(model = map(data, accrual_model_mjones)) 

mj_resids <- mj %>%
  mutate(residuals = map2(data, model, add_residuals)) %>%
  unnest(residuals) %>%
  rename(mj_da = resid) %>%
  select(gvkey, fyear, mj_da)

mj_adjr2s <- mj %>%
  mutate(glance = map(model, glance)) %>%
  unnest(glance) %>%
  rename(mj_adjr2 = adj.r.squared) %>%
  select(ff48_ind, fyear, mj_adjr2)

mj_coefs <- mj %>%
  mutate(coef = map(model, tcoef)) %>%
  select(ff48_ind, fyear, coef) %>%
  unnest(coef) %>%
  rename(mj_intercept = "(Intercept)",
         mj_inverse_a = inverse_a,
         mj_drev = drev,
         mj_ppe = ppe)


# --- Calculate Dechow/Dichev accruals and statistics --------------------------

# Methodology is based on Dechow and Dichev (TAR, 2002)
# https://doi.org/10.2308/accr.2002.77.s-1.35

dd <- us_base_sample %>%
  group_by(gvkey) %>%
  mutate(avgta = (at + mleadlag(at, -1, fyear))/2,
         cfo = oancf/avgta,
         lagcfo = mleadlag(cfo, -1, fyear),
         leadcfo = mleadlag(cfo, +1, fyear),
         dwc = -(recch + invch + apalch + txach + aoloch)/avgta) %>%
  filter(!is.na(dwc),
         !is.na(cfo),
         !is.na(lagcfo),
         !is.na(leadcfo)) %>%
  group_by(ff48_ind, fyear) %>%
  filter(n() >= 10) %>%
  winsorize(include = c("dwc", "cfo", "lagcfo", "leadcfo")) %>%
  nest() %>%
  mutate(model = map(data, accrual_model_dd)) 

dd_resids <- dd %>%
  mutate(residuals = map2(data, model, add_residuals)) %>%
  unnest(residuals) %>%
  rename(dd_da = resid) %>%
  select(gvkey, fyear, dd_da)

dd_adjr2s <- dd %>%
  mutate(glance = map(model, glance)) %>%
  unnest(glance) %>%
  rename(dd_adjr2 = adj.r.squared) %>%
  select(ff48_ind, fyear, dd_adjr2)

dd_coefs <- dd %>%
  mutate(coef = map(model, tcoef)) %>%
  select(ff48_ind, fyear, coef) %>%
  unnest(coef) %>%
  rename(dd_intercept = "(Intercept)",
         dd_lagcfo = lagcfo,
         dd_cfo = cfo,
         dd_leadcfo = leadcfo)


# --- Merge data and prepare samples -------------------------------------------

base_grid <- expand.grid(gvkey = unique(us_base_sample$gvkey),
                         fyear = unique(us_base_sample$fyear),
                         stringsAsFactors = FALSE) %>%
  arrange(gvkey, fyear) 

wide_sample <- base_grid %>%
  left_join(us_base_sample, by = c("gvkey", "fyear")) %>%
  left_join(mj_resids, by = c("gvkey", "fyear")) %>%
  left_join(mj_adjr2s, by = c("fyear", "ff48_ind")) %>%
  left_join(mj_coefs, by = c("fyear", "ff48_ind")) %>%
  left_join(dd_resids, by = c("gvkey", "fyear")) %>%
  left_join(dd_adjr2s, by = c("fyear", "ff48_ind")) %>%
  left_join(dd_coefs, by = c("fyear", "ff48_ind")) %>%
  left_join(rd, by = c("gvkey", "fyear")) %>%
  mutate(ta = at,
         avgta = (at + lag(at))/2,
         sales = sale,
         mktcap = csho * prcc_f,
         ln_ta = log(at),
         ln_sales = log(sales),
         ln_mktcap = log(mktcap),
         mtb = (csho * prcc_f)/ceq,
         sales_growth = sale/lag(sale),
         leverage = lt/at,
         sd_cfo = rollapply(oancf/lag(at), 
                            width = 5, FUN = sd, align = "right", fill = NA),
         sd_rev = rollapply((sale + recch)/lag(at), width = 5, FUN = sd, 
                            align = "right", fill = NA),
         ppe_ta = ppent/at,
         int_ta = intan/at,
         gwill_ta = gdwl/at,
         acq_sales = (ifelse(!is.na(aqs), aqs, 0) + 
                        ifelse(!is.na(acqsc), acqsc, 0))/sale,
         cogs_sales = cogs/sale,
         ebit_sales = (ib + xint)/sale,
         ebit_avgta = (ib + xint)/avgta,
         cfo_avgta = oancf/avgta,
         tacc_avgta = (ibc - oancf)/avgta,
         ceq_ta = ceq/at,
         mj_ada = abs(mj_da),
         dd_ada = abs(dd_da),
         res_accounting = ifelse(is.na(res_accounting), FALSE, res_accounting), 
         res_fraud = ifelse(is.na(res_fraud), FALSE, res_fraud), 
         res_sec_invest = ifelse(is.na(res_sec_invest), FALSE, res_sec_invest), 
         res_adverse = ifelse(is.na(res_adverse), FALSE, res_adverse), 
         restatement = res_accounting | res_fraud | res_sec_invest) %>%
  select(gvkey, conm, fyear, ff12_ind, ff48_ind,
         ta, sales, mktcap, ln_ta, ln_sales, ln_mktcap,
         mj_da, dd_da, mj_ada, dd_ada,
         mtb, sales_growth, leverage, sd_cfo, sd_rev,
         ppe_ta, int_ta, gwill_ta, 
         ceq_ta, leverage, 
         acq_sales, cogs_sales, ebit_sales, 
         ebit_avgta, cfo_avgta, tacc_avgta,
         restatement, res_accounting, res_fraud, res_sec_invest, res_adverse,
         mj_intercept, mj_inverse_a, mj_drev, mj_ppe, mj_adjr2,
         dd_intercept, dd_lagcfo, dd_cfo, dd_leadcfo, dd_adjr2) %>%
  filter(!is.na(mj_da),
         fyear > 1993,
         fyear < 2017)

expand_sample <- wide_sample %>%
  select(gvkey, conm, fyear, ff12_ind, ff48_ind,
         mj_da, mj_ada, dd_da, dd_ada,
         ln_ta, ln_mktcap,
         mtb, sales_growth, leverage, sd_cfo, sd_rev,
         int_ta, leverage, 
         cfo_avgta, tacc_avgta, acq_sales,
         restatement, res_accounting, res_fraud, res_sec_invest, res_adverse)


# --- Start ExPanD -------------------------------------------------------------

df_def <- tibble(
  var_name = names(expand_sample),
  var_def = c("Compustat identifier",
              "Company name",
              "Fiscal year",
              "Fama/French 12 industries group",
              "Fama/French 48 industries group",
              "Signed discretionary accruals (modified Jones model)",
              "Unsigned discretionary accruals (modified Jones model)",
              "Signed discretionary accruals (Dechow/Dichev model)",
              "Unsigned discretionary accruals (Dechow/Dichev model)",
              "Natural log of total assets (in US-$ Mio.)",
              "Natural log of market capitalization (in US-$ Mio.)",
              "Market to book ratio",
              "Sales growth (sales over prior year sales)",
              "Leverage (total liabilities over total assets)",
              "5 year rolling firm-level standard deviation of cash flow from operations, deflated by lagged total assets",
              "5 year rolling firm-level standard deviation of cash revenue, deflated by lagged total assets",
              "Intangible assets over total assets",
              "Cash flow from operations over average total assets",
              "Total accruals over average total assets",
              "Sales contribution of acquistions over sales",
              "One if fiscal year overlaps with a restatement period that is linked to GAAP application, fraud, and/or where the SEC is involved",
              "One if restatement is linked to GAAP application failure",
              "One if restatement is linked to financial fraud, irregularities and/or misrepresentations",
              "One if SEC in involved in the restatement process",
              "One if (net) effect of restatement on financial statement is adverse"),
  type = c("cs_id", "cs_id", "ts_id", "factor", "factor",
           rep("numeric", 15), rep("logical", 5))
)

theme_set(theme_minimal())

ExPanD(expand_sample, df_def = df_def,
       title = "Explore Discretionary Accruals",
       abstract = paste(
         "<p>&nbsp;</p>",
         "<p>This display uses data from Compustat and Audit Analytics",
         "to let you explore the association of established discretionary",
         "accrual measures with selected firm-year accounting data",
         "for a large panel of U.S. incorporated non-financial firms.",
         "</p><p>The methodology loosely follows",
         "<a href = https://doi.org/10.1111/j.1475-679X.2007.00259.x>",
         "Hribar and Nichols (JAR, 2007)</a>. The discretionary",
         "accruals are estimated for all Fama/French 48 industry-year bins",
         "with at least 10 observations available.</p>",
         "<p>The code to",
         "create this display is avaiable on", 
         "<a href=https://github.com/joachim-gassen>Github</a>.</p>",
         "<p>Scroll down and explore!<p>"),
       components = c(grouping =T, bar_chart = T, 
                      descriptive_table = T, udvars = T, missing_values = T, 
                      histogram = T, by_group_bar_graph = T, 
                      by_group_violin_graph = T, 
                      trend_graph = T, quantile_trend_graph = T, 
                      corrplot = T, scatter_plot = T, regression = T),
       config_list = readRDS("data/expand_config.RDS"))
