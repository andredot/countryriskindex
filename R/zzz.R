# Fix “no visible binding for global variable” warnings
# These are caused by the use of non-standard evaluation (NSE) in packages like
# dplyr, ggplot2, etc.

utils::globalVariables(c(
  # Existing tidyverse/NSE variables
  "...2", ".data", ":=", "aes", "all_of", "as.formula", "bin", "capacity",
  "cause_id", "cause_label", "correlation", "count", "country", "country.x",
  "country.y", "crisis_impact", "delta", "delta_diff", "desc", "dummy",
  "element_text", "everything", "geom_linerange", "geom_point", "ggplot",
  "if_all", "improved_sanitation", "improved_water", "indicator_name", "iso3",
  "iso_a3", "labs", "location_id", "location_name", "operating_environment",
  "overall_risk", "people_conditions", "quantile", "reorder", "risk",
  "risk_type", "setNames", "severity_adjusted_risk", "theme", "val", "value",
  "vulnerability_score", "where", "year_id", "capacity_score",
  "per capita public and private expenditure on health care",

  # Domain-specific variables
  "Access to electricity",
  "Access to essential medicines at health facilites",
  "Adult literacy rate", "Age Group of Women", "All causes",
  "Antibiotic consumption pattern", "COUNTRY", "CRISIS", "CoPA-IotC", "Color",
  "Complexity of the crisis", "Conditions of people affected", "Country",
  "DIM_GEO_CODE_TYPE", "DIM_TIME", "DRIVERS", "Definition",
  "Density of doctors",
  "Development assistance to medical research and basic health",
  "Drinking water", "Enteric infections", "Food Security", "GEO_NAME_SHORT",
  "General government expenditure on domestic health", "Geographic area",
  "Global_Avg", "Governance", "HAZARD & EXPOSURE", "Highest",
  "Household health expenditure greater than 25% of household budget",
  "INFORM RISK", "INFORM Severity Index", "ISO3", "ISO_A3",
  "Impact of the crisis", "Index", "Index_adjusted", "Indicator",
  "Internet users", "Interval", "LACK OF COPING CAPACITY",
  "Lack of Reliability (*)", "Location ID", "Lowest",
  "Mobile cellular subscriptions",
  "Neglected tropical diseases and malaria", "Observation Status",
  "Observation Value", "Operating environment", "Other injuries",
  "Physical infrastructure", "REF_AREA", "Reference Date", "Regional group",
  "Respiratory infections and tuberculosis", "Road density...16", "Sanitation",
  "Sex", "Sexually transmitted infections", "Size", "Society and safety",
  "Socio-Economic Vulnerability", "Time Since First Birth", "Total",
  "Transport injuries", "UHC Service coverage index", "Unit of measure",
  "Unsafe water, sanitation and hygiene services deaths", "Uprooted people",
  "VULNERABILITY", "Value", "Var1", "Var2", "Variable", "Violence injuries",
  "Wealth Quintile", "Cardiovascular diseases", "Other NCDs",
  # v4.10
  "cause_name", "cause_label", "cause_vars", "year_uhc", "year_doctors",
  "DIM_TIME", "Country", "ISO3", "COUNTRY", "n_crises",
  "severity_uplift", "overall_risk", "low_confidence", "data_completeness",
  "Ora di completamento", "Your Location",

  # Variables from countryriskindex functions
  "response", "disease", "question", "prop", "n", "score",
  "diagnosis delay", "transport delay",
  "diagnosis_question", "transport_question",
  "diagnosis_prop", "transport_prop",
  "vc_prop",
  "4h diagnosis delay", "4h transport delay",
  "Patient wellbeing impact", "Patient life impact",

  # New global variables to silence NOTES
  "final_score", "any_of", "col_name"
))
