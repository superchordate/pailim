# Load prior data for validation into separate variables
prior_env <- new.env()
load('prior-data.RData', envir = prior_env)

# Extract prior data from the environment
pa_prior <- prior_env$pa
il_prior <- prior_env$il
cm_prior <- prior_env$cm

il %<>% mutate(
  Date = as.character(Date),
  Month = as.character(Month),
  Add = as.integer(Add),
  MonthNum = as.integer(MonthNum),
  Week = as.integer(Week),
) %>%
  arrange(Case.ID)

il_prior %<>% arrange(Case.ID)

validate.equal(il_prior, il[1:nrow(il_prior), ])

pa %<>% mutate(
  Date = as.character(Date),
  Month = as.character(Month),
  Add = as.integer(Add),
  MonthNum = as.integer(MonthNum),
  Week = as.integer(Week),
  Consequence.3 = as.logical(Consequence.3),
) %>%
  arrange(Case.ID)

pa_prior %<>% arrange(Case.ID)

validate.equal(pa_prior, pa)

cm %<>% mutate(
  Date = as.character(Date),
  Month = as.character(Month),
  Add = as.integer(Add),
  MonthNum = as.integer(MonthNum),
  Week = as.integer(Week)
)

validate.equal(cm_prior, cm)
