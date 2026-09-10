library(tidyverse)
library(tidycensus)

#I'm going to load some data now
pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

#knowing how many rows and columns
dim(pa_income) #67 rows, 5 column

#knowing more about the data
glimpse(pa_income) #67rows, 5 column

#the row numbers match the number of county in PA. It's because we set "county" as geography when we load the census data

#viewing the first 10 rows of the data
head(pa_income,10)

#look at the geoid
pa_income$GEOID

as.numeric("01001")
#the zero in the beginning is gone

#filtering variable
filter(pa_income, estimate > 60000) #56 counties
filter(pa_income, moe > 3000) #15 counties
filter(pa_income, estimate < 50000) #1 county, Cameron County

#select column
select(pa_income, NAME, estimate, moe)
select(pa_income, GEOID, estimate)

#mutate, make new column
mutate(pa_income, moe_pct = moe / estimate * 100)
pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)
pa_income
#moe_pct tell us about how high/large the moe compared to the estimate so that we can determine whether the data is reliable or not

#arrange -> Sort
arrange(pa_income, moe_pct)
arrange(pa_income, desc(moe_pct)) #the top county: Cameron County


#pipe %>%, and then
step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income %>%
  filter(moe_pct >5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)


# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct
pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(estimate) %>%
  select (NAME, moe_pct)

worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(estimate) %>%
  select (NAME, moe_pct)

#group_by() + summarize ()
pa_income <- mutate(pa_income, reliable = moe_pct < 5)
pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean (estimate))

#case_when() — sorting into categories
pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High Confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE ~ "Low Confidance"
  ))
count(pa_income, reliability) #High Confidence = 26, Low Confidance = 7, Moderate = 43

#Two variables, and a shape problem
pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)
pa_two #there are 134 rows because each variable has its own row


pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)
pa_wide #E=estimate, M=moe


pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
#large moe in counties with fewer population


#Massachussets
ma_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "MA", year = 2023, survey = "acs5",
  output = "wide")
ma_wide

ma_wide %>%
  mutate(moe_pct = incomeM / incomeE *100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
# MA also have large moe in counties with fewer population


#Other Variable
load_variables(2023, "acs5")
view(load_variables(2023, "acs5"))


pa_drive <- get_acs(
  geography = "county",
  variables = c(drive = "B08101_009",
                pop = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)
pa_drive

pa_drive %>%
  mutate(moe_pct = driveM / driveE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, driveE, moe_pct) %>%
  head(10)
# same pattern, large moe in counties with fewer population

#Census Tract Philadelphia
phl_tract <- get_acs(
  geography = "tract",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "PA", county = "Philadelphia", year = 2023, survey = "acs5",
  output = "wide"
)
phl_tract
phl_tract %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
#very high moe_pct
