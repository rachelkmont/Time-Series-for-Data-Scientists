EGA Robustness
================

# Exploratory Graph Analysis (EGA) Robustness of the BFI-2 Dataset

In this assignment, we delve into the robustness of the Big Five
Inventory-2 (BFI-2) dataset using Exploratory Graph Analysis (EGA). We
aim to investigate measurement invariance across different times and age
groups, and identify any redundancies in the survey data.

## Setting up the Environment

## Goal of Analysis

*Our goal is to answer these research questions:*

1.  Is there measurement invariance between survey time 1 and time 2 for
    each age group?

2.  Is there measurement invariance between survey time 1 between the
    ages (younger and middle-aged)?

3.  Are there any redundancies in survey time 1 for either age?

## Loading and Pre-processing the Data

Let’s start by loading the data and the codebook:

``` r
# Loading the BFI-2 survey data
bfi2_data <- read.csv("bfi-2_data.csv")

# Importing the BFI-2 codebook for item descriptions
bfi2_codebook <- readxl::read_excel("bfi-2_codebook.xlsx")

# Renaming the variables in bfi2_data using item descriptions from the codebook
colnames(bfi2_data)[grepl("BFI", colnames(bfi2_data))] <- bfi2_codebook$item_description
```

## Data Summary

The data is derived from the Big Five Inventory 2 (BFI-2), which you can
find more about here: [BFI-2
Reference.](https://psycnet.apa.org/record/2016-17156-001?doi=1)

BFI-2 is built on the Big Five personality theory and consists of 60
items—12 for each trait. Each of these traits is further broken down
into 3 facets, each with 4 items. These facets aim to reflect specific
attributes of personality under the umbrella of broader traits. For a
detailed breakdown, you can refer to the `bfi-2_codebook.xlsx.`

The core traits of the Big Five, or the “Five Factor Model,” are:

- Openness to Experience (O)

- Conscientiousness (C)

- Extraversion (E)

- Agreeableness (A)

- Neuroticism (N)

The dataset at hand is sourced from the following study: [Study
Link.](https://osf.io/preprints/psyarxiv/bzkwd/)

For context on the sample used:

The study titled “LOOPRP” explored the replicability of the connections
between personality and outcomes (as discussed by Soto, 2019). From the
6,126 initial participants, they were categorized into two age groups:

- Ages 18 to 50 (with 1,662 participants having an average age of 35.09
  years)

- Ages 18 to 25 (with 3,459 participants, averaging at 21.89 years of
  age)

It’s worth noting that due to some age overlaps, the two groups have
shared participants.

## Analysis

### Data Preparation and Grouping

We started by segmenting the BFI-2 survey data into two distinct subsets
based on the survey time (Time 1 and Time 2). This step is crucial for
comparing the survey results across different times.

``` r
# Assign the original bfi2 dataset to a more descriptive variable name
bfi2_survey_data <- bfi2_data

# Adding a 'survey_time' column to the dataset
# This column categorizes the survey data based on the survey time: Time 1 or Time 2
# If 'CaseID' contains 'Survey1', it's labeled as 'Time1'; otherwise, it's labeled as 'Time2'
bfi2_labeled <- bfi2_survey_data %>%
  mutate(time = ifelse(str_detect(CaseID, 'Survey1'),1,2))
```

Further, we divided the data from both Time 1 and Time 2 into two age
groups: younger (18-25 years) and middle-aged (26-50 years). This
categorization allows for an age-based comparative analysis within each
time frame.

``` r
# adding an 'age_group' column
bfi2_labeled <- bfi2_labeled %>%
  mutate(age_group = ifelse(Age >17 & Age < 26, 'y', 'ma'))
```

We established the theoretical structure of the survey based on the
BFI-2 traits. This structure forms the foundation for our subsequent
invariance analysis.

``` r
# establishing the theoretical structure based on BFI-2 traits
theoretical_structure <- rep(1:5, times = 12)
voi <- !(colnames(bfi2_labeled) %in% c("CaseID", "Age", "Sex", "time", "age_group"))
```

### Invariance Between Survey Times

#### Invariance in Younger Age Group

The invariance between Time 1 and Time 2 was tested specifically for the
younger age group. We analyzed whether the survey’s measurement
properties remained consistent over time for this demographic.

``` r
# filtering the labeled survey data to focus on the younger age group
younger_data <- bfi2_labeled %>%
  filter(age_group == "y")

# selecting variables of interest (voi) for the younger age group
younger_voi <- younger_data[,voi]

# conducting invariance analysis to compare survey times within the younger age group
# this helps in understanding if the measurement properties are consistent over time for this group
young_group_invariance <- invariance(
  data = younger_voi,
  groups = younger_data$time,
  structure = theoretical_structure,
  seed = 1234  # Setting a seed for reproducibility
)
```

``` r
# plotting the invariance results for easy visualization
plot(young_group_invariance, node.size = 5, label.size = 1.5)
```

![](EGA-Robustness_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# filtering and transforming invariance results to identify items of interest
# focusing on items where the direction of difference is significant
significant_items <- young_group_invariance$results %>%
  filter(Direction != '') %>%
  select(Membership, Difference, p, Direction) %>%
  mutate(TraitName = case_when(
    Membership == 1 ~ "Extraversion",
    Membership == 2 ~ "Agreeableness",
    Membership == 3 ~ "Conscientiousness",
    Membership == 4 ~ "Negative Emotionality",
    Membership == 5 ~ "Open-mindedness"
  ))

# displaying the significant items
significant_items
```

                                                  Membership Difference     p
    Stays optimistic after experiencing a setback          4      0.037 0.004
                                                  Direction             TraitName
    Stays optimistic after experiencing a setback     1 > 2 Negative Emotionality

The analysis suggests that there isn’t complete measurement invariance
across time points for the younger age group. The item ‘Stays optimistic
after experiencing a setback,’ linked to ‘Negative Emotionality,’
significantly varies between Survey 1 and Survey 2 (p = 0.004). This
indicates a change in how this aspect of Negative Emotionality is
perceived or responded to by the younger demographic over time.

#### Invariance in Middle-Aged Age Group

A similar invariance test was conducted for the middle-aged age group,
comparing the survey data across Time 1 and Time 2. This analysis helps
in understanding if the perceptions or responses of the middle-aged
group changed over time.

``` r
# filtering the labeled survey data to isolate the middle-aged age group
ma_data <- bfi2_labeled %>%
  filter(age_group == 'ma')  # 'ma' represents the middle-aged group

# selecting variables of interest (voi) specifically for the middle-aged group
ma_voi <- ma_data[, voi]

# performing invariance analysis to examine if the measurement properties are stable over time
# this analysis is specific to the middle-aged group across different survey times
ma_invariance <- invariance(
  data = ma_voi,
  groups = ma_data$time,  # Grouping data by survey time
  structure = theoretical_structure,  # Using the predefined theoretical structure
  seed = 1234  # Setting a seed for reproducibility of results
)
```

``` r
# plotting the invariance results to visually assess the stability of measurements
# node size and label size are adjusted for clear visualization
plot(ma_invariance, node.size = 5, label.size = 1.5)
```

![](EGA-Robustness_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# extracting and labeling significant items from the invariance analysis
# filtering for items where the direction of measurement difference is notable
ma_items <- ma_invariance$results %>%
  filter(Direction != '') %>%
  select(Membership, Difference, p, Direction) %>%
  mutate(TraitName = case_when(
    Membership == 1 ~ "Extraversion",
    Membership == 2 ~ "Agreeableness",
    Membership == 3 ~ "Conscientiousness",
    Membership == 4 ~ "Negative Emotionality",
    Membership == 5 ~ "Open-mindedness"
  ))

# displaying the significant items with noticeable measurement differences
ma_items
```

                                       Membership Difference     p Direction
    Is full of energy                           1     -0.054 0.008     1 < 2
    Prefers to have others take charge          1     -0.039 0.050     1 < 2
    Shows a lot of enthusiasm                   1     -0.041 0.044     1 < 2
    Tends to be lazy                            3      0.045 0.040     1 > 2
    Worries a lot                               4      0.520 0.002     1 > 2
                                                   TraitName
    Is full of energy                           Extraversion
    Prefers to have others take charge          Extraversion
    Shows a lot of enthusiasm                   Extraversion
    Tends to be lazy                       Conscientiousness
    Worries a lot                      Negative Emotionality

The analysis indicates partial measurement invariance between the two
time points for the younger age group. The items ‘Is full of energy,’
‘Prefers to have others take charge,’ ‘Shows a lot of enthusiasm’ show
significant differences with lower scores in Survey 1 compared to Survey
2 (p-values: 0.008, 0.050, and 0.044, respectively) which suggests that
extraversion is better described in Survey 2. The items ‘Tends to be
lazy’ and ‘Worries a lot’ have higher scores in Survey 1, which suggests
a decrease in traits related to nonscientiousness and negative
emotionality over time.

#### Conclusions

The demographics of the participation in surveys 1 and 2 are not the
same, as indicated by p-values less than 0.05. These differences suggest
variations in how certain traits are perceived or rated by participants
at different times, pointing to potential changes in demographics,
attitudes, or perceptions between Survey 1 and Survey 2.

### Invariance Between Age Groups at Time 1

Using the survey data from Time 1, we performed an invariance analysis
between the younger and middle-aged groups. This step was aimed at
understanding if the survey’s measurements were consistent across
different age groups at a single time point.

``` r
# filtering the labeled survey data for responses from Time 1
time1_data <- bfi2_labeled %>%
  filter(time == '1')  # '1' indicates Time 1

# selecting the variables of interest (voi) for Time 1 survey data
time1_voi <- time1_data[, voi]

# performing invariance analysis to compare age groups at Time 1
# this helps to understand if measurement properties differ across age groups
time1_invariance <- invariance(
  data = time1_voi,
  groups = time1_data$age_group,  # grouping by age group
  structure = theoretical_structure,  # using the theoretical structure of the survey
  seed = 1234  # setting a seed for reproducibility
)
```

We visualized and summarized the invariance results, focusing on items
that were invariant between the two age groups. This analysis provided
insights into how different age demographics perceived various traits at
a specific time.

``` r
# plotting the invariance results for visual analysis
plot(time1_invariance, node.size = 5, label.size = 1.5)
```

![](EGA-Robustness_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# extracting and labeling significant items from the invariance analysis
# focusing on items where the direction of difference is significant between age groups
time1_items <- time1_invariance$results %>%
  filter(Direction != '') %>%
  select(Membership, Difference, p, Direction) %>%
  mutate(TraitName = case_when(
    Membership == 1 ~ "Extraversion",
    Membership == 2 ~ "Agreeableness",
    Membership == 3 ~ "Conscientiousness",
    Membership == 4 ~ "Negative Emotionality",
    Membership == 5 ~ "Open-mindedness"
  ))

# displaying the significant items with notable differences between age groups
time1_items
```

                                                 Membership Difference     p
    Is sometimes shy, introverted                         1      0.042 0.028
    Is respectful, treats others with respect             2     -0.052 0.010
    Is suspicious of others’ intentions                   2     -0.053 0.012
    Can be tense                                          4     -0.038 0.014
    Is curious about many different things                5     -0.040 0.042
    Is inventive, finds clever ways to do things          5     -0.042 0.034
                                                 Direction             TraitName
    Is sometimes shy, introverted                   y > ma          Extraversion
    Is respectful, treats others with respect       y < ma         Agreeableness
    Is suspicious of others’ intentions             y < ma         Agreeableness
    Can be tense                                    y < ma Negative Emotionality
    Is curious about many different things          y < ma       Open-mindedness
    Is inventive, finds clever ways to do things    y < ma       Open-mindedness

Measurement is not invariant between the younger and middle-aged groups.

The item ‘Is sometimes shy, introverted’ shows higher scores in the
younger group compared to the middle-aged group (p = 0.028), suggesting
that extraversion is better described in the younger age group. The
other items “Is respectful, treats others with respect”, “Is suspicious
of others’ intentions”, “Can be tense” , “Is curious about many
different things”, and “Is inventive,finds clever ways to do things”
have higher scores in the middle aged group which suggests that their
respective traits are better described in the middle aged group than the
younger age group.

### Redundancy Checks

#### Redundancy Check in Younger Age Group at Time 1

We conducted a redundancy analysis for the younger age group at Time 1,
identifying any survey items with significant overlap in their
measurement (redundancies greater than 0.25).

``` r
# filtering the survey data to focus on the younger age group at Time 1
time1_young_group <- younger_data %>%
  filter(time == 1)  # Selecting responses from Time 1

# selecting variables of interest from the Time 1 younger group data
time1_young_group_voi <- time1_young_group[, voi]

# performing Univariate Analysis (UVA) to identify any redundancies in the survey items
# redundancies can indicate overlapping or similar items in the survey
time1_young_group_redundancies <- UVA(time1_young_group_voi)

# printing the summary of the UVA analysis
# focusing on identifying any redundancies with values greater than 0.25
print(time1_young_group_redundancies)
```

    Variable pairs with wTO > 0.30 (large-to-very large redundancy)

              node_i                        node_j   wto
     Often feels sad Tends to feel depressed, blue 0.323

    ----

    Variable pairs with wTO > 0.25 (moderate-to-large redundancy)

                                         node_i                node_j   wto
     Is fascinated by art, music, or literature Values art and beauty 0.283

    ----

    Variable pairs with wTO > 0.20 (small-to-moderate redundancy)

                                           node_i
     Is systematic, likes to keep things in order
                                Tends to be quiet
                            Is dependable, steady
        Is respectful, treats others with respect
                                    node_j   wto
                Keeps things neat and tidy 0.249
             Is sometimes shy, introverted 0.228
     Is reliable, can always be counted on 0.225
            Is polite, courteous to others 0.214

In examining redundancies within the younger age group at Time 1, two
pairs of items show redundancies with correlation values greater than
0.25, indicating a significant overlap in what these items measure.

‘Often feels sad’ and ‘Tends to feel depressed, blue’ exhibit a high
redundancy with a correlation value of 0.3225802. Both items seem to tap
into similar aspects of emotional state, relating to feelings of sadness
or depression.

‘Is fascinated by art, music, or literature’ and ‘Values art and beauty’
show moderate redundancy with a correlation value of 0.2828442. They
both appear to measure an interest or appreciation for artistic and
aesthetic experiences.

It should also be noted that the item pairs ‘Is systematic, likes to
keep things in order’ with ‘Keeps things neat and tidy’ have a value of
0.249, which is approximately at the threshold for having
moderate-to-large redundancy.

#### Redundancy Check in Middle-Aged Age Group at Time 1

A similar analysis was carried out for the middle-aged group at Time 1,
pinpointing survey items with considerable redundancies. This step helps
in understanding the overlap in the measurement of different traits
within the same age group.

``` r
# filtering the survey data to focus on the middle-aged age group at Time 1
time1_middle_group <- ma_data %>%
  filter(time == 1)  # Selecting responses from Time 1

# selecting variables of interest from the Time 1 middle-aged group data
time1_middle_group_voi <- time1_middle_group[, voi]

# performing Univariate Analysis (UVA) to identify redundancies in the survey items
# this analysis helps to find overlapping or similar items that might not be adding unique value
time1_middle_group_redundancies <- UVA(time1_middle_group_voi)

# printing the summary of the UVA analysis
print(time1_middle_group_redundancies)
```

    Variable pairs with wTO > 0.30 (large-to-very large redundancy)

    ----

    Variable pairs with wTO > 0.25 (moderate-to-large redundancy)

                                           node_i                        node_j
       Is fascinated by art, music, or literature         Values art and beauty
     Is systematic, likes to keep things in order    Keeps things neat and tidy
                                  Often feels sad Tends to feel depressed, blue
       wto
     0.296
     0.296
     0.280

    ----

    Variable pairs with wTO > 0.20 (small-to-moderate redundancy)

                                           node_i
        Is respectful, treats others with respect
     Is inventive, finds clever ways to do things
                            Is dependable, steady
                                Tends to be quiet
                                Tends to be quiet
                                    node_j   wto
            Is polite, courteous to others 0.246
      Is original, comes up with new ideas 0.246
     Is reliable, can always be counted on 0.238
             Is sometimes shy, introverted 0.211
                              Is talkative 0.207

In examining redundancies within the middle age group at Time 1, three
pairs of items show redundancies with correlation values greater than
0.25, indicating a significant overlap in what these items measure.

‘Is fascinated by art, music, or literature’ and ‘Values art and beauty’
reflect a strong redundancy with a correlation value of 0.296. Both
items appear to assess an appreciation for artistic and aesthetic
experiences, suggesting they are measuring similar aspects of interest
in arts and culture.

‘Is systematic, likes to keep things in order’ and ‘Keeps things neat
and tidy’ have a high level of redundancy with a correlation value of
0.295.These items both seem to capture elements of organization and
tidiness, indicating a potential overlap in measuring orderly and
systematic behavior.

‘Often feels sad’ and ‘Tends to feel depressed, blue’ show significant
redundancy with a correlation value of 0.2798. Both items are likely to
be tapping into feelings of sadness or depressive states, suggesting a
similarity in measuring aspects of emotional well-being or mood states.

It should also be noted that the item pairs “is respectful, treats
others with respect” with “is polite, courteous to others” have a value
of 0.24613 and “is inventive, finds clever ways to do things” with “is
original, comes up with new ideas” have a value of 0.2460036 which are
approximately at the threshold for having moderate-to-large redundancy.

In general, these redundancies could impact the survey’s efficiency, as
similar items might not be contributing additional unique information
about different personality traits.

### Conclusion

In this assignment, we found partial measurement invariance in the BFI-2
survey across different time points and between younger and middle-aged
groups, with significant differences in certain items suggesting
variations in how traits are perceived or responded to over time and
across age groups. Redundancies were identified in both age groups, with
items related to emotional states, organization, and artistic interests
showing high correlations, indicating potential overlap in what they
measure. These findings highlight the importance of considering
age-specific responses and item uniqueness in personality assessments.
Overall, the results suggest the need for refinement of certain BFI-2
items to enhance its reliability and applicability across diverse age
groups and over time. Most pysch studies are phasing out the Big-5,
likely because of these reasons.
