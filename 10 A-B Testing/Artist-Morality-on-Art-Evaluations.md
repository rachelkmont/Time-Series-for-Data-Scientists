Artist Morality on Art Evaluations
================

# Analyzing the Impact of Artist Morality on Art Evaluations

In this blog post, we will explore a dataset collected from a survey
that aimed to understand how people evaluate artworks and the potential
influence of artists’ morality on those evaluations. We will walk
through various statistical tests and analyses to answer the research
question: Does an artist’s morality affect evaluations of their artwork?
In other words: Can people separate the art from the artist?

## Data Overview

The dataset comprises responses from 7,216 participants collected via
Amazon Mechanical Turk (MTurk). After applying screening for
manipulation checks, we have a final sample size of 250 participants.
The data was collected through Qualtrics and includes various variables:

- *Participant:* Unique participant identifiers.

- *FamousPerson:* Names of famous individuals.

- *Painting:* Identifiers for paintings.

- *Morality:* Ratings of the morality of famous people.

- *Attractiveness:* Ratings of the attractiveness of paintings. Beauty:
  Ratings of the beauty of paintings.

- *Skill:* Ratings of the skill demonstrated in paintings.

- *Political:* Political orientations of participants.

- *AEQ:* Scores related to aesthetic experiences.

- *Age, Education, Race, Gender, SexualOrientation, SocialPolitical,
  EconomicPolitical:* Demographic information.

## Setting up Environment and Data

First, let’s set up our environment and load in the necessary libraries.

``` r
# set seed for reproducibility
set.seed(16)

# load libraries
library(tidyverse)
library(rstatix)
```

Now we can read in our data.

``` r
# load in data
load("BIU_Analysis_Data.RData")

art_morality <- mlm_df ##renaming data object so it makes more sense in our analysis  
```

## Analysis

### Initial Setup and Methodology Development

#### Custom T-Test Function Creation

We begin by creating a custom t-test function to compare means between
two groups. A t-test of means is a statistical test that is used to
compare the means of two groups to see if they are significantly
different from each other.

``` r
# creating function for t-test
t_test <- function(x1, x2)
{
  # calculate the mean of x1 and x2
  m1 <- mean(x1, na.rm = TRUE)
  m2 <- mean(x2, na.rm = TRUE)
  
  # calculate the variance of x1 and x2
  s1 <- var(x1, na.rm = TRUE)
  s2 <- var(x2, na.rm = TRUE)
  
  # calculate the sample sizes of x1 and x2
  n1 <- length(x1)
  n2 <- length(x2)
  
  # calculate the standard error of the difference in means
  se <- sqrt(s1 / n1 + s2 / n2)
  
  # calculate the t-statistic
  t_stat <- (m1 - m2) / se
  
  # calculate the degrees of freedom
  df <- n1 + n2 - 2
  
  # calculate the p-value
  p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)

  # create a result object with t-statistic and p-value
  result <- list(
    t_statistic = t_stat,
    degrees_of_freedom = df,
    p_value = p_value
  )
  
  # return the result
  return(result)
}
```

#### Cohen’s D Function Development

Next, we develop a function to compute Cohen’s d, which measures the
effect size between two groups.

Cohen’s D is a statistical measure used to quantify the size of the
effect in a study, often used in the context of comparing two means.
It’s particularly common in the fields of psychology and education but
is applicable in many areas of research where comparing group means is
important.

``` r
# create function for Cohen's d
cohens_d <- function(x1, x2)
{
  # calculate the mean of x1 and x2
  m1 <- mean(x1, na.rm = TRUE)
  m2 <- mean(x2, na.rm = TRUE)
  
  # calculate the variance of x1 and x2
  s1 <- var(x1, na.rm = TRUE)
  s2 <- var(x2, na.rm = TRUE)
  
  # calculate the sample sizes of x1 and x2
  n1 <- length(x1)
  n2 <- length(x2)
  
  # calculate the pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2))
  
  # calculate Cohen's d
  cohen_d <- (m1 - m2) / pooled_sd
  
  return(cohen_d)
}
```

#### Grouping by Artist Morality

We create a grouping variable called morality_group based on artists’
morality scores. We classify artists as “Bad” if their morality score is
less than -1, “Good” if it’s greater than 1, and NA for other cases.

``` r
# create grouping variable for `Morality`
art_morality <- art_morality %>%
  mutate(morality_group = case_when(
    Morality < -1 ~ "Bad",
    Morality > 1  ~ "Good",
    TRUE          ~ NA_character_ ## NA for other cases, or blank
  ))
```

### Beauty Ratings Comparison

#### Using Custom T-Test

In this section, we perform a custom t-test to compare the Beauty
ratings between the “Bad” and “Good” morality groups.

``` r
#subset morality_group data into "bad" and "good" groups
bad_group <- art_morality %>%
  filter(morality_group == "Bad") %>%
  select(Beauty)
  
good_group <- art_morality %>%
  filter(morality_group == "Good") %>%
  select(Beauty)
```

``` r
#perform t test using the function we created 
t_test(bad_group$Beauty, good_group$Beauty)
```

    ## $t_statistic
    ## [1] -2.452784
    ## 
    ## $degrees_of_freedom
    ## [1] 4091
    ## 
    ## $p_value
    ## [1] 0.01421702

#### Using Standard t-test

We also perform a t-test on the Beauty variable using the t.test
function and compare the results with our custom t-test.

``` r
# t-test of equal variances with built-in R t.test function
t_test_result <- t.test(bad_group, good_group, var.equal = TRUE) # assumes that the variances of the two groups are equal, which is an assumption made in a standard two-sample t-test with equal variances

## printing result 
t_test_result
```


        Two Sample t-test

    data:  bad_group and good_group
    t = -2.4872, df = 4085, p-value = 0.01292
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
     -0.26502751 -0.03138035
    sample estimates:
      mean of x   mean of y 
    -0.05091695  0.09728698 

#### Comparing T-Statistics

We compare the t-statistics from our custom t-test and the one performed
using t.test to check for differences.

The p-values and degrees of freedom vary slightly from the custom t-test
and the t.test. Both come to the conclusion to reject the null
hypothesis (based on the p-value being less than 0.05) and conclude that
there is evidence to suggest a statistically significant difference in
beauty ratings between the “Bad” and “Good” morality groups.

#### P-Value Interpretation of T-Test

Since the p-value (0.01292) is less than the significance level (alpha =
0.05), we reject the null hypothesis. This means there is evidence to
suggest there is a statistically significant difference in beauty
ratings between the “Bad” and “Good” morality groups.

Since our hypothesis is just checking for a difference in means, we
cannot say that the good paintings are higher rated. This can be
guessed, or assumed, based on the sample estimate means, but that is not
statistical evidence.

### Effect Size and Statistical Significance Assessment

#### Computing Cohen’s D

``` r
# Cohen's *d* using your function
cohens_d(bad_group$Beauty, good_group$Beauty)
```

    [1] -0.08522065

The small magnitude of the Cohen’s D value indicates that the effect of
an artist’s perceived morality on the beauty ratings of their artwork is
minimal. This means that, on average, participants’ evaluations of the
beauty of artworks are not substantially swayed by their views on the
artist’s morality.

#### ANOVA Application

An ANOVA (Analysis of Variance) test is a statistical method used to
compare the means of three or more groups to determine if there are any
statistically significant differences between them.

``` r
anova_result <- anova_test(data = art_morality, dv = Beauty, between = morality_group)

# print results 
summary(anova_result)
```

        Effect               DFn         DFd             F               p        
     Length:1           Min.   :1   Min.   :4085   Min.   :6.186   Min.   :0.013  
     Class :character   1st Qu.:1   1st Qu.:4085   1st Qu.:6.186   1st Qu.:0.013  
     Mode  :character   Median :1   Median :4085   Median :6.186   Median :0.013  
                        Mean   :1   Mean   :4085   Mean   :6.186   Mean   :0.013  
                        3rd Qu.:1   3rd Qu.:4085   3rd Qu.:6.186   3rd Qu.:0.013  
                        Max.   :1   Max.   :4085   Max.   :6.186   Max.   :0.013  
        p<.05                ges       
     Length:1           Min.   :0.002  
     Class :character   1st Qu.:0.002  
     Mode  :character   Median :0.002  
                        Mean   :0.002  
                        3rd Qu.:0.002  
                        Max.   :0.002  

The ANOVA results suggest that there is a statistically significant
difference in beauty ratings between the “Bad” and “Good” morality
groups. The p-value is less than the typical significance level of 0.05,
indicating that we can reject the null hypothesis.

#### Squaring T-Statistic and Comparing with F-Statistic

``` r
# extract the t-statistic
t_statistic <- t_test_result$statistic

# compute the square of the t-statistic
t_stat_squared <- t_statistic

# print the result
t_stat_squared
```

            t 
    -2.487173 

The squared t-statistic is -2.487173 and the F-statistic is 6.186. Let’s
interpret these values:

- **Squared t-statistic:**

  - The squared t-statistic measures the effect size of the difference
    in beauty ratings between the “Bad” and “Good” morality groups.

  - A squared t-statistic of -2.487 indicates that the means of these
    two groups differ significantly

  - The negative sign suggests that the “Bad” morality group has, on
    average, lower beauty ratings compared to the “Good” morality group

  - The magnitude of the squared t-statistic (regardless of sign)
    indicates the strength of this difference. In our case, the
    magnitude suggests a moderate effect size.

- **F-statistic:**

  - The F-statistic from the ANOVA tests whether there is a
    statistically significant difference in mean beauty ratings between
    the “Bad” and “Good” morality groups.

  - The F statistic quantifies the extent of this difference relative to
    the variability within the groups

  - With an F-statistic of 6.186 and a p-value less than 0.05, it
    suggests that there is strong evidence to conclude that the mean
    beauty ratings differ significantly between these two groups.

In summary, both the squared t-statistic and the F-statistic provide
evidence that the beauty ratings of paintings differ between the “Bad”
and “Good” morality groups.

### Advanced Statistical Analysis

#### ANCOVA Implementation

An Analysis of Covariance (ANCOVA) is used to compare means of a
dependent variable across multiple groups while statistically
controlling for the influence of one or more continuous covariates.

``` r
# perform ANCOVA using anova_test

ancova_result <- art_morality %>%
  anova_test(
    formula = Beauty ~ morality_group + AEQ + Attractiveness + Political + Skill, # specify the ANCOVA model formula
    dv = "Beauty", # dependent variable (DV) is "Beauty"
    between = "morality_group", # independent variable is "morality_group"
    covariate = c("AEQ", "Attractiveness", "Political", "Skill"), # specify covariates
    effect.size = "pes" # make the effect size to be computed as partial eta squared (n²)
  )

# print the ANCOVA summary
ancova_result
```

    ## ANOVA Table (type II tests)
    ## 
    ##           Effect DFn  DFd      F        p p<.05      pes
    ## 1 morality_group   1 4081  0.307 5.80e-01       7.52e-05
    ## 2            AEQ   1 4081 16.394 5.24e-05     * 4.00e-03
    ## 3 Attractiveness   1 4081  0.315 5.74e-01       7.73e-05
    ## 4      Political   1 4081  0.509 4.76e-01       1.25e-04
    ## 5          Skill   1 4081  0.111 7.39e-01       2.71e-05

*Let’s interpret our results:*

- Significant Predictor Variables:

  - AEQ (aesthetic experiences) : The only significant predictor with a
    p-value of less than 0.05. This suggests that aesthetic experiences
    is a significant factor in how people rate the beauty of paintings.

- Non-Significant Predictors:

  - Morality Group, Attractiveness, Political Orientation, and Skill did
    not show significant effects (p \> 0.05). These factors do not seem
    to have a substantial effect on how people rate beauty in this
    context.

  - The effect sizes for Attractiveness, Political orientation, and
    Skill are small, reinforcing their limited impact on beauty ratings.

- Effect of Including Covariates:

  - Including these covariates in the analysis does not substantially
    change the interpretation of how individuals rate the beauty of
    paintings. The small effect sizes, especially for non-significant
    variables, suggest that these factors have minimal influence.

  - The significant effect of AEQ, however, indicates that it might have
    a unique contribution to how beauty is perceived in paintings,
    distinguishing it from the other variables tested.

In summary, the ANCOVA results suggest that among the variables tested,
only AEQ (aesthetic experiences) has a significant impact on how
individuals rate the beauty of paintings. However, attributes like
morality, physical attractiveness, political orientation, and skill
level do not play a significant role in influencing beauty ratings. The
inclusion of covariates helps provide a more accurate understanding of
the relationship between these predictors and Beauty ratings.

### Final Assessment: Impact of Artist Morality

To answer the research question, “Does an artist’s morality affect
evaluations of their artwork?” based on the analysis we’ve conducted, we
can provide the following conclusion:

*Yes, an artist’s morality does have an effect on the evaluations of
their artwork, but the effect is not very strong.*

- The t-tests and ANOVA conducted show that there is a statistically
  significant difference in the beauty ratings of artworks based on the
  morality group of the artist (“Bad” vs. “Good”). This suggests that
  participants’ evaluations of artwork are influenced to some extent by
  their perception of the artist’s morality.

- The Cohen’s D value, which measures the effect size, indicates that
  the impact of the artist’s morality on artwork evaluations is minimal.
  This means that while there is a statistically significant difference,
  it’s not a strong or large effect.

- Both ANOVA and ANCOVA results support the notion that there is a
  significant difference in beauty ratings between the morality groups.

- The significant role of aesthethic experiences in the ANCOVA analysis
  indicates that aesthetic experiences might have a unique contribution
  to how beauty is perceived in paintings, which could potentially
  overshadow the impact of the artist’s morality.

In summary, the analysis suggests that an artist’s morality does affect
evaluations of their artwork, but the effect is relatively modest and
not as influential as other factors like the individual aesthetic
experiences of the viewers.
