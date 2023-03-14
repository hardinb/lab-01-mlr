Lab 10 - Grading the professor, Pt. 2
================
Ben Hardin
3/14/2023

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

### Exercise 1

The linear model: score = 3.88 + .067(average beauty)

R^2 = 3.5% Adjusted R^2 = 3.3%

``` r
m_bty <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg, data = evals)

tidy(m_bty)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.88      0.0761     51.0  1.56e-191
    ## 2 bty_avg       0.0666    0.0163      4.09 5.08e-  5

``` r
glance(m_bty)$r.squared
```

    ## [1] 0.03502226

``` r
glance(m_bty)$adj.r.squared
```

    ## [1] 0.03292903

### Exercise 2

The linear model: score = 3.75 + .07(beauty) + .17(gender)

R^2 = 5.9% Adjusted R^2 = 5.5%

``` r
m_bty_gen <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg + gender, data = evals)

tidy(m_bty_gen)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.75      0.0847     44.3  6.23e-168
    ## 2 bty_avg       0.0742    0.0163      4.56 6.48e-  6
    ## 3 gendermale    0.172     0.0502      3.43 6.52e-  4

``` r
glance(m_bty_gen)$r.squared
```

    ## [1] 0.05912279

``` r
glance(m_bty_gen)$adj.r.squared
```

    ## [1] 0.05503202

### Exercise 3

Intercept: Female instructors who have a beauty rating of 0 are
predicted, on average, to receive a course evaluation score of about
3.75

Slope of gender: Holding beauty constant, course evaluation scores are
predicted, on average, to be .17 points higher for male instructors
compared to female instructors.

### Exercise 4

The model including average beauty and gender explains about 5.9% of the
variance in course evaluation scores.

### Exercise 5

Linear model for male instructors: score = 3.92 + .07(beauty)

### Exercise 6

For two professors who had the same beauty rating, this model predicts
that male professors would have higher course evaluation scores.

### Exercise 7

This is a question about an interaction, but I only tested main effects
in my previous model. So now, let’s run a model including the
interaction term to answer this question.

``` r
m_bty_gen <- linear_reg() %>%
  set_engine("lm") %>%
  fit(score ~ bty_avg * gender, data = evals)

tidy(m_bty_gen)
```

    ## # A tibble: 4 × 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          3.95      0.118      33.5  2.92e-125
    ## 2 bty_avg              0.0306    0.0240      1.28 2.02e-  1
    ## 3 gendermale          -0.184     0.153      -1.20 2.32e-  1
    ## 4 bty_avg:gendermale   0.0796    0.0325      2.45 1.46e-  2

``` r
glance(m_bty_gen)$r.squared
```

    ## [1] 0.07128874

``` r
glance(m_bty_gen)$adj.r.squared
```

    ## [1] 0.06521873

The slope of the interaction is about .08, indicating that the slope for
beauty predicting course evaluation scores differs between male versus
female instructors. Specifically, the slope for beauty is predicted to
be about .08 points greater for male instructors than for female
instructors, meaning there is a stronger relationship between beauty and
course evaluations for male instructors than for female instructors.
