Cross Validation
================
November 12, 2019

## Cross validation

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point() + theme_bw()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Training and testing

Cross validation by hand

``` r
train_df = sample_frac(nonlin_df, size = .8)
test_df = anti_join(nonlin_df, train_df, by = "id")

ggplot(train_df, aes(x = x, y = y)) + 
  geom_point() + 
  geom_point(data = test_df, color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Fit three models of varying goodness:

  - Linear regression, which generally doesn’t work as well

  - Smooth curve

  - Wiggly
    model

<!-- end list -->

``` r
linear_mod = lm(x ~ x, data = train_df)
```

    ## Warning in model.matrix.default(mt, mf, contrasts): the response appeared
    ## on the right-hand side and was dropped

    ## Warning in model.matrix.default(mt, mf, contrasts): problem with term 1 in
    ## model.matrix: no columns are assigned

``` r
smooth_mod = mgcv::gam(y ~ s(x), data = train_df)
wiggly_mod = mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

Let’s look at some fits.

``` r
train_df %>%
  add_predictions(linear_mod) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_point(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
train_df %>%
  add_predictions(smooth_mod) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
train_df %>%
  add_predictions(wiggly_mod) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
train_df %>% 
  gather_predictions(linear_mod, smooth_mod, wiggly_mod) %>% 
  mutate(model = fct_inorder(model)) %>% 
  
  ggplot(aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(aes(y = pred), color = "red") + 
  facet_wrap(~model)
```

![](cross_validation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Calculate root mean square error where larger values are worse

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.2805514

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.2221774

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.289051

We are interested in the model that will do better on the test dataset.
The training dataset is the one where you put in too much stuff. Putting
more stuff into the training dataset makes the rmse decrease.

## Do this all using modelr

crossv\_mc generates test-training pairs for cross-validation

``` r
cv_df = 
  crossv_mc(nonlin_df, 100) 
```

one note about resample

why are my datasets the same even after changing the number in \[\[\]\]

``` r
cv_df %>% pull(train) %>% .[[1]] %>% as_tibble
```

    ## # A tibble: 79 x 3
    ##       id      x       y
    ##    <int>  <dbl>   <dbl>
    ##  1     1 0.266   1.11  
    ##  2     2 0.372   0.764 
    ##  3     3 0.573   0.358 
    ##  4     4 0.908  -3.04  
    ##  5     6 0.898  -1.99  
    ##  6     7 0.945  -3.27  
    ##  7     8 0.661  -0.615 
    ##  8     9 0.629   0.0878
    ##  9    10 0.0618  0.392 
    ## 10    11 0.206   1.63  
    ## # … with 69 more rows

``` r
cv_df =
  cv_df %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))
```

try fitting the linear model to all of these…

``` r
cv_df = 
  cv_df %>% 
  mutate(linear_mods  = map(train, ~lm(y ~ x, data = .x)),
         smooth_mods  = map(train, ~mgcv::gam(y ~ s(x), data = .x)),
         wiggly_mods  = map(train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))) %>% 
  mutate(rmse_linear = map2_dbl(linear_mods, test, ~rmse(model = .x, data = .y)),
         rmse_smooth = map2_dbl(smooth_mods, test, ~rmse(model = .x, data = .y)),
         rmse_wiggly = map2_dbl(wiggly_mods, test, ~rmse(model = .x, data = .y)))
```

visualize this

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(
    everything(),
    names_to = "model", 
    values_to = "rmse",
    names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
