Writing functions
================
Yifan Su
2020-11-09

## Do something simple

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  return(z)
  
}

z_scores(x_vec) # the input is the x_vec
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

Try my function on some other things， this should give errors.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}

z_scores(3) # it shows NA
```

    ## Error in z_scores(3): Z scores cannot be computed for length 1 vectors

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Argument x should be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Argument x should be numeric

``` r
z_scores(iris)
```

    ## Error in z_scores(iris): Argument x should be numeric

``` r
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

    ## Error in z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE)): Argument x should be numeric

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
# or we can use list() function
  tibble(mean = mean_x, # we want to give both mean and sd, hope it returns both
       sd = sd_x)
}
```

Check tha the function works:

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.51  2.85

## Multiple intputs

I’d like to do this a function.

``` r
sim_data = 
  tibble(
  x = rnorm(30, mean = 2, sd = 3)
)

sim_data %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   1.63      3.06

``` r
sim_mean_sd = function(sample_size, mu, sigma = 6) { # you can set a defult
  
  sim_data = 
    tibble(
    x = rnorm(n = sample_size, mean = mu, sd = sigma),
  )
  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}

sim_mean_sd(100, 6, 3) # numbers matches names # this overwrite the function
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   6.08      2.78

``` r
sim_mean_sd(100, 6)
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   6.24      6.25

url =
“<https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1>”

dynamite\_html = read\_html(url)

review\_titles = dynamite\_html %\>% html\_nodes(“.a-text-bold span”)
%\>% html\_text()

review\_stars = dynamite\_html %\>% html\_nodes(“\#cm\_cr-review\_list
.review-rating”) %\>% html\_text() %\>% str\_extract(“^\\d”) %\>%
as.numeric()

review\_text = dynamite\_html %\>% html\_nodes(“.review-text-content
span”) %\>% html\_text() %\>% str\_replace\_all(“”, "") %\>% str\_trim()

reviews = tibble( title = review\_titles, stars = review\_stars, text =
review\_text )

read\_page\_reviews \<- function(url) {

html = read\_html(url)

review\_titles = html %\>% html\_nodes(“.a-text-bold span”) %\>%
html\_text()

review\_stars = html %\>% html\_nodes(“\#cm\_cr-review\_list
.review-rating”) %\>% html\_text() %\>% str\_extract(“^\\d”) %\>%
as.numeric()

review\_text = html %\>% html\_nodes(“.review-text-content span”) %\>%
html\_text() %\>% str\_replace\_all(“”, "") %\>% str\_trim()

tibble( title = review\_titles, stars = review\_stars, text =
review\_text ) }

url\_base =
“<https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=>”
vec\_urls = str\_c(url\_base, 1:5)

dynamite\_reviews = bind\_rows( read\_page\_reviews(vec\_urls\[1\]),
read\_page\_reviews(vec\_urls\[2\]),
read\_page\_reviews(vec\_urls\[3\]),
read\_page\_reviews(vec\_urls\[4\]), read\_page\_reviews(vec\_urls\[5\])
)

dynamite\_reviews
