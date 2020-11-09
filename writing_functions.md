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

## Let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>% # extract the first digit from 1-9 of that string inputed
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% # get rid of \n at the beginning and the end, replace it to ""
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews… Let’s turn that code into a
function

``` r
read_page_reviews <- function(url) {
  
   html = read_html(url)

  review_titles =
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars =
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()

  review_text =
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>%
    str_replace_all("\n", "") %>%
    str_trim()

  reviews =
   tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  reviews
}
```

Let me try my function.

``` r
dynamite_html = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"

read_page_reviews(dynamite_html)
```

    ## # A tibble: 10 x 3
    ##    title                     stars text                                         
    ##    <chr>                     <dbl> <chr>                                        
    ##  1 Best.Movie!                   5 "I enjoyed showing my children this \"classi~
    ##  2 Great Movie                   5 "I love this movie. Showed it to my middle s~
    ##  3 Tina, you fat lard, come~     5 "A very quotable, awkard and hilarious movie~
    ##  4 Funny!                        4 "It is a great movie although it’s a little ~
    ##  5 Excellent for families        5 "Highly recommend for family entertainment"  
    ##  6 Hilarious!                    5 "Hilarious!"                                 
    ##  7 Excellent in all fronts.      5 "Excellent in all fronts."                   
    ##  8 good                          5 "good"                                       
    ##  9 Buy                           5 "Very good movie not very expensive"         
    ## 10 If you like silly, it's ~     5 "My husband and teen boys loved it, I though~

Let’s read a few pages of reviews.

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 x 3
    ##    title                  stars text                                            
    ##    <chr>                  <dbl> <chr>                                           
    ##  1 Just watch the freaki~     5 "Its a great movie, gosh!!"                     
    ##  2 Great Value                5 "Great Value"                                   
    ##  3 I LOVE THIS MOVIE          5 "THIS MOVIE IS SO FUNNY ONE OF MY FAVORITES"    
    ##  4 Don't you wish you co~     5 "Watch it 100 times. Never. Gets. Old."         
    ##  5 Stupid, but very funn~     5 "If you like stupidly funny '90s teenage movies~
    ##  6 The beat                   5 "The best"                                      
    ##  7 Hilarious                  5 "Super funny! Loved the online rental."         
    ##  8 Love this movie            5 "We love this product.  It came in a timely man~
    ##  9 Entertaining, limited~     4 "Entertainment level gets a 5 star but having p~
    ## 10 Boo                        1 "We rented this movie because our Adventure Dat~
    ## # ... with 40 more rows

## Functions as arguments

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 1.058145

``` r
my_summary(x_vec, IQR)
```

    ## [1] 1.064439

``` r
my_summary(x_vec, var)
```

    ## [1] 1.11967

## Scoping and names

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
