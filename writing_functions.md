Writing Functions
================
Xun Wang
10/24/2019

## Get started

We’re going to write some functions.

Here’s z scores.

``` r
x = rnorm(n = 30, mean = 4, sd = 2.3)
x_again = rnorm(n = 30, mean = 6, sd = .3)
y = rnorm(n = 30, mean = 24, sd = 2.3)

(x - mean(x)) / sd(x)
```

    ##  [1]  0.37919943 -0.49381258  1.02746458 -2.42498461  0.72626944
    ##  [6] -1.24138557 -1.18580745 -0.16873321  0.37054019  0.48479143
    ## [11] -0.32216318  0.77613465  2.40889997  0.63423705  0.02751085
    ## [16] -0.76144179  1.59363498  0.16266720 -0.05441730  0.14406752
    ## [21] -0.90282952 -0.10726822  0.71064242 -1.70485956  0.41736662
    ## [26] -1.16384683 -0.28620518  0.38537077  1.16761917 -0.59866128

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1] -0.30923969 -0.25907595  0.35035756 -1.03009504  1.07437745
    ##  [6]  0.99993578 -0.61318592  0.03031266 -0.57736218  0.08357951
    ## [11]  2.12581912 -0.78150481  0.17379751 -2.07669469  1.37757258
    ## [16] -0.13066842 -1.18916487  0.41094406  0.75247280 -1.38594540
    ## [21]  0.09743748 -0.29824755  1.84571175  0.17579245  0.74259898
    ## [26]  1.07604889 -0.36904539  0.20753948 -0.63370369 -1.87036446

Now a function.

``` r
z_score = function(x_arg) {
  
  if (!is.numeric(x_arg)) {
    stop("x should be numeric")
  } else if (length(x_arg) < 3) {
    stop("x should be longer than 3")
  }
  
  (x_arg - mean(x_arg)) / sd(x_arg)
  
}
```

Try out the function.

``` r
z_score(x_arg = y)
```

    ##  [1]  0.79118367  0.65780412 -0.05897862  1.41120042  0.26607494
    ##  [6]  0.18345889  1.33819377 -1.40265059  1.03236127  0.15984541
    ## [11] -1.23848417  0.08632017  0.11098848  0.22581598  0.07780537
    ## [16] -0.20405485  1.40091272  0.31683208  0.58018435 -1.47149774
    ## [21] -1.93193394 -1.67827498 -0.34336435 -0.77913671 -0.32512444
    ## [26] -1.88719133 -0.34909493  0.36440043  1.22229915  1.44410541

``` r
z_score(x_arg = 3)
```

    ## Error in z_score(x_arg = 3): x should be longer than 3

``` r
z_score(x_arg = "my name is jeff")
```

    ## Error in z_score(x_arg = "my name is jeff"): x should be numeric

``` r
z_score(x_arg = c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(x_arg = c(TRUE, TRUE, FALSE, TRUE)): x should be numeric

``` r
z_score(x_arg = iris)
```

    ## Error in z_score(x_arg = iris): x should be numeric

## Multiple outputs

``` r
mean_and_sd = function(input_x) {
  
  if (!is.numeric(input_x)) {
    stop("x should be numeric")
  } else if (length(input_x) < 3) {
    stop("x should be longer than 3")
  } 
  
  list(
  mean_input = mean(input_x),
  sd_input = sd(input_x),
  z_score = (input_x - mean(input_x)) / sd(input_x)
  )
}
```

test this function

``` r
mean_and_sd(input_x = y)
```

    ## $mean_input
    ## [1] 23.70473
    ## 
    ## $sd_input
    ## [1] 2.155245
    ## 
    ## $z_score
    ##  [1]  0.79118367  0.65780412 -0.05897862  1.41120042  0.26607494
    ##  [6]  0.18345889  1.33819377 -1.40265059  1.03236127  0.15984541
    ## [11] -1.23848417  0.08632017  0.11098848  0.22581598  0.07780537
    ## [16] -0.20405485  1.40091272  0.31683208  0.58018435 -1.47149774
    ## [21] -1.93193394 -1.67827498 -0.34336435 -0.77913671 -0.32512444
    ## [26] -1.88719133 -0.34909493  0.36440043  1.22229915  1.44410541

## Multiple inputs
