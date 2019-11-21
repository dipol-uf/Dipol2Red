
# `Dipol2Red`

## Example usage

``` r
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(Dipol2Red, quietly = TRUE, warn.conflicts = FALSE)
```

  - Simulating one observation with `1 %` polarization in `x` direction.
    
    ``` r
    x_1 <- tibble(
        JD = c(1, 2, 3, 4), 
        Obs = 2.5 * log10(c(102, 100, 98, 100)))
    fsigma_2(x_1)
    ```
    
        ## # A tibble: 1 x 11
        ##      JD    Px    Py    SG     P     A  SG_A   Itt     N Ratio Q            
        ##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <dbl> <list>       
        ## 1   2.5 1.000     0     0 1.000     0     0     1     1     0 <dbl[,2] [2 ~

  - Simulating total polarization of `1 %` and equal `x` and `y`
    components.
    
    ``` r
    x_2 <- tibble(
        JD = 10 + c(1, 2, 3, 4), 
        Obs = 2.5 * log10(c(101.4142, 101.4142, 98.5858, 98.5858)))
    fsigma_2(x_2)
    ```
    
        ## # A tibble: 1 x 11
        ##      JD    Px    Py    SG     P     A  SG_A   Itt     N Ratio Q            
        ##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <dbl> <list>       
        ## 1  12.5 0.707 0.707     0 1.000  22.5     0     1     1     0 <dbl[,2] [2 ~

  - Combining two datasets to obtain average over several observations.
    
    ``` r
    x_3 <- bind_rows(x_1, x_2)
    fsigma_2(x_3)
    ```
    
        ## # A tibble: 1 x 11
        ##      JD    Px    Py    SG     P     A  SG_A   Itt     N Ratio Q            
        ##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <dbl> <list>       
        ## 1   7.5 0.854 0.354 0.271 0.924  11.2  8.16     2     2     0 <dbl[,2] [2 ~

  - Assigning groups, separating observations by `Run`. Recreates two
    original measurements. `fsigma_2` automatically detects grouping
    variables and appends these values to the output.
    
    ``` r
    x_4 <- mutate(x_3, Run = factor(if_else(JD < 10, "First", "Second")))
    x_4 <- group_by(x_4, Run)
    fsigma_2(x_4)
    ```
    
        ## # A tibble: 2 x 12
        ##      JD    Px    Py    SG     P     A  SG_A   Itt     N Ratio Q       Run  
        ##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int> <int> <dbl> <list>  <fct>
        ## 1   2.5 1.000 0         0 1.000   0       0     1     1     0 <dbl[,~ First
        ## 2  12.5 0.707 0.707     0 1.000  22.5     0     1     1     0 <dbl[,~ Seco~
