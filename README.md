
# `Dipol2Red`

`Dipol2Red` is a package for reducing data obtained with a
double-imaging polarimeter *Dipol-2* (and any later versions of *Dipol*
family polarimeters).

The input data are the differences (in magnitudes) between ordinary and
extraordinary rays of a given target on the image and respective
timestamps (or any other sortable numeric identifier). The data are
generated as a result of subsequent observations with different phase
plate angle (22.5 deg step). Every 16 measurement correspond to one full
plate rotation, every 4 measurements produce one (independent
polarization measurement).

Such stream of `4n` data is transformed into polarization measurement.
If no grouping is provided, all data are used to compute one average
(over the whole data set). Alternatively, data can be grouped by `4m`
subsequent observations (using `dplyr::group_by`), and the averaging
procedure is applied to each group separately, providing the best
control.

## Example usage

``` r
library(tibble, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(Dipol2Red, quietly = TRUE, warn.conflicts = FALSE)
```

  - Simulating one observation with `1 %` polarization in `x` direction.
    4 measurements corresponding to plate angles 0, 22.5, 45, 67.5 deg.
    
    ``` r
    x_1 <- tibble(
        JD = c(1, 2, 3, 4), 
        Obs = 2.5 * log10(c(102, 100, 98, 100)))
    fsigma_2(x_1)
    ```
    
        ## [90m# A tibble: 1 x 11[39m
        ##      JD    Px    Py     P    SG     A  SG_A   Itt     N Ratio               Q
        ##   [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<int>[39m[23m [3m[90m<int>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<list<dbl[,2]>>[39m[23m
        ## [90m1[39m   2.5  1.00     0  1.00     0     0     0     1     1     0         [90m[2 × 2][39m

  - Simulating total polarization of `1 %` and equal `x` and `y`
    components.
    
    ``` r
    x_2 <- tibble(
        JD = 10 + c(1, 2, 3, 4), 
        Obs = 2.5 * log10(c(101.4142, 101.4142, 98.5858, 98.5858)))
    fsigma_2(x_2)
    ```
    
        ## [90m# A tibble: 1 x 11[39m
        ##      JD    Px    Py     P    SG     A  SG_A   Itt     N Ratio               Q
        ##   [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<int>[39m[23m [3m[90m<int>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<list<dbl[,2]>>[39m[23m
        ## [90m1[39m  12.5 0.707 0.707  1.00     0  22.5     0     1     1     0         [90m[2 × 2][39m

  - Combining two datasets to obtain average over several observations.
    
    ``` r
    x_3 <- bind_rows(x_1, x_2)
    fsigma_2(x_3)
    ```
    
        ## [90m# A tibble: 1 x 11[39m
        ##      JD    Px    Py     P    SG     A  SG_A   Itt     N Ratio               Q
        ##   [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<int>[39m[23m [3m[90m<int>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<list<dbl[,2]>>[39m[23m
        ## [90m1[39m   7.5 0.854 0.354 0.924 0.271  11.2  8.16     2     2     0         [90m[2 × 2][39m

  - Assigning groups, separating observations by `Run`. Recreates two
    original measurements. `fsigma_2` automatically detects grouping
    variables and appends these values to the output.
    
    ``` r
    x_4 <- mutate(x_3, Run = factor(if_else(JD < 10, "First", "Second")))
    x_4 <- group_by(x_4, Run)
    fsigma_2(x_4)
    ```
    
        ## [90m# A tibble: 2 x 12[39m
        ##      JD    Px    Py     P    SG     A  SG_A   Itt     N Ratio            Q Run  
        ##   [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<int>[39m[23m [3m[90m<int>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<list<dbl[,>[39m[23m [3m[90m<fct>[39m[23m
        ## [90m1[39m   2.5 1.00  0      1.00     0   0       0     1     1     0      [90m[2 × 2][39m First
        ## [90m2[39m  12.5 0.707 0.707  1.00     0  22.5     0     1     1     0      [90m[2 × 2][39m Seco…
