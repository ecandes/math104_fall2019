Principal Component Analysis: a Simple Example
================

The data are the results of qualifying examinations for 25 graduate students in mathematics. diffgeom refers to differential geometry and reals is for real analysis. Note that the differential geometry and complex analysis examinations were closed book, while the remaining three exams were open book.

``` r
# Load data
setwd("/Users/emmanuel/Dropbox/Work/Teaching/Stanford/Math/Math104/2019-2020/Lectures/gitHub")
testscores = read.table("testscores.dat.txt")
testscores
```

    ##    diffgeom complex algebra reals statistics
    ## 1        36      58      43    36         37
    ## 2        62      54      50    46         52
    ## 3        31      42      41    40         29
    ## 4        76      78      69    66         81
    ## 5        46      56      52    56         40
    ## 6        12      42      38    38         28
    ## 7        39      46      51    54         41
    ## 8        30      51      54    52         32
    ## 9        22      32      43    28         22
    ## 10        9      40      47    30         24
    ## 11       32      49      54    37         52
    ## 12       40      62      51    40         49
    ## 13       64      75      70    66         63
    ## 14       36      38      58    62         62
    ## 15       24      46      44    55         49
    ## 16       50      50      54    52         51
    ## 17       42      42      52    38         50
    ## 18        2      35      32    22         16
    ## 19       56      53      42    40         32
    ## 20       59      72      70    66         62
    ## 21       28      50      50    42         63
    ## 22       19      46      49    40         30
    ## 23       36      56      56    54         52
    ## 24       54      57      59    62         58
    ## 25       14      35      38    29         20

We now compute the covariance matrix of the data and display it.

``` r
covX = var(testscores)
covX
```

    ##            diffgeom   complex   algebra    reals statistics
    ## diffgeom   348.7733 181.69167 137.54500 176.8850   233.6583
    ## complex    181.6917 145.75000  91.28333 108.4750   142.5000
    ## algebra    137.5450  91.28333  95.39333 106.1383   135.1833
    ## reals      176.8850 108.47500 106.13833 166.9567   167.5500
    ## statistics 233.6583 142.50000 135.18333 167.5500   272.6667

We compute the correlation matrix and display it as well. The correlation matrix is the same as before, except that the variables are standardized to have unit variance.

``` r
rcorr = cor(testscores)
rcorr
```

    ##             diffgeom   complex   algebra     reals statistics
    ## diffgeom   1.0000000 0.8058591 0.7540743 0.7330228  0.7576936
    ## complex    0.8058591 1.0000000 0.7741555 0.6953821  0.7148165
    ## algebra    0.7540743 0.7741555 1.0000000 0.8410298  0.8382009
    ## reals      0.7330228 0.6953821 0.8410298 1.0000000  0.7852836
    ## statistics 0.7576936 0.7148165 0.8382009 0.7852836  1.0000000

We compute the eigenvalues and eigenvectors of the covariance matrix

``` r
# Eigen analysis
my.PCs = eigen(covX)
my.PCs
```

    ## eigen() decomposition
    ## $values
    ## [1] 845.48108  85.04139  45.38813  39.18836  14.44103
    ## 
    ## $vectors
    ##            [,1]       [,2]       [,3]        [,4]        [,5]
    ## [1,] -0.5982782  0.6745404 -0.1852556 -0.38597894  0.06131111
    ## [2,] -0.3607532  0.2450733  0.2490064  0.82871854 -0.24701742
    ## [3,] -0.3021774 -0.2140882  0.2114109  0.13484564  0.89441442
    ## [4,] -0.3890403 -0.3384022  0.6999921 -0.37537871 -0.32129949
    ## [5,] -0.5188995 -0.5697232 -0.6074477  0.07178665 -0.17892129

We look at the fraction of variance explained by the top principal components

``` r
cumsum(my.PCs$values)/sum(my.PCs$values)
```

    ## [1] 0.8212222 0.9038235 0.9479094 0.9859733 1.0000000

We can of course use R built-in functions

``` r
pcTest = prcomp(testscores,scale=FALSE)
plot(pcTest)
```

![](PCA_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
summary(pcTest)
```

    ## Importance of components:
    ##                            PC1    PC2     PC3     PC4     PC5
    ## Standard deviation     29.0772 9.2218 6.73707 6.26006 3.80014
    ## Proportion of Variance  0.8212 0.0826 0.04409 0.03806 0.01403
    ## Cumulative Proportion   0.8212 0.9038 0.94791 0.98597 1.00000

``` r
biplot(pcTest)
```

![](PCA_files/figure-markdown_github/unnamed-chunk-6-2.png)

Normalize variables first

``` r
pcTest.scaled = prcomp(testscores,scale=TRUE)
summary(pcTest.scaled)
```

    ## Importance of components:
    ##                           PC1     PC2     PC3     PC4     PC5
    ## Standard deviation     2.0202 0.61144 0.46535 0.45253 0.35163
    ## Proportion of Variance 0.8162 0.07477 0.04331 0.04096 0.02473
    ## Cumulative Proportion  0.8162 0.89100 0.93431 0.97527 1.00000

``` r
biplot(pcTest)
```

![](PCA_files/figure-markdown_github/unnamed-chunk-7-1.png)
