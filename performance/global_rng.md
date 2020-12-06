Performance of using the global RNG interface
================

## Comparison

The following comparison shows that using the interface to `dqrng`s RNG
does not come at a performance cost.

``` r
n <- 1e3L
min <- -1
max <- 1
rate <- 0.4
mean <- 0.05
sd <- 0.2

bench::mark(
  dqrunif_internal = dqrng::dqrunif(n, min, max),
  dqrunif_external = test_dqrunif(n, min, max),
  dqrexp_internal = dqrng::dqrexp(n, rate),
  dqrexp_external = test_dqrexp(n, rate),
  dqrnorm_internal = dqrng::dqrnorm(n, mean, sd),
  dqrnorm_external = test_dqrnorm(n, mean, sd),
  check = FALSE,
  min_iterations = 1e2L
) %>%
  mutate(expression = factor(
    expression,
    levels = c(
      "dqrunif_internal", "dqrunif_external",
      "dqrexp_internal", "dqrexp_external",
      "dqrnorm_internal", "dqrnorm_external"
    )
  ))
```

    ## # A tibble: 6 x 6
    ##   expression            min   median `itr/sec` mem_alloc `gc/sec`
    ##   <fct>            <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
    ## 1 dqrunif_internal   56.4µs   66.7µs    11963.   12.05KB     2.05
    ## 2 dqrunif_external   50.2µs     55µs    12713.  135.08KB     2.06
    ## 3 dqrexp_internal   100.8µs  107.4µs     8448.   11.86KB     2.04
    ## 4 dqrexp_external    96.4µs     99µs     9133.    7.86KB     0   
    ## 5 dqrnorm_internal  100.3µs  109.9µs     8214.   12.05KB     2.08
    ## 6 dqrnorm_external   97.4µs  101.4µs     9039.    7.86KB     0

## Rcpp code

``` cpp
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
// [[Rcpp::depends(dqrng,sitmo,BH)]]
#include <dqrng.h>
#include <mystdint.h>
#include <Rcpp.h>
#include <dqrng_generator.h>
#include <dqrng_distribution.h>
#include <xoshiro.h>
#include <pcg_random.hpp>
#include <threefry.h>
#include <convert_seed.h>
#include <R_randgen.h>
#include <minimal_int_set.h>
#include <dqrng_get_rng.h>

template<typename _Distribution>
struct distribution_caller {
  using dist_t = _Distribution;
  using parm_t = typename dist_t::param_type;
  template<typename _Engine, typename... Args>
  static Rcpp::NumericVector call(_Engine&& engine, const std::size_t n, Args&&... args) {
    const auto parm = parm_t{std::forward<decltype(args)>(args)...};
    auto dist = dist_t{};
    auto out = Rcpp::NumericVector(Rcpp::no_init(n));
    std::generate(out.begin(), out.end(), [&dist, &parm, &engine]() {
      return dist(engine, parm);
    });
    return out;
  }
};

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector test_dqrunif(const std::size_t n, const double min = 0.0, const double max = 1.0) {
  using caller_t = distribution_caller<dqrng::uniform_distribution>;

  auto rng = dqrng::get_rng();
  return caller_t::call(*rng, n, min, max);
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector test_dqrexp(const std::size_t n, const double rate = 1.0) {
  using caller_t = distribution_caller<dqrng::exponential_distribution>;

  auto rng = dqrng::get_rng();
  return caller_t::call(*rng, n, rate);
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector test_dqrnorm(const std::size_t n, const double mean = 0.0, const double sd = 1.0) {
  using caller_t = distribution_caller<dqrng::normal_distribution>;

  auto rng = dqrng::get_rng();
  return caller_t::call(*rng, n, mean, sd);
}
```
