#ifndef DQRNG_GET_RNG_H
#define DQRNG_GET_RNG_H 1

#include <Rcpp.h>
#include "dqrng.h"
#include "dqrng_generator.h"

namespace dqrng {

dqrng::rng64_t::element_type* get_rng() {
	return Rcpp::XPtr<dqrng::rng64_t::element_type>(dqrng::get_sxprng()).get();
}


}

#endif // DQRNG_GET_RNG_H
