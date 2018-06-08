/*
 Original C Code written in 2016 and 2018 by David Blackman and Sebastiano Vigna (vigna@acm.org)
 Adapted to C++ in 2018 by Ralf Stubner (daqana GmbH)

 To the extent possible under law, the author has dedicated all copyright
 and related and neighboring rights to this software to the public domain
 worldwide. This software is distributed without any warranty.

 See <http://creativecommons.org/publicdomain/zero/1.0/>. */

#ifndef XOSHIRO_H
#define XOSHIRO_H 1

#include <array>
#include <dqrng_generator.h>

namespace dqrng {
template<size_t N, int_fast8_t A, int_fast8_t B, int_fast8_t C>
class xoshiro : public random_64bit_generator {
  static_assert(N == 2 || N == 4, "Unsupported size of RNG state.");
private:
  std::array<result_type, N> state;

  struct SplitMix {
    SplitMix(const uint64_t& k) : state(k) {}

    uint64_t operator() () {
      uint64_t z = (state += 0x9e3779b97f4a7c15ULL);
      z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
      z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
      return z ^ (z >> 31);
    }

  private:
    uint64_t state;
  };

  result_type rotl(const uint64_t x, int k) {
    return (x << k) | (x >> (64 - k));
  }

  template<std::size_t N_>
  typename std::enable_if<N_ == 2, result_type>::type
    next() {
      const uint64_t s0 = state[0];
      uint64_t s1 = state[1];
      const uint64_t result = state[0] + state[N_ - 1];

      s1 ^= s0;
      state[0] = rotl(s0, A) ^ s1 ^ (s1 << B); // a, b
      state[1] = rotl(s1, C); // c

      return result;
    }

  template<std::size_t N_>
  typename std::enable_if<N_ == 4, result_type>::type
    next() {
      const uint64_t result = state[0] + state[N_ - 1];

      const uint64_t t = state[1] << A;

      state[2] ^= state[0];
      state[3] ^= state[1];
      state[1] ^= state[2];
      state[0] ^= state[3];

      state[2] ^= t;

      state[3] = rotl(state[3], B);

      return result;
    }

public:
  xoshiro(result_type _seed = 0x85c6ea9eb065ebeeULL) {
    seed(_seed);
  }

  void seed(std::function<result_type(void)> rng) {
    std::generate(state.begin(), state.end(), rng);
  }

  void seed(result_type _seed) {
    seed(SplitMix(_seed));
  }

  virtual result_type operator() () {
    return next<N>();
  }

  void jump();
  void jump(uint64_t n) {
    for( ; n > 0; --n) jump();
  }
};

/* This is xoroshiro128+ 1.0, our best and fastest small-state generator
 for floating-point numbers. We suggest to use its upper bits for
 floating-point generation, as it is slightly faster than
 xoroshiro128**. It passes all tests we are aware of except for the four
 lower bits, which might fail linearity tests (and just those), so if
 low linear complexity is not considered an issue (as it is usually the
 case) it can be used to generate 64-bit outputs, too; moreover, this
 generator has a very mild Hamming-weight dependency making our test
 (http://prng.di.unimi.it/hwd.php) fail after 8 TB of output; we believe
 this slight bias cannot affect any application. If you are concerned,
 use xoroshiro128** or xoshiro256+.

 We suggest to use a sign test to extract a random Boolean value, and
 right shifts to extract subsets of bits.

 The state must be seeded so that it is not everywhere zero. If you have
 a 64-bit seed, we suggest to seed a splitmix64 generator and use its
 output to fill s.

 NOTE: the parameters (a=24, b=16, b=37) of this version give slightly
 better results in our test than the 2016 version (a=55, b=14, c=36).
 */

using xoroshiro128plus = xoshiro<2, 24, 16, 37>;

/* This is the jump function for the generator. It is equivalent
 to 2^64 calls to next(); it can be used to generate 2^64
 non-overlapping subsequences for parallel computations. */
template<>
void xoroshiro128plus::jump() {
  static const uint64_t JUMP[] = { 0xdf900294d8f554a5, 0x170865df4b3201fc };

  uint64_t s0 = 0;
  uint64_t s1 = 0;
  for(int i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
    for(int b = 0; b < 64; b++) {
      if (JUMP[i] & UINT64_C(1) << b) {
        s0 ^= state[0];
        s1 ^= state[1];
      }
      operator()();
    }

  state[0] = s0;
  state[1] = s1;
}
/* This is xoshiro256+ 1.0, our best and fastest generator for floating-point
 numbers. We suggest to use its upper bits for floating-point
 generation, as it is slightly faster than xoshiro256**. It passes all
 tests we are aware of except for the lowest three bits, which might
 fail linearity tests (and just those), so if low linear complexity is
 not considered an issue (as it is usually the case) it can be used to
 generate 64-bit outputs, too.

 We suggest to use a sign test to extract a random Boolean value, and
 right shifts to extract subsets of bits.

 The state must be seeded so that it is not everywhere zero. If you have
 a 64-bit seed, we suggest to seed a splitmix64 generator and use its
 output to fill s. */

using xoshiro256plus   = xoshiro<4, 17, 45, 0>;

/* This is the jump function for the generator. It is equivalent
 to 2^128 calls to next(); it can be used to generate 2^128
 non-overlapping subsequences for parallel computations. */
template<>
void xoshiro256plus::jump() {
  static const uint64_t JUMP[] = { 0x180ec6d33cfd0aba, 0xd5a61266f0c9392c, 0xa9582618e03fc9aa, 0x39abdc4529b1661c };

  uint64_t s0 = 0;
  uint64_t s1 = 0;
  uint64_t s2 = 0;
  uint64_t s3 = 0;
  for(int i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
    for(int b = 0; b < 64; b++) {
      if (JUMP[i] & UINT64_C(1) << b) {
        s0 ^= state[0];
        s1 ^= state[1];
        s2 ^= state[2];
        s3 ^= state[3];
      }
      operator()();
    }

  state[0] = s0;
  state[1] = s1;
  state[2] = s2;
  state[3] = s3;
}

}
#endif // XOSHIRO_H