#include <Rcpp.h>
// This file is part of MorphoDiTa <http://github.com/ufal/morphodita/>.
//
// Copyright 2015 Institute of Formal and Applied Linguistics, Faculty of
// Mathematics and Physics, Charles University in Prague, Czech Republic.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#include "english_lemma_addinfo.h"
#include "english_morpho_encoder.h"
#include "english_morpho_guesser_encoder.h"
#include "morpho_dictionary_encoder.h"
#include "utils/binary_encoder.h"
#include "utils/compressor.h"

namespace ufal {
namespace nametag {
namespace morphodita {

void english_morpho_encoder::encode(istream& dictionary, int max_suffix_len, istream& guesser, istream& negations, ostream& out) {
  binary_encoder enc;

  Rcpp::Rcout << "Encoding dictionary." << endl;
  morpho_dictionary_encoder<english_lemma_addinfo>::encode(dictionary, max_suffix_len, enc);

  Rcpp::Rcout << "Encoding guesser." << endl;
  english_morpho_guesser_encoder::encode(guesser, negations, enc);

  Rcpp::Rcout << "Compressing dictionary." << endl;
  if (!compressor::save(out, enc)) runtime_failure("Cannot compress and write dictionary to file!");
  Rcpp::Rcout << "Dictionary saved." << endl;
}

} // namespace morphodita
} // namespace nametag
} // namespace ufal
