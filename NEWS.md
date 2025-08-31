## CHANGES IN nametagger VERSION 0.1.5

- avoid warning: overlapping comparisons always evaluate to true in parse_int 
   - replacing: !(str.str[0] >= '0' || str.str[0] <= '9') with (str.str[0] < '0' || str.str[0] > '9')

## CHANGES IN nametagger VERSION 0.1.4

- nametagger_download_model now allows to download a model for Czech: czech-cnec-140304

## CHANGES IN nametagger VERSION 0.1.3

- Add explicit initialization to silence false positive valgrind report in compressor_save.cpp
- Drop C++11 specification in Makevars
- Remove use of std::iterator by incorporating fixes of https://github.com/ufal/nametag/commit/2aa1d1de78d2f562c0770423f94cc7d7e1347ff7 in utf8.h and utf16.h

## CHANGES IN nametagger VERSION 0.1.2

- use snprintf instead of sprintf to handle the R CMD check deprecating note on M1mac
- added example in README

## CHANGES IN nametagger VERSION 0.1.1

- Move udpipe to Suggests instead of Imports, remove crfsuite from Suggests
- Make example conditionally on availability of udpipe

## CHANGES IN nametagger VERSION 0.1.0

- Initial package based on https://github.com/ufal/nametag commit 598666b5aa2f3ebbb9658976fe06749f551aed02
