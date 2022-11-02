# Named Entity Recognition with the Nametag Maximum Entropy Markov model 

This repository contains an R package which wraps the NameTag C++ library (https://github.com/ufal/nametag), allowing the following:
  
- Fit a **Maximum Entropy Markov model** machine learning model
- Use the model to get predictions alongside the model on new data
- The focus of the implementation is in the area of Natural Language Processing where this R package allows you to easily build and apply models for **named entity recognition, text chunking, part of speech tagging, intent recognition or classification** of any category you have in mind.


## Example

- Downloading a pretrained named entity recognition model. 
- Note: look to the help of function `?nametagger` which allows you to train your own named entity recognition model on your own data

```
library(nametagger)
model <- nametagger_download_model("english-conll-140408", model_dir = tempdir())
x     <- data.frame(doc_id      = c(1, 1, 2),
                    sentence_id = c(1, 2, 1),
                    text        = c("I\nlive\nin\nNew\nYork\nand\nI\nwork\nfor\nApple\nInc.", 
                                    "Why\ndon't\nyou\ncome\nvisit\nme", 
                                    "Good\nnews\nfrom\nAmazon\nas\nJohn\nworks\nthere\n."))
predict(model, x)                          

 doc_id sentence_id term_id   term entity
      1           1       1      I      O
      1           1       2   live      O
      1           1       3     in      O
      1           1       4    New  B-LOC
      1           1       5   York  I-LOC
      1           1       6    and      O
      1           1       7      I      O
      1           1       8   work      O
      1           1       9    for      O
      1           1      10  Apple  B-ORG
      1           1      11   Inc.  I-ORG
      1           2       1    Why      O
      1           2       2  don't      O
      1           2       3    you      O
      1           2       4   come      O
      1           2       5  visit      O
      1           2       6     me      O
      2           1       1   Good      O
      2           1       2   news      O
      2           1       3   from      O
      2           1       4 Amazon  B-LOC
      2           1       5     as      O
      2           1       6   John  B-PER
      2           1       7  works      O
      2           1       8  there      O
      2           1       9      .      O
```

## Installation

- For regular users, install the package from your local CRAN mirror `install.packages("nametagger")`
- For installing the development version of this package: `remotes::install_github("bnosac/nametagger")`

Look to the documentation of the functions.

```
help(package = "nametagger")
```

## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be
