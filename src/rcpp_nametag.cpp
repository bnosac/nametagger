#include <Rcpp.h>
#include <memory>
#include <fstream>
#include "ner/ner.h"
#include "utils/getpara.h"
#include "ner/bilou_ner_trainer.h"
#include "ner/ner_ids.h"
#include "tagger/tagger.h"
#include "utils/iostreams.h"
#include "utils/options.h"

// #include <Rcpp.h>
// #include <memory>
// #include <fstream>
// #include "nametag.h"
using namespace ufal::nametag;


// [[Rcpp::export]]
Rcpp::List nametag_info(SEXP model) {
  Rcpp::XPtr<ufal::nametag::ner> recognizer(model);
  std::vector<std::string> entities;
  std::vector<std::string> gazetteers;
  std::vector<int> gazetteer_types;
  recognizer->entity_types(entities);
  recognizer->gazetteers(gazetteers, &gazetteer_types);
  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("entities") = entities,
    Rcpp::Named("gazetteers") = Rcpp::List::create(
      Rcpp::Named("gazetteers") = gazetteers, 
      Rcpp::Named("gazetteer_types") = gazetteer_types)
  );
  return out;
}


// [[Rcpp::export]]
SEXP nametag_load_model(const char* file_model) {
  ufal::nametag::ner *languagemodel;
  languagemodel = ufal::nametag::ner::load(file_model);
  Rcpp::XPtr<ufal::nametag::ner> ptr(languagemodel, true);
  return ptr;
}


void sort_entities(std::vector<named_entity>& entities) {
  struct named_entity_comparator {
    static bool lt(const named_entity& a, const named_entity& b) {
      return a.start < b.start || (a.start == b.start && a.length > b.length);
    }
  };
  
  // Many models return entities sorted -- it is worthwhile to check that.
  if (!is_sorted(entities.begin(), entities.end(), named_entity_comparator::lt))
    sort(entities.begin(), entities.end(), named_entity_comparator::lt);
}


// [[Rcpp::export]]
Rcpp::DataFrame nametag_annotate(SEXP model, Rcpp::StringVector x, Rcpp::StringVector docid, Rcpp::IntegerVector sentenceid) {
  //Rcpp::Rcout << "Loading model " << fname << std::endl;
  //std::unique_ptr<ner> recognizer(ner::load(fname));
  Rcpp::XPtr<ufal::nametag::ner> recognizer(model);
  std::unique_ptr<tokenizer> tokenizer(tokenizer::new_vertical_tokenizer());
  
  std::vector< std::string > result_doc_id;
  std::vector< int > result_sentence_id;
  std::vector< unsigned int > result_token_id;
  std::vector< std::string > result_form;
  std::vector< std::string > result_iob;
  
  std::string para;
  std::vector<string_piece> forms;
  std::vector<named_entity> entities;
  
  std::istringstream is;
  
  std::string entity_none;
  entity_none = 'O';
  
  for (int idx = 0; idx < x.size(); idx++){
    std::string doc_id;
    doc_id = docid[idx];
    int sentence_id;
    sentence_id = sentenceid[idx];
    is.str (Rcpp::as<std::string>(x[idx]));
    while (getpara(is, para)) {
      // Tokenize and tag
      tokenizer->set_text(para);
      while (tokenizer->next_sentence(&forms, nullptr)) {
        recognizer->recognize(forms, entities);
        sort_entities(entities);
        
        std::string entity_type;
        unsigned in_entity = 0;
        bool entity_start = false;
        for (unsigned i = 0, e = 0; i < forms.size(); i++) {
          result_token_id.push_back(i+1);
          result_doc_id.push_back(doc_id);
          result_sentence_id.push_back(sentence_id);
          std::string token(forms[i].str, forms[i].len);
          result_form.push_back(token);
          
          if (!in_entity && e < entities.size() && entities[e].start == i) {
            in_entity = entities[e].length;
            entity_start = true;
            entity_type = entities[e].type;
            e++;
          }
          if (in_entity) {
            if(entity_start){
              result_iob.push_back("B-" + entity_type);
            }else{
              result_iob.push_back("I-" + entity_type);  
            }
            entity_start = false;
            in_entity--;
          } else {
            result_iob.push_back(entity_none);
          }
        }
      }
    }
    is.clear();
  }
  Rcpp::DataFrame out = Rcpp::DataFrame::create(
    Rcpp::Named("doc_id") = result_doc_id,
    Rcpp::Named("sentence_id") = result_sentence_id,
    Rcpp::Named("term_id") = result_token_id,
    Rcpp::Named("term") = result_form,
    Rcpp::Named("entity") = result_iob,
    Rcpp::Named("stringsAsFactors") = false
  );
  return out;
}




// [[Rcpp::export]]
Rcpp::List nametag_train(std::string modelname, 
                         std::string file, 
                         const std::string type, 
                         const char* features_file, const std::string input_type = "trivial", int stages = 1, int iterations = 30,
                         double missing_weight = -0.2, double initial_learning_rate = 0.1, double final_learning_rate = 0.01,
                         double gaussian = 0.5, int hidden_layer = 0,
                         Rcpp::Nullable<Rcpp::CharacterVector> file_holdout = R_NilValue) {
  std::ifstream is (file);
  std::ofstream os;
  os.open (modelname, std::ios::binary);
  
  ner_id id;
  if (!ner_ids::parse(type, id)) Rcpp::stop("type is not correct");
  // Encode the ner_id
  os.put(id);
  
  // Create and encode the tagger
  std::unique_ptr<tagger> tagger(tagger::create_and_encode_instance(input_type, os));
  if (!tagger) Rcpp::stop("Cannot load and encode tagger!");
  
  // Options
  network_parameters parameters;
  parameters.iterations = iterations;
  parameters.missing_weight = missing_weight;
  parameters.initial_learning_rate = initial_learning_rate;
  parameters.final_learning_rate = final_learning_rate;
  parameters.gaussian_sigma = gaussian;
  parameters.hidden_layer = hidden_layer;
  const char* heldout_file;
  if (file_holdout.isNotNull()) {
    Rcpp::CharacterVector s(file_holdout);
    heldout_file = std::string(s[0]).c_str();
  }else{
    heldout_file = nullptr;
  }
  
  // Open features / heldout files
  std::ifstream features(features_file);
  if (!features.is_open()) Rcpp::stop("Cannot open features file");
  
  std::ifstream heldout;
  if (heldout_file) {
    heldout.open(heldout_file);
    if (!heldout.is_open()) Rcpp::stop("Cannot open heldout file");
  } else {
    heldout.setstate(std::ios::failbit);
  }
  // Train
  bilou_ner_trainer::train(id, stages, parameters, *tagger, features, is, heldout, os);
  
  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("doc_id") = 1,
    Rcpp::Named("stringsAsFactors") = false
  );
  return out;
  
}