# TheForeign // Scripts and Resources for thesis project "The Birth of the Foreign: A Digital Conceptual History of Buitenland in Dutch newspapers 1815-1914"

This repository contains (mostly) scripts for normalizing, cleaning and analyzing newspaper data for my thesis project on the concept of "the foreign" (_het buitenland_). If you use the scripts, please use the following citation:

- Ros, R., (2019). Conceptual Vocabularies and Changing Meanings of “Foreign” in Dutch Foreign News (1815-1914). In Proceedings of the Digital Humanities (DH) conference 2019, Utrecht, The Netherlands.

The folders in the repository correspond roughly to the research structure of my thesis. Scripts are categorized in the following folders:
1. `Data Processing` scripts that clean and process the raw data downloaded through the KB API. I generate 1-,2- and 3-grams and create scripts that train word embeddings (word2vec) and are able to extract "keywords in context".
2. `Metadata Analysis` scripts to study data on 1) newspaper (token) size 2) newspaper timespans 3) and newspaper titles 
3. `General Pattern Detection` scripts for tracing long-term patterns in ngram frequencies
4. `Semantic Properties` scripts for studying the 'semantic properties' that attached themselves to the concept of _buitenland_. 
5. `Neighbour(ing) Concepts` scripts that explore the concepts 'around' _buitenland_.
6. `Resources` files and scripts used repeatedly (such as lists of stopwords).
