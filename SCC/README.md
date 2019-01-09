# Subcorpus Creation (SCC)

The SCC folder contains the following scripts:
1. `SCC_clean_tokenize`   script that removes punctuation and tokenizes data. New column with tokens (separated by spaces) is generated for each newspaper .csv file. Duplicates are also removed by article ID.
2. `SCC_get_ngrams` script that ngramizes the tokenized data. Each newspaper .csv is transformed into 1-,2-,3-,4- and 5-gram csv's that contain the n-gram itself and the year it occurs.
3. `Vocabulary Diachronic Change Detection (VCC)`
