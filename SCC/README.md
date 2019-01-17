# Subcorpus Creation (SCC)

The SCC folder contains the following scripts:
1. `SCC_tokens_ngrams`   script that loops over newspaper .csv's. Files are checked for duplicate article ID's, subsequently tokenized and ngramized. Output is ngram (1&2) files for every year and .txt files with articles written as lines. When amount of articles > 5000 per year the data is outputted based on months to prevent flushing the memory.
2. `SCC_get_ngrams` script that ngramizes the tokenized data. Each newspaper .csv is transformed into 1-,2-,3-,4- and 5-gram csv's that contain the n-gram itself and the year it occurs.
3. `Vocabulary Diachronic Change Detection (VCC)`
