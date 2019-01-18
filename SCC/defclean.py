
# coding: utf-8

# In[2]:


import csv
import os
import glob
import pandas as pd
import nltk
import string
import re
from nltk import ngrams
import multiprocessing
import time


# In[4]:


def clean_and_split_str(txt):
    #strip_special_chars = re.compile("[^A-Za-z0-9#]+")
    translator = str.maketrans('', '', string.punctuation)
    txt = txt.translate(translator)
    txt = re.sub('\s+', ' ', txt).strip()
    txt = txt.lower()
    return txt


# In[5]:


def find_ngrams(sentence, n_list):
    """Magic n-gram function."""
    inp, grams = sentence.split(), []
    for n in n_list:
      grams += [' '.join(x) for x in zip(*[inp[i:] for i in range(n)])]
    return grams


# In[7]:


# Change to Dir of file
os.chdir("path")
list_csv = glob.glob("*.csv")
list_csv


# In[32]:


# Function for extracting ngrams and generating tokenized files

def PreProcess(filename):
    # Iterate over CSVs
    df = pd.read_csv(filename, sep = "\t")
    print(filename + " imported")
    
    # Drop duplicates
    lbf = len(df)
    df = df.drop_duplicates(subset=['id'], keep='first')
    laf = len(df)
    print(str(lbf-laf) + " duplicates dropped")
    
    # Check av. number of articles per year
    start_year = int(df.date[0][0:4])
    end_year = int(df.date[len(df)-1][0:4])
    
    try:
        aay = len(df) / (end_year - start_year) 
        if aay < 5000:
            df.date = df.date.str.slice(0, 4)
        if aay > 4999:
            df.date = df.date.str.replace("/", "")
            df.date = df.date.str.slice(0, 6)
            print('average number of articles/year = ', str(aay))

    except ZeroDivisionError:
        df.date = df.date.str.slice(0, 4)
  
        
        
    # Iterate over years
    for year in sorted(list(set(df.date))):
        df_subset_year = df[df.date == year]
        df_subset_year = df_subset_year.reset_index(drop=True)
        print(str(year) + " = " + str(len(df_subset_year)) + " articles")
        bigram_df = pd.DataFrame()
        unigram_df = pd.DataFrame()
        
        list_tok_articles_year = list()
        
        # Iterate over Articles in Subsetted CSV
        for i in range(0,(len(df_subset_year.ocr) - 1)):
            
            
            article = df_subset_year.ocr[i]
            
            tokens = clean_and_split_str(article)
            list_tok_articles_year.append(tokens)
            
            
            # Get ngrams from tokenized_article
            list_ngrams = find_ngrams(tokens, [1,2])
            list_unigrams = [word for word in list_ngrams if len(word.split(" ")) == 1]
            list_bigrams = [word for word in list_ngrams if len(word.split(" ")) == 2]

        
            df_unigrams_article = pd.DataFrame(list_unigrams)
            df_unigrams_article['year'] = year
            df_unigrams_article['count'] = 1
            df_unigrams_article.columns = ['ngram', 'year', 'count']
            unigram_df = unigram_df.append(df_unigrams_article)
            
            
            df_bigrams_article = pd.DataFrame(list_bigrams)
            df_bigrams_article['year'] = year
            df_bigrams_article['count'] = 1
            df_bigrams_article.columns = ['ngram', 'year', 'count']
            bigram_df = bigram_df.append(df_bigrams_article)
            print(str(i) + "/" + str(len(df_subset_year)) + ": " + df_subset_year.date[i] + " " + df_subset_year.id[i] + " processed")
        
        #Write list of senttok articles to one file
        txt_name = filename[0:4] + "_lines_" + str(year) + ".txt"
                        
        with open(txt_name, 'w') as f:
            for item in list_tok_articles_year:
                f.write("%s\n" % item)

        
        ## Group Ngram DFS
        unigram_df = unigram_df.groupby(['ngram', 'year']).sum()
        unigram_df = unigram_df.reset_index()
        unigram_df = unigram_df[unigram_df['count'] > 1]
        fn = filename[0:4] + "_unigram_" + str(year) + ".csv"
        unigram_df.to_csv(fn, index = False)
    
        bigram_df = bigram_df.groupby(['ngram', 'year']).sum()
        bigram_df = bigram_df.reset_index()
        bigram_df = bigram_df[bigram_df['count'] > 1]
        fn = filename[0:4] + "_" + "_bigram_" + str(year) + ".csv"
        bigram_df.to_csv(fn, index = False)

