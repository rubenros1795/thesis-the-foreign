from multiprocessing import Pool
import multiprocessing
import defclean
import time
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

os.chdir("path")
list_csv = glob.glob("*.csv")
list_csv

def clean_and_split_str(string):
    strip_special_chars = re.compile("[^A-Za-z0-9#]+")
    string = re.sub(strip_special_chars, " ", string)
    string = string.lower()
    return string



def find_ngrams(sentence, n_list):
    """Magic n-gram function."""
    inp, grams = sentence.split(), []
    for n in n_list:
      grams += [' '.join(x) for x in zip(*[inp[i:] for i in range(n)])]
    return grams

if __name__ == '__main__':
    p = multiprocessing.Pool(processes = multiprocessing.cpu_count() - 1)
    #timing it...
    start = time.time()
    for file in list_csv:
        p.apply_async(defclean.PreProcess, [file])

    p.close()
    p.join()
    print("Complete")
    end = time.time()
    print('total time (s)= ' + str(end-start))