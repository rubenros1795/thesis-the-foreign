
# coding: utf-8

# In[2]:


import glob
import os
from os import path
import string
import numpy
import re
import pandas as pd
import multiprocessing
import gensim
import numpy as np
import random
import re
from flashtext import KeywordProcessor


# In[3]:


os.chdir("C://Users//Ruben//Documents//GitHub//TheForeign//SCC//output-data")
correction_dict = np.load('correction_dict.npy').item()
keyword_processor = KeywordProcessor()
#keyword_processor.add_keyword(<unclean name>, <standardised name>)
for key, value in correction_dict.items():
    keyword_processor.add_keyword(key, value)
keyword_processor.add_keyword('buitenl', 'buitenland')
keyword_processor.add_keyword('binnenl', 'binnenland')


# In[4]:


os.chdir('D://Scriptie//Data//lines//year')
list_txt = glob.glob('*.txt')
list_txt = sorted(list_txt)
list_txt = [l for l in list_txt if int(l[0:4]) > 1901] 

# In[5]:


from tqdm import tqdm

for txt in tqdm(list_txt):
    
    with open(txt, encoding = 'utf-8') as infile:
        lines = infile.read().splitlines() 
    
    sentences = list()

    for line in lines:
        tmp = keyword_processor.replace_keywords(line)
        tmp = tmp.split(' ')
        tmp = [l for l in tmp if len(l) > 1]
        tmp = ' '.join(tmp)
        sentences.append(tmp)
    
    os.chdir('D://Scriptie//Data//lines//cleaned')
    with open(txt[0:4] + '_cleaned.txt', 'w', encoding = 'utf-8') as file:
        for item in sentences:
            file.write("%s\n" % item)
    os.chdir('D://Scriptie//Data//lines//year')

