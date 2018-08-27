import glob
import re
from nltk.corpus import wordnet as wn
from multiprocessing import Lock
import sys
import re
import argparse
import glob
reload(sys)
sys.setdefaultencoding("ISO-8859-1")

MAX_PROCESSES = 20
parser = argparse.ArgumentParser()
parser.add_argument('file_dir')
args = parser.parse_args()
files = glob.glob(args.file_dir+'*')

non_letter_re = re.compile(r'[^a-z\-]') # we split on anything that's not a letter
space_re = re.compile(r'\s+')

synonym_sets = dict() # massive dictionary of all synonym sets
antonym_sets = dict() # massive dictionary of all synonym sets

def print_sync(lock, data):
    lock.acquire()
    print data
    lock.release()

def find_synonyms(word):
    if word not in synonym_sets:
        synonyms_temp = set()
        syn_sets = wn.synsets(word)
        for syn_set in syn_sets:
            for synonym in syn_set.lemma_names():
                synonym = synonym.replace('_',' ').lower() # for multi-word
                if word not in synonym.split(): # ensure original word is not included
                    synonyms_temp.add(synonym)
        synonym_sets[word] = synonyms_temp
    return synonym_sets[word]

def find_antonyms(word):
    if word not in antonym_sets:
        antonyms_temp = set()
        for synset in wn.synsets(word):
            for lemma in synset.lemmas():
                for antonym in lemma.antonyms():
                    antonyms_temp.add(antonym.name().split('.')[0].lower())
        antonym_sets[word] = antonyms_temp
    return antonym_sets[word]

def calc_synonym_ratios(words):
    word_types = set(words)

    num_synonyms = 0
    total_synonyms = 0
    for word in words:
        # is this word an synonym of any other word in the document?
        num_synonyms+=int(len(find_synonyms(word) & word_types)>0) # 1 if found

        for word2 in words:
            # is this word in the set of antonyms of word2?
            total_synonyms += int(word in find_synonyms(word2)) # 1 if found

    if len(words) < 2 :
        ratio_1 = 0.0
        ratio_2 = 0.0
    else:
        ratio_1 = float(total_synonyms/2)/(len(words)*(len(words)-1)/2)
        ratio_2 = float(num_synonyms)/len(words)

    return ratio_1,ratio_2

def calc_antonym_ratios(words):
    word_types = set(words)

    num_antonyms = 0
    total_antomyms = 0
    for word in words:
        # is this word an antonym of any other word in the document?
        num_antonyms+=int(len(find_antonyms(word) & word_types)>0) # 1 if found

        for word2 in words:
            # is this word in the set of antonyms of word2?
            total_antomyms += int(word in find_antonyms(word2)) # 1 if found

    if len(words) < 2 :
        ratio_1 = 0.0
        ratio_2 = 0.0
    else:
        ratio_1 = float(total_antomyms/2)/(len(words)*(len(words)-1)/2)
        ratio_2 = float(num_antonyms)/len(words)

    return ratio_1,ratio_2

def process_files(files, print_lock):
    for file in files:
        in_file = open(file,'rb')
        line = in_file.read().lower()
        in_file.close()
        line = non_letter_re.sub(' ',line)
        line = space_re.sub(' ',line)

        words = []
        for word in line.split():
            word = word.strip()
            if len(word) > 0:
                words.append(word)

        synonyms_ratio1, synonyms_ratio2 = calc_synonym_ratios(words)
        antonyms_ratio1, antonyms_ratio2 = calc_antonym_ratios(words)

        antonyms_ratio1_over_synonyms_ratio1 = '0.0'
        if (synonyms_ratio1!=0):
            antonyms_ratio1_over_synonyms_ratio1 = str(antonyms_ratio1/synonyms_ratio1)

        antonyms_ratio2_over_synonyms_ratio2 = '0.0'
        if (synonyms_ratio2!=0):
            antonyms_ratio2_over_synonyms_ratio2 = str(antonyms_ratio2/synonyms_ratio2)


        print_sync(print_lock, file+'\t'+ str(antonyms_ratio1)+'\t'+str(antonyms_ratio2)+\
              '\t'+str(synonyms_ratio1)+'\t'+str(synonyms_ratio2)+\
              '\t'+antonyms_ratio1_over_synonyms_ratio1+'\t'+antonyms_ratio2_over_synonyms_ratio2)

files_list = []
print_lock = Lock()

for i in range(0,MAX_PROCESSES):
    files_list.append([])

for num,file_in in enumerate(files):
    files_list[num%MAX_PROCESSES].append(file_in)

for file_list in files_list:
    process_files(file_list,print_lock)