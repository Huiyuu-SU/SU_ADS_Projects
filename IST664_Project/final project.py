#!/usr/bin/env python
# coding: utf-8

# In[1]:

# Acquire data part


from __future__ import print_function
import argparse
import requests
import sys
from urllib import request
from urllib.error import HTTPError
from urllib.parse import quote
from lxml import etree
import time
import random
import ast
from urllib.parse import urlencode
from nltk.corpus import wordnet
import gensim
# create doc2vec vector columns
from gensim.test.utils import common_texts
from gensim.models.doc2vec import Doc2Vec, TaggedDocument
import json
import re
from pprint import pprint
from wordcloud import WordCloud
import matplotlib.pyplot as plt
import string
from nltk import pos_tag
from nltk.corpus import stopwords
from nltk.tokenize import WhitespaceTokenizer
from nltk.stem import WordNetLemmatizer


# https://www.yelp.com/developers/v3/manage_app
API_KEY = '6s4LDYFlyyOHSGoetDhNn9LHNR-fvOBUX6ktwVHFNYCx9YtY4FSW6WOSGo3tFtD56ilbErYmPmhN1BjPf71QorJyaQpjZ3GmOpttdd4oUmZnx9KrP4SSMIOCPazBXXYx'

# API constants, you shouldn't have to change these.
API_HOST = 'https://api.yelp.com'
SEARCH_PATH = '/v3/businesses/search'
BUSINESS_PATH = '/v3/businesses/'  # Business ID will come after slash.

# Defaults for our simple example.
DEFAULT_TERM = 'food'
DEFAULT_LOCATION = 'Syracuse,NY'
SEARCH_LIMIT = 50


def request(host, path, api_key, url_params=None):
    """Given your API_KEY, send a GET request to the API.
    Args:
        host (str): The domain host of the API.
        path (str): The path of the API after the domain.
        API_KEY (str): Your API Key.
        url_params (dict): An optional set of query parameters in the request.
    Returns:
        dict: The JSON response from the request.
    Raises:
        HTTPError: An error occurs from the HTTP request.
    """
    url_params = url_params or {}
    url = '{0}{1}'.format(host, quote(path.encode('utf8')))
    headers = {
        'Authorization': 'Bearer %s' % api_key,
    }

    print(u'Querying {0} ...'.format(url))

    response = requests.request('GET', url, headers=headers, params=url_params)

    return response.json()


def search(api_key, term, location):
    """Query the Search API by a search term and location.
    Args:
        term (str): The search term passed to the API.
        location (str): The search location passed to the API.
    Returns:
        dict: The JSON response from the request.
    """

    url_params = {
        'term': term.replace(' ', '+'),
        'location': location.replace(' ', '+'),
        'limit': SEARCH_LIMIT
    }
    return request(API_HOST, SEARCH_PATH, api_key, url_params=url_params)


def get_business(api_key, business_id):
    """Query the Business API by a business ID.
    Args:
        business_id (str): The ID of the business to query.
    Returns:
        dict: The JSON response from the request.
    """
    business_path = BUSINESS_PATH + business_id + '/reviews'

    return request(API_HOST, business_path, api_key)


def query_api(term, location):
    """Queries the API by the input values from the user.
    Args:
        term (str): The search term to query.
        location (str): The location of the business to query.
    """
    response = search(API_KEY, term, location)

    businesses = response.get('businesses')

    if not businesses:
        print(u'No businesses for {0} in {1} found.'.format(term, location))
        return

    #write restaurant into json file
    #then query the review
    for business in businesses:
        write_json(business)
        print(business)



def get_review(businessid,business_name):
    response = get_business(API_KEY, businessid)
    reviews = response.get('reviews')
    for review in reviews:
        review['businessid'] = businessid
        review['restaurant_name'] = business_name
        write_review(review)


def write_json(business):
    with open('Restaurant.json', 'a') as f:
        f.write(json.dumps(business,ensure_ascii=False)+"\n")

def write_json2(review_list):
    with open('Reviews.json', 'a') as f:
        for item in review_list:
            f.write(json.dumps(item, ensure_ascii=False) + "\n")


def write_review(review):
    with open('Reviews.json', 'a') as f:
        f.write(json.dumps(review,ensure_ascii=False)+",\n")


def get_reviews(url,name):
    header = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/62.0.3202.94 Safari/537.36'}
    req = requests.get(url, headers=header)
    page = req.text
    pattern = re.compile(r'Page 1 of \d+')
    result1 = pattern.findall(page)
    page_num = int(result1[0].split()[3])
    html = etree.HTML(page)
    # page_info = html.xpath("//div[@class='lemon--div__373c0__1mboc u-padding-b2 border-color--default__373c0__2oFDT text-align--center__373c0__1l506']")
    # page_num = int(page_info[0].xpath('string(.)').split()[3])

    reviews_list = []
    print("base:url:"+url)

    current_page = 0
    base_url = url
    for i in range(0,page_num):
        current_url = base_url +'&start='+ str(current_page)
        print("process url:" + current_url)
        stop_time = random.randint(1,5)
        time.sleep(stop_time)
        tmp_list = parse_morereview(current_url,name,header)
        write_json2(tmp_list)
        current_page = current_page + 20



def parse_morereview(url,name,header):
    req = requests.get(url, headers=header)
    page = req.content
    html = etree.HTML(page)

    review_list = []
    reviews = html.xpath("//p[@class='lemon--p__373c0__3Qnnj text__373c0__2pB8f comment__373c0__3EKjH text-color--normal__373c0__K_MKN text-align--left__373c0__2pnx_']//span[@class='lemon--span__373c0__3997G']")

    if len(reviews) == 0:
        print("been blocked, please click the website to correct it")
        time.sleep(60)
        parse_morereview(url,name,header)


    ratings = html.xpath("//section[@class='lemon--section__373c0__fNwDM u-space-t4 u-padding-t4 border--top__373c0__19Owr border-color--default__373c0__2oFDT']//div[@class='lemon--div__373c0__1mboc arrange-unit__373c0__1piwO border-color--default__373c0__2oFDT']//span//div//@aria-label")
    current_rating = 0

    for item in reviews:
        review = item.xpath('string(.)')
        rating  = str(ratings[current_rating])
        # put all the review into list
        review_info = {}
        review_info['name'] = name
        review_info['review'] = review
        review_info['rating'] = rating
        review_list.append(review_info)
        current_rating += 1

    return review_list


def main():
    parser = argparse.ArgumentParser()

    parser.add_argument('-q', '--term', dest='term', default=DEFAULT_TERM,
                        type=str, help='Search term (default: %(default)s)')
    parser.add_argument('-l', '--location', dest='location',
                        default=DEFAULT_LOCATION, type=str,
                        help='Search location (default: %(default)s)')

    input_values = parser.parse_args()

    try:
        query_api(input_values.term, input_values.location)
    except HTTPError as error:
        sys.exit(
            'Encountered HTTP error {0} on {1}:\n {2}\nAbort program.'.format(
                error.code,
                error.url,
                error.read(),
            )
        )

#Get Resturant list and review
main()
result = []

#process data
with open('Restaurant.json') as f:
    for line in f.readlines():
        tmp = json.loads(line)
        result.append(tmp)

    for item in result:
        item_url = item['url']
        item_name = item['name']
        get_reviews(item_url,item_name)


#load the file and change json file into dict
def load_file(filename):
    result = []
    with open(filename) as f:
        for line in f.readlines():
            tmp = json.loads(line)
            result.append(tmp)
    return result

def get_category(restaurant_list):
    category_dict = {}
    for item in restaurant_list:
        types = item['categories']
        for type in types:
            tmp = type['title']
            if tmp in category_dict:
                category_dict[tmp] += 1
            else:
                category_dict.setdefault(tmp,1)
    list = sorted(category_dict.items(), key=lambda d: d[1],reverse=True)

    name_list = []
    value_list = []
    for i in range(10):
        tmp = list[i]
        name_list.append(tmp[0])
        value_list.append(tmp[1])
    plt.barh(range(len(value_list)), value_list, tick_label=name_list)
    plt.show()


#extract the data from file
def get_review(reviews_list,restaurant_name):
    reviews = []
    for item in reviews_list:
        if(item['name'] == restaurant_name):
            tmp = item['review']
            reviews.append(tmp)
    return reviews

#process data
    restaurant_list = load_file('Restaurant.json')
    reviews_list = load_file('Reviews.json')
    get_category(restaurant_list)








# In[2]:


review = []
with open('Reviews 2.json') as json_data:
    for line in json_data.readlines():
        review.append(line)


# In[3]:


review[1]


# In[4]:


name = []
text = []
rating = []
for i in range(0, len(review)):
    re.findall(r"\"name(.*?)review", review[i])
    name.append(str(re.findall(r"\"name(.*?)review", review[i])[0]).replace('\"',' ').replace(':',' ').                replace(',',' ').replace('\"',' ').replace('  ',' ').strip())
    text.append(str(re.findall(r"\"review(.*?)rating",review[i])[0]).replace('\"',' ').replace(':',' ').                replace(',',' ').replace('}',' ').replace('  ',' ').strip())
    rating.append(str(re.findall(r"\"rating(.*?)star",review[i])[0]).replace('\"',' ').replace(':',' ').                replace(',',' ').replace('}',' ').replace('  ',' ').strip())


# In[5]:


import pandas as pd 
df = pd.DataFrame(list(zip(name, text, rating)), 
               columns =['name', 'review', 'rating']) 
df.head()


# In[6]:


df['review_type'] = df['rating']
for i in range(0,len(df)):
    if float(df['rating'][i]) >= 4:
        df['review_type'][i] = "0"
    elif float(df['rating'][i]) > 2 and float(df['rating'][i]) < 4:
        df['review_type'][i] = "1"
    else:
        df['review_type'][i] = "2"


df['is_bad_review'] = df['rating']
for i in range(0,len(df)):
    if float(df['rating'][i]) > 2:
        df['is_bad_review'][i] = "0"
    else:
        df['is_bad_review'][i] = "1"
df.head()


# In[7]:


df_new = df.drop("rating", axis = 1)
# .drop("name", axis = 1)
df_new


# In[8]:


import nltk
from nltk.sentiment.vader import SentimentIntensityAnalyzer
# nltk.download()


# In[9]:


reviews_df = df_new.copy()


# In[10]:


# # remove 'No Negative' or 'No Positive' from text
# reviews_df["review"] = reviews_df["review"].apply(lambda x: x.replace("No Negative", "").replace("No Positive", ""))


# In[11]:


# return the wordnet object value corresponding to the POS tag

def get_wordnet_pos(pos_tag):
    if pos_tag.startswith('J'):
        return wordnet.ADJ
    elif pos_tag.startswith('V'):
        return wordnet.VERB
    elif pos_tag.startswith('N'):
        return wordnet.NOUN
    elif pos_tag.startswith('R'):
        return wordnet.ADV
    else:
        return wordnet.NOUN


def clean_text(text):
    # lower text
    text = text.lower()
    # tokenize text and remove puncutation
    text = [word.strip(string.punctuation) for word in text.split(" ")]
    # remove words that contain numbers
    text = [word for word in text if not any(c.isdigit() for c in word)]
    # remove stop words
    stop = stopwords.words('english')
    text = [x for x in text if x not in stop]
    # remove empty tokens
    text = [t for t in text if len(t) > 0]
    # pos tag text
    pos_tags = pos_tag(text)
    # lemmatize text
    text = [WordNetLemmatizer().lemmatize(t[0], get_wordnet_pos(t[1])) for t in pos_tags]
    # remove words with only one letter
    text = [t for t in text if len(t) > 1]
    # join all
    text = " ".join(text)
    return(text)

# clean text data
reviews_df["review_clean"] = reviews_df["review"].apply(lambda x: clean_text(x))


# In[12]:


reviews_df.head()


# In[88]:


reviews_df['review'][5]


# In[ ]:


reviews_df['review'}.head()


# In[13]:


from nltk.sentiment.vader import SentimentIntensityAnalyzer

sid = SentimentIntensityAnalyzer()
reviews_df["sentiments"] = reviews_df["review"].apply(lambda x: sid.polarity_scores(x))
reviews_df = pd.concat([reviews_df.drop(['sentiments'], axis=1), reviews_df['sentiments'].apply(pd.Series)], axis=1)


# In[14]:


reviews_df.head()


# In[15]:


# add number of characters column
reviews_df["nb_chars"] = reviews_df["review"].apply(lambda x: len(x))

# add number of words column
reviews_df["nb_words"] = reviews_df["review"].apply(lambda x: len(x.split(" ")))


# In[16]:


reviews_df.head()


# In[17]:


a = reviews_df.groupby(
   ['name']
).agg(
    {
        'compound':sum,    
        'review': "count",  
    }
)
a['average'] = a['compound'] / a['review']
b = a.loc[a['review'] >= 30]
b.sort_values(by=['average'], ascending=False).head(5)


# In[18]:


b.sort_values(by=['average'], ascending= True).head(5)


# In[19]:




documents = [TaggedDocument(doc, [i]) for i, doc in enumerate(reviews_df["review_clean"].                                                              apply(lambda x: x.split(" ")))]

# train a Doc2Vec model with our text data
model = Doc2Vec(documents, vector_size=5, window=2, min_count=1, workers=4)

# transform each document into a vector data
doc2vec_df = reviews_df["review_clean"].apply(lambda x: model.infer_vector(x.split(" "))).apply(pd.Series)
doc2vec_df.columns = ["doc2vec_vector_" + str(x) for x in doc2vec_df.columns]
reviews_df = pd.concat([reviews_df, doc2vec_df], axis=1)

reviews_df.head()


# In[20]:


# add tf-idfs columns
from sklearn.feature_extraction.text import TfidfVectorizer
tfidf = TfidfVectorizer(min_df = 10)
tfidf_result = tfidf.fit_transform(reviews_df["review_clean"]).toarray()
tfidf_df = pd.DataFrame(tfidf_result, columns = tfidf.get_feature_names())
tfidf_df.columns = ["word_" + str(x) for x in tfidf_df.columns]
tfidf_df.index = reviews_df.index
reviews_df = pd.concat([reviews_df, tfidf_df], axis=1)


# In[21]:


reviews_df.head()


# In[22]:


## Exploratory data analysis


# In[23]:


len(reviews_df["is_bad_review"])


# In[24]:


# show review_type distribution
reviews_df["review_type"].value_counts(normalize = True)


# In[71]:


# highest positive sentiment reviews (with more than 5 words)
most_pos = reviews_df[reviews_df["nb_words"] >= 5].sort_values("pos", ascending = False)[["review", "review_clean", "pos"]]

most_pos.head(10)


# In[72]:


# lowest negative sentiment reviews (with more than 5 words)
most_neg = reviews_df[reviews_df["nb_words"] >= 10].sort_values("neg", ascending = False)[["review", "review_clean", "neg"]]
most_neg.head(10)


# In[73]:


# wordcloud function
import wordcloud



def show_wordcloud(data, title = None):
    wordcloud = WordCloud(
        background_color = 'white',
        max_words = 200,
        max_font_size = 40, 
        scale = 3,
        random_state = 42
    ).generate(str(data))

    fig = plt.figure(1, figsize = (20, 20))
    plt.axis('off')
    if title: 
        fig.suptitle(title, fontsize = 20)
        fig.subplots_adjust(top = 2.3)

    plt.imshow(wordcloud)
    plt.show()


# In[74]:


most_pos["review_clean"].head(20)


# In[75]:


most_neg["review_clean"].head(20)


# In[76]:


show_wordcloud(most_pos["review_clean"].head(20))


# In[77]:


show_wordcloud(most_neg["review_clean"].head(20))


# In[32]:


# plot sentiment distribution for positive and negative reviews

import seaborn as sns

for x in [0, 1, 2]:
    subset = reviews_df[reviews_df['review_type'] == str(x)]
    
    # Draw the density plot
    if x == 0:
        label = "Good reviews"
    elif x == 1:
        label = "Neutral reviews"
    else:
        label = "Bad reviews"
    
    sns.distplot(subset['compound'], hist = False, label = label)


# In[33]:


# "What is compund"


# In[34]:


# feature selection
label = "is_bad_review"
ignore_cols = [label, "review", "review_clean","name","review_type"]
features = [c for c in reviews_df.columns if c not in ignore_cols]

# split the data into train and test
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(reviews_df[features], reviews_df[label],                                                    test_size = 0.20, random_state = 42)


# In[115]:


# train a random forest classifier
rf = RandomForestClassifier(n_estimators = 100, random_state = 42)
rf.fit(X_train, y_train)

# show feature importance
feature_importances_df = pd.DataFrame({"feature": features, "importance": rf.feature_importances_}).sort_values("importance", ascending = False)
feature_importances_df.head(20)


# In[37]:


# ROC curve

from sklearn.metrics import roc_curve, auc, roc_auc_score
import matplotlib.pyplot as plt

y_pred = [x[1] for x in rf.predict_proba(X_test)]
fpr, tpr, thresholds = roc_curve(y_test, y_pred, pos_label = '1')

roc_auc = auc(fpr, tpr)

plt.figure(1, figsize = (15, 10))
lw = 2
plt.plot(fpr, tpr, color='darkorange',
         lw=lw, label='ROC curve (area = %0.2f)' % roc_auc)
plt.plot([0, 1], [0, 1], lw=lw, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.0])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver operating characteristic example')
plt.legend(loc="lower right")
plt.show()


# In[117]:


y_test


# In[118]:


# PR curve

from sklearn.metrics import average_precision_score, precision_recall_curve
from sklearn.utils.fixes import signature

average_precision = average_precision_score(y_test, y_pred, pos_label="1")

precision, recall, _ = precision_recall_curve(y_test, y_pred, pos_label="1")

# In matplotlib < 1.5, plt.fill_between does not have a 'step' argument
step_kwargs = ({'step': 'post'}
               if 'step' in signature(plt.fill_between).parameters
               else {})

plt.figure(1, figsize = (15, 10))
plt.step(recall, precision, color='b', alpha=0.2,
         where='post')
plt.fill_between(recall, precision, alpha=0.2, color='b', **step_kwargs)

plt.xlabel('Recall')
plt.ylabel('Precision')
plt.ylim([0.0, 1.05])
plt.xlim([0.0, 1.0])
plt.title('2-class Precision-Recall curve: AP={0:0.2f}'.format(average_precision))


# In[81]:


# numbers 
from sklearn.metrics import accuracy_score

accuracy_score(y_test, y_pred_class)


# In[90]:


len(y_test.loc[y_test == "0"])/len(y_test)


# In[120]:


y_pred
pred = pd.DataFrame(list(zip(y_pred_class, y_test)), 
               columns =['pred', 'test']) 


# In[78]:


y_pred_class = rf.predict(X_test)

pred = pd.DataFrame(list(zip(y_pred_class, y_test)), 
               columns =['pred', 'test']) 
pred.loc[pred.test == '0'].tail(30)

