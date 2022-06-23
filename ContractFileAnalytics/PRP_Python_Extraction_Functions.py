####
## 
## IRS OCPO ART
## Procurement Research Partnership (PRP) Contract
## Data and Analytic Solutions, Inc. (DAS) (https://dasconsultants.com/)
## Contract Officer Representative: David Gill
## Author and Sr. Data Scientist: John Zheng 
## Project Manager: Robert Carlson
## Contract Manager: Dawn Li
## Revised: 2022-06-21
##
####
##
## This Python library contains Natural Language Processing (NLP) functions for extracting interesting data from 
## procurement documents, such as Statements of Work (SOW), Performance Work Statements (PWS), Solicitations, Forms, and Contracts.
##
####
##
# # Function Index:
# #
# # load_text(fp=None, url=None)
# # remove_line_breaks(text)
# # remove_url(text):
# # extract_dates(text)
# # extract_title(text)
# # tokenize_and_stem(text, stopwords=None)
# # tokenize_only(text, stopwords=None)
# # extract_tfidf_keywords(    documents,    stopwords=None,    max_df=0.8,    max_features=2000,    min_df=0.04,    use_idf=True,    tokenizer=tokenize_and_stem,    ngram_range=(1, 3),)
# # extract_keywords(text, method="rake", **kwargs)
# # extract_keywords_info(text, method="rake", **kwargs)
# # extract_policy_keywords(text)
# # extract_all(text, **kwargs)
# # extract_piid(text: str) - Extract PIID from text
# # search_piid(path: str) -  Search PIID in the folder name, file name, and within the file
# # extract_sow(text: str)
# # search_files(directory: Path, ext: str)
# # find_authoritative_version(directory: Path, ext: str)
# # get_most_recently_modified(directory: Path, ext: str = "pdf")
# # extract_evaluation_factors(text)
# # contain_evaluation_factors(text: str)
# # extract_pdf_elements(fp, first_page=False)
# # get_page_bbox(elements)
# # extract_form_type(elements)


import logging
import re
import typing
from distutils.command.clean import clean
from operator import itemgetter
from pathlib import Path
from pkgutil import extend_path

import nltk
import numpy
import requests
from bs4 import BeautifulSoup
from gensim.summarization import keywords
from nltk.stem.snowball import SnowballStemmer
from pdfminer.high_level import extract_pages
from rake_nltk import Rake
from sklearn.feature_extraction.text import TfidfVectorizer

from .pdf_parser import Parser

logging.basicConfig(filename="text_extraction.log", filemode="w", level=logging.DEBUG)


def load_text(fp=None, url=None):
    parser = Parser()
    if fp is not None:
        text = parser.extract(fp, "pdfminer").decode("utf-8")
        return text
    if url is not None:
        page = requests.get(url)
        soup = BeautifulSoup.BeautifulSOAP(page.content)
        return soup


def remove_line_breaks(text):
    # remove line breaks
    # pattern = "(?<!\.)\n"
    # r = re.subn(pattern, " ", text)
    r = text.replace(".\n", "$$$").replace("\n", " ").replace("$$$", "\n")
    return r


def remove_url(text):
    pattern = r"(http(s)?:\/\/.)?(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=()]*)"
    return re.subn(pattern, "", text)[0]


def extract_dates(text):
    dates = re.findall("([0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{4})", text)
    if len(dates) > 0:
        return dates
    else:
        dates = re.findall(
            "((?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\.?\s+\d{1,2},?\s+\d{4})",
            text,
        )
        if len(dates) > 0:
            return dates


def extract_title(text):
    # pp = re.search("\n.*?(executive summary|background|introduction)", text, flags=re.IGNORECASE)
    tt = re.search(
        "^.*?(executive\ssummary|background|introduction|this\sis)",
        text,
        flags=re.IGNORECASE | re.MULTILINE,
    )
    date_pattern = "([0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{2,4}|(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\.?\s+\d{1,2},?\s+\d{4})"
    d = re.search(date_pattern, text)
    if tt is not None:
        if d is not None:
            start = min(tt.start(), d.start())
        else:
            start = tt.start()
        title = text[:start]
        title = re.sub("[\n\s]+", " ", title)
        return title


def tokenize_and_stem(text, stopwords=None):
    stemmer = SnowballStemmer("english")
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [
        word for sent in nltk.sent_tokenize(text) for word in nltk.word_tokenize(sent)
    ]
    filtered_tokens = []
    # filter out any tokens not containing letters (e.g., numeric tokens, raw punctuation)
    for token in tokens:
        if re.search("[a-zA-Z]", token):
            filtered_tokens.append(token)
    if stopwords:
        stems = [stemmer.stem(t) for t in filtered_tokens if t not in stopwords]
    else:
        stems = [stemmer.stem(t) for t in filtered_tokens]
    return stems


def tokenize_only(text, stopwords=None):
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [
        word.lower()
        for sent in nltk.sent_tokenize(text)
        for word in nltk.word_tokenize(sent)
    ]
    filtered_tokens = []
    # filter out any tokens not containing letters (e.g., numeric tokens, raw punctuation)
    for token in tokens:
        if re.search("[a-zA-Z]", token):
            filtered_tokens.append(token)
    if stopwords:
        r = [i for i in filtered_tokens if i not in stopwords]
    else:
        r = filtered_tokens
    return r


def extract_tfidf_keywords(
    documents,
    stopwords=None,
    max_df=0.8,
    max_features=2000,
    min_df=0.04,
    use_idf=True,
    tokenizer=tokenize_and_stem,
    ngram_range=(1, 3),
):

    # define vectorizer parameters
    tfidf_vectorizer = TfidfVectorizer(
        max_df=max_df,
        max_features=max_features,
        min_df=min_df,
        stop_words=stopwords,
        use_idf=use_idf,
        tokenizer=tokenizer,
        ngram_range=ngram_range,
    )

    tfidf_vectorizer.fit_transform(documents)
    terms = tfidf_vectorizer.get_feature_names()
    return terms


def extract_keywords(text, method="rake", **kwargs):
    if method == "rake":
        rake = Rake()
        rake.extract_keywords_from_text(text, **kwargs)
        return rake.get_ranked_phrases()
    if method == "textrank":
        return keywords(text, **kwargs)
    if method == "tf-idf":
        # stopwords = tokenize_and_stem(" ".join(RAKE.SmartStopList()))
        stopwords = nltk.corpus.stopwords.words("english")
        stopwords += ["shall", "pdf", "_x__"]
        stopwords = tokenize_and_stem(" ".join(stopwords))
        docs = list(filter(None, text.split("\n")))
        return extract_tfidf_keywords(docs, stopwords, **kwargs)


def extract_keywords_info(text, method="rake", **kwargs):
    keywords = extract_keywords(text, method=method, **kwargs)
    for i, kw in enumerate(keywords):
        if i < 10:
            print(kw)


def extract_policy_keywords(text):
    pattern = "digital"
    re.search(pattern, text, flags=re.IGNORECASE | re.MULTILINE)


def extract_all(text, **kwargs):
    r = dict()
    try:
        r["title"] = extract_title(text)
        r["dates"] = extract_dates(text)
        r["keywords"] = extract_keywords(text, **kwargs)
    except Exception as e:
        logging.error(e)
    return r


def extract_piid(text: str) -> list:
    """Extract PIID from text

    :param text: text to find extract PIID from
    :type text: str
    :return: list of found PIIDs
    :rtype: list
    """
    pp = r"\b[0-9A-Z-]+\b"
    matches = re.findall(pp, text)
    piid = [
        m
        for m in matches
        if not (
            m.replace("-", "").isdecimal()
            or m.replace("-", "").isalpha()
            or m.replace("-", "") == ""
            or len(m) < 10
        )
    ]
    return set(piid)


def search_piid(path: str) -> dict:
    """Search PIID in the folder name, file name, and within the file

    :param path:
    :type path: str
    :return: dictionary of found PIIDs in folder name, file name, and the file.
    :rtype: dict
    """
    r = dict()
    path = Path(path)
    text = load_text(path)
    text_piid = extract_piid(text)
    r["PIID within file"] = text_piid
    filename_piid = extract_piid(path.stem)
    r["PIID in filename"] = filename_piid
    foldername_piid = extract_piid(str(path.parents[0]))
    r["PIID in foldername"] = foldername_piid
    return r


def extract_sow(text: str) -> str:
    p = re.compile(
        "(statement\s+of\s+work|statement\s+objectives?|performance\s+work\s+statement)",
        flags=re.IGNORECASE | re.MULTILINE,
    )
    r = []
    for m in p.finditer(text):
        sow_text = text[m.start() :]
        lines = sow_text.split(".\n")
        r.append(" ".join(lines[:2]))
    return r


def search_files(directory: Path, ext: str) -> list:
    file_list = []
    for x in directory.iterdir():
        if x.is_file():
            if ext and ext.lower() in x.suffix.lower():
                file_list.append(x)
        else:
            file_list.append(search_files(directory / x))
    return file_list


def find_authoritative_version(directory: Path, ext: str) -> Path:
    files = search_files(directory, ext)
    for f in files:
        if "executed" in f.stem.lower():
            return f
        if "signed" in f.stem.lower():
            return f


def get_most_recently_modified(directory: Path, ext: str = "pdf") -> list:
    file_list = search_files(directory, ext)
    file_mod_time_list = [f.lstat().st_mtime for f in file_list]
    max_index = numpy.argwhere(file_mod_time_list == numpy.amax(file_mod_time_list))
    return itemgetter(*max_index.ravel().tolist())(file_list)


def extract_evaluation_factors(text):
    p = re.compile(
        "evaluation\s+(factor|criteria)",
        flags=re.IGNORECASE | re.MULTILINE,
    )
    r = []
    for m in p.finditer(text):
        extracted_text = text[m.end() :]
        lines = extracted_text.split(".\n")
        first_pass = " ".join(lines[:5])
        if re.search("evaluat(ion|ed|ing)", first_pass):
            r.append(first_pass)
    return r


def contain_evaluation_factors(text: str) -> bool:
    r = extract_evaluation_factors(text)
    if len(r) == 0:
        return False
    else:
        return True


def extract_pdf_elements(fp, first_page=False):
    for page_layout in extract_pages(fp):
        if first_page and page_layout.pageid > 1:
            break
        for element in page_layout:
            element.pageid = page_layout.pageid
            yield element


def get_page_bbox(elements):
    coords = numpy.array([element.bbox for element in elements])
    xmin = coords[:, 0].min()
    ymin = coords[:, 1].min()
    xmax = coords[:, 2].max()
    ymax = coords[:, 3].max()
    return xmin, ymin, xmax, ymax


def extract_digital_signature(elements):
    p = re.compile(r"digitally\s+signed\s+by", flags=re.IGNORECASE)
    signatures = []
    for element in elements:
        if p.search(str(element)):
            signatures.append(str(element))
    signers = []
    for signature in signatures:
        extracted_signer = re.findall(
            r"(?<=digitally\ssigned\sby\s)\w+",
            re.sub(r"\s+", " ", signature),
            flags=re.IGNORECASE,
        )
        signers += extracted_signer
    signers = [signer.capitalize() for signer in signers]
    signers = list(set(signers))
    return signers, signatures


def extract_form_type(elements):
    p = re.compile(r"\bF[Oo][Rr][Mm]\s+[0-9]{1,5}")
    form: str = None
    for element in elements:
        found_forms = p.findall(str(element))
        if len(found_forms) > 0 and (element.bbox[-1] < 200 or element.bbox[-1] > 700):
            cleaned_forms = [re.sub(r"\s+", r" ", i).upper() for i in found_forms]
            form = cleaned_forms[0]
            break
    return form
