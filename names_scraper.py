# -*- coding: utf-8 -*-
import time
import random
import json
import requests
import steam.webauth as wa

login = 'DeckeZip'
password = 'WeWillRockYou'
gameID = '440'

user = wa.WebAuth(login)
session = user.cli_login(password)

def get_items(currPos):
    r = session.get('https://steamcommunity.com/market/search/render/?start='+str(currPos)+'&count=100&search_descriptions=0&sort_column=default&sort_dir=desc&appid='+gameID+'&norender=1&count=5000')
    allItems = json.loads(r.text)['results']
    items = [item['hash_name'] for item in allItems]
    return items

def save_items(items):
    with open('./names.txt', 'a', encoding='utf-8') as f:
        for item in items:
            f.write("%s\n" % item)

def scrape_names():
    # 1 Step get all items count
    r = session.get('https://steamcommunity.com/market/search/render/?search_descriptions=0&sort_column=default&sort_dir=desc&appid='+gameID+'&norender=1&count=100')
    totalItems = json.loads(r.text)['total_count']
    print(totalItems)
    # 2 Step parse items
    for currPos in range(0,totalItems+50,50):
        time.sleep(random.uniform(0.5, 2.5))
        save_items(get_items(currPos))
        print("{}/{} items".format(currPos, totalItems))
    
def main():
    scrape_names()

if __name__ == '__main__':
    main()
