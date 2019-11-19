# -*- coding: utf-8 -*-
import scrapy
from csv import writer
import urllib.request
import requests
import bs4
from bs4 import BeautifulSoup

from selenium import webdriver

# Create the Spider class

class mediumIPASpider(scrapy.Spider):

    name = 'medium_ipa'

    allowed_domains = ['medium.com']

    handle_httpstatus_list = [401, 400]

    autothrottle_enabled = True
    
    custom_settings={ 'FEED_URI': "medium_ipa.csv",
                       'FEED_FORMAT': "csv"}

    # start_requests method
    def start_requests(self):
        urls = ['https://medium.com/search?q=voice%20Assistant']
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse_list)

    # First parse method
    def parse_list(self, response):
        links = response.css('div.postArticle-content > a::attr(href)').extract()
        print(links)
        
        # Follow each of the extracted links
        for link in links:
            yield response.follow(url=link, callback=self.parse_article)

    # Second parsing method
    def parse_article(self, response):
        print('Processing:' + response.url)
        #print(response.text)
        article_url = response.url

        # Open in headless browser
        driver = webdriver.Chrome('/Users/tracykubert/Google Drive/05 Pratt/07_2019_Fall/INFO-640-01 Data Analysis/03 Final Project/_code/chromedriver')
        driver.get(response.url)
        # Load HTML
        html = driver.execute_script(
            "return document.documentElement.outerHTML")
        driver.quit()
        # Load into BS
        soup = BeautifulSoup(html, 'lxml')
        type(soup)
        # Get the article title
        article_title = soup.title.get_text()
        print(article_title)
        # Get article author, date (which has a href of article URL)
        article_meta = soup.select(
            'article span div a', attrs={'rel': 'noopener'})
        len(article_meta)
        # Pull out strings
        article_author_str = str(article_meta[0])
        article_date_str = str(article_meta[1])

        article_author = BeautifulSoup(
            article_author_str, "html.parser").get_text()
        article_date = BeautifulSoup(
            article_date_str, "html.parser").get_text()
        # Get the article body
        article = soup.article

        article_body = soup.article.get_text()
        with open('medium_ipa.csv', 'a', newline='') as csv_file:
            csv_writer = writer(csv_file)
            csv_writer.writerow(
                [article_title, article_url, article_date, article_author, article_body])