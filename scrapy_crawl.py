import scrapy
import xml.etree.ElementTree as ET

class SitemapSpider(scrapy.Spider):
    name = "sitemap_spider"

    # Read URLs from the input file
    with open('haul.txt', 'r') as file:
        start_urls = [line.strip() for line in file.readlines()]

    def parse(self, response):
        # Parse the XML response
        try:
            tree = ET.ElementTree(ET.fromstring(response.body))
            # Extract URLs from <loc> tags
            for elem in tree.iter():
                if elem.tag.endswith('loc'):
                    link = elem.text
                    yield {'url': link}  # Yield the link
        except ET.ParseError as e:
            self.log(f"Failed to parse XML from {response.url}: {e}")

    def closed(self, reason):
        # Save all crawled links to the output file when the spider closes
        with open('crawled_links.txt', 'w') as out_file:
            for item in self.crawler.stats.get_value('item_scraped_count'):
                out_file.write(f"{item['url']}\n")
        self.log(f"All links have been saved to crawled_links.txt")

scrapy crawl sitemap_spider