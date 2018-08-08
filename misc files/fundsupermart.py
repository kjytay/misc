# This script takes a list of unit trust fund codes and scrapes their
# prices from fundsupermart.com.

# list of fund codes. user to key in whichever fund codes they want to look up.
fund_codes = [
					("FSGBRE", "First State Glb Resources"),
					("370248", "United Asian Bond Fund Class SGD"),
					("UBGEMS", "United Emerging Markets Bond Fund"),
					("PHP009", "Phillip Spore Real Estate Income CL A SGD"),
					("PPMIPM", "Eastspring Investments MIP M")
				]

#################################

import re
import urllib2

url_prefix = "https://secure.fundsupermart.com/main/fundinfo/viewFund.svdo?sedolnumber="
DATE = "Can't find date"

for (code, fund_name) in fund_codes:
	# read in the URL
	url_full = url_prefix + code
	source_code = (urllib2.urlopen(url_full)).read()
	
	# find the amount. Here, the price is wedged in between a <td> and a
	# <small> tag
	price_pattern = re.compile(r'<td>(.*?)<small>\((.*?)\)</small>')
	price_result = price_pattern.search(source_code)
	if price_result is None:
		print "Can't find fund", code
	else:
		price = price_result.groups()[0]
		print fund_name + ": " + price
		DATE = price_result.groups()[1]

# get date
print
print "Date:", DATE
