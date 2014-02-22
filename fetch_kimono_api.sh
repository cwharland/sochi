#!/bin/bash
api=$(<api_key.ignore)
url="http://sochi.kimonolabs.com/api/"
params="?limit=9999999&apikey="$api
endpoints=("countries" "sports" "athletes")

for i in "${endpoints[@]}"; do
	echo "Fetching: "$i
	curl --include --request GET $url$i$params > "temp.txt"
	sed '1,12d' temp.txt > $i".txt"
done
rm temp.txt