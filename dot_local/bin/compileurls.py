#!/usr/bin/env python
## this script is used to convert json to newsboat url file

import json

inputfile = "/home/ethan/.dotfiles/homedots/.config/newsboat/urls.json"
outputfile = "/home/ethan/.config/newsboat/urls"

def write_with_tag(urls, tag, f):
    for url in urls:
        f.write(url + " \"" + tag + "\"\n" )

    f.write("\n")

def main():
    # get urls from json
    with open(inputfile, "r") as read_file:
        dic = json.load(read_file)
        read_file.close()

    with open(outputfile, "w") as write_file:
        for (tag,urls) in dic.items():
            write_with_tag(urls,tag,write_file)

main()
