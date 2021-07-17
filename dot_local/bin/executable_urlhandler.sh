#!/bin/bash

# inspired by this script: https://github.com/gotbletu/shownotes/blob/master/urlportal.sh

VIDEO="mpv --really-quiet"
AUDIO="$TERMINAL --class audio -e mpv --no-video"
IMAGE="feh --quiet"
SVG="feh --quiet --conversion-timeout 1"

SVGFILE="/tmp/svg.svg"
PDFFILE="/tmp/pdf.pdf"

url="$1"
case "$url" in
    *.jpg|*.jpeg|*.png)
	$IMAGE "$url" &
	;;
    *.svg)
	curl -s "$url" > "$SVGFILE"
    $SVG "$SVGFILE" &
	;;
    *.pdf)
	curl -s "$url" > "$PDFFILE"
	zathura "$PDFFILE" &
	;;
    *.mp3|*.m4a|*.wav|*.ogg|*.oga|*.flac)
	$AUDIO "$url" &
	;;
    *.mp4|*.gif|*.mkv|*.avi|*.wmv|*.m4v|*.mpg|*.mpeg|*.flv|*.ogm|*.ogv|*.gifv)
	$VIDEO "$url" &
	;;
    *youtube.com/watch*|*youtu.be/*|*clips.twitch.tv/*)
	$VIDEO "$url" &
        ;;
    *)
	$BROWSER "$url" &

esac

