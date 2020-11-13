#!/bin/sh

if [[ -f ./zoomhub.pid ]] ; then
  kill $(cat zoomhub.pid) > /dev/null
fi

# # See: http://apple.stackexchange.com/questions/3271/how-to-get-rid-of-firewall-accept-incoming-connections-dialog/121010

# # Find app binary:
# zoomhub=$(find .stack-work/dist -type f -name zoomhub | tr -d '\n')

# # Self-sign app to avoid constant Mac OS X firewall warnings:
# sudo codesign --force --sign - "$zoomhub"

AWS_ACCESS_KEY_ID='<TODO>' \
AWS_SECRET_ACCESS_KEY='<TODO>' \
BASE_URI='http://localhost:8000' \
CONTENT_BASE_URI='http://cache.zoomhub.net/content' \
PGDATABASE='zoomhub_development' \
PGUSER="$(whoami)" \
PROCESS_CONTENT="ProcessExistingAndNewContent" \
PROCESSING_WORKERS='2' \
S3_CACHE_BUCKET='cache-development.zoomhub.net' \
S3_SOURCES_BUCKET='sources-development.zoomhub.net' \
UPLOADS='true' \
  stack exec zoomhub | jq &

echo $! > zoomhub.pid
