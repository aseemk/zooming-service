#!/bin/bash

set -eo pipefail

while IFS='' read -r LINE || [ -n "${LINE}" ]; do
  echo ''
  echo '------------------------------------------------------------------------'
  echo "Processing '${LINE}'..."
  echo '------------------------------------------------------------------------'

  rclone copy \
    zoomhub-rackspace-cloudfiles:cache/content/${LINE}.dzi \
    zoomhub-aws-s3:cache.zoomhub.net/content \
    --progress \
    --fast-list \
    --low-level-retries 8 \
    --retries 8 \
    --retries-sleep=2s \
    --s3-acl='public-read' # --dry-run
  rclone copy \
    zoomhub-rackspace-cloudfiles:cache/content/${LINE}_files \
    zoomhub-aws-s3:cache.zoomhub.net/content/${LINE}_files \
    --progress \
    --fast-list \
    --transfers=32 \
    --checkers=32 \
    --low-level-retries 8 \
    --retries 8 \
    --retries-sleep=2s \
    --s3-acl='public-read' # --dry-run

done
