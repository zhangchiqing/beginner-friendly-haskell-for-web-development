#!/bin/bash

# Referred to https://github.com/fpco/haskell-multi-docker-example/blob/master/entrypoint.sh

USER_ID=9001
APP_DIR=/opt/app

useradd --shell /bin/bash -u $USER_ID -o -c "" -m user
export HOME=/home/user

# set correct permissions on APP_DIR and subfolders
chown -R user. $APP_DIR

exec /sbin/pid1 -u user -g user "$@"

