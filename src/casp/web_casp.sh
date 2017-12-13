#!/bin/bash

unamestr=`uname`
if [[ "$OSTYPE" == "linux-gnu" ]]; then
cd server
php -S localhost:8000 &
firefox 'http://localhost:8000/index.html'
elif [[ "$OSTYPE" == "darwin"* ]]; then
php -S localhost:8000 &
open 'http://localhost:8000/server/index.html'
fi

