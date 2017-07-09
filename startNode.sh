#!/bin/bash

echo "*********** RUN NODEJS ***********"
cd ~/Research/JSDomTest/JS/
# /usr/local/bin/node geneticServer1.js 
/usr/local/Cellar/node@6/6.10.2/bin/node --max-old-space-size=4096 geneticServerES6.js

