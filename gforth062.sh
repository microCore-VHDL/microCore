#!/bin/bash
sudo sh -c 'docker run --rm -i --device=/dev/ttyUSB0:/dev/ttypUSB0 --user $(id -u):$(id -g) -t -v $PWD:/home/gforth microcore/gforth_062:latest'
