#!/bin/sh

for i in $(seq 1 151); do
    wget "https://pokemoncries.com/cries-old/$i.mp3"
    sleep 10
done
