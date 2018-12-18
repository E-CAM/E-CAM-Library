#!/bin/bash
grep -A 1 --no-group-separator Lambda md.log | grep -v Step | awk '{print $1}' > mdlogging1
grep -A 1 --no-group-separator Temp md.log | grep -v Temp | awk '{print $2}' > mdlogging2
paste mdlogging1 mdlogging2 
paste mdlogging1 mdlogging2 >temperature
rm mdlogging1 mdlogging2 
