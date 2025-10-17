#!/bin/bash

kubectl delete -f database.yaml
echo -n "rm -Rf pg-data/*:            "
rm -Rf pg-data/*
if [[ "$?" -eq 0 ]]; then
    echo "Success"
else
    echo "Fail"
fi
echo -n "chmod 777 pg-data directory: "
chmod 777 pg-data
if [[ "$?" -eq 0 ]]; then
    echo "Success"
else
    echo "Fail"
fi
