#!/bin/bash

rm apps/smpp_router/ebin/*
rebar compile
cp -R apps/smpp_router/ebin rel/smpp_router/lib/smpp_router-1/
cp -R  apps/smpp_router/include rel/smpp_router/lib/smpp_router-1/
rm rel/smpp_router/mnesia/*
rel/smpp_router/bin/smpp_router console
