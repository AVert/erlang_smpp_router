#!/bin/bash

rm apps/smpp_router/ebin/*
rebar compile
cd rel
rm -rf smpp_router
rebar generate
smpp_router/bin/smpp_router console
