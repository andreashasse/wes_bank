# wes bank
This is a small demo application for the [wes library](https://github.com/wooga/wes).
Each session can lock a set of bank accounts that make atomic actions to them.
So in wes terms a session is a channel and a account is a type of actor.
wes bank also exposes the available actions through a http api.

## Running
Clone and build this repo:

    git clone git@github.com:wooga/wes_bank.git
    cd wes_bank
    make

Start the app:

    make start

## HTTP api
Use [httpie](https://github.com/jkbr/httpie#installation) to be able to
copy paste these example commands.

Log in:

    http POST localhost:8080 User-Id:user1 -j

Insert money:

    http POST localhost:8080/a1/insert User-Id:user1 amount:=10 -j

Check balance:

    http GET localhost:8080/a1/balance User-Id:user1 -j

Transfer money between accounts:

    http POST localhost:8080/a1/transfer User-Id:user1 to=a2 amount:=10 -j

Check balance on other account:

    http GET localhost:8080/a2/balance User-Id:user1 -j

## Overview of modules & architecture.
