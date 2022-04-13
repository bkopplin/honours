Honours Project
===============

Eneo is a minimal [matrix](matrix.org) homeserver implemented in Erlang. The implementation focuses on scalability and fault-tolerance. 

Matrix is an open, decentralized and federated real-time communication standard, having the potential to gradually replace e-mail.

This Project was carried out in the context of the 2022 honours project at the University of Dundee.

The following features are currently implemented:
- getting a single event
- getting events for a room with basic filter support
- sending a message event to a room
- authenticating existing users using access tokens

# Setup
The following dependencies are required:
- Erlang/OTP (tested with version 24.2.1)
- rebar3

Use the following to compile and run eneo:

```
rebar3 compile
rebar3 run
```
# Tests
Eunit tests can be run with `ENEO_AUTOSTART=true rebar3 eunit`. Note that the environment varialbe `ENEO_AUTOSTART` needs to be set to false if tests were to be run with `rebar3 shell`.

# Docker
For testing purposes, the project contains dockerfiles in the folder test/cluster. A docker-compose file is provided that allows for local testing. It depends on the images eneo, eneo_lb and eneo_db, which can be found in test/cluster/images.
