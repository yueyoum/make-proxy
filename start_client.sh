#!/bin/bash

erl -pa ./_build/default/lib/*/ebin +K true -config app -s make_proxy start_client -detached
