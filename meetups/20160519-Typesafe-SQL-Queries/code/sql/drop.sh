#!/bin/bash

#
# Drop all tables
#

psql -d postgres -c "DROP TABLE todos"
psql -d postgres -c "DROP TABLE todo"
psql -d postgres -c "DROP TABLE hashtags"
psql -d postgres -c "DROP TABLE hashtag"

