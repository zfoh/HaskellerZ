#!/bin/bash

#
# Make sure that you have a database called 'postgres' and a user with
# owner role named 'postgres'
#

## For postgres-simple and Opaleye

psql -d postgres -c "
create table if not exists todos (
    id serial primary key,
    title varchar(50) not null,
    due_date date not null,
    prio int
)"

psql -d postgres -c "
create table if not exists hashtags (
    hashtag varchar(50) not null,
    todo_id int not null,
    primary key (hashtag, todo_id)
)"

## For HRR

psql -d postgres -c "
create table if not exists todo (
    id serial primary key,
    title varchar(50) not null,
    due_date date not null,
    prio int
)"

psql -d postgres -c "
create table if not exists hashtag (
    hashtag_str varchar(50) not null,
    todo_id int not null,
    primary key (hashtag_str, todo_id)
)"
