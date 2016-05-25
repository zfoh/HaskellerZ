#!/bin/bash

#
# Insert data for demo
#

#
# Insert for postgres-simple and Opaleye
#

psql -d postgres -c "
INSERT INTO todos (title, due_date, prio) VALUES
('Call parents', '2016-05-20', 11),                -- should insert as id = 1
('Grab plane', '2016-05-28', 20),
('Call boss', '2016-05-18', 11),
('Shop for food', '2016-05-25', null),
('Finish packing board', '2016-05-26', 16),
('Dont forget sunscreen', '2016-05-27', 13),
('Do laundry', '2016-05-17', null),
('Presentation at HaskellerZ', '2016-05-19', 25),
('Learn profunctors', '2016-05-15', 12),
('Learn Common Lisp', '2016-06-02', 20),
('Go surfing', '2016-06-03', null)
"

psql -d postgres -c "
INSERT INTO hashtags (todo_id, hashtag) VALUES
(1, '#good-son'),
(1, '#family'),
(2, '#vacation'),
(2, '#surf'),
(2, '#good-employee'),
(4, '#responsible'),
(7, '#responsible'),
(8, '#fun'),
(8, '#haskell'),
(8, '#programming'),
(10, '#programming')
"

#
# Insert for HRR
#

psql -d postgres -c "
INSERT INTO todo (title, due_date, prio) VALUES
('Call parents', '2016-05-20', 11),                -- should insert as id = 1
('Grab plane', '2016-05-28', 20),
('Call boss', '2016-05-18', 11),
('Shop for food', '2016-05-25', null),
('Finish packing board', '2016-05-26', 16),
('Dont forget sunscreen', '2016-05-27', 13),
('Do laundry', '2016-05-17', null),
('Presentation at HaskellerZ', '2016-05-19', 25),
('Learn profunctors', '2016-05-15', 12),
('Learn Common Lisp', '2016-06-02', 20),
('Go surfing', '2016-06-03', null)
"

psql -d postgres -c "
INSERT INTO hashtag (todo_id, hashtag_str) VALUES
(1, '#good-son'),
(1, '#family'),
(2, '#vacation'),
(2, '#surf'),
(2, '#good-employee'),
(4, '#responsible'),
(7, '#responsible'),
(8, '#fun'),
(8, '#haskell'),
(8, '#programming'),
(10, '#programming')
"
