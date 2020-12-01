module Task3Message
where

-- ┌       ┐
-- │ X     │
-- │       │
-- │       │
-- └       ┘
-- seed: -592434091520407327
-- encoding: Ben

message :: String
--message = "de"
message = "d4:lastld4:datali0ei1e1:Xeee4:prevd4:lastld4:datali2ei2e1:Oeeeee"
            --"d4:lastld4:datali0ei1e1:Xeee4:prevd4:lastld4:datali2ei2e1:Oeeeee"
           --"d4:lastld4:datali0ei0e1:Xeeee"
-- ┌       ┐
-- │ X X X │
-- │ O     │
-- │   O   │
-- └       ┘


--ARR [0,1], [0,1], ['X', 'O'] 
--LIL [[(0, 'X')], [(2, 'O')]]
--COO [[(0, 1, 'O')], [(1, 0, 'X')]]

-- Raw message bellow:
-- d4:lastld4:datali0ei0e1:Xeee4:prevd4:lastld4:datali1ei2e1:Oeee4:prevd4:lastld4:datali2ei0e1:Xeee4:prevd4:lastld4:datali0ei1e1:Oeee4:prevd4:lastld4:datali1ei0e1:Xeeeeeeee