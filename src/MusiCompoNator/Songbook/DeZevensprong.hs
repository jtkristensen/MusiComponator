module MusiCompoNator.Examples.DeZevensprong where


import MusiCompoNator.Core
-- import MusiCompoNator.Composition

swing8 = id -- todo

-- rhythm = withSignature (Times 18 (4,4)) $ Repeat 2 $
--   swing8 (bar1 :|: bar2 :|: bar3) :|: bar4 :|: bar5 :|:
--   swing8 bar6 :|: bar7 :|: swing8 bar8 :|: bar9
--   where
--     bar1 = ens 4 <> qns 1 <> ens 2
--     bar2 = qns 1 <> ens 2 <> qns 2
--     bar3 = bar1
--     bar4 = swing8 (qns 1 <> ens 2) <> qns 1 <> ens 2
--     bar5 = dotted (qns 1) <> ens 1 <> dotted (qns 1) <> ens 1
--     bar6 = qns 3 <> ens 2
--     bar7 = bar5
--     bar8 = qns 2 <> ens 4
--     bar9 = qns 2 <> hns 1

-- changes = (chord 1 [] :<: wn) <> (chord 5 [] :<: hn)
