module MusiCompoNator.Examples.DeZevensprong where


import MusiCompoNator.Core
-- import MusiCompoNator.Composition

swing8 = id -- todo

rhythm = fromTime (Times 9 (4,4)) $
  bar1 :|: bar2 :|: bar3 :|: bar4 :|:
  bar5 :|: bar6 :|: bar7 :|: bar8 :|:
  (qns 2 <> wn)
  where
    bar1 = swing8 $ ens 4 <> qn <> ens 2
    bar2 = swing8 $ qn <> ens 2 <> qns 2
    bar3 = bar1
    bar4 = swing8 (qn <> ens 2) <> qn <> ens 2
    bar5 = dotted qn <> en <> dotted qn <> en
    bar6 = swing8 $ qns 3 <> ens 2
    bar7 = bar5
    bar8 = swing8 $ qns 2 <> ens 4

-- changes = (chord 1 [] :<: wn) <> (chord 5 [] :<: hn)
