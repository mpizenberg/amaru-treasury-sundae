module Treasury.Scopes exposing (..)


type alias Scopes a =
    { ledger : a
    , consensus : a
    , mercenaries : a
    , marketing : a
    }


toList : Scopes a -> List a
toList { ledger, consensus, mercenaries, marketing } =
    [ ledger, consensus, mercenaries, marketing ]


toResult : Scopes (Result err a) -> Result err (Scopes a)
toResult { ledger, consensus, mercenaries, marketing } =
    Result.map4 Scopes ledger consensus mercenaries marketing


map : (a -> b) -> Scopes a -> Scopes b
map f { ledger, consensus, mercenaries, marketing } =
    { ledger = f ledger
    , consensus = f consensus
    , mercenaries = f mercenaries
    , marketing = f marketing
    }


map2 : (a -> b -> c) -> Scopes a -> Scopes b -> Scopes c
map2 f s1 s2 =
    { ledger = f s1.ledger s2.ledger
    , consensus = f s1.consensus s2.consensus
    , mercenaries = f s1.mercenaries s2.mercenaries
    , marketing = f s1.marketing s2.marketing
    }


map4 : (a -> b -> c -> d -> e) -> Scopes a -> Scopes b -> Scopes c -> Scopes d -> Scopes e
map4 f s1 s2 s3 s4 =
    { ledger = f s1.ledger s2.ledger s3.ledger s4.ledger
    , consensus = f s1.consensus s2.consensus s3.consensus s4.consensus
    , mercenaries = f s1.mercenaries s2.mercenaries s3.mercenaries s4.mercenaries
    , marketing = f s1.marketing s2.marketing s3.marketing s4.marketing
    }
