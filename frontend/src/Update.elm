module Update exposing (andThen)

{-| Helper module for update functions.
-}


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen f ( model, cmds ) =
    let
        ( newModel, newCmds ) =
            f model
    in
    ( newModel, Cmd.batch [ cmds, newCmds ] )
