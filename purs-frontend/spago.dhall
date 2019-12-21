{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff-coroutines"
    , "console"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "psci-support"
    , "routing"
    , "web-socket"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
