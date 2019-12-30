{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff-coroutines"
    , "argonaut-codecs"
    , "console"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "psci-support"
    , "routing"
    , "web-socket"
    , "random"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
