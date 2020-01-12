{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff-coroutines"
    , "argonaut-codecs"
    , "bigints"
    , "console"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "psci-support"
    , "random"
    , "routing"
    , "web-socket"
    , "b64"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
