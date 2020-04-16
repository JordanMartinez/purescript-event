{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "event"
, dependencies =
  [ "console"
  , "effect"
  , "filterable"
  , "halogen-hooks"
  , "js-timers"
  , "now"
  , "nullable"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
