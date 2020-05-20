{ name = "bertrand"
, dependencies = [ "console", "effect", "psci-support", "halogen", "random" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
