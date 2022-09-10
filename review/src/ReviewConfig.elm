module ReviewConfig exposing (config)

import NoAlways
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDuplicatePorts
import NoEmptyText
import NoExposingEverything
import NoImportingEverything
import NoInvalidRGBValues
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoRecursiveUpdate
import NoRedundantConcat
import NoRedundantCons
import NoUnsafePorts
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import NoUselessSubscriptions
import Review.Rule exposing (Rule, ignoreErrorsForDirectories, ignoreErrorsForFiles)
import UseCamelCase



--noinspection ElmUnusedSymbol


config : List Rule
config =
    [ UseCamelCase.rule UseCamelCase.default
    , NoDuplicatePorts.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoUnusedPorts.rule
    , NoUnused.CustomTypeConstructors.rule [] |> ignoreErrorsForFiles [ "src/Model.elm" ]
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule |> ignoreErrorsForDirectories [ "tests/" ]
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoRedundantCons.rule
    , NoRedundantConcat.rule
    , NoMissingSubscriptionsCall.rule
    , NoRecursiveUpdate.rule
    , NoUselessSubscriptions.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoInvalidRGBValues.rule
    , NoEmptyText.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoBooleanCase.rule
    , NoAlways.rule
    , UseCamelCase.rule UseCamelCase.default
    ]
