module IconHelpers

open Fable.React
open Fable.FontAwesome

// Centralized icon aliases for easier Font Awesome 7 migration.
// If newer Fable.FontAwesome exposes FaceGrin etc., map here; otherwise keep legacy Grin.

module Icons =
    // Using legacy FA5 identifiers exposed by Fable.FontAwesome 3.0
    let externalLink = Fa.Solid.ExternalLinkAlt
    let ellipsis = Fa.Solid.EllipsisH
    let grin = Fa.Regular.Grin
    let check = Fa.Solid.Check
    let users = Fa.Solid.Users
    let spinner = Fa.Solid.Spinner
    let exclamation = Fa.Solid.Exclamation
    let question = Fa.Solid.Question

// Helper builder
let inline icon i = Fa.i [ i ] []