# Inventory

This is a utility that will give you a variety of statistics about your Haskell
project. These include:

- A list of type signatures that are shared among multiple functions,
  enumerating those functions along with their definition sites.
- Lists of the most used and least used definitions in the project.
- A breakdown of the local definitions, telling you the number of each type of
  definition as well as how many lines they encompass.

To use it, compile your project with these ghc options to generate the
necessary HIE files: `-fwrite-ide-info` and `-hiedir=.hie`. then execute
`inventory` from your project's root.

Here are some excerpts of the output that was produced by running `inventory`
on the `stack` codebase:

### Definition counts
![Definiton Counts](images/defcounts.png)

### Most used definitions
![Most Used](images/mostused.png)

### Matching type signatures
![Equivalent Signatures](images/dupesigs.png)

The output for matching signatures can be useful for discovering functions that
are duplicates of one another. For instance, the `packageIdent` and
`packageIdentifier` functions in the above output turned out to be duplicates.

### Known Issues/Limitations
- Context such as constraints and foralls are elided in the printed type
  signatures.
- Standalone kind signatures are not yet included in definition counts.
- Support for GHC 9.0 is forthcoming. Currently only 8.8 and 8.10 are
  supported.
- Does not unfold type synonyms when comparing type signatures.
