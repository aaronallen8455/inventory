# Inventory

This is a utility that provides a variety of statistics about your Haskell
project. These include:

- A list of type signatures that are shared among multiple locally defined
  functions, enumerating those functions along with their definition sites.
- Lists of the most used and least used definitions in the project.
- A breakdown of local definitions, telling you the number of each type of
  definition as well as how many lines of code they take up.

## Using inventory

Install with `stack update && stack install inventory` or `cabal update &&
cabal install inventory`. The version used to compile `inventory` must be the
same as the version used to compile your project. For stack users, this means
you may have to `stack install` from within your project to use the right GHC.

Inventory uses `.hie` files to gather information about all haskell files in
the project. Once you have generated `.hie` files for your project, execute
`inventory` from your project's root.

## How to generate `.hie` files
### Cabal

Add this to your `cabal.project.local` file:

```
package *
  ghc-options: -fwrite-ide-info -hiedir=.hie
```

Then do a full rebuild of the project:

```
cabal clean
cabal build all
```

### Stack

Add this to your `stack.yaml` file:

```
ghc-options:
  "$locals": -fwrite-ide-info
             -hiedir=.hie
```

Then do a full rebuild:

```
stack clean
stack build
```

By default `inventory` looks for HIE files in the `.hie` directory. You can
override this using the `HIE_DIR` environment variable: `HIE_DIR=path/to/dir
inventory`.

## Examples

Here are some excerpts of the output that was produced by running `inventory`
on the `stack` codebase:

### Definition counts
![Definiton counts image](images/defcounts.png)

### Most used definitions
![Most used image](images/mostused.png)

### Matching type signatures
![Equivalent signatures image](images/dupesigs.png)

The output for matching signatures can be useful for discovering functions that
are duplicates of one another. For instance, the `packageIdent` and
`packageIdentifier` functions in the above output turned out to be duplicates.

### Known Issues/Limitations
- Context such as constraints and foralls do not appear in the printed type
  signatures for GHC versions less than 9.0.1.
- Standalone kind signatures are not yet included in definition counts.
- Does not unfold type synonyms when comparing type signatures.
- GHC versions other than 8.8, 8.10, and 9.0 are not currently supported.
