# Inventory

This is a utility that will give you various statistics about your haskell
project. These include:

- A list of type signatures that are shared among multiple functions with the
  definiton site of each one
- Lists of the most used and least used definitions in the project.
- A breakdown of the local definitions, telling you the number of each type of
  definition as well as how many lines they encompass.

To use it, compile your project with these ghc options to generate the
necessary HIE files: `-fwrite-ide-info` and `-hiedir=.hie`. then execute
`inventory` from your project's root.
