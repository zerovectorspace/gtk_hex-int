# Keynames to Hex or Int

Convert keynames to hexidecimal or integer. Uses Haskell and Gtk with Glade layout files.

## Prerequisites

* [gtk3](http://hackage.haskell.org/package/gtk3)
* [hex](http://hackage.haskell.org/package/hex)

## Run

```
> git clone <this repository>
> cd gtk_hex-int
> ghc -dynamic src/main.hs -o out
> ./out
```
Note: `-dynamic` is required when `hex` and `gtk3` are installed by the OS package manager.
