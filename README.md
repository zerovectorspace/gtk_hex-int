# Keynames to Hexidecimal, Decimal, or Binary

Convert letters and numbers to hexidecimal, decimal, or binary. Uses Haskell and Gtk with Glade layout files.

## Prerequisites

* [gtk3](http://hackage.haskell.org/package/gtk3) - REQUIRED
* [glade](https://glade.gnome.org/) - OPTIONAL - to edit the layout file

## Run

```
> git clone <this repository>
> cd gtk_hex-int
> ghc -dynamic src/main.hs -o out
> ./out

# Note: `-dynamic` is required when `hex` and `gtk3` are installed by the OS package manager.
```
or 
```
> git clone <this repository>
> cd gtk_hex-int
> stack init
> stack build
> stack run main-exe
```
