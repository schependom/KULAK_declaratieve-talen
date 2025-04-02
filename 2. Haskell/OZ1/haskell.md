# OZ 1

## Installatie

Installeer GHCup en voeg het toe aan `PATH`:

```bash
brew install ghcup      # universal installer
echo 'export PATH="$HOME/.ghcup/bin:$PATH"' > ~/.zshrc
```

Installeer de Haskell toolstack:

```bash
ghcup install ghc       # Glasgow Haskell Compiler
ghcup set ghc
ghcup install hls       # Haskell Language Server
ghcup install cabal     # Cabal = package manager
```

## Introductie

### GHCi directives

-   GHCi = Glasgow Haskell Compiler interactive
-   `:something` = GHCi _directive_

```haskell
-- load program
:l first_file.hs

-- reload last file
:r

-- find out type of an expression (function!)
:t expr
{-
    ghci> :t not True
    not True :: Bool
-}

-- find out more information about a type
:i type
{-
    ghci> :i []
    type List :: * -> *
    data List a = [] | a : [a]
            -- Defined in ‘GHC.Types’
    ...
-}

-- exit GHCi
:q

-- multiline
:{
```

### HLint

HLint geeft suggesties om codeerstijl te verbeteren in een gegeven file:

-   Overbodige haakjes,
-   Beter gebruik van ingebouwde functies,
-   $\eta$-reducties,
-   ...

Run dit in de terminal (buiten de GHCi environment):

```bash
hlint first_file.hs
# Found:
#  (x)
# Perhaps:
#   x
#
# 1 hint
```

## Oefeningen

### 1. List Operations

```bash
ghci> :l oef1.hs
# [1 of 2] Compiling Main             ( oef1.hs, interpreted )
# Ok, one module loaded.
ghci> count [1,2,3]
# 3

ghci> :r
# [1 of 2] Compiling Main             ( oef1.hs, interpreted ) [Source file changed]
# Ok, one module loaded.
ghci> myAnd [True, False]
# False
ghci> myAnd [True, True, True]
# True
ghci> myAnd []
# True

ghci> myOr [True, False]
# True
ghci> myOr [False, False, False, False]
# False
ghci> myOr []
# False
```

Enzovoort.
