# OZ 1

### Introductie

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
```

### HLint

HLint geeft suggesties om codeerstijl te verbeteren in een gegeven file:

-   Overbodige haakjes,
-   Beter gebruik van ingebouwde functies,
-   $\eta$-reducties,
-   ...

Run dit in de terminal (buiten de GHCi environment):

```bash
> hlint first_file.hs
-- Found:
--  (x)
-- Perhaps:
--   x
--
-- 1 hint
```

## Oefeningen

### 1. List Operations
