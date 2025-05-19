module Phonebook
  ( Name,
    PhoneNumber,
    Entry,
    mkEntry,
    name,
    phone,
    PhoneBook,
    names,
    phones,
    owner,
    Index (findEntry, empty, singleton, (<+>)),
    Assoc,
    byName,
    byPhone,
    emptyBook,
    addToBook,
    fromEntries,
    number,
    callerID,
    bill,
    bob,
    jeb,
    val,
    billbook,
    bobbook,
    jebbook,
    valbook,
    Lookup,
  )
where

import Data.Foldable
import Data.List (elemIndex, findIndex, intercalate)
import Data.Maybe

-- - 1. Entry
type Name = String

type PhoneNumber = [Int]

showPhone :: PhoneNumber -> String
showPhone = intercalate " " . map show

-- -- a. Complete the definitions of Entry, name and phone
data Entry
  = MkEntry Name PhoneNumber
  deriving (Eq, Show)

mkEntry :: Name -> PhoneNumber -> Entry
mkEntry = MkEntry

name :: Entry -> Name
name (MkEntry n _) = n

phone :: Entry -> PhoneNumber
phone (MkEntry _ p) = p

-- 2. Index

class Index i where
  findEntry :: (Eq k) => k -> i k -> Maybe Entry
  empty :: (Eq k) => i k
  singleton :: (Eq k) => k -> Entry -> i k
  (<+>) :: (Eq k) => i k -> i k -> i k

-- a. Complete the definition of Assoc
data Assoc k
  = MkAssoc [(k, Entry)]
  deriving (Eq, Show)

-- b. Complete the instance of Index for Assoc
instance Index Assoc where
  findEntry :: (Eq k) => k -> Assoc k -> Maybe Entry
  findEntry k (MkAssoc l) = case find (\(x, _) -> x == k) l of
    Nothing -> Nothing
    Just (_, entry) -> Just entry

  empty :: (Eq k) => Assoc k
  empty = MkAssoc []

  singleton :: (Eq k) => k -> Entry -> Assoc k
  singleton k e = MkAssoc [(k, e)]

  (<+>) :: (Eq k) => Assoc k -> Assoc k -> Assoc k
  (<+>) (MkAssoc l1) (MkAssoc l2) =
    MkAssoc
      ( l1
          ++ filter (\(k, _) -> not (any (\(k1, _) -> k1 == k) l1)) l2
      )

-- complete this instance

-- 3. Complete the definition of PhoneBook, names, phones and owner
data PhoneBook
  = MkPhoneBook
      Entry -- eigenaar
      (Assoc Name) -- index met namen
      (Assoc PhoneNumber) -- index met telefoonnummers

names :: PhoneBook -> Assoc Name
names (MkPhoneBook _ n _) = n

phones :: PhoneBook -> Assoc PhoneNumber
phones (MkPhoneBook _ _ p) = p

owner :: PhoneBook -> Entry
owner (MkPhoneBook o _ _) = o

-- 4. Implement byName and byPhone, emptyBook, addToBook, fromEntries

byName :: Name -> PhoneBook -> Maybe Entry
byName n (MkPhoneBook _ an _) = findEntry n an

byPhone :: PhoneNumber -> PhoneBook -> Maybe Entry
byPhone p (MkPhoneBook _ _ ap) = findEntry p ap

emptyBook :: Entry -> PhoneBook
emptyBook owner = MkPhoneBook owner (MkAssoc []) (MkAssoc [])

addToBook :: Entry -> PhoneBook -> PhoneBook
addToBook entry (MkPhoneBook owner aName aPhone) =
  let (MkEntry naam tel) = entry
   in MkPhoneBook owner (aName <+> singleton naam entry) (aPhone <+> singleton tel entry)

fromEntries :: Entry -> [Entry] -> PhoneBook
fromEntries owner entries = foldr addToBook (MkPhoneBook owner empty empty) entries

-- 5. Implement the callerID function.

data Telephone
  = MkTelephone PhoneNumber (PhoneNumber -> IO ())

number :: Telephone -> PhoneNumber
number (MkTelephone pn _) = pn

receive :: Telephone -> PhoneNumber -> IO ()
receive (MkTelephone _ r) = r

callerID :: PhoneBook -> Telephone
callerID (MkPhoneBook (MkEntry ownerName ownerPhone) _ aPhone) =
  MkTelephone
    ownerPhone
    ( \p -> do
        putStr "caller ID: "
        let eM = findEntry p aPhone
         in case eM of
              Nothing -> print p
              Just (MkEntry naam _) -> putStrLn naam
        putStrLn "Ring ring!"
    )

-- 6. Calling someone

call :: PhoneBook -> [Telephone] -> IO ()
call phonebook telefoonnummers = do
  putStrLn "Who would you like to call?"
  naam <- getLine
  case byName naam phonebook of
    Nothing -> do
      putStrLn "No such entry!"
      return ()
    Just entry ->
      case find (\(MkTelephone t _) -> t == phone entry) telefoonnummers of
        Nothing -> do
          putStrLn "The number you dialed does not exist."
          return ()
        Just tel -> receive tel (phone $ owner phonebook)

-- examples -- do NOT change

bill, bob, jeb, val :: Entry
bill = mkEntry "Bill" [32, 444, 123]
bob = mkEntry "Bob" [32, 444, 124]
jeb = mkEntry "Jebediah" [32, 444, 125]
val = mkEntry "Valentina" [32, 444, 126]

billbook, bobbook, jebbook, valbook :: PhoneBook
billbook = fromEntries bill [bob, jeb]
bobbook = fromEntries bob [bill, jeb]
jebbook = fromEntries jeb [bill, bob, val]
valbook = fromEntries val [bill, bob, jeb]

telephones :: [Telephone]
telephones = map callerID [billbook, bobbook, jebbook, valbook]

-- 7. Complete the Index instance for Lookup

data Lookup k = MkLookup (k -> Maybe Entry)

-- instance Index Lookup

-- complete this instance
