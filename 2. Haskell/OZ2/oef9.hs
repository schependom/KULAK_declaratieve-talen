{-
    9. HTML (EXTRA)
-}

data Attr = Attr String String
  deriving (Eq, Show)

data HtmlElement
  = HtmlString String
  | HtmlTag String [Attr] HtmlElements
  deriving (Eq, Show)

-- alias
type HtmlElements = [HtmlElement]

example :: HtmlElement
example = HtmlTag "a" [Attr "href" "https://www.kuleuven.be/kuleuven/"] [HtmlString "KU Leuven"]

-- type class
class HTML a where
  toHtml :: a -> HtmlElement

data Link
  = Link
      String -- Link target.
      String -- Text to show.

instance HTML Link where
  toHtml :: Link -> HtmlElement
  toHtml (Link l t) =
    HtmlTag "a" [Attr "href" l] [HtmlString l]

unorderedList :: HtmlElement
unorderedList = HtmlTag "ul" [] [HtmlTag "li" [] [HtmlString "Apples", HtmlString "Bananas", HtmlString "Oranges"]]

instance (Show a) => HTML ([] a) where
  toHtml :: [] a -> HtmlElement
  toHtml l = HtmlTag "ul" [] (map (\t -> HtmlTag "li" [] [HtmlString (show t)]) l)

data AddressBook
  = AddressBook [Contact]

data Contact
  = Contact
      Name -- name
      [EmailAddress] -- email addressses

data Name
  = Name
      String
      String -- first and last name

data EmailAddress
  = PrivateMail String
  | WorkMail String

exampleAddressBook :: AddressBook
exampleAddressBook = AddressBook [Contact (Name "Vincent" "Van Schependom") [PrivateMail "vincent.van.schependom@gmail.com", WorkMail "vincent@nuuf.be"]]

instance HTML AddressBook where
  toHtml :: AddressBook -> HtmlElement
  toHtml (AddressBook contacts) =
    HtmlTag "addressbook" [] (map toHtml contacts)

instance HTML Contact where
  toHtml :: Contact -> HtmlElement
  toHtml (Contact name emails) =
    HtmlTag "contact" [] [toHtml name, HtmlTag "emails" [] (map toHtml emails)]

instance HTML EmailAddress where
  toHtml :: EmailAddress -> HtmlElement
  toHtml (PrivateMail email) =
    HtmlTag "Private mail: " [] [HtmlString email]
  toHtml (WorkMail email) =
    HtmlTag "Work mail: " [] [HtmlString email]

instance HTML Name where
  toHtml :: Name -> HtmlElement
  toHtml (Name first last) =
    HtmlTag "name" [] [HtmlString (first ++ " " ++ last)]