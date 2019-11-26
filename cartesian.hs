module Test where

data AuthorName = AuthorName String String
type FirstName = String
type LastName = String
type MiddleName = String

data Creator = AuthorCreator Author | ArtistCreator Artist
data Author = Author Name
data Artist = Person Name | Band String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName | TwoInitialsWithLast Char Char LastName | FirstNameWithTwoInits FirstName Char Char


instance Show Creator where
    show (AuthorCreator (Author (Name f l))) = f ++ " " ++ l
    show (AuthorCreator (Author (NameWithMiddle f m l))) =
        f ++ " " ++ m ++ " " ++ l
    show (AuthorCreator (Author (TwoInitialsWithLast f m l))) =
        f : (" " ++ m : (" " ++ l))
    show (AuthorCreator (Author (FirstNameWithTwoInits f m l))) =
        f ++ " " ++ (show m) ++ " " ++ (show l)
    show (ArtistCreator (Person (Name f l))) = f ++ " " ++ l
    show (ArtistCreator (Person (NameWithMiddle f m l))) =
        f ++ " " ++ m ++ " " ++ l
    show (ArtistCreator (Person (TwoInitialsWithLast f m l))) =
        f : (" " ++ m : (" " ++ l))
    show (ArtistCreator (Person (FirstNameWithTwoInits f m l))) =
        f ++ " " ++ (show m) ++ " " ++ (show l)
    show (ArtistCreator (Band band)) = band

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))


data Book = Book {
        author :: Creator,
        isbn :: String,
        bookTitle :: String,
        bookYear :: String,
        bookPrice :: Double
}

data VinylRecord = VinylRecord {
        artist :: Creator,
        recordTitle :: String,
        recordYear :: Int,
        recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
        name :: String,
        description :: String,
        toyPrice :: Double
}

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy


price :: StoreItem -> Double
price (BookItem   book  ) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem    toy   ) = toyPrice toy

madeBy :: StoreItem -> String
madeBy (BookItem   book  ) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _                   = "unknown"
