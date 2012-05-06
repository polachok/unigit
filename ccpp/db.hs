import Data.List
import Text.Printf
import System.IO

data Field = Artist String | Album String | Title String | Year Int | Plays Int
	deriving (Eq,Ord)

data Record = Record { artist :: Field,
		       album  :: Field,
		       title  :: Field,
		       year   :: Field,
		       plays  :: Field 
 		     }
-- XXX: show abuse
instance Show Field where
	show (Artist s) = s
	show (Album s) = s
	show (Title s) = s
	show (Year i) = show i
	show (Plays i) = show i

instance Show Record where
	show (Record a b c d e) = foldl (\a x -> a ++ (show x) ++ "\n") [] [a,b,c,d,e]

fields = [("Artist", artist), ("Album", album), ("Title", title),
	  ("Year", year),     ("Plays", plays)]

-- for testing
a = Record (Artist "Skullflower") (Album "Ruins") (Title "Eat the stars") (Year 1990) (Plays 1)
b = Record (Artist "Boris") (Album "Heavy Rocks") (Title "Koei") (Year 2002) (Plays 2)

cmpconv (Artist a) cs = compare a cs
cmpconv (Album a) cs  = compare a cs
cmpconv (Title a) cs  = compare a cs
cmpconv (Year a) cs   = compare a (read cs)
cmpconv (Plays a) cs  = compare a (read cs)

filterRecordBy cmp get what xs = filter (\x -> (cmpconv (get x) what `cmp` EQ)) xs
searchRecordBy get what xs = filterRecordBy (==) get what xs
deleteRecordBy get what xs = filterRecordBy (/=) get what xs

sortRecordBy f xs = sortBy (\a b -> (compare (f a) (f b))) xs

addRecord :: Record -> [Record] -> [Record]
addRecord r rs = r:rs

readRecord :: String -> String -> String -> String -> String -> Record
readRecord a b c d e = (Record (Artist a) (Album b) (Title c) (Year $ read d) (Plays $ read e))

readRecords :: [String] -> [Record]
readRecords cs = with5Lines (readRecord) cs

with5Lines f [] = []
with5Lines f (a:b:c:d:e:cs) = (f a b c d e):(with5Lines f cs)
with5Lines f cs = []

printRecord :: Record -> String
--printRecord x = printf "%-20s%-20s%-20s%4s%6s" (show $ artist x) (show $ album x) (show $ title x) (show $ year x) (show $ plays x)
printRecord x = printf "%-20s%-20s%-20s%4s%6s" (show $ artist x) (show $ album x) (show $ title x) (show $ year x) (show $ plays x)

showRecord x = lines (show x)
showRecords [] = []
showRecords rs = foldl (\a x -> a++(showRecord x)) [] rs

promptLine :: String -> IO String
promptLine line = do
	   putStrLn (line ++ ": ")
	   getLine

readDB = do
	  file <- openFile "db.in" ReadWriteMode -- creates if not exists
	  hClose file
	  input <- readFile "db.in"
	  return (readRecords (lines input))

writeDB rs = do
	  output <- (return (unlines (showRecords rs)))
	  writeFile "db.out" output

menu :: Int -> [Record] -> IO ()
menu 1 rs = do
	  putStrLn (printf "%-20s%-20s%-20s%4s%6s" "Artist" "Album" "Title" 
		"Year" "Plays")
	  putStrLn (take 70 (repeat '-'))
	  putStrLn (unlines (map (printRecord) rs))

menu 2 rs = do
	  rec <- mapM (\x -> promptLine $ fst x) fields
	  let new = readRecords rec
	  writeDB (rs++new)

menu 3 rs = do
	  field <- promptLine "Field"
	  value <- promptLine "Value"
	  menu 1 (searchRecordBy (dispatch field) value rs)

menu 4 rs = do
	  field <- promptLine "Field"
	  value <- promptLine "Value"
	  let newrs = deleteRecordBy (dispatch field) value rs
	  writeDB newrs
	  menu 1 newrs

menu 5 rs = do
	  field <- promptLine "Field"
	  let newrs = sortRecordBy (dispatch field) rs
	  writeDB newrs
	  menu 1 newrs

menu _ rs = error "Exiting"

dispatch fld = snd $ head $ filter (\x -> (fst x) == fld) fields

main = do
	records <- readDB
	putStrLn "1.Print file contents"
	putStrLn "2.Add new record"
	putStrLn "3.Search for records"
	putStrLn "4.Delete record"
	putStrLn "5.Sort records"
	choice <- getLine
	putStrLn ""
	menu (read choice) records
	main
