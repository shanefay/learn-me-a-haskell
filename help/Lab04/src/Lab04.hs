{- Patrick O'Boyle 14310068 -}

module Lab04 where
import BinTree
import System.IO
import Data.Maybe

thisIsLab04 = "This is Lab 4"


{- Lab 04 implements a REPL that allows
the user to maintain and build a simple database
that records insured items: a description string along with their valuation.

The REPL provides facilites to add items, display them
and keeps a running total of the value of everything listed

When the REPL exists, it returns the running total -}

type State
  = ( BinTree String Float  -- database
    , Float )               -- total valuation


{- Task 1 ======= (1 mark)

The prompt string should display the running total (somewhere) -}

prompt04 hout state = hPutStr hout $ mkprompt state
mkprompt state = show (snd state)


{- Task 2 ======= (2 marks)

The user should enter the command "exit" to exit. -}

done04 command = if command == "exit"
                  then True
                 else False


{- Task 3 ======= (1 mark)

  The running total should be returned on exit -}

exit04 state = return (snd state)


{- Tasks 4.1--4.5 ====== (16 marks)

  All commands are single lowercase words -}

treeTotal :: Floating d => BinTree k d -> d
treeTotal Empty = 0.0
treeTotal (Leaf k d) = d
treeTotal (Branch ltree k d rtree) = (treeTotal ltree) + d + (treeTotal rtree)

treeRemove :: Ord k => k -> d -> BinTree k d -> BinTree k d
treeRemove key datum Empty = Empty
treeRemove key datum (Leaf k d)
 | key == k   = Leaf k datum
 | otherwise  = Leaf k d
treeRemove key datum (Branch ltree k d rtree)
 | key < k    =  Branch (treeRemove key datum ltree) k d rtree
 | key > k    =  Branch ltree k d (treeRemove key datum rtree)
 | otherwise  =  Branch ltree k datum rtree

{- I had additional backslashes before adding the filtering code below, as did a few
  others. -}

treeList :: (Show k, Show d) => BinTree k d -> String
treeList Empty = ""
treeList (Leaf k d) = ("" ++ (filter (/='"')) (show k) ++" "++ show d ++ "\n")
treeList (Branch ltree k d rtree) = (treeList ltree ++ "" ++ (filter (/='"')) (show k) ++ " " ++ show d ++ "\n"++ treeList rtree)

getTreeList :: State -> String
getTreeList state = (treeList (fst state)) ++ "\n"

add_to :: State -> String -> Float -> (BinTree String Float)
add_to state k v = treeInsert k v (fst state)

{- Task 4.1 ----- (4 marks)
 command "add" will issue two prompts, one to get the description, the second to get the value.
 The tree will have this information inserted into it,
 and the running total updated appropriately.

 use 'hGetLine handle' rather than 'getLine' !

 hint: to convert a string  'str' to a float 'f' use  f = read str :: Float
 This will give a runtime  error if the string does not represetn a float.
 All tests will use strings that do represent a float -}

execute04 hin hout "add" state =
  do hPutStr hout "Enter item description to add:\n"
     desc_in <- hGetLine hin
     let desc = (desc_in :: String)

     hPutStr hout "Enter item value:\n"
     val_in <- hGetLine hin
     let val = read val_in :: Float

     let bt = add_to state desc val
     let current_total = snd state
     let running_total = treeTotal bt

     return (bt, running_total)

{- Task 4.2 ---- (4 marks)
 command "fix" will compute the total in the database tree
 and compare to the running total.
 If different it will issue a warning, and then fix it.
 If not different, it will silently return to the user prompt

 command "_zero",  already implemented, will set total to zero but leave tree untouched -}

execute04 hin hout "fix" state =
  do let current_total = snd state
     let running_total = treeTotal (fst state)

     if(current_total /= running_total)
       then
         hPutStr hout "Error detcted, fixing\n"
     else
       return()

     return (fst state, running_total)


{- Task 4.3 ---- (4 marks)
 command "remove" will issue one prompt to get a description.
 It will remove the item by setting its value to zero,
 and correcting the running total -}

execute04 hin hout "remove" state =
   do hPutStr hout "Enter item description to remove:\n"
      desc_in <- hGetLine hin
      let desc = (desc_in :: String)
      let val = treeLookup desc (fst state)
      let value = fromJust val
      let datum = 0

      let bt = treeInsert desc datum (fst state)
      let newTotal = snd state - value
      return (bt, newTotal)


{- Task 4.4 ---- (2 marks)
 command "list" will list each entry, one per line,
 in the form 'description value' -}

execute04 hin hout "list" state
  = do let output = getTreeList state
       hPutStr   hout output
       return state

{- Task 4.5 ---- (2 marks)
 command "?" will list all the commands on one line, except for _zero -}

execute04 hin hout "?" state
 = do hPutStr hout "? add exit fix list remove"
      return state

-- Ignore empty command lines
execute04 hin hout "" state = return state

-- if all above fail, then report unknown command error to user
execute04 hin   hout command state
 = do hPutStr   hout "Command '"
      hPutStr   hout command
      hPutStrLn hout "' not recognised!"
      return state
