data Item = I String
	deriving (Show,Eq)

data User = U String 
	deriving (Show,Eq)
	
data Rating = NoRating | R Double
	deriving (Show,Eq)
	
dis :: Eq a=> [a] -> [a]
dis []=[]
dis (x:xs) | elem x xs == True = dis xs
              | otherwise = (x:(dis xs)) 


fromRatingsToItems:: Eq a => [(b,a,c)] -> [a]
fromRatingsToItems x = dis (fromRatingsToItemsHelper x)

fromRatingsToItemsHelper []=[]
fromRatingsToItemsHelper ((_,item,_):xs) = (item:(fromRatingsToItemsHelper xs))

fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a] 
fromRatingsToUsers x = dis (fromRatingsToUsersHelper x)

fromRatingsToUsersHelper []=[]
fromRatingsToUsersHelper ((user,_,_):xs) = (user:(fromRatingsToUsersHelper xs))

hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating _ _ [] =False
hasRating user item ((x,y,_):xs) =if (x==user && y==item) then True else hasRating user item xs

getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c 
getRating _ _ [] = error "No given rating"
getRating user item ((x,y,rating):xs) = if (user==x&&item==y) then rating else getRating user item xs

formMatrixUser _ [] _ = []
formMatrixUser user (item:items) database |( hasRating user item database)= ((R (getRating user item database)) : (formMatrixUser user items database))
																| otherwise = ( NoRating : (formMatrixUser user items database))
										
formMatrix [] _ _ = []
formMatrix (user:users) items database = ((formMatrixUser user items database) : formMatrix users items database)

numberRatingsGivenItem _ []=0
numberRatingsGivenItem index (x:xs)= if (x!!index)==NoRating 
			then numberRatingsGivenItem index xs
			else 1+ numberRatingsGivenItem index xs
						
differeneRatings NoRating _ = 0
differeneRatings _ NoRating = 0
differeneRatings (R a) (R b) = a-b	

matrixPairs n = matrixPairshelper 0 n

matrixPairshelper x n = if (x==n) then [] else ((matrixPairshelper2 x 0 n)++(matrixPairshelper (x+1) n))

matrixPairshelper2 x y n = if(y==n) then [] else ((x,y):matrixPairshelper2 x (y+1) n)   

		
dMatrix (x:xs) = dMatrixhelper (x:xs) 0 0 n where n = length x

dMatrixhelper list m1 m2 n | m2==n = dMatrixhelper list (m1+1) 0 n 
                           | m1==n = [] 
						   | otherwise = dMatrixhelper2 list m1 m2:dMatrixhelper list m1 (m2+1) n

dMatrixhelper2 [] _ _ = 0
dMatrixhelper2 (x:xs) m1 m2 = differeneRatings (x!!m1) (x!!m2) + dMatrixhelper2 xs m1 m2 

freqMatrix (x:xs) = freqMatrixhelper (x:xs) 0 0 n where n = length x

freqMatrixhelper list m1 m2 n | m2==n = freqMatrixhelper list (m1+1) 0 n 
                           | m1==n = [] 
						   | otherwise = freqMatrixhelper2 list m1 m2:freqMatrixhelper list m1 (m2+1) n

freqMatrixhelper2 [] _ _ = 0
freqMatrixhelper2 (x:xs) m1 m2 = if(x!!m1 == NoRating || x!!m2==NoRating) then freqMatrixhelper2 xs m1 m2 else 1+ freqMatrixhelper2 xs m1 m2			

diffFreqMatrix list = diffFreqMatrixhelper (dMatrix list) (freqMatrix list)
diffFreqMatrixhelper [] [] = []
diffFreqMatrixhelper (x:xs) (y:ys) = (x / y):(diffFreqMatrixhelper xs ys) 				 

--3adel el brackets

predict array i j=getAvg (     addEach ( getAvgDiffsI i j (getCols 0 (matrix!!i)) matrix) (getRatings  (getCols 0 (matrix!!i)) (matrix!!i) ) ) where matrix=getMatrix array

getSum [] =0
getSum (x:xs)= x+sum xs

getSum2D i x  	| i==length x =[]
							|otherwise=(R (getSum2DHelper i x)):getSum2D (i+1) x
							
getSum2DHelper _ []=0
getSum2DHelper i (x:xs) = y+getSum2DHelper i xs where (R y)=(x!!i)


getAvg x=(getSum x)/(fromIntegral (length x))

getLengthWithoutZeros []=0
getLengthWithoutZeros (x:xs) | x==0 =getLengthWithoutZeros xs
												 | otherwise=1+ (getLengthWithoutZeros xs)

addEach [] []=[]
addEach ((R x):xs) ((R y):ys)= (x+y):addEach xs ys

getRatings [] user=[]
getRatings (x:xs) user= (user!!x):(getRatings xs user)

getCols index [] = []
getCols index (x:xs) | x==NoRating = getCols (index+1) xs
									| otherwise = index:(getCols (1+index) xs)

getAvgDiffsI row j col [] =[]
getAvgDiffsI row j cols matrix = getSum2D 0 (getDifferenceMatrix row j cols matrix matrix)

getDifferenceMatrix _ _ _ [] _ = []
getDifferenceMatrix row j cols (currentUser:users) originalMatrix   | originalMatrix!!row==currentUser = getDifferenceMatrix row j cols users originalMatrix
																											| otherwise=(getDifferences j cols currentUser):(getDifferenceMatrix row j cols users originalMatrix)

getDifferences targetCol cols user= getDifferencesSubtractor (getRatings cols user) ( user!!targetCol)

getDifferencesSubtractor [] _=[]
getDifferencesSubtractor (rating:ratings) targetRating = (R (differeneRatings targetRating rating)):(getDifferencesSubtractor ratings targetRating)

getMatrix matrix=formMatrix ( fromRatingsToUsers matrix) (fromRatingsToItems matrix) matrix







