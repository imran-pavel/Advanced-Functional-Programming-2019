import System.Environment
import qualified Data.List as DL
import qualified Control.Applicative as CA



type Name = String
type Story = [String]
type East = String
type North = String
type West = String
type South = String
type Owner = String
type PeopleInThisLocation = [String]
type ObjectsInThisLocation = [String]
type PossibleCurrentLocations = [String]
type ListOfVisitedLocations = [String]
type NegatedLocations = [String]
type ListOfObjectsInHand = [String]
type CompleteListOfLocations = [Location]
type CompleteListOfObjects = [Object]
type CompleteListOfPeople = [Person]
type CompleteRecord = (CompleteListOfPeople, CompleteListOfObjects, CompleteListOfLocations)



data Person = Person {   getPersonName :: Name
                       , getPossibleCurrentLocationsOfPerson :: PossibleCurrentLocations
                       , getListOfObjectsInHand :: ListOfObjectsInHand
                       , getNegatedLocations :: NegatedLocations
                       , getListOfVisitedLocations :: ListOfVisitedLocations } deriving (Show)

data Object = Object {    getObjectName :: Name
                        , getOwner :: Owner
                        , getPossibleCurrentLocationsOfObject :: PossibleCurrentLocations } deriving (Show)

data Location = Location {   getLocationName :: Name
                           , getEastOfThisLocation :: East
                           , getNorthOfThisLocation :: North
                           , getWestOfThisLocation :: West
                           , getSouthOfThisLocation :: South
                           , getPeopleInThisLocation :: PeopleInThisLocation
                           , getObjectsInThisLocation :: ObjectsInThisLocation } deriving (Show)



instance Eq Person where
    ( Person name1 _ _ _ _ ) == ( Person name2 _ _ _ _ ) = name1 == name2

instance Eq Object where
    ( Object name1 _ _ ) == ( Object name2 _ _ ) = name1 == name2

instance Eq Location where
    ( Location name1 _ _ _ _ _ _ ) == ( Location name2 _ _ _ _ _ _ ) = name1 == name2





subStr1 = "moved to the"
subStr2 = "journeyed to the"
subStr3 = "went to the"
subStr4 = "is in the"
subStr5 = "picked up the"
subStr6 = "dropped the"
subStr7 = "handed the"
subStr8 = "took the"
subStr9 = "discarded the"
subStr10 = "got the"
subStr11 = "travelled to the"
subStr12 = "is no longer in the"
subStr13 = "is either in the"

eitherLocationSpecificStatement :: String -> Bool
eitherLocationSpecificStatement statement
    | DL.isInfixOf subStr13 statement = True
    | otherwise = False

negateLocationSpecificStatement :: String -> Bool
negateLocationSpecificStatement statement
    | DL.isInfixOf subStr12 statement = True
    | otherwise = False

locationSpecificStatement :: String -> Bool
locationSpecificStatement statement
    | DL.isInfixOf subStr1 statement = True
    | DL.isInfixOf subStr2 statement = True
    | DL.isInfixOf subStr3 statement = True
    | DL.isInfixOf subStr4 statement = True
    | DL.isInfixOf subStr11 statement = True
    | otherwise = False

addObjectSpecificStatement :: String -> Bool
addObjectSpecificStatement statement
    | DL.isInfixOf subStr5 statement = True
    | DL.isInfixOf subStr8 statement = True
    | DL.isInfixOf subStr10 statement = True
    | otherwise = False

removeObjectSpecificStatement :: String -> Bool
removeObjectSpecificStatement statement
    | DL.isInfixOf subStr6 statement = True
    | DL.isInfixOf subStr9 statement = True
    | otherwise = False

handOverObjectSpecificStatement :: String -> Bool
handOverObjectSpecificStatement statement
    | DL.isInfixOf subStr7 statement = True
    | otherwise = False

directionSpecificStatement :: String -> Bool
directionSpecificStatement statement = (((words statement) !! 0) == "The") && (((words statement) !! 2) == "is") && ((words statement) !! 4) == "of" && (((words statement) !! 5) == "the")

parseNegateLocationSpecificStatement :: String -> (String, String)
parseNegateLocationSpecificStatement statement = (((words statement) !! 0), ((words statement) !! 6))

parseEitherLocationSpecificStatement :: String -> (String, String, String)
parseEitherLocationSpecificStatement statement = (((words statement) !! 0), ((words statement) !! 5), ((words statement) !! 8))

parseAddObjectSpecificStatement :: String -> (String, String)
parseAddObjectSpecificStatement statement
    | DL.isInfixOf subStr5 statement = (((words statement) !! 0), ((words statement) !! 4))
    | (DL.isInfixOf subStr8 statement) || (DL.isInfixOf subStr10 statement) = (((words statement) !! 0), ((words statement) !! 3))
    | otherwise = ("Nonsense", "Nonsense")   
    
parseRemoveObjectSpecificStatement :: String -> (String, String)
parseRemoveObjectSpecificStatement statement = (((words statement) !! 0), ((words statement) !! 3))


parseHandOverObjectSpecificStatement :: String -> (String, String, String)
parseHandOverObjectSpecificStatement statement
    | DL.isInfixOf subStr7 statement = (((words statement) !! 0), ((words statement) !! 3), ((words statement) !! 5))
    | otherwise = ("Nonsense", "Nonsense", "Nonsense") 


parseTravellingStatement :: String -> (String, String)
parseTravellingStatement statement = ((words statement) !! 0, (words statement) !! 4)

parseDirectionSpecificStatement :: String -> (String, String, String)
parseDirectionSpecificStatement statement = ((((words statement) !! 1)), (((words statement) !! 3)), (((words statement) !! 6)))


removePersonFromLocation :: Person -> Location -> Location
removePersonFromLocation person (Location name east north west south peopleInThisLocation objectsInThisLocation) = Location name east north west south (DL.delete (getPersonName person) peopleInThisLocation) objectsInThisLocation

removeTheseObjectsFromCompleteRecord :: [String] -> CompleteListOfObjects -> CompleteListOfObjects
removeTheseObjectsFromCompleteRecord objectsToRemove allObjects = filter (\object -> not $ elem (getObjectName object) objectsToRemove ) allObjects                                                        

createObject :: Name -> Owner -> PossibleCurrentLocations -> Object
createObject name owner possibleCurrentLocations = Object name owner possibleCurrentLocations


updateLocationOfObjects :: String -> [String] -> ListOfObjectsInHand -> CompleteListOfObjects -> CompleteListOfObjects
updateLocationOfObjects owner possibleNewLocations objectsInHand allObjects =
    let allObjectsAfterRemoval = removeTheseObjectsFromCompleteRecord objectsInHand allObjects
        getZippedList = CA.getZipList $ (,,) <$> CA.ZipList objectsInHand <*> CA.ZipList (repeat owner) <*> CA.ZipList (repeat possibleNewLocations)
    in  foldl (\acc (oName, oOwner, oPossibleNewLocations) -> (createObject oName oOwner oPossibleNewLocations) : acc ) allObjectsAfterRemoval getZippedList



removePersonFromThisLocation :: Name -> Location -> Location
removePersonFromThisLocation personName locObj = 
    Location (getLocationName locObj) (getEastOfThisLocation locObj) (getNorthOfThisLocation locObj) (getWestOfThisLocation locObj) (getSouthOfThisLocation locObj) (DL.delete personName $ getPeopleInThisLocation locObj) (getObjectsInThisLocation locObj)

removeObjectsFromThisLocation :: [String] -> Location -> Location
removeObjectsFromThisLocation objectsInHand locObj = 
    Location (getLocationName locObj) (getEastOfThisLocation locObj) (getNorthOfThisLocation locObj) (getWestOfThisLocation locObj) (getSouthOfThisLocation locObj) (getPeopleInThisLocation locObj) (foldl (\acc object -> DL.delete object acc) (getObjectsInThisLocation locObj) objectsInHand)


removePersonAndHisObjectsFromCurrentLocation :: Name -> ListOfObjectsInHand -> PossibleCurrentLocations -> CompleteListOfLocations -> CompleteListOfLocations
removePersonAndHisObjectsFromCurrentLocation _ _ [] locations = locations
removePersonAndHisObjectsFromCurrentLocation person listOfObjectsInHand (x:possibleCurrentLocations) locations = 
    let currLocObj = getOrCreateLocation x locations
        locationsAfterCurrLocRemoval = DL.delete currLocObj locations
        currLocationAfterPersonRemoval = removePersonFromThisLocation person currLocObj
        currLocationAfterObjectsRemoval = removeObjectsFromThisLocation listOfObjectsInHand currLocationAfterPersonRemoval
        updatedLocations = currLocationAfterObjectsRemoval : locationsAfterCurrLocRemoval
    in  removePersonAndHisObjectsFromCurrentLocation person listOfObjectsInHand possibleCurrentLocations updatedLocations





addPersonInThisLocation :: Name -> Location -> Location
addPersonInThisLocation personName locObj =
    Location (getLocationName locObj) (getEastOfThisLocation locObj) (getNorthOfThisLocation locObj) (getWestOfThisLocation locObj) (getSouthOfThisLocation locObj) (personName : (getPeopleInThisLocation locObj)) (getObjectsInThisLocation locObj)                                            



addObjectsInThisLocation :: [String] -> Location -> Location
addObjectsInThisLocation objectsInHand locObj = 
    Location (getLocationName locObj) (getEastOfThisLocation locObj) (getNorthOfThisLocation locObj) (getWestOfThisLocation locObj) (getSouthOfThisLocation locObj) (getPeopleInThisLocation locObj) (foldl (\acc object -> object : acc) (getObjectsInThisLocation locObj) objectsInHand)
    


addPersonAndHisObjectsInNewCurrentLocation :: Name -> ListOfObjectsInHand -> PossibleCurrentLocations -> CompleteListOfLocations -> CompleteListOfLocations
addPersonAndHisObjectsInNewCurrentLocation _ _ [] locations = locations
addPersonAndHisObjectsInNewCurrentLocation person listOfObjectsInHand (x:newPossibleCurrentLocations) locations =
    let newLocObj = getOrCreateLocation x locations
        locationsAfterNewLocationRemoval = DL.delete newLocObj locations
        newLocationAfterAddingPerson = addPersonInThisLocation person newLocObj
        newLocationAfterAddingObjects = addObjectsInThisLocation listOfObjectsInHand newLocationAfterAddingPerson
        updatedLocations = newLocationAfterAddingObjects : locationsAfterNewLocationRemoval
    in addPersonAndHisObjectsInNewCurrentLocation person listOfObjectsInHand newPossibleCurrentLocations updatedLocations



updateCurrentLocationOfPerson :: Person -> PossibleCurrentLocations -> Person
updateCurrentLocationOfPerson personObj newCurrentLocations = 
    let newVisitedLocations = if (length newCurrentLocations == 1) then
                                (getListOfVisitedLocations personObj) ++ newCurrentLocations
                              else
                                (getListOfVisitedLocations personObj)                        
    in Person (getPersonName personObj) newCurrentLocations (getListOfObjectsInHand personObj) (foldl (\acc newCurrLoc -> DL.delete newCurrLoc acc) (getNegatedLocations personObj) newCurrentLocations) newVisitedLocations


getOrCreatePerson :: Maybe Person -> String -> Person
getOrCreatePerson (Just x) personName = x  
getOrCreatePerson Nothing personName = Person personName [] [] [] []  


getOrCreateObject :: Name -> CompleteListOfObjects -> Object
getOrCreateObject objectName objects = case (DL.find (\objObj -> (getObjectName objObj) == objectName) objects) of
                                                Just foundObj -> foundObj
                                                Nothing -> Object objectName "" []

getOrCreateLocation :: Name -> CompleteListOfLocations -> Location
getOrCreateLocation locationName locations = case (DL.find (\locObj -> (getLocationName locObj) == locationName) locations) of
                                                Just location -> location
                                                Nothing -> Location locationName "UnKnown" "UnKnown" "UnKnown" "UnKnown" [] []


updateRecords1 :: String -> CompleteRecord -> CompleteRecord
updateRecords1 statement (people, objects, locations) =
    (updatedPeople, updatedObjects, locationsAfterAddingPersonAndHisObjectsInNewCurrentLocation)
    where
        (personName, locationName) = parseTravellingStatement statement
        personObject = getOrCreatePerson (DL.find ( == (Person personName [] [] [] []) ) people) personName
        locationsAfterRemovingPersonAndHisObjectsFromCurrentLocation = removePersonAndHisObjectsFromCurrentLocation (getPersonName personObject) (getListOfObjectsInHand personObject) (getPossibleCurrentLocationsOfPerson personObject) locations
        locationsAfterAddingPersonAndHisObjectsInNewCurrentLocation = addPersonAndHisObjectsInNewCurrentLocation (getPersonName personObject) (getListOfObjectsInHand personObject) [locationName] locationsAfterRemovingPersonAndHisObjectsFromCurrentLocation
        personAfterCurrentLocationUpdate = updateCurrentLocationOfPerson personObject [locationName]
        updatedObjects = updateLocationOfObjects (getPersonName personAfterCurrentLocationUpdate) (getPossibleCurrentLocationsOfPerson personAfterCurrentLocationUpdate) (getListOfObjectsInHand personAfterCurrentLocationUpdate) objects
        updatedPeople = personAfterCurrentLocationUpdate : (DL.delete personObject people)


removeObjectFromLocations :: Name -> PossibleCurrentLocations -> CompleteListOfLocations -> CompleteListOfLocations   
removeObjectFromLocations _ [] locations = locations 
removeObjectFromLocations object (x:possibleCurrentLocations) locations =
    let [currLocObj] = filter (\locObj -> (getLocationName locObj) == x) locations
        locationsAfterCurrLocRemoval = DL.delete currLocObj locations
        currLocationAfterObjectsRemoval = removeObjectsFromThisLocation [object] currLocObj
        updatedLocations = currLocationAfterObjectsRemoval : locationsAfterCurrLocRemoval
    in  removeObjectFromLocations object possibleCurrentLocations updatedLocations


addObjectToLocations :: Name -> PossibleCurrentLocations -> CompleteListOfLocations -> CompleteListOfLocations   
addObjectToLocations _ [] locations = locations 
addObjectToLocations object (x:newPossibleCurrentLocations) locations =
    let newLocObj = getOrCreateLocation x locations
        locationsAfterNewLocationRemoval = DL.delete newLocObj locations
        newLocationAfterAddingObjects = addObjectsInThisLocation [object] newLocObj
        updatedLocations = newLocationAfterAddingObjects : locationsAfterNewLocationRemoval
    in addObjectToLocations object newPossibleCurrentLocations updatedLocations



addObjectToPerson :: Name -> Name -> CompleteListOfPeople -> CompleteListOfPeople
addObjectToPerson object person people =
    let
        personObj = getOrCreatePerson (DL.find ( == (Person person [] [] [] []) ) people) person
        peopleAfterRemovingCurrentPersonObj = DL.delete personObj people
        updatedPersonObj = Person (getPersonName personObj) (getPossibleCurrentLocationsOfPerson personObj) (object : (getListOfObjectsInHand personObj)) (getNegatedLocations personObj) (getListOfVisitedLocations personObj)
    in
        (updatedPersonObj : peopleAfterRemovingCurrentPersonObj)


removeObjectFromPerson :: Name -> Name -> CompleteListOfPeople -> CompleteListOfPeople
removeObjectFromPerson object person people =
    let
        personObj = getOrCreatePerson (DL.find ( == (Person person [] [] [] []) ) people) person
        peopleAfterRemovingCurrentPersonObj = DL.delete personObj people
        updatedPersonObj = Person (getPersonName personObj) (getPossibleCurrentLocationsOfPerson personObj) (DL.delete object (getListOfObjectsInHand personObj)) (getNegatedLocations personObj) (getListOfVisitedLocations personObj)
    in
        (updatedPersonObj : peopleAfterRemovingCurrentPersonObj)



updateOwnerOfObject :: Name -> Name -> CompleteListOfObjects -> CompleteListOfObjects
updateOwnerOfObject object newOwner objects =
    let currentObj = getOrCreateObject object objects
        objectsAfterCurrentObjRemoval = DL.delete currentObj objects
        newCurrentObj = Object (getObjectName currentObj) newOwner (getPossibleCurrentLocationsOfObject currentObj)
    in
        (newCurrentObj : objectsAfterCurrentObjRemoval)


updateLocationOfObject :: Name -> PossibleCurrentLocations -> CompleteListOfObjects -> CompleteListOfObjects
updateLocationOfObject object possibleCurrentLocations objects =
    let currentObj = getOrCreateObject object objects
        objectsAfterCurrentObjRemoval = DL.delete currentObj objects
        newCurrentObj = Object (getObjectName currentObj) (getOwner currentObj) possibleCurrentLocations
    in
        (newCurrentObj : objectsAfterCurrentObjRemoval)

resolvePeople :: Name -> Name -> CompleteListOfPeople -> CompleteListOfPeople
resolvePeople _ "" people = people
resolvePeople object currentOwner people = removeObjectFromPerson object currentOwner people

addOjectUpdate :: Name -> Name -> CompleteRecord -> CompleteRecord
addOjectUpdate newOwner objectToAdd (people, objects, locations) =
    let
        objObj = getOrCreateObject objectToAdd objects
        currentOwner = getOwner objObj
        updatedPeople1 = resolvePeople objectToAdd currentOwner people
        updatedPeople2 = addObjectToPerson objectToAdd newOwner updatedPeople1
        updatedObjects1 = updateOwnerOfObject objectToAdd newOwner objects
        currentLocationsOfTheObject = getPossibleCurrentLocationsOfObject objObj
        newOwnerObj = getOrCreatePerson (DL.find ( == (Person newOwner [] [] [] []) ) updatedPeople2) newOwner 
        updatedObjects2 = updateLocationOfObject (getObjectName objObj) (getPossibleCurrentLocationsOfPerson newOwnerObj) updatedObjects1
        updatedLocations1 = removeObjectFromLocations objectToAdd currentLocationsOfTheObject locations
        updatedLocations2 = addObjectToLocations objectToAdd (getPossibleCurrentLocationsOfPerson newOwnerObj) updatedLocations1
    in
        (updatedPeople2, updatedObjects2, updatedLocations2)

removeOjectUpdate :: Name -> Name -> CompleteRecord -> CompleteRecord
removeOjectUpdate currentOwner objectToRemove (people, objects, locations) =
    let
        objObj = getOrCreateObject objectToRemove objects
        updatedPeople1 = resolvePeople objectToRemove currentOwner people
        updatedObjects1 = updateOwnerOfObject objectToRemove "" objects
    in
        (updatedPeople1, updatedObjects1, locations)

handOverObjectUpdate :: Name -> Name -> Name -> CompleteRecord -> CompleteRecord
handOverObjectUpdate fromPerson objectToAdd toPerson (people, objects, locations) =
    let 
        objObj = getOrCreateObject objectToAdd objects
        currentOwner = fromPerson
        updatedPeople1 = resolvePeople objectToAdd currentOwner people
        updatedPeople2 = addObjectToPerson objectToAdd toPerson updatedPeople1
        updatedObjects1 = updateOwnerOfObject objectToAdd toPerson objects
        currentLocationsOfTheObject = getPossibleCurrentLocationsOfObject objObj
        newOwnerObj = getOrCreatePerson (DL.find ( == (Person toPerson [] [] [] []) ) updatedPeople2) toPerson 
        updatedObjects2 = updateLocationOfObject (getObjectName objObj) (getPossibleCurrentLocationsOfPerson newOwnerObj) updatedObjects1
        updatedLocations1 = removeObjectFromLocations objectToAdd currentLocationsOfTheObject locations
        updatedLocations2 = addObjectToLocations objectToAdd (getPossibleCurrentLocationsOfPerson newOwnerObj) updatedLocations1
    in
        (updatedPeople2, updatedObjects2, updatedLocations2)




    
negateLocationUpdate :: Name -> Name -> CompleteRecord -> CompleteRecord
negateLocationUpdate person negatedPlace (people, objects, locations) =
    let
        personObj = getOrCreatePerson (DL.find ( == (Person person [] [] [] []) ) people) person
        updatedLocations1 = removePersonAndHisObjectsFromCurrentLocation person (getListOfObjectsInHand personObj) [negatedPlace] locations
        updatedPersonObj = Person person (DL.delete negatedPlace (getPossibleCurrentLocationsOfPerson personObj)) (getListOfObjectsInHand personObj) (negatedPlace : (getNegatedLocations personObj)) (getListOfVisitedLocations personObj)
        updatedPeople1 = DL.delete personObj people
        updatedPeople2 = updatedPersonObj : updatedPeople1
        updatedObjects1 = updateLocationOfObjects person (getPossibleCurrentLocationsOfPerson updatedPersonObj) (getListOfObjectsInHand updatedPersonObj) objects
    in
        (updatedPeople2, updatedObjects1, updatedLocations1)


eitherLocationUpdate :: Name -> Name -> Name -> CompleteRecord -> CompleteRecord
eitherLocationUpdate person location1 location2 (people, objects, locations) =
    let
        personObj = getOrCreatePerson (DL.find ( == (Person person [] [] [] []) ) people) person
        updatedLocations1 = removePersonAndHisObjectsFromCurrentLocation person (getListOfObjectsInHand personObj) (getPossibleCurrentLocationsOfPerson personObj) locations
        updatedLocations2 = addPersonAndHisObjectsInNewCurrentLocation person [] [location1, location2] locations
        updatedObjects1 = updateLocationOfObjects person [] (getListOfObjectsInHand personObj) objects
        updatedPersonObj = updateCurrentLocationOfPerson personObj [location1, location2]
        updatedPeople1 = DL.delete personObj people
        updatedPeople2 = updatedPersonObj : updatedPeople1
    in
        (updatedPeople2, updatedObjects1, updatedLocations2)

reverseDirection :: String -> String
reverseDirection "east" = "west"
reverseDirection "north" = "south"
reverseDirection "west" = "east"
reverseDirection "south" = "north"
reverseDirection _ = "UnKnown"
    
getDirectionUpdatedLocationObj :: String -> Location -> Location -> Location
getDirectionUpdatedLocationObj "east" (Location name east north west south people objects) locObj2 = 
    Location name east north (getLocationName locObj2) south people objects

getDirectionUpdatedLocationObj "north" (Location name east north west south people objects) locObj2 = 
    Location name east north west (getLocationName locObj2) people objects

getDirectionUpdatedLocationObj "west" (Location name east north west south people objects) locObj2 = 
    Location name (getLocationName locObj2) north west south people objects

getDirectionUpdatedLocationObj "south" (Location name east north west south people objects) locObj2 = 
    Location name east (getLocationName locObj2) west south people objects

directionLocationUpdate :: String -> String -> String -> CompleteRecord -> CompleteRecord
directionLocationUpdate location1 directionFrom location2 (people, objects, locations) =
    let
        locObj1 = getOrCreateLocation location1 locations
        locObj2 = getOrCreateLocation location2 locations
        updatedLocObj1 = getDirectionUpdatedLocationObj directionFrom locObj1 locObj2
        updatedLocObj2 = getDirectionUpdatedLocationObj (reverseDirection directionFrom) locObj2 locObj1
        updatedLocations1 = DL.delete locObj1 locations
        updatedLocations2 = DL.delete locObj2 updatedLocations1
        updatedLocations3 = (updatedLocObj1 : updatedLocObj2 : updatedLocations2)
    in
        (people, objects, updatedLocations3)


updateRecords2 :: String -> CompleteRecord -> CompleteRecord
updateRecords2 statement completeRecord =
    let
        (personName, object) = parseAddObjectSpecificStatement statement
    in
        addOjectUpdate personName object completeRecord


updateRecords3 :: String -> CompleteRecord -> CompleteRecord
updateRecords3 statement completeRecord =
    let
        (personName, object) = parseRemoveObjectSpecificStatement statement
    in
        removeOjectUpdate personName object completeRecord


updateRecords4 :: String -> CompleteRecord -> CompleteRecord
updateRecords4 statement completeRecord =
    let 
        (fromPerson, object, toPerson) = parseHandOverObjectSpecificStatement statement
    in
        handOverObjectUpdate fromPerson object toPerson completeRecord


updateRecords5 :: String -> CompleteRecord -> CompleteRecord
updateRecords5 statement completeRecord =
    let
        (person, negatedPlace) = parseNegateLocationSpecificStatement statement
    in
        negateLocationUpdate person negatedPlace completeRecord


updateRecords6 :: String -> CompleteRecord -> CompleteRecord
updateRecords6 statement completeRecord =
    let
        (person, location1, location2) = parseEitherLocationSpecificStatement statement
    in
        eitherLocationUpdate person location1 location2 completeRecord

updateRecords7 :: String -> CompleteRecord -> CompleteRecord
updateRecords7 statement completeRecord =
    let
        (location1, directionFrom, location2) = parseDirectionSpecificStatement statement
    in
        directionLocationUpdate location1 directionFrom location2 completeRecord
        

isItInTheQuestion :: String -> Bool
isItInTheQuestion statement = (((words statement) !! 0) == "Is") && (((words statement) !! 2) == "in") && (((words statement) !! 3) == "the")

whereIsTheQuestion :: String -> Bool
whereIsTheQuestion statement =
    let stWords = words statement
    in ((stWords !! 0) ++ (stWords !! 1) ++ (stWords !! 2)) == "Whereisthe"

howManyObjectsQuestion :: String -> Bool
howManyObjectsQuestion statement = DL.isPrefixOf "How many objects is" statement

whereWasQuestion :: String -> Bool
whereWasQuestion statement = DL.isPrefixOf "Where was" statement

howDoYouGoQuestion :: String -> Bool
howDoYouGoQuestion statement = DL.isPrefixOf "How do you go from" statement
       

determineLocationPerson :: String -> PossibleCurrentLocations -> NegatedLocations -> String
determineLocationPerson location [] [] = "maybe"
determineLocationPerson location possibleCurrentLocations [] =
    if (elem location possibleCurrentLocations) then
        if ((length possibleCurrentLocations) == 1) then
            "yes"
        else
            "maybe"
    else 
        "no"
determineLocationPerson location [] negateLocations =
    if (elem location negateLocations) then
        "no"
    else
        "maybe"
determineLocationPerson location possibleCurrentLocations negateLocations =
    if (elem location possibleCurrentLocations) then
        if ((length possibleCurrentLocations) == 1) then
            "yes"
        else
            "maybe"
    else
        "no"
    


determineLocationObject :: [String] -> String
determineLocationObject [] = "don't know"
determineLocationObject [location] = location
determineLocationObject _ = "don't know"


findPerson :: String -> String -> CompleteListOfPeople -> String
findPerson person location people =
    let result = DL.find (\personObj -> (getPersonName personObj) == person) people
    in case result of
        Just personObj -> determineLocationPerson location (getPossibleCurrentLocationsOfPerson personObj) (getNegatedLocations personObj)
        Nothing -> "maybe"


findObjectLocation :: String -> CompleteListOfObjects -> String
findObjectLocation object objects =
    let result = DL.find (\objObj -> (getObjectName objObj) == object) objects
    in case result of
        Just objObj -> determineLocationObject (getPossibleCurrentLocationsOfObject objObj)
        Nothing -> "don't know"


answerInTheQuestion :: String -> String -> CompleteRecord -> IO ()
answerInTheQuestion person location (people, objects, locations) = 
    do
        let answer = findPerson person location people
        putStrLn answer

answerWhereIsTheQuestion :: Name -> CompleteRecord -> IO ()
answerWhereIsTheQuestion object (people, objects, locations) = 
    do
        let answer = findObjectLocation object objects
        putStrLn answer

findObjectCount :: Name -> CompleteListOfPeople -> String
findObjectCount name people =
    let result = DL.find (\personObj -> (getPersonName personObj) == name) people
    in case result of
        Just personObj -> show $ length (getListOfObjectsInHand personObj)
        Nothing -> "don't know"

answerHowManyQuestion :: Name -> CompleteRecord -> IO ()
answerHowManyQuestion name (people, objects, locations) =
    do
        let answer = findObjectCount name people
        putStrLn answer


getTheLocation :: String -> Int -> [String] -> String
getTheLocation "before" index visitedLocations =
    if( (index - 1) < 0 ) then
        "don't know"
    else
        visitedLocations !! (index - 1)

getTheLocation "after" index visitedLocations =
    if( (index + 1) >= (length visitedLocations) ) then
        "don't know"
    else
        visitedLocations !! (index + 1)


previousLocation :: String -> String -> [String] -> String
previousLocation beforeOrAfter location visitedLocations =
    if (elem location visitedLocations) then
        let Just index = DL.elemIndex location visitedLocations
        in (getTheLocation beforeOrAfter index visitedLocations)
    else
        "don't know"

findPastLocation :: Name -> String -> Name -> CompleteListOfPeople -> String
findPastLocation personName beforeOrAfter location people =
    let result = DL.find (\personObj -> (getPersonName personObj) == personName) people
    in case result of
        Just personObj -> previousLocation beforeOrAfter location (getListOfVisitedLocations personObj)
        Nothing -> "don't know"

answerWhereWasQuestion :: Name -> String -> Name -> CompleteRecord -> IO ()
answerWhereWasQuestion personName beforeOrAfter location (people, objects, locations) =
    do
        let answer = findPastLocation personName beforeOrAfter location people
        putStrLn answer



breadthFirstSearch :: [String] -> [String] -> [(String, String)] -> CompleteListOfLocations -> [(String, String)]
breadthFirstSearch [] _ parentChildren _ = parentChildren
breadthFirstSearch (currentLocation:locationsToCheckNext) locationsChecked parentChildren locations = 
    let
        locObj = getOrCreateLocation currentLocation locations
        adjacentLocations = [(getEastOfThisLocation locObj), (getNorthOfThisLocation locObj), (getWestOfThisLocation locObj), (getSouthOfThisLocation locObj)]
        eligibleAdjacentLocations = filter (\adjacentLocation -> (not $ elem adjacentLocation locationsToCheckNext) && (not $ elem adjacentLocation locationsChecked) && (adjacentLocation /= "UnKnown")) adjacentLocations
        updatedLocationsToCheckNext = locationsToCheckNext ++ eligibleAdjacentLocations
        updatedLocationsChecked = (currentLocation : locationsChecked)
        newParentChildren = foldl (\acc adjacentLocation -> (currentLocation, adjacentLocation) : acc ) [] eligibleAdjacentLocations
        updatedParentChildren = parentChildren ++ newParentChildren
    in
        breadthFirstSearch updatedLocationsToCheckNext updatedLocationsChecked updatedParentChildren locations

getDirection :: String -> String -> CompleteListOfLocations -> String
getDirection child parent locations
    | (getEastOfThisLocation childLocObj) == (getLocationName parentLocObj) = "east"
    | (getNorthOfThisLocation childLocObj) == (getLocationName parentLocObj) = "north"
    | (getWestOfThisLocation childLocObj) == (getLocationName parentLocObj) = "west"
    | otherwise = "south"
    where
        childLocObj = getOrCreateLocation child locations
        parentLocObj = getOrCreateLocation parent locations
        

generatePath :: String -> String -> [(String, String)] -> [String] -> CompleteListOfLocations -> [String]
generatePath "UnKnown" _ _ _ _ = []
generatePath source destination parentChildren path locations
    | source == destination = path
    | [] == filter (\(parent, children) -> children == destination ) parentChildren = []
    | otherwise = let [(parent, child)] = filter (\(parent, children) -> children == destination ) parentChildren
                  in generatePath source parent parentChildren ((reverseDirection ((getDirection child parent locations))) : path) locations

resolvePath :: [String] -> String
resolvePath [] = "don't know"
resolvePath path = DL.intercalate ", " path

answerHowdoYouGoQuestion :: String -> String -> CompleteRecord -> IO ()
answerHowdoYouGoQuestion from to (people, objects, locations) =
    do
        let path = generatePath from to (breadthFirstSearch [from] [] [("UnKnown", from)] locations) [] locations
            answer = resolvePath path
        putStrLn answer

        
dispatchQuestion :: String -> CompleteRecord -> IO ()
dispatchQuestion question completeRecord
    | isItInTheQuestion question = answerInTheQuestion ((words question) !! 1) ((words question) !! 4) completeRecord
    | whereIsTheQuestion question = answerWhereIsTheQuestion ((words question) !! 3) completeRecord
    | howManyObjectsQuestion question = answerHowManyQuestion ((words question) !! 4) completeRecord
    | whereWasQuestion question = answerWhereWasQuestion ((words question) !! 2) ((words question) !! 3) ((words question) !! 5) completeRecord
    | howDoYouGoQuestion question = answerHowdoYouGoQuestion ((words question) !! 6) ((words question) !! 9) completeRecord
    | otherwise = putStrLn "Invalid Question"


askQuestion completeRecord =
    do
        question <- getLine
        dispatchQuestion question completeRecord
        askQuestion completeRecord


interpretStory :: Story -> CompleteRecord -> CompleteRecord
interpretStory [] completeRecord = completeRecord
interpretStory (x:xs) completeRecord
    | locationSpecificStatement x = interpretStory xs (updateRecords1 x completeRecord)
    | addObjectSpecificStatement x = interpretStory xs (updateRecords2 x completeRecord)
    | removeObjectSpecificStatement x = interpretStory xs (updateRecords3 x completeRecord)
    | handOverObjectSpecificStatement x = interpretStory xs (updateRecords4 x completeRecord)
    | negateLocationSpecificStatement x = interpretStory xs (updateRecords5 x completeRecord)
    | eitherLocationSpecificStatement x = interpretStory xs (updateRecords6 x completeRecord)
    | directionSpecificStatement x = interpretStory xs (updateRecords7 x completeRecord)
    | otherwise = completeRecord


main = do
        (inputFileName : otherArguments) <- getArgs
        story <- readFile inputFileName
        let completeRecord = interpretStory (lines story) ([], [], [])
        askQuestion completeRecord
    