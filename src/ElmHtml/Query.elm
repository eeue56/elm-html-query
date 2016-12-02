module ElmHtml.Query
    exposing
        ( query
        , queryChildren
        , queryChildrenAll
        , queryById
        , queryByClassName
        , queryByClassList
        , queryByTagName
        , queryByAttribute
        , queryByBoolAttribute
        , queryAll
        , queryInNode
        , Selector(..)
        )

{-|
Query things using ElmHtml

@docs query, queryAll, queryChildren, queryChildrenAll, queryInNode
@docs Selector
@docs queryById, queryByClassName, queryByClassList, queryByTagName, queryByAttribute, queryByBoolAttribute
-}

import Dict
import String
import ElmHtml.InternalTypes exposing (..)


{-| Selectors to query a Html element
- Id, classname, classlist, tag are all what you'd expect
- Attribute and bool attribute are attributes
- ConainsText just searches inside for the given text
-}
type Selector
    = Id String
    | ClassName String
    | ClassList (List String)
    | Tag String
    | Attribute String String
    | BoolAttribute String Bool
    | ContainsText String
    | Multiple (List Selector)


{-| Query for a node with a given tag in a Html element
-}
queryByTagName : String -> ElmHtml -> List ElmHtml
queryByTagName tagname =
    query (Tag tagname)


{-| Query for a node with a given id in a Html element
-}
queryById : String -> ElmHtml -> List ElmHtml
queryById id =
    query (Id id)


{-| Query for a node with a given classname in a Html element
-}
queryByClassName : String -> ElmHtml -> List ElmHtml
queryByClassName classname =
    query (ClassName classname)


{-| Query for a node with all the given classnames in a Html element
-}
queryByClassList : List String -> ElmHtml -> List ElmHtml
queryByClassList classList =
    query (ClassList classList)


{-| Query for a node with a given attribute in a Html element
-}
queryByAttribute : String -> String -> ElmHtml -> List ElmHtml
queryByAttribute key value =
    query (Attribute key value)


{-| Query for a node with a given attribute in a Html element
-}
queryByBoolAttribute : String -> Bool -> ElmHtml -> List ElmHtml
queryByBoolAttribute key value =
    query (BoolAttribute key value)


{-| Query a Html element using a selector
-}
query : Selector -> ElmHtml -> List ElmHtml
query selector =
    queryInNode selector


{-| Query a Html node using multiple selectors, considering both the node itself
as well as all of its descendants.
-}
queryAll : List Selector -> ElmHtml -> List ElmHtml
queryAll selectors =
    query (Multiple selectors)


{-| Query a Html node using a selector, considering both the node itself
as well as all of its descendants.
-}
queryChildren : Selector -> ElmHtml -> List ElmHtml
queryChildren =
    queryInNodeHelp (Just 1)


{-| Query to ensure a html node has all selectors given, without considering
any descendants lower than its immediate children.
-}
queryChildrenAll : List Selector -> ElmHtml -> List ElmHtml
queryChildrenAll selectors =
    queryInNodeHelp (Just 1) (Multiple selectors)


{-| Query a Html node using a selector, considering both the node itself
as well as all of its descendants.
-}
queryInNode : Selector -> ElmHtml -> List ElmHtml
queryInNode =
    queryInNodeHelp Nothing


queryInNodeHelp : Maybe Int -> Selector -> ElmHtml -> List ElmHtml
queryInNodeHelp maxDescendantDepth selector node =
    case node of
        NodeEntry record ->
            let
                childEntries =
                    descendInQuery maxDescendantDepth selector record.children

                predicate =
                    predicateFromSelector selector
            in
                if predicate record then
                    node :: childEntries
                else
                    childEntries

        TextTag { text } ->
            case selector of
                ContainsText innerText ->
                    if text == innerText then
                        [ node ]
                    else
                        []

                _ ->
                    []

        _ ->
            []


descendInQuery : Maybe Int -> Selector -> List ElmHtml -> List ElmHtml
descendInQuery maxDescendantDepth selector children =
    case maxDescendantDepth of
        Nothing ->
            -- No maximum, so continue.
            List.concatMap
                (queryInNodeHelp Nothing selector)
                children

        Just depth ->
            if depth > 0 then
                -- Continue with maximum depth reduced by 1.
                List.concatMap
                    (queryInNodeHelp (Just (depth - 1)) selector)
                    children
            else
                []


predicateFromSelector : Selector -> (NodeRecord -> Bool)
predicateFromSelector selector =
    case selector of
        Id id ->
            hasAttribute "id" id

        ClassName classname ->
            hasClass classname

        ClassList classList ->
            hasClasses classList

        Tag tag ->
            (==) tag << .tag

        Attribute key value ->
            hasAttribute key value

        BoolAttribute key value ->
            hasBoolAttribute key value

        ContainsText text ->
            always False

        Multiple selectors ->
            hasAllSelectors selectors


hasAllSelectors : List Selector -> NodeRecord -> Bool
hasAllSelectors selectors record =
    List.map predicateFromSelector selectors
        |> List.map (\selector -> selector record)
        |> List.all identity


hasAttribute : String -> String -> NodeRecord -> Bool
hasAttribute attribute query { facts } =
    case Dict.get attribute facts.stringAttributes of
        Just id ->
            id == query

        Nothing ->
            False


hasBoolAttribute : String -> Bool -> NodeRecord -> Bool
hasBoolAttribute attribute value { facts } =
    case Dict.get attribute facts.boolAttributes of
        Just id ->
            id == value

        Nothing ->
            False


hasClass : String -> NodeRecord -> Bool
hasClass query record =
    List.member query (classnames record)


hasClasses : List String -> NodeRecord -> Bool
hasClasses classList record =
    containsAll classList (classnames record)


classnames : NodeRecord -> List String
classnames { facts } =
    Dict.get "className" facts.stringAttributes
        |> Maybe.withDefault ""
        |> String.split " "


containsAll : List a -> List a -> Bool
containsAll a b =
    b
        |> List.foldl (\i acc -> List.filter ((/=) i) acc) a
        |> List.isEmpty
