{-# LANGUAGE TypeOperators, Arrows #-}
{- |
List arrows for querying, creating and modifying XML trees.
-}
module Text.XML.Light.Arrow
(

-- * Selection.

  name
, children
, attributes
, key
, value
, text
, kind

-- * Filter.

, isElem
, isText
, isCRef

-- * By name.

, elem
, attr
, child

, hasAttr

-- * Deep selection.

, deep
, deepWhen
, deepWhenNot
, deepText

-- * Creation with only arrow components.

, toElem
, toAttr
, toText

-- * Creation with fixed components.

, mkElem
, mkAttr
, mkAttrValue
, mkText

-- * Processing child nodes.

, process
, process1
, processDeep
, processText
, processAttrs
, setAttrs

-- * Parsing / printing.

, printXml
, parseXml

-- * Qualified name variants.

, nameQ
, keyQ
, elemQ
, attrQ
, childQ
, hasAttrQ
, toElemQ
, toAttrQ
, mkElemQ
, mkAttrQ
, mkAttrValueQ

)
where

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Category
import Prelude hiding (elem, (.), id)
import Text.XML.Light

import qualified Data.Map as Map

nameQ :: ArrowList (~>) => Content ~> QName
nameQ = arr elName . getElem

name :: ArrowList (~>) => Content ~> String
name = arr qName . nameQ

children :: ArrowList (~>) => Content ~> Content
children = arrL elContent . getElem

attributes :: ArrowList (~>) => Content ~> Attr
attributes = arrL elAttribs . getElem

keyQ :: Arrow (~>) => Attr ~> QName
keyQ = arr attrKey

key :: Arrow (~>) => Attr ~> String
key = arr (qName . attrKey)

value :: Arrow (~>) => Attr ~> String
value = arr attrVal

text :: ArrowList (~>) => Content ~> String
text = arr cdData . getText

kind :: ArrowList (~>) => Content ~> CDataKind
kind = arr cdVerbatim . getText

-- Helpers.

getElem :: ArrowList (~>) => Content ~> Element
getElem = arrL (\c -> case c of Elem e -> [e]; _ -> [])

getText :: ArrowList (~>) => Content ~> CData
getText = arrL (\c -> case c of Text t -> [t]; _ -> [])

----------------

isElem, isText, isCRef :: ArrowList (~>) => Content ~> Content
isElem = isA (\c -> case c of Elem {} -> True; _ -> False)
isText = isA (\c -> case c of Text {} -> True; _ -> False)
isCRef = isA (\c -> case c of CRef {} -> True; _ -> False)

----------------

elemQ :: ArrowList (~>) => (QName -> Bool) -> Content ~> Content
elemQ f = arrL (\c -> case c of Elem e | f (elName e) -> [c]; _ -> [])

elem :: ArrowList (~>) => String -> Content ~> Content
elem n = elemQ ((==n) . qName)

attrQ :: (ArrowList (~>), ArrowChoice (~>)) => (QName -> Bool) -> Content ~> String
attrQ f = (isA f . keyQ `guards` value) . attributes

attr :: (ArrowList (~>), ArrowChoice (~>)) => String -> Content ~> String
attr n = attrQ ((==n) . qName)

childQ :: ArrowList (~>) => (QName -> Bool) -> Content ~> Content
childQ f = elemQ f . children

child :: ArrowList (~>) => String -> Content ~> Content
child n = childQ ((==n) . qName)

hasAttrQ :: (ArrowList (~>), ArrowChoice (~>)) => (QName -> Bool) -> Content ~> Content
hasAttrQ f = filterA (isA f . keyQ . attributes)

hasAttr :: (ArrowList (~>), ArrowChoice (~>)) => String -> Content ~> Content
hasAttr n = hasAttrQ ((==n) . qName)

----------------

deep :: (ArrowList (~>), ArrowPlus (~>)) => (Content ~> a) -> (Content ~> a)
deep e = e <+> deep e . children

deepWhen :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => Content ~> c -> Content ~> a -> Content ~> a
deepWhen g e = e <+> g `guards` deepWhen g e . children

deepWhenNot :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => Content ~> c -> Content ~> a -> Content ~> a
deepWhenNot g = deepWhen (notA g)

deepText :: (ArrowPlus (~>), ArrowList (~>)) => Content ~> String
deepText = arr concat . list (deep text)

----------------

toElemQ :: (ArrowPlus (~>), ArrowList (~>)) => (a ~> QName) -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
toElemQ q as cs = proc i ->
  do n <- q -< i
     a <- uniques . list (concatA as) -< i
     c <- list (concatA cs) -< i
     id -< Elem (Element n a c Nothing)
  where
    uniques = arr (Map.elems . Map.fromListWith const . map (key &&& id))

toElem :: (ArrowPlus (~>), ArrowList (~>)) => (a ~> String) -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
toElem n = toElemQ (arr unqual . n)

toAttrQ :: Arrow (~>) => (a ~> QName) -> (a ~> String) -> a ~> Attr
toAttrQ q s = proc i ->
  do n <- q -< i
     v <- s -< i
     id -< Attr n v

toAttr :: Arrow (~>) => (a ~> String) -> (a ~> String) -> a ~> Attr
toAttr n = toAttrQ (arr unqual . n)

toText :: Arrow (~>) => String ~> Content
toText = arr (\t -> Text (CData CDataText t Nothing))

----------------

mkElemQ :: (ArrowPlus (~>), ArrowList (~>)) => QName -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
mkElemQ q = toElemQ (arr (const q))

mkElem :: (ArrowPlus (~>), ArrowList (~>)) => String -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
mkElem n = toElem (arr (const n))

mkAttrQ :: Arrow (~>) => QName -> String ~> Attr
mkAttrQ k = toAttrQ (arr (const k)) id

mkAttr :: Arrow (~>) => String -> String ~> Attr
mkAttr k = toAttr (arr (const k)) id

mkAttrValueQ :: Arrow (~>) => QName -> String -> a ~> Attr
mkAttrValueQ k v = mkAttrQ k . arr (const v)

mkAttrValue :: Arrow (~>) => String -> String -> a ~> Attr
mkAttrValue k v = mkAttr k . arr (const v)

mkText :: Arrow (~>) => String -> a ~> Content
mkText t = toText . arr (const t)

----------------

process :: (ArrowApply (~>), ArrowList (~>), ArrowChoice (~>)) => [Content] ~> [Content] -> Content ~> Content
process a = processor `when` isElem
  where processor = proc (Elem (Element n b c l)) ->
                    do s <- a -< c
                       id -<< Elem (Element n b s l)

process1 :: (ArrowApply (~>), ArrowList (~>), ArrowChoice (~>)) => Content ~> Content -> Content ~> Content
process1 a = process (list (a . unlist))

-- | If the condition holds, apply the arrow and continue processing
-- the children. Otherwise, do nothing and stop recursing.
processDeep :: (ArrowApply (~>), ArrowList (~>), ArrowChoice (~>)) => Content ~> c -> Content ~> Content -> Content ~> Content
processDeep c a = (process1 (processDeep c a) . a) `when` c

processText :: ArrowList (~>) => String ~> String -> Content ~> Content
processText a = toText . a . text

processAttrs :: (ArrowPlus (~>), ArrowList (~>), ArrowChoice (~>)) => (Content ~> Attr) -> Content ~> Content
processAttrs attrArr = toElem name [attrArr] [children] `when` isElem

-- | When the input is an element, calculate attributes from the
-- input, and add them to the attributes already on the input element.
setAttrs :: (ArrowList (~>), ArrowPlus (~>), ArrowChoice (~>)) => (Content ~> Attr) -> Content ~> Content
setAttrs attrArr = processAttrs (concatA [attributes, attrArr])

----------------

printXml :: Arrow (~>) => Content ~> String
printXml = arr showContent

parseXml :: ArrowList (~>) => String ~> Content
parseXml = arrL parseXML

