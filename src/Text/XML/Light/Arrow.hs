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
, addAttrs

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
import Control.Arrow.ArrowF
import Control.Category
import Prelude hiding (elem, (.), id)
import Text.XML.Light

nameQ :: ArrowF (~>) => Content ~> QName
nameQ = arr elName . getElem

name :: ArrowF (~>) => Content ~> String
name = arr qName . nameQ

children :: ArrowF (~>) => Content ~> Content
children = arrL elContent . getElem

attributes :: ArrowF (~>) => Content ~> Attr
attributes = arrL elAttribs . getElem

keyQ :: Arrow (~>) => Attr ~> QName
keyQ = arr attrKey

key :: Arrow (~>) => Attr ~> String
key = arr (qName . attrKey)

value :: Arrow (~>) => Attr ~> String
value = arr attrVal

text :: ArrowF (~>) => Content ~> String
text = arr cdData . getText

kind :: ArrowF (~>) => Content ~> CDataKind
kind = arr cdVerbatim . getText

-- Helpers.

getElem :: ArrowF (~>) => Content ~> Element
getElem = arrL (\c -> case c of Elem e -> [e]; _ -> [])

getText :: ArrowF (~>) => Content ~> CData
getText = arrL (\c -> case c of Text t -> [t]; _ -> [])

----------------

isElem, isText, isCRef :: ArrowF (~>) => Content ~> Content
isElem = isA (\c -> case c of Elem {} -> True; _ -> False)
isText = isA (\c -> case c of Text {} -> True; _ -> False)
isCRef = isA (\c -> case c of CRef {} -> True; _ -> False)

----------------

elemQ :: ArrowF (~>) => (QName -> Bool) -> Content ~> Content
elemQ f = arrL (\c -> case c of Elem e | f (elName e) -> [c]; _ -> [])

elem :: ArrowF (~>) => String -> Content ~> Content
elem n = elemQ ((==n) . qName)

attrQ :: (ArrowF (~>), ArrowChoice (~>)) => (QName -> Bool) -> Content ~> String
attrQ f = (isA f . keyQ `guards` value) . attributes

attr :: (ArrowF (~>), ArrowChoice (~>)) => String -> Content ~> String
attr n = attrQ ((==n) . qName)

childQ :: ArrowF (~>) => (QName -> Bool) -> Content ~> Content
childQ f = elemQ f . children

child :: ArrowF (~>) => String -> Content ~> Content
child n = childQ ((==n) . qName)

hasAttrQ :: (ArrowF (~>), ArrowChoice (~>)) => (QName -> Bool) -> Content ~> Content
hasAttrQ f = filterA (isA f . keyQ . attributes)

hasAttr :: (ArrowF (~>), ArrowChoice (~>)) => String -> Content ~> Content
hasAttr n = hasAttrQ ((==n) . qName)

----------------

deep :: (ArrowF (~>), ArrowPlus (~>)) => (Content ~> a) -> (Content ~> a)
deep e = e <+> deep e . children

deepWhen :: (ArrowF (~>), ArrowPlus (~>), ArrowChoice (~>)) => Content ~> c -> Content ~> a -> Content ~> a
deepWhen g e = e <+> g `guards` deepWhen g e . children

deepWhenNot :: (ArrowF (~>), ArrowPlus (~>), ArrowChoice (~>)) => Content ~> c -> Content ~> a -> Content ~> a
deepWhenNot g = deepWhen (notA g)

deepText :: (ArrowPlus (~>), ArrowF (~>)) => Content ~> String
deepText = arr concat . list (deep text)

----------------

toElemQ :: (ArrowPlus (~>), ArrowF (~>)) => (a ~> QName) -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
toElemQ q as cs = proc i ->
  do n <- q -< i
     a <- list (concatA as) -< i
     c <- list (concatA cs) -< i
     id -< Elem (Element n a c Nothing)

toElem :: (ArrowPlus (~>), ArrowF (~>)) => (a ~> String) -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
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

mkElemQ :: (ArrowPlus (~>), ArrowF (~>)) => QName -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
mkElemQ q = toElemQ (arr (const q))

mkElem :: (ArrowPlus (~>), ArrowF (~>)) => String -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
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

process :: (ArrowApply (~>), ArrowF (~>), ArrowChoice (~>)) => [Content] ~> [Content] -> Content ~> Content
process a = processor `when` isElem
  where processor = proc (Elem (Element n b c l)) ->
                    do s <- a -< c
                       id -<< Elem (Element n b s l)

process1 :: (ArrowApply (~>), ArrowF (~>), ArrowChoice (~>)) => Content ~> Content -> Content ~> Content
process1 a = process (list (a . unlist))

-- | If the condition holds, apply the arrow and continue processing
-- the children. Otherwise, do nothing and stop recursing.
processDeep :: (ArrowApply (~>), ArrowF (~>), ArrowChoice (~>)) => Content ~> c -> Content ~> Content -> Content ~> Content
processDeep c a = (process1 (processDeep c a) . a) `when` c

processText :: ArrowF (~>) => String ~> String -> Content ~> Content
processText a = toText . a . text

processAttrs :: (ArrowPlus (~>), ArrowF (~>)) => (Attr ~> Attr) -> Content ~> Content
processAttrs attrArr = toElem name [attrArr . attributes] [children]

-- | When the input is an element, calculate attributes from the
-- input, and add them to the attributes already on the input element.
addAttrs :: (ArrowF (~>), ArrowPlus (~>), ArrowChoice (~>)) => (Content ~> Attr) -> Content ~> Content
addAttrs attrArr = toElem name [concatA [attrArr, attributes]] [children] `when` isElem

----------------

printXml :: Arrow (~>) => Content ~> String
printXml = arr showContent

parseXml :: ArrowF (~>) => String ~> Content
parseXml = arrL parseXML

