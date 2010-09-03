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

-- * Creation with only arrow components.

, toElem
, toAttr
, toText

-- * Creation with fixed components.

, mkElem
, mkAttr
, mkAttrValue
, mkText

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
-- import Data.Either
import Prelude hiding (elem, (.), id)
import Text.XML.Light

nameQ :: ArrowList (~>) => Content ~> QName
nameQ = arr (\(Elem e) -> elName e) . isElem

name :: ArrowList (~>) => Content ~> String
name = arr qName . nameQ

children :: ArrowList (~>) => Content ~> Content
children = arrL (\(Elem e) -> elContent e) . isElem

attributes :: ArrowList (~>) => Content ~> Attr
attributes = arrL (\(Elem e) -> elAttribs e) . isElem

keyQ :: Arrow (~>) => Attr ~> QName
keyQ = arr attrKey

key :: Arrow (~>) => Attr ~> String
key = arr (qName . attrKey)

value :: Arrow (~>) => Attr ~> String
value = arr attrVal

text :: ArrowList (~>) => Content ~> String
text = arr (\(Text cd) -> cdData cd) . isText

kind :: ArrowList (~>) => Content ~> CDataKind
kind = arr (\(Text cd) -> cdVerbatim cd) . isText

----------------

isElem, isText, isCRef :: ArrowList (~>) => Content ~> Content
isElem = isA (\c -> case c of Elem {} -> True; _ -> False)
isText = isA (\c -> case c of Text {} -> True; _ -> False)
isCRef = isA (\c -> case c of CRef {} -> True; _ -> False)

----------------

elemQ :: ArrowList (~>) => QName -> Content ~> Content
elemQ q = isA (\(Elem e) -> elName e == q) . isElem

elem :: ArrowList (~>) => String -> Content ~> Content
elem n = elemQ (unqual n)

attrQ :: (ArrowList (~>), ArrowChoice (~>)) => QName -> Content ~> String
attrQ q = (isA (==q) . keyQ `guards` value) . attributes

attr :: (ArrowList (~>), ArrowChoice (~>)) => String -> Content ~> String
attr n = attrQ (unqual n)

childQ :: ArrowList (~>) => QName -> Content ~> Content
childQ q = elemQ q . children

child :: ArrowList (~>) => String -> Content ~> Content
child n = childQ (unqual n)

hasAttrQ :: (ArrowList (~>), ArrowChoice (~>))
         => QName -> Content ~> Content
hasAttrQ q = filterA (isA (==q) . keyQ . attributes)

hasAttr :: (ArrowList (~>), ArrowChoice (~>))
         => String -> Content ~> Content
hasAttr n = hasAttrQ (unqual n)

----------------

deep :: (ArrowList (~>), ArrowPlus (~>)) => (Content ~> c) -> (Content ~> c)
deep e = e <+> deep e . children

----------------

toElemQ :: (ArrowPlus (~>), ArrowList (~>))
        => (a ~> QName) -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
toElemQ q as cs = proc i ->
  do n <- q -< i
     a <- collect (concatA as) -< i
     c <- collect (concatA cs) -< i
     id -< Elem (Element n a c Nothing)

toElem :: (ArrowPlus (~>), ArrowList (~>))
       => (a ~> String) -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
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

mkElemQ :: (ArrowPlus (~>), ArrowList (~>))
        => QName -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
mkElemQ q = toElemQ (arr (const q))

mkElem :: (ArrowPlus (~>), ArrowList (~>))
       => String -> [a ~> Attr] -> [a ~> Content] -> a ~> Content
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

printXml :: Arrow (~>) => Content ~> String
printXml = arr ppContent

parseXml :: ArrowList (~>) => String ~> Content
parseXml = arrL parseXML


