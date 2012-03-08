{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2012 Jun Furuse <jun.furuse@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Hatena
   Copyright   : Copyright (C) 2012 Jun Furuse
   License     : GNU GPL, version 2 or above 

   Maintainer  : Jun Furuse <jun.furuse@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of a 'Pandoc' document to a Hatena blog representation.

Note:  If @writerStandalone@ is @False@, only the document body
is represented; otherwise, the full 'Pandoc' document, including the
metadata.
-}
module Text.Pandoc.Writers.Hatena ( writeHatena )
where

import Text.Pandoc.Shared ( WriterOptions(..) )
import Data.List ( intersperse )
import Text.Pandoc.Definition
import Text.Pandoc.Pretty

prettyList :: [Doc] -> Doc
prettyList ds = "[" <> (cat $ intersperse (cr <> ",") $ map (nest 1) ds) <> "]"
    
wrapHTML tag atrs doc = 
    text ("<"++tag++sp_atrs++">") 
    <> doc 
    <> text ("</"++tag++">")
    where 
        sp_atrs = case atrs of
            Nothing -> ""
            Just s -> " " ++ s

inlines = cat . intersperse space . map inline
    
inline i = case i of
    Str str -> text str
    Space -> space
    Link is (url, _title) -> wrapHTML "a" (Just $ "href=\"" ++ url ++ "\"") $ inlines is
    Emph is        -> wrapHTML "i"      Nothing $ inlines is
    Strong is      -> wrapHTML "b"      Nothing $ inlines is
    Strikeout is   -> wrapHTML "strike" Nothing $ inlines is
    Superscript is -> wrapHTML "sup"    Nothing $ inlines is
    Subscript is   -> wrapHTML "sub"    Nothing $ inlines is
    
     -- CR jfuruse todo
    SmallCaps is -> inlines is
    Quoted quoteType is -> inlines is
    Code attr str -> wrapHTML "code" Nothing $ text str
    LineBreak -> text "<br/>"
    Note blks -> "((" <> blocks blks <> "))" -- don't know if complex thing is inside
    Cite cites is -> text ("CITE " ++ show cites) <> inlines is
    Math mathType str -> text ("MATH " ++ show mathType) <> text str
    RawInline fmt str -> text ("RAWINLINE " ++ show fmt) <> text str
    Image is (url, _title) ->
        let alt = case is of
                [] -> ""
                _ -> " alt=\"" ++ show is ++ "\""
        in
        text $ "<img" ++ alt ++ " src=\"" ++ url ++ "\"/>"

blocks :: [Block] -> Doc
blocks blks = cat $ intersperse cr $ map block blks

block :: Block -> Doc

block Null = empty

block (Plain is) = inlines is

block (Para is) = blankline <> inlines is -- CR jfuruse: it inserts an unnecessary blankline at the head of pars

block (BlockQuote blks) = 
    ">>" 
    $$ blocks blks 
    $$ "<<"

block (BulletList blockLists) = 
    "<ul>" 
    $$ vcat (map (\x -> "<li>" <> space <> nest 2 x <> space <> "</li>") $ map blocks blockLists)
    $$ "</ul>"

-- block (OrderedList attribs blockLists) = 
-- --   "OrderedList" <> space <> text (show attribs) $$
-- --   (prettyList $ map (prettyList . map block) blockLists)

block (CodeBlock attr str) =
    ">||"
    $$ text str
    $$ "||<"

block (Header lev is) = blankline <> text (stars lev) <> inlines is -- CR jfuruse: No break allowed!!!
    where
        stars 1 = "*"
        stars 2 = "**"
        stars _ = "***"

block HorizontalRule = cr <> text "<hr>" <> cr

-- TODOs
block blk = text $ show blk
-- block (OrderedList attribs blockLists) = 
--   "OrderedList" <> space <> text (show attribs) $$
--   (prettyList $ map (prettyList . map block) blockLists)

-- block (DefinitionList items) = "DefinitionList" $$
--   (prettyList $ map deflistitem items)
--     where deflistitem (term, defs) = "(" <> text (show term) <> "," <> cr <>
--            nest 1 (prettyList $ map (prettyList . map block) defs) <> ")"
-- block (Table caption aligns widths header rows) = 
--   "Table " <> text (show caption) <> " " <> text (show aligns) <> " " <>
--   text (show widths) $$
--   prettyRow header $$
--   prettyList (map prettyRow rows)
--     where prettyRow cols = prettyList (map (prettyList . map block) cols)
-- block (RawBlock fmt str) =
--    | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
--                            -- and a list of items, each a list of blocks)
--    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
--                            -- Each list item is a pair consisting of a
--                            -- term (a list of inlines) and one or more
--                            -- definitions (each a list of blocks)
--    | Header Int [Inline]   -- ^ Header - level (integer) and text (inlines)
--    | HorizontalRule        -- ^ Horizontal rule
--    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
--                            -- with caption, column alignments,
--                            -- relative column widths (0 = default),
--                            -- column headers (each a list of blocks), and
--                            -- rows (each a list of lists of blocks)
--    | Null                  -- ^ Nothing

-- | Prettyprint Pandoc document.
writeHatena :: WriterOptions -> Pandoc -> String
writeHatena opts (Pandoc meta blks) =
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
      withHead = \bs -> text ("Pandoc " ++ "(" ++ show meta ++ ")") $$ bs $$ cr
  in  render colwidth $ withHead $ blocks blks
