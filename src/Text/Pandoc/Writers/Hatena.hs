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
    
wrapInline pref post doc = text pref <> doc <> text post
wrapBlock pref post doc = text pref $$ doc $$ text post

wrapHTML tag atrs = wrapInline ("<"++tag++sp_atrs++">") ("</"++tag++">")
    where 
        sp_atrs = case atrs of
            Nothing -> ""
            Just s -> " " ++ s

inlines = cat . map inline
    
inline (Str str) = text str
inline Space =space
inline (Link is (url, _title)) = wrapHTML "a" (Just $ "href=\"" ++ url ++ "\"") $ inlines is
inline (Emph is)        = wrapHTML "i"      Nothing $ inlines is
inline (Strong is)      = wrapHTML "b"      Nothing $ inlines is
inline (Strikeout is)   = wrapHTML "strike" Nothing $ inlines is
inline (Superscript is) = wrapHTML "sup"    Nothing $ inlines is
inline (Subscript is)   = wrapHTML "sub"    Nothing $ inlines is
-- CR jfuruse todo
inline (SmallCaps is) = inlines is
inline (Quoted quoteType is) = inlines is
inline (Code attr str) = wrapHTML "code" Nothing $ text str
inline (LineBreak) = text "<br/>"
inline (Note blks) = "((" <> blocks blks <> "))" -- don't know if complex thing is inside
inline (Cite cites is) = text ("CITE " ++ show cites) <> inlines is
inline (Math mathType str) = text ("MATH " ++ show mathType) <> text str
inline (RawInline fmt str) = text ("RAWINLINE " ++ show fmt) <> text str
inline (Image is (url, _title) )=
    text $ "<img" ++ alt ++ " src=\"" ++ url ++ "\"/>"
    where
        alt = case is of
            [] -> ""
            _ -> " alt=\"" ++ show is ++ "\""

-- The first Para in the blocks are not prefixed by a blankline.
blocks :: [Block] -> Doc
blocks [] = empty
blocks [blk] = block blk
blocks (blk:blks@(Para _:_)) = block blk <> blankline <> blocks blks
blocks (blk:blks) = block blk <> cr <> blocks blks

block :: Block -> Doc

block Null = empty
block (Plain is) = inlines is
block (Para is)  = inlines is -- the newline is inserted by blocks
block (BlockQuote blks) = wrapBlock ">>" "<<" $ blocks blks 

block (BulletList blockLists) = 
    wrapBlock "<ul>" "</ul>"
    $ vcat (map (\x -> "<li>" <> space <> nest 2 x <> space <> "</li>") $ map blocks blockLists)

-- block (OrderedList attribs blockLists) = 
-- --   "OrderedList" <> space <> text (show attribs) $$
-- --   (prettyList $ map (prettyList . map block) blockLists)

block (CodeBlock attr str) = 
    wrapInline "<!-- " " -->" (text $ show attr)
    $$ wrapBlock start "||<" (text str)
    where
        start = case language of
            Nothing -> ">||"
            Just s -> ">|" ++ s ++ "|"
        language = case classes of
            [] -> Nothing
            ["sourceCode", lang] -> Just lang
            _ -> Nothing -- CR jfuruse: I do not know well
        (_, classes, _) = attr
            

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
