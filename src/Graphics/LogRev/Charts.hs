----------------------------------------------------------------------------
-- |
-- Module      :  Data.LogRev
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/logrev/
-- Repository  :  https://github.com/dmw/ApacheLogRev
--
-- An Apache Access Log Statistics extractor modules
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------

module Graphics.LogRev.Charts (
  plotPngPieChart
  , getActionValuesPie
  , logRevCntPer
  , logRevSzPer
  ) where


import Data.Char
import Data.Accessor
import Data.Colour
import Data.Colour.Names
import Data.List
import Data.LogRev.LogStats
import Data.String.Utils
import qualified Data.Map as M
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Text.Printf


plotPngPieChart :: LogRevOptions -> LogRevStatsAction -> IO (PickFn ())
plotPngPieChart o l = renderableToPNGFile chart 800 600 fname
                      where chart = toRenderable layout
                            layout = pie_title ^= title
                                     $ pie_plot ^: pie_colors ^= color_local
                                     $ pie_plot ^: pie_data ^= map pitem values
                                     $ defaultPieLayout
                            values = getActionValuesPie o l
                            pitem (s, v, o) = defaultPieItem { pitem_value_ = v
                                                             , pitem_label_ = s
                                                             , pitem_offset_= 1.0
                                                             }
                            fname = buildFileName o l
                            title = printf "Apache %s" $ aHeader l
                            color_local = cycle $ map opaque [brown, green,
                                                              yellow, cyan,
                                                              magenta, goldenrod]

getActionValuesPie :: LogRevOptions -> LogRevStatsAction -> [(String, Double, Double)]
getActionValuesPie o l = fmap (buildTuple l) kxs
                         where kxs = sort $ M.keys (sMap (aOutput l))
                               buildTuple u v = let x = logRevCntPer u v
                                                    y = buildLabel v x
                                                    in (y, x, 0.5)

buildLabel :: String -> Double -> String
buildLabel = printf "%s / %3.2f%%"

buildFileName :: LogRevOptions -> LogRevStatsAction -> String
buildFileName o l = fmap toLower $ strip $ replace " " "_" $ printf "%s_%s_pie.png" out rep
                    where out = outFile o
                          rep = aHeader l

logRevCntPer :: LogRevStatsAction -> String -> Double
logRevCntPer a k = 100.0 * n / t
                   where o = aOutput a
                         t = fromIntegral $ sTot o
                         m = sMap o
                         n = fromIntegral $ m M.! k

logRevSzPer :: LogRevStatsAction -> String -> Double
logRevSzPer a k = 100.0 * n / t
                  where o = aOutput a
                        t = fromIntegral $ sSzTot o
                        m = sSz o
                        n = fromIntegral $ m M.! k
