----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.LogRev.Charts
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
  , plotPngBarChart
  , logRevCntPer
  , logRevSzPer
  , getActionValuesPie
  , getActionValuesBar
  ) where


import qualified Data.Colour as C
import qualified Data.Colour.Names as CN
import Data.Char
import Data.Accessor
import Data.List
import Data.LogRev.LogStats
import Data.String.Utils
import qualified Data.Map as M
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Text.Printf


colorLocal :: [C.AlphaColour Double]
colorLocal = cycle $ fmap C.opaque [CN.darkslateblue, CN.darkcyan, CN.darkgreen,
                                    CN.darkmagenta, CN.darkblue, CN.darkgrey,
                                    CN.darkorange, CN.darkseagreen, CN.darkviolet,
                                    CN.dimgray, CN.dodgerblue, CN.deepskyblue]

plotPngPieChart :: LogRevOptions -> LogRevStatsAction -> IO (PickFn ())
plotPngPieChart o l = renderableToPNGFile chart 800 600 fname
                      where chart = toRenderable layout
                            layout = pie_title ^= title
                                     $ pie_plot ^: pie_colors ^= colorLocal
                                     $ pie_plot ^: pie_data ^= map pitem values
                                     $ defaultPieLayout
                            values = getActionValuesPie o l
                            pitem (s, v, o) = defaultPieItem { pitem_value_ = v
                                                             , pitem_label_ = s
                                                             , pitem_offset_= 5.0
                                                             }
                            fname = buildFileName o l
                            title = printf "Apache %s" $ aHeader l

plotPngBarChart :: LogRevOptions -> LogRevStatsAction -> IO (PickFn ())
plotPngBarChart o l = renderableToPNGFile chart 800 600 fname
                      where chart = toRenderable layout
                            layout = layout1_title ^= title
                                     $ layout1_title_style ^: font_size ^= 10
                                     $ layout1_bottom_axis ^: laxis_generate ^= autoIndexAxis alabels
                                     $ layout1_plots ^= [ Left (plotBars bars) ]
                                     $ layout1_legend ^= Just lstyle
                                     $ defaultLayout1 :: Layout1 PlotIndex Double
                            bars = plot_bars_titles ^= [title_q, title_s]
                                   $ plot_bars_values ^= addIndexes values
                                   $ plot_bars_style ^= BarsClustered
                                   $ plot_bars_spacing ^= BarsFixGap 10.0 10.0
                                   $ plot_bars_item_styles ^= map mkstyle (cycle colorLocal)
                                   $ defaultPlotBars
                            btitle = ""
                            (hdr, out, sz, tot) = (aHeader l, aOutput l, sSzTot out, sTot out)
                            bstyle = Just (solidLine 1.0 $ C.opaque CN.black)
                            mkstyle c = (solidFillStyle c, bstyle)
                            alabels = sort $ M.keys (sSz out)
                            values = getActionValuesBar o l
                            fname = buildFileName o l
                            lstyle = legend_orientation ^= LORows 3 $ defaultLegendStyle
                            title_q = printf "Apache %s Hit%% | %d (count)" hdr tot
                            title_s = printf "Apache %s Size%% | %d (bytes)" hdr sz
                            title = printf "Apache %s" hdr

buildLabel :: String -> Double -> String
buildLabel = printf "%s / %3.2f%%"

buildFileName :: LogRevOptions -> LogRevStatsAction -> String
buildFileName o l = fmap toLower (strip (replace
                                         " " "_"
                                         (printf "%s_%s.png" out rep)))
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

getActionValuesPie :: LogRevOptions -> LogRevStatsAction -> [(String, Double, Double)]
getActionValuesPie o l = fmap (buildTuple l) kxs
                         where kxs = sort $ M.keys (sMap (aOutput l))
                               buildTuple u v = let x = logRevCntPer u v
                                                    y = buildLabel v x
                                                    in (y, x, 0.5)

getActionValuesBar :: LogRevOptions -> LogRevStatsAction -> [[Double]]
getActionValuesBar o l = fmap (buildValue l) kxs
                         where kxs = sort $ M.keys (sMap (aOutput l))
                               buildValue u v = [logRevCntPer u v, logRevSzPer u v]
