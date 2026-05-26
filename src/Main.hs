{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.GI.Base (get, on)
import qualified GI.Gtk as Gtk (Box, Label, mainQuit, onWidgetDestroy, gridSetRowSpacing, gridNew, widgetDestroy, onWidgetDestroy, widgetSetHexpand, gridSetColumnHomogeneous, gridSetColumnSpacing, buttonSetRelief, buttonSetLabel, boxNew, buttonNew, boxPackStart, labelNew, labelSetMarkup, labelSetXalign, labelSetYalign, styleContextAddProviderForScreen, init, windowNew, setContainerBorderWidth, setWindowTitle, setWindowResizable, setWindowDefaultWidth, setWindowDefaultHeight, setWindowWindowPosition, cssProviderNew, cssProviderLoadFromData, windowSetDecorated, widgetSetName, widgetGetStyleContext, styleContextAddClass, main, pattern ReliefStyleNone, pattern OrientationHorizontal, pattern OrientationVertical, pattern WindowTypeToplevel, pattern WindowPositionCenter, pattern STYLE_PROVIDER_PRIORITY_USER)
import GI.Gdk (screenGetDefault, keyvalToUnicode)
import Data.Aeson (eitherDecode, encode, eitherDecodeStrict)
import Data.Aeson.Types (ToJSON, FromJSON, Value, parseJSON, parseMaybe)
import GHC.Generics (Generic)
import Network.HTTP.Req (JsonResponse, jsonResponse, responseBody, (/:), defaultHttpConfig, (=:), https, runReq, req, pattern NoReqBody, pattern GET)
import qualified Data.ByteString.Lazy.UTF8 as BLU (fromString, toString)
import Data.Text (pack, unpack)
import Text.JSON.Generic (Typeable)
import Data.Aeson.Schema (schema, Object, get)
import Data.Aeson.Casing.Internal (snakeCase)
import Data.Typeable (typeOf)
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

type ForecastSchema = [schema|
  {
  '': List   {
      language: Text
    }
  }
|]

type AstroSchema = [schema|
  {
  metadata: {
    language: Text
  }
  ,
  astroData: List {
     dateLocal: Text,
     visibleLight: {
       hours: Int,
       minutes: Int,
       seconds: Int
     },
     lengthOfDay: {
       hours: Int,
       minutes: Int,
       seconds: Int
     },
     tomorrowDaylightDifference: {
       sign: Text,
       minutes: Int,
       seconds: Int
     },
     sun: {
       riseSet: {
         riseLocal: Text,
         riseUTC: Text,
         setLocal: Text,
         setUTC: Text
       }
     },
     moon: {
       riseSet: {
         riseLocal: Text,
         riseUTC: Text,
         setLocal: Text,
         setUTC: Text,
         risePhrase: Text,
         setPhrase: Text,
         moonage: Float,
         percentIlluminated: Int
       }
     }
  },
  astroPhases: List {
    date: Text,
    moonPhase: Text,
    moonAge: Float,
    moonAgeFromPhase: Int
  }

  }
|]

type WeatherSchema = [schema|
  {
  values: List
  {
    id: Text,
    currentObservation: {
      cloudCeiling: Maybe Float,
      cloudCoverPhrase: Text,
      dayOfWeek: Text,
      dayOrNight: Text,
      expirationTimeUtc: Int,
      iconCode: Int,
      iconCodeExtend: Int,
      obsQualifierCode: Maybe Text,
      obsQualifierSeverity: Maybe Int,
      precip1Hour: Float,
      precip6Hour: Float,
      precip24Hour: Float,
      pressureAltimeter: Float,
      pressureChange: Float,
      pressureMeanSeaLevel: Float,
      pressureTendencyCode: Int,
      pressureTendencyTrend: Text,
      relativeHumidity: Int,
      snow1Hour: Int,
      snow6Hour: Int,
      snow24Hour: Int,
      sunriseTimeLocal: Text,
      sunriseTimeUtc: Int,
      sunsetTimeLocal: Text,
      sunsetTimeUtc: Int,
      temperature: Int,
      temperatureChange24Hour: Int,
      temperatureDewPoint: Int,
      temperatureFeelsLike: Int,
      temperatureHeatIndex: Int,
      temperatureMax24Hour: Int,
      temperatureMaxSince7Am: Int,
      temperatureMin24Hour: Int,
      temperatureWindChill: Int,
      uvDescription: Text,
      uvIndex: Int,
      validTimeLocal: Text,
      validTimeUtc: Int,
      visibility: Int,
      windDirection: Int,
      windDirectionCardinal: Text,
      windGust: Maybe Int,
      windSpeed: Int,
      wxPhraseLong: Text,
      wxPhraseMedium: Text,
      wxPhraseShort: Text
    }
  }
  }
|]

type ObservationSchema = [schema|
 {
  observations: List
    {
      stationID: Text,
      obsTimeUtc: Text,
      obsTimeLocal: Text,
      neighborhood: Text,
      softwareType: Maybe Text,
      country: Text,
      solarRadiation: Float,
      lon: Float,
      realtimeFrequency: Maybe Float,
      epoch: Int,
      lat: Float,
      uv: Int,
      winddir: Int,
      humidity: Int,
      qcStatus: Int,
      imperial: {
        temp: Int,
        heatIndex: Int,
        dewpt: Int,
        windChill: Int,
        windSpeed: Int,
        windGust: Int,
        pressure: Float,
        precipRate: Float,
        precipTotal: Float,
        elev: Int
      }
    }
}
|]

data Weather = Weather {
    id:: String,
     observation3:: V3WxObservationsCurrent
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

data V3WxObservationsCurrent = V3WxObservationsCurrent {
      cloudCeiling:: Maybe Double,
      cloudCoverPhrase:: String,
      dayOfWeek:: String,
      dayOrNight:: String,
      expirationTimeUtc:: Integer,
      iconCode:: Integer,
      iconCodeExtend:: Integer,
      obsQualifierCode:: Maybe String,
      obsQualifierSeverity:: Maybe Int,
      precip1Hour:: Float,
      precip6Hour:: Float,
      precip24Hour:: Float,
      pressureAltimeter:: Float,
      pressureChange:: Float,
      pressureMeanSeaLevel:: Float,
      pressureTendencyCode:: Int,
      pressureTendencyTrend:: String,
      relativeHumidity:: Int,
      snow1Hour:: Int,
      snow6Hour:: Int,
      snow24Hour:: Int,
      sunriseTimeLocal:: String,
      sunriseTimeUtc:: Int,
      sunsetTimeLocal:: String,
      sunsetTimeUtc:: Int,
      temperature:: Int,
      temperatureChange24Hour:: Int,
      temperatureDewPoint:: Int,
      temperatureFeelsLike:: Int,
      temperatureHeatIndex:: Int,
      temperatureMax24Hour:: Int,
      temperatureMaxSince7Am:: Int,
      temperatureMin24Hour:: Int,
      temperatureWindChill:: Int,
      uvDescription:: String,
      uvIndex:: Int,
      validTimeLocal:: String,
      validTimeUtc:: Int,
      visibility:: Int,
      windDirection:: Int,
      windDirectionCardinal:: String,
      windGust:: Maybe Int,
      windSpeed:: Int,
      wxPhraseLong:: String,
      wxPhraseMedium:: String,
      wxPhraseShort:: String
} deriving (Show, Generic, Eq, ToJSON, FromJSON, Typeable)

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Nothing"

fromIntJust :: Maybe Int -> Int
fromIntJust (Just x) = x
fromIntJust Nothing = 0

firstOf :: [a] -> a
firstOf (x:_) = x
firstOf [] = error "firstOf: empty API response list"

apiKey :: String
apiKey = "e1f10a1e78da46f5b10a1e78da96f525"

forecastApi :: IO Value
forecastApi = do
  response <- runReq defaultHttpConfig $ req
    GET (https "api.weather.com" /: "v3" /: "wx" /: "forecast" /: "daily" /: "10day") NoReqBody jsonResponse $
    "apiKey" =: (apiKey :: String) <>
    "geocode" =: ("45.18,-93.32" :: String) <>
    "units" =: ("e" :: String) <>
    "language" =: ("en-US" :: String) <>
    "format" =: ("json" :: String)
  return (responseBody response)

astroApi :: IO Value
astroApi = do
    now <- getCurrentTime
    response <- runReq defaultHttpConfig $ req
      GET
      (https "api.weather.com" /: "v2" /: "astro") NoReqBody jsonResponse $
      "apiKey" =: (apiKey :: String) <>
      "geocode" =: ("45.18,-93.32" :: String) <>
      "days" =: ("1" :: String) <>
      "date" =: (formatTime defaultTimeLocale "%Y%m%d" now :: String) <>
      "format" =: ("json" :: String)
    return (responseBody response)

weatherApi :: IO Value
weatherApi = do
  response <- runReq defaultHttpConfig $ req
     GET (https "api.weather.com" /: "v3" /: "aggcommon" /: "v3-wx-observations-current") NoReqBody jsonResponse $
    "apiKey" =: (apiKey :: String) <>
    "geocodes" =: ("45.18,-93.32" :: String) <>
    "units" =: ("e" :: String) <>
    "language" =: ("en-US" :: String) <>
    "format" =: ("json" :: String)
  return (responseBody response)

getWeather :: IO Value
getWeather = do
  response <- runReq defaultHttpConfig $ req
    GET (https "api.weather.com" /: "v2" /: "pws" /: "observations" /: "current") NoReqBody jsonResponse $
    "apiKey" =: (apiKey :: String) <>
    "units" =: ("e" :: String) <>
    "stationId" =: ("KMNCOONR65" :: String) <>
    "format" =: ("json" :: String)
  return (responseBody response)

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

getApiWeather :: IO Weather
getApiWeather = do
  payload <- weatherApi
  let justObservations = fromJSONValue payload :: Maybe Weather
  let observationList = fromJust justObservations
  return observationList

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s@(x:xs) find repl =
    if take (length find) s == find
        then repl ++ replace (drop (length find) s) find repl
        else x : replace xs find repl

getAstroObservation :: IO (Object AstroSchema)
getAstroObservation = do
  payload <- astroApi
  let myPayload = Data.Aeson.encode payload
  either fail return $ eitherDecode myPayload :: IO (Object AstroSchema)

getWeatherObservation :: IO (Object WeatherSchema)
getWeatherObservation = do
  payload <- weatherApi
  let myPayload = Data.Aeson.encode payload
  let payloadUpdated = BLU.toString myPayload
  let payloadFinal = replace payloadUpdated "v3-wx-observations-current" "currentObservation"
  let payloadx = BLU.fromString  ("{\"values\": " ++ payloadFinal ++ "}")
  either fail return $ eitherDecode payloadx :: IO (Object WeatherSchema)

-- Extract HH:MM from ISO local time string like "2024-05-17T05:45:00-0500"
fmtLocalTime :: String -> String
fmtLocalTime s = if length s >= 16 then take 5 (drop 11 s) else s

styles = mconcat
    [ "window { background-color: #0d1117; }"
    , "box#header { background-color: #161b22; padding: 20px 24px; }"
    , "label#temp { font-size: 64px; font-weight: 900; color: #58a6ff; }"
    , "label#phrase { font-size: 15px; color: #8b949e; }"
    , "label#feelslike { font-size: 12px; color: #6e7681; }"
    , "label#location { font-size: 11px; color: #484f58; }"
    , "button#close { color: #6e7681; font-size: 16px; background: transparent; border: 1px solid #30363d; border-radius: 6px; padding: 2px 8px; }"
    , "button#close:hover { color: #f85149; border-color: #f85149; }"
    , "box.card { background-color: #161b22; border-radius: 6px; padding: 10px; margin: 4px; }"
    , "label.mname { font-size: 9px; color: #484f58; font-weight: bold; }"
    , "label.mval { font-size: 18px; font-weight: bold; color: #c9d1d9; }"
    ]

makeCard :: String -> IO (Gtk.Box, Gtk.Label)
makeCard title = do
  card <- Gtk.boxNew Gtk.OrientationVertical 2
  ctx <- Gtk.widgetGetStyleContext card
  Gtk.styleContextAddClass ctx (pack "card")
  nl <- Gtk.labelNew (Just $ pack title)
  nctx <- Gtk.widgetGetStyleContext nl
  Gtk.styleContextAddClass nctx (pack "mname")
  Gtk.labelSetXalign nl 0
  vl <- Gtk.labelNew (Just $ pack "\8212")
  vctx <- Gtk.widgetGetStyleContext vl
  Gtk.styleContextAddClass vctx (pack "mval")
  Gtk.labelSetXalign vl 0
  Gtk.boxPackStart card nl False False 0
  Gtk.boxPackStart card vl False False 0
  return (card, vl)

main :: IO ()
main = do
  Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.setContainerBorderWidth win 0
  Gtk.setWindowTitle win "Weather"
  Gtk.setWindowResizable win False
  Gtk.setWindowDefaultWidth win 920
  Gtk.setWindowDefaultHeight win 520
  Gtk.setWindowWindowPosition win Gtk.WindowPositionCenter
  Gtk.windowSetDecorated win False

  screen <- maybe (fail "No screen?!") return =<< screenGetDefault
  css <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData css styles
  Gtk.styleContextAddProviderForScreen screen css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  -- Close button
  closeBtn <- Gtk.buttonNew
  Gtk.buttonSetRelief closeBtn Gtk.ReliefStyleNone
  Gtk.buttonSetLabel closeBtn (pack "\x2715")
  Gtk.widgetSetName closeBtn (pack "close")
  on closeBtn #clicked $ Gtk.widgetDestroy win

  -- Header labels (placeholders updated after data loads)
  tempLabel <- Gtk.labelNew (Just $ pack "\8212")
  Gtk.widgetSetName tempLabel (pack "temp")
  Gtk.labelSetXalign tempLabel 0
  Gtk.labelSetYalign tempLabel 0.5

  phraseLabel <- Gtk.labelNew (Just $ pack "Loading...")
  Gtk.widgetSetName phraseLabel (pack "phrase")
  Gtk.labelSetXalign phraseLabel 0

  feelsLabel <- Gtk.labelNew (Just $ pack "")
  Gtk.widgetSetName feelsLabel (pack "feelslike")
  Gtk.labelSetXalign feelsLabel 0

  locationLabel <- Gtk.labelNew (Just $ pack "Coon Rapids, MN  45.18, -93.32")
  Gtk.widgetSetName locationLabel (pack "location")
  Gtk.labelSetXalign locationLabel 0

  infoBox <- Gtk.boxNew Gtk.OrientationVertical 4
  Gtk.boxPackStart infoBox tempLabel False False 0
  Gtk.boxPackStart infoBox phraseLabel False False 0
  Gtk.boxPackStart infoBox feelsLabel False False 0
  Gtk.boxPackStart infoBox locationLabel False False 0
  Gtk.widgetSetHexpand infoBox True

  rightBox <- Gtk.boxNew Gtk.OrientationVertical 0
  Gtk.boxPackStart rightBox closeBtn False False 0

  header <- Gtk.boxNew Gtk.OrientationHorizontal 0
  Gtk.widgetSetName header (pack "header")
  Gtk.boxPackStart header infoBox True True 0
  Gtk.boxPackStart header rightBox False False 0

  -- Metric cards (4 columns x 4 rows)
  (windSpeedCard, windSpeedLbl) <- makeCard "WIND SPEED"
  (windGustCard,  windGustLbl)  <- makeCard "WIND GUST"
  (windDirCard,   windDirLbl)   <- makeCard "WIND DIR"
  (windChillCard, windChillLbl) <- makeCard "WIND CHILL"
  (pressureCard,  pressureLbl)  <- makeCard "PRESSURE"
  (dewPointCard,  dewPointLbl)  <- makeCard "DEW POINT"
  (heatIndexCard, heatIndexLbl) <- makeCard "HEAT INDEX"
  (humidityCard,  humidityLbl)  <- makeCard "HUMIDITY"
  (uvIndexCard,   uvIndexLbl)   <- makeCard "UV INDEX"
  (uvCard,        uvLbl)        <- makeCard "UV"
  (cloudCard,     cloudLbl)     <- makeCard "CLOUD COVER"
  (visCard,       visLbl)       <- makeCard "VISIBILITY"
  (sunriseCard,   sunriseLbl)   <- makeCard "SUNRISE"
  (sunsetCard,    sunsetLbl)    <- makeCard "SUNSET"
  (moonriseCard,  moonriseLbl)  <- makeCard "MOON RISE"
  (moonsetCard,   moonsetLbl)   <- makeCard "MOON SET"

  metricsGrid <- Gtk.gridNew
  Gtk.gridSetColumnSpacing metricsGrid 0
  Gtk.gridSetRowSpacing metricsGrid 0
  Gtk.gridSetColumnHomogeneous metricsGrid True

  #attach metricsGrid windSpeedCard 0 0 1 1
  #attach metricsGrid windGustCard  1 0 1 1
  #attach metricsGrid windDirCard   2 0 1 1
  #attach metricsGrid windChillCard 3 0 1 1

  #attach metricsGrid pressureCard  0 1 1 1
  #attach metricsGrid dewPointCard  1 1 1 1
  #attach metricsGrid heatIndexCard 2 1 1 1
  #attach metricsGrid humidityCard  3 1 1 1

  #attach metricsGrid uvIndexCard   0 2 1 1
  #attach metricsGrid uvCard        1 2 1 1
  #attach metricsGrid cloudCard     2 2 1 1
  #attach metricsGrid visCard       3 2 1 1

  #attach metricsGrid sunriseCard   0 3 1 1
  #attach metricsGrid sunsetCard    1 3 1 1
  #attach metricsGrid moonriseCard  2 3 1 1
  #attach metricsGrid moonsetCard   3 3 1 1

  mainBox <- Gtk.boxNew Gtk.OrientationVertical 0
  Gtk.boxPackStart mainBox header False False 0
  Gtk.boxPackStart mainBox metricsGrid True True 8

  #add win mainBox

  on win #keyPressEvent $ \keyEvent -> do
    key <- keyEvent `Data.GI.Base.get` #keyval >>= keyvalToUnicode
    if key == 27 then Gtk.mainQuit else pure ()
    return False

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win

  astroObs <- getAstroObservation
  obs <- getWeatherObservation

  let tempVal    = show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.temperature |])
  let phraseVal  = unpack (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.wxPhraseLong |])
  let feelsVal   = show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureFeelsLike |])

  Gtk.labelSetMarkup tempLabel   (pack tempVal)
  Gtk.labelSetMarkup phraseLabel (pack phraseVal)
  Gtk.labelSetMarkup feelsLabel  (pack $ "Feels like " ++ feelsVal ++ "\xb0F")

  Gtk.labelSetMarkup windSpeedLbl (pack $ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.windSpeed |]) ++ " mph")
  Gtk.labelSetMarkup windGustLbl  (pack $ show (fromIntJust (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.windGust |])) ++ " mph")
  Gtk.labelSetMarkup windDirLbl   (pack $ unpack (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.windDirectionCardinal |]) ++ "  " ++ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.windDirection |]) ++ "\xb0")
  Gtk.labelSetMarkup windChillLbl (pack $ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureWindChill |]) ++ "\xb0F")
  Gtk.labelSetMarkup pressureLbl  (pack $ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.pressureAltimeter |]) ++ " inHg")
  Gtk.labelSetMarkup dewPointLbl  (pack $ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureDewPoint |]) ++ "\xb0F")
  Gtk.labelSetMarkup heatIndexLbl (pack $ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.temperatureHeatIndex |]) ++ "\xb0F")
  Gtk.labelSetMarkup humidityLbl  (pack $ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.relativeHumidity |]) ++ "%")
  Gtk.labelSetMarkup uvIndexLbl   (pack $ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.uvIndex |]))
  Gtk.labelSetMarkup uvLbl        (pack $ unpack (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.uvDescription |]))
  Gtk.labelSetMarkup cloudLbl     (pack $ unpack (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.cloudCoverPhrase |]))
  Gtk.labelSetMarkup visLbl       (pack $ show (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.visibility |]) ++ " mi")
  Gtk.labelSetMarkup sunriseLbl   (pack $ fmtLocalTime $ unpack (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.sunriseTimeLocal |]))
  Gtk.labelSetMarkup sunsetLbl    (pack $ fmtLocalTime $ unpack (firstOf [Data.Aeson.Schema.get| obs.values[].currentObservation.sunsetTimeLocal |]))
  Gtk.labelSetMarkup moonriseLbl  (pack $ fmtLocalTime $ unpack (firstOf [Data.Aeson.Schema.get| astroObs.astroData[].moon.riseSet.riseLocal |]))
  Gtk.labelSetMarkup moonsetLbl   (pack $ fmtLocalTime $ unpack (firstOf [Data.Aeson.Schema.get| astroObs.astroData[].moon.riseSet.setLocal |]))

  Gtk.main
