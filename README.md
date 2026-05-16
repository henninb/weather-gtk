# weather-gtk

A Haskell GTK desktop application that displays current weather conditions and forecasts by querying the Weather.com API (The Weather Channel API v3).

## Tech Stack

- Haskell (GHC via Stack)
- GTK bindings (`gi-gtk`)
- `aeson` — JSON parsing
- HTTP client for Weather.com API calls

## Prerequisites

Install Stack: [haskellstack.org](https://docs.haskellstack.org/en/stable/README/)

GTK development libraries:

```bash
# Arch Linux
sudo pacman -S gtk3 gobject-introspection

# Debian/Ubuntu
sudo apt install libgtk-3-dev libgirepository1.0-dev
```

## Build & Run

```bash
stack build
./run.sh
```

Or:

```bash
stack run
```

## API

Uses the Weather.com API (requires an API key). Example queries:

```bash
# Current observations
curl -s 'https://api.weather.com/v3/aggcommon/v3-wx-observations-current?apiKey=<key>&geocodes=45.18,-93.32&language=en-US&units=e&format=json' | jq

# 10-day forecast
curl -s 'https://api.weather.com/v3/wx/forecast/daily/10day?apiKey=<key>&geocode=44.977,-93.265&units=e&language=en-US&format=json' | jq
```

## Related

- [flutter-weather-app](../flutter-weather-app) — Flutter version of the weather app
