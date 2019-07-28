# Changelog

## 1.0.0 (2019-07-28)

* License changed from MPL 2.0 to MIT.

* Fixes
  * Add support for non-gzipped databases (like `.zip`, see [#1](https://github.com/potatosalad/erlang-geolite2data/issues/1))

## 0.0.3 (2017-04-12)

* Fixes
  * Include `priv/releases` directory.

## 0.0.2 (2017-04-12)

* Fixes
  * Support for [geolix 0.13+](https://github.com/elixir-geolix/geolix/releases/tag/v0.13.0).

## 0.0.1 (2016-05-23)

* Initial Release

* Enhancements
  * Basic support for automatically updating the [GeoLite2 databases from MaxMind](https://dev.maxmind.com/geoip/geoip2/geolite2/).
  * Logger (disabled by default).
  * Support for updating [geolix](https://github.com/mneudert/geolix) (disabled by default).
