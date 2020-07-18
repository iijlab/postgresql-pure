# Changelog for postgresql-pure

## 0.2.2.0

2020.07.19

- Default implementations get to be given for `FromRecord` and `ToRecord`.

## 0.2.1.0

2020.07.14

- Add necessary files.

## 0.2.0.0

2020.07.13

### Breaking changes

- Remove orphan `IsLabel` instance definitions; [#2](https://github.com/iij-ii/postgresql-pure/pull/2) thanks to [@nakaji-dayo](https://github.com/nakaji-dayo).

### Other changes

- Add cleartext password authentication; [#1](https://github.com/iij-ii/postgresql-pure/pull/1) thanks to [@goodlyrottenapple](https://github.com/goodlyrottenapple).
- Change the Haskell type corresponding to `timetz` from `(TimeOfDay, TimeZone)` to `TimeOfDayWithTimeZone`.
- Not only tuples but also other types which have `Length` type function can be record.

## 0.1.3.0

2020.06.15

- Expose a function for type class instance implementations.
- A new instance `FromField String`.
- New instances for `FromRecord` and `ToRecord`.

## 0.1.2.0

2019.10.21

- Fix a compilation error with network ≧ 3.0.0.

## 0.1.1.0

2019.10.21

- Fix an issue sometimes missing “Error” responses after “Sync” requests.

## 0.1.0.0

2019.10.21

- Release.

### Known Issues

- Sometimes missing “Error” responses after “Sync” requests
