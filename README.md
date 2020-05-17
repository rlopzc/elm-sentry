# Elm Sentry

Send errors to Sentry

## Install

```sh
elm install romariolopezc/elm-sentry
```

## Example

Inside the [example](/example) directory. Modify `config` using your DSN
to test this library.

## Disclaimer

This doesn't parse the DSN automatically. You need to provide the
PublicKey, Host and ProjectID.

Example:
DSN: `https://1590942c446340fdacb4c9369cac2cbd@o353541.ingest.sentry.io/8242816`

PublicKey: 1590942c446340fdacb4c9369cac2cbd

Host: o353541.ingest.sentry.io

ProjectID: 8242816

## Contributing
Sentry documentation: https://develop.sentry.dev/sdk/overview/

PR's welcome :)
