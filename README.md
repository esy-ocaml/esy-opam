# Esy Opam

Converter from opam to esy's package.json.

## Installation & Usage

Install with:

```
% npm install @esy-ocaml/esy-opam
```

Then see `dist/esy-opam.js.flow` for the available API.

## Development

Install Esy:

```
% npm install -g esy
```

Initialize and clone submodules:

```
% make bootstrap
```

Watch for changes and rebuild instantly:

```
% make watch
```

Publishing a new version (choose one of `major`, `minor` or `patch` for
`VERSION`):

```
% make VERSION=major|minor|patch bump-version publish
```
