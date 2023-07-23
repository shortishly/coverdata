<br>

<p align="center">
    <img alt="Erlang/OTP 25+" src="https://img.shields.io/badge/Erlang%2FOTP-25%2B-green?style=flat-square">
    <img alt="Apache-2.0" src="https://img.shields.io/github/license/shortishly/scran?style=flat-square">
</p>

## Why?

`coverdata` easily converts [Erlang][erlang-org] [cover
data][erlang-org-cover] files into a JSON object for simple
integration with a [Shields IO Dynamic JSON
badge][shields-io-dynamic-json-badge] to show a code coverage level
updated with each build:

<p align="center">
  <img alt="Dynamic JSON Badge" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fcoverdata%2Fscran.json&query=%24.total&suffix=%25&label=coverage">
</p>

## What?

This repository creates an [escript][erlang-org-escript] that reads
coverage data from `.coverdata` files, outputting a JSON object of
the percentage coverage per module and the total coverage percentage.

Without an `--input` argument `cover2json` will import all `.coverdata`
files under the current directory.

```shell
cover2json --output _site/cover/coverage.json
```

Each module has a coverage rate, with `total` being combined rate: 

```json
{
  "total": 67,
  "scran_combinator": 96,
  "scran_debug": 0,
  "scran_number_be": 5,
  "scran_number": 23,
  "scran_multi": 97,
  "scran_bytes": 83,
  "scran_branch": 100,
  "scran_sequence": 95,
  "scran_result": 33,
  "scran_number_le": 0,
  "scran_bits": 100,
  "scran_character_complete": 98
}
```

Options:

- `--input` is a single filename of `.coverdata` to load (optional)
- `--output` is an output filename for the JSON object (required)
- `--level LEVEL` is the [analyse][erlang-org-cover-analyse-1] level for
  [cover][erlang-org-cover] to use. Only `module` is supported at
  present, and is the default (optional)
- `--format FORMAT` is the output format to use, currently only `json`
  is supported (default, optional).
  
`coverdata` can be used as part of a GitHub Action copying the JSON
object into a GitHub Pages environment. [A shields.io dynamic JSON
badge][shields-io-dynamic-json-badge] can then use the `coverage.json`
as input.

In this [GitHub Action fragment][shortishly-scran-main-yml]:

```yaml
  site:
    needs: build
    strategy:
      matrix:
        otp:
          - 26
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{matrix.otp}}
      - run: make tests edoc
      - uses: shortishly/coverdata@main
        with:
          input: _site/cover
          output: _site/cover/coverage.json
          otp-version: ${{matrix.otp}}
      - uses: actions/upload-pages-artifact@v1
        with:
          path: _site
```

The `site` job runs the tests with coverage enabled, generating edoc
which are output into directories under `_site`, which are then
uploaded to GitHub Pages via the
`actions/upload-pages-artifact@v1`. Each successful build will update
the `_site/cover/coverage.json`.

The [README.md][shortishly-scran-readme-md] references the
current coverage level via (using [scran][shortishly-scran] as a live example):

```html
<a href="https://shortishly.github.io/scran/cover/">
  <img alt="Test Coverage" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fscran%2Fcover%2Fcoverage.json&query=%24.total&suffix=%25&style=flat-square&label=Test%20Coverage&color=green">
</a>
```

The encoded URL used points to the generated `coverage.json` on the
GitHub Pages environment used by the project:

```bash
https://shortishly.github.io/scran/cover/coverage.json
```

The query into the JSON object is `$.total` to select the total
coverage of the project within the badge.

## Build

`coverdata` uses [erlang.mk][erlang-mk].

To fetch dependencies and compile:

```shell
make app
```

To run the unit tests:

```shell
make tests
```

[erlang-mk]: https://erlang.mk
[erlang-org-cover-analyse-1]: https://www.erlang.org/doc/man/cover.html#analyse-1
[erlang-org-cover]: https://www.erlang.org/doc/apps/tools/cover_chapter.html
[erlang-org-escript]: https://www.erlang.org/doc/man/escript.html
[erlang-org]: https://www.erlang.org
[shields-io-dynamic-json-badge]: https://shields.io/badges/dynamic-json-badge
[shortishly-scran-main-yml]: https://github.com/shortishly/scran/blob/main/.github/workflows/main.yml
[shortishly-scran-readme-md]: https://github.com/shortishly/scran/blob/main/README.md
[shortishly-scran]: https://github.com/shortishly/scran/
