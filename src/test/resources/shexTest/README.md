# shexTest

## Directories:
### `schemas`

* Test schemas in ShExC (`.shex`), ShExJ (`.json`) and sometimes SHACL (`.shacl`).

The ShExC and ShExJ files with the same stem name are equivalent.
A ShExC syntax test consists of these steps:

* parse the ShExC version of the document with some base URI.
* parse the ShExJ, as JSON; evaluate the values of the following as relative IRIs:
  * values of the `start`, `inclusion`, `predicate`, and `datatype` properties.
  * shape names (keys in the `shapes` object).
  * terms in `values` properties.
* ensure that no `ValueAnd` or `ValueOr` expression contains `ValueAnd` or `ValueOr` expressions in the list of `valueExprs`.
* the two parsed products should be equivalent, with blank node substitution.


## `negativeSyntax`
These tests violate the ShEx2 grammar.

## `negativeStrucutre`
These tests should raise errors when parsed, noting the rule about nested `ValueAnd` and `ValueOr` expressions.

### `validation`

* Validation tests in a manifest (Turtle - `manifest.ttl`, [ShExJ][http://shex.io/shex-primer/ShExJ) - `manifest.json`).
* Input data in Turtle (`.ttl`).
* [ShEx Results format](http://shex.io/shex-primer/ShExJ) (`.val`).

A ShEx validator is `logic-conformant` when it returns success for the tests of type `ValidationTest` and failure for the tests of type `ValidationFailure`.
A ShEx validator is `result-conformant` (experimental) when it executes as `ValidationTest` and produces the same result structure as produced by this procedure:
* parse the result file as JSON.
* parse the ShExJ, as JSON; evaluate the values of the following as relative IRIs:
  * values of the `node`, `shape`, `subject`, `predicate`, and `object` properties.
* the two parsed products should be equivalent, with blank node substitution.
A ShEx validator is `error-conformant` (even more experimental) when it executes a `ValidationFailure` and produces the same result structure as produced by the procedure above.

#### `coverage`

One frequently wants to ask "does the test suite include X".
One way to test that is to guess by the relatively formulaic filenames and test names in `validation/manifest`.
Another is to "grep" through the JSON representations of the queries for something with the appropriate structure, e.g. using [jq](https://stedolan.github.io/jq/) to `EachOf`s that include a pattern with a `min` cardinality of 0:

    (for f in schemas/*.json; do
      jq -e '.[]|..|objects|select(.type=="EachOf").expressions[]|select(.min==0)' $f > /dev/null &&
      echo $f;
      done
    )

which yields the files which include this pattern:

    schemas/1val1IRIREFExtra1Or.json
    schemas/3circularRef1.json
    schemas/kitchenSink.json

