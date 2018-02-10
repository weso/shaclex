= 3 non-unique validation comments =

- "<S> { <p1> [<v1>] } on { <s1> <p1> <v1> }"
-- 1val1IRIREF.shex == 1val1iri.shex
- "<S1> { <p1> LITERAL PATTERN \"^bc$\" } on { <s1> <p1> \"^bc$\" }"
-- <#1literalPattern_pass-StartlitEnd-match> == <#1literalStartPatternEnd_CarrotbcDollar>
- ":S1 {:p1 .|:p2 .,:p3 .} / { :s1 :p1 \"p1-0\"; :p2 \"p2-0\"; :p3 \"p3-0\" . }"
-- <#1dotOne2dot-oneOf_fail_p1p2p3> ~<#1dotOne2dot-someOf_fail_p1p2p3> ~sht:EachOf-unvisited

```
c = data["@graph"][0].entries.reduce((acc, t) => { if (t.comment in acc) { acc[t.comment].push(t); } else { acc[t.comment] = [t]; } return acc; }, {})
d = Object.keys(c).filter(k => c[k].length > 1)
```

= What do we do with recursive structures like: =
```
<http://all.example/S1> {
  $<http://all.example/S1e> (
    &<http://all.example/S1e>;
    <http://all.example/p1> . ?)
}
```
```
{
  "@context": "https://shexspec.github.io/context.jsonld",
  "type": "Schema",
  "shapes": [
    {
      "id": "http://all.example/S1",
      "type": "Shape",
      "expression": {
        "id": "http://all.example/S1e",
        "type": "EachOf",
        "expressions": [
          "http://all.example/S1e",
          {
            "type": "TripleConstraint",
            "predicate": "http://all.example/p1",
            "min": 0,
            "max": 1
          }
        ]
      }
    }
  ]
}
```
```
BASE <http://all.example/>
PREFIX sx: <http://shex.io/ns/shex#>

[] a sx:Schema ;
  sx:shapes <S1> .

<S1> a sx:Shape ;
  sx:expression <S1e> .

<S1e> a sx:EachOf ;
  sx:expressions (
    <S1e>
    [ a sx:TripleConstraint ; sx:predicate <p1> ; sx:min 0 ; sx:max 1 ]
  ) .
```
