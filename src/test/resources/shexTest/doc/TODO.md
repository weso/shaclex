What do we do with recursive structures like:
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
