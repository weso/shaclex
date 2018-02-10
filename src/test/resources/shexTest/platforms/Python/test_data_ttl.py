import ShExJ
from pyjsg.jsglib.jsg import loads
from rdflib import Graph, plugin
import json



with open('../../schemas/manifest.jsonld') as data_file:
    data = json.load(data_file)

for graph in data['@graph']:
    if "entries" in graph.keys():
        for entry in graph["entries"]:
                    with open ("../../schemas/" + entry["ttl"]) as filename:
                        try:
                            data = Graph().parse(filename,  format='turtle')
                            print(entry["ttl"] + " is valid turtle: True")
                        except Exception as ex:
                            print(entry["ttl"]+" is valid turtle: False")
                            print(ex)
                    with open("../../schemas/" + entry["json"]) as filename:
                        s: ShExJ.Schema = loads(filename.read(), ShExJ)
                        # Validate the JSON
                        print(entry["json"]+" is valid ShExJ: {}".format(s._is_valid()))
                    print("===================================")