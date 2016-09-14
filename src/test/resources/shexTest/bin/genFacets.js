#!/usr/bin/env node

var shexMinProfiles = {
  "INTEGER": { "": "5", "Lead": "05" },
  "xsd:integer": { "": "\"5\"^^<http://www.w3.org/2001/XMLSchema#integer>", "Lead": "05" },
  "xsd:decimal": { "": "\"4.5\"^^<http://www.w3.org/2001/XMLSchema#decimal>" },
  "xsd:float": { "": "\"4.5\"^^<http://www.w3.org/2001/XMLSchema#float>" },
  "xsd:double": { "": "\"4.5e0\"^^<http://www.w3.org/2001/XMLSchema#double>" },
  "xsd:byte": { "": "5" },
  "roman:numeral": { "": "\"V\"^^<http://roman.example/numeral>" },
  "DECIMAL": { "": "4.5", "LeadTrail": "04.50", "int": "5.0", "intLeadTrail": "05.00" },
  "DOUBLE": { "": "4.5e0", "LeadTrail": "04.50e0", "int": "5.0e0", "intLeadTrail": "05.00e0" },
}

var shexMaxProfiles = {
  "INTEGER": { "": "5", "Lead": "05" },
  "xsd:integer": { "": "\"5\"^^<http://www.w3.org/2001/XMLSchema#integer>", "Lead": "05" },
  "xsd:decimal": { "": "\"5.5\"^^<http://www.w3.org/2001/XMLSchema#decimal>" },
  "xsd:float": { "": "\"5.5\"^^<http://www.w3.org/2001/XMLSchema#float>" },
  "xsd:double": { "": "\"5.5e0\"^^<http://www.w3.org/2001/XMLSchema#double>" },
  "xsd:byte": { "": "5" },
  "roman:numeral": { "": "\"V\"^^<http://roman.example/numeral>" },
  "DECIMAL": { "": "5.5", "LeadTrail": "05.50", "int": "5.0", "intLeadTrail": "05.00" },
  "DOUBLE": { "": "5.5e0", "LeadTrail": "05.50e0", "int": "5.0e0", "intLeadTrail": "05.00e0" },
}

var shexProfiles = {
  Mininclusive: shexMinProfiles, Minexclusive: shexMinProfiles,
  Maxinclusive: shexMaxProfiles, Maxexclusive: shexMaxProfiles,
};
var _floatDecMax = {"low": "4.4", "equal": "5.5", "equalLeadTrail": "05.50", "high": "5.6"};
var _doubleMax = {"low": "4.4e0", "equal": "5.5e0", "equalLeadTrail": "05.50e0", "high": "5.6e0"}
var dataProfiles = {
  "xsd:integer": {
    label: "INT", 
    values: {"low": "4", "equal": "5", "equalLead": "05", "high": "6"}
  },
  "xsd:decimal": {
    label: "DEC",
    values: {"low": "4.4", "equal": "4.5", "equalLeadTrail": "04.50", "high": "5.6"},
    Maxinclusive: _floatDecMax, Maxexclusive: _floatDecMax
  },
  "xsd:float": {
    label: "FLT", datatype: '<http://www.w3.org/2001/XMLSchema#float>',
    values: {"low": "4.4", "equal": "4.5", "equalLeadTrail": "04.50", "high": "5.6"},
    Maxinclusive: _floatDecMax, Maxexclusive: _floatDecMax
  },
  "xsd:double": {
    label: "DBL",
    values: {"low": "4.4e0", "equal": "4.5e0", "equalLeadTrail": "04.50e0", "high": "5.6e0"},
    Maxinclusive: _doubleMax, Maxexclusive: _doubleMax
  },
  "xsd:byte": {
    label: "BYT", datatype: '<http://www.w3.org/2001/XMLSchema#byte>',
    values: {"low": "4", "equal": "5", "equalLead": "05", "high": "6"}
  },
  "xsd:dateTime": {
    label: "DT", datatype: '<http://www.w3.org/2001/XMLSchema#dateTime>',
    values: {"equal": "2015-12-25T01:23:45Z"}
  },
  "xsd:string": {
    label: "STR", datatype: '<http://www.w3.org/2001/XMLSchema#string>',
    values: {"equal": "ab"}
  },
  "roman:numeral": {
    label: "RMN", datatype: '<http://roman.example/numeral>',
    values: {"low": "IV", "equal": "V", "high": "VI"}
  }
};

var fs = require('fs');
var xlsx = require('xlsx');
var args = process.argv.slice(2);
var workbook = xlsx.readFile(args[0]);
var testList = [];
var testDfns = {};
var checked = {};

/**
 * Parses spreadsheet and creates tests in sht Turtle format.
 * @example
 * Spreadsheet:
 *   A   B         C     D           E       F   G    H     I           J     K    L          M              N    O
 * 1 range         count nodeKind    argument/--features--\ estDT       low   equal equalLead equalLeadTrail high ShEx filename
 * 2 Min inclusive
 * 3               1     xsd:integer DECIMAL int            xsd:integer        pass                               1integerMininclusiveDECIMAL
 * 4               1     xsd:integer DECIMAL int Lead Trail xsd:integer  fail  pass  pass                    pass 1integerMininclusiveDECIMALLeadTrail
 * gen( "O", 3, 4, 1); // ShEs filename, row 3 - row 4, headings in row 1.
 * // returns 5 tests
 */
function gen (worksheetName, shexCol, start, end, headings) {
  var worksheet = workbook.Sheets[worksheetName];
  for (var row = start; row <= end; ++row) {
    var testAddress = shexCol + row;                                 // O4
    function warn (msg, obj, key) {
      var m = worksheetName + '.' + testAddress + ": " + msg;
      console.warn(m);
      return key ? extend(obj, {key: m}) : "[["+m+"]]";
    }
    var shex = worksheet[testAddress];
    if (shex && "f" in shex && shex.v !== "") {
      // f: =IF(COUNTA(J6:N6) > 0, CONCATENATE(C6,MID(D6, FIND(":", D6)+1, 99),A$5,B$5,E6,F6,G6,H6), "")
      var m = shex.f.match(/^IF\(COUNTA\(([A-Z]+)([0-9]+):([A-Z]+)(?:[0-9]+)\) > 0, CONCATENATE\(([A-Z]+[0-9]+),MID\(([A-Z]+[0-9]+), FIND\(":", (?:[A-Z]+[0-9]+)\)\+1, 99\),([A-Z]+)\$([0-9]+),([A-Z]+)\$(?:[0-9]+)/); // (?:,[A-Z]+[0-9]+)+\), ""\)$
      // m: ["...", "J", "4", "N", "C4", "D4", "A", "2", "B"]

      var type = worksheet[m[6]+m[7]].v+worksheet[m[8]+m[7]].v;      // Mininclusive
      var facet = type.toUpperCase();                                // MININCLUSIVE
      var nodeKind = worksheet[colToAddr(addrToCol(m[1])-6)+m[2]].v; // xsd:integer
      var argument = worksheet[colToAddr(addrToCol(m[1])-5)+m[2]].v; // DECIMAL
      var shexFeatures =
        [4, 3, 2].map(function (delta) {
          var contents = worksheet[colToAddr(addrToCol(m[1])-delta)+m[2]];
          return contents ? contents.v : "";
        }).join("");                                                 // intLeadTrail
      var testDT = worksheet[colToAddr(addrToCol(m[1])-1)+m[2]].v;   // xsd:integer
      var shexProfile = shexProfiles[type] && shexProfiles[type][argument] ||
        warn("undefined feature shexProfiles["+type+"]["+argument+"]", {}, shexFeatures);
      var shexRepresentation = shexProfile[shexFeatures] ||
        warn("undefined feature shexProfiles["+type+"]["+argument+"]["+shexFeatures+"]");
      var testProfile = dataProfiles[testDT] ||
        warn("undefined feature dataProfiles["+testDT+"]", {values: {}}, "label");
      // scan for enabled tests.
      for (var testColNo = addrToCol(m[1]);                          // 9..13
           testColNo <= addrToCol(m[3]);
           ++testColNo) {
        var testCol = colToAddr(testColNo);                          // J..N
	testAddress = testCol + row;                                 // M4
        var passfail = worksheet[testCol + row];                     // {v: "pass"}
        if (passfail) {
          var testHeading = worksheet[testCol + headings].v;         // equalLead
          var testName = shex.v + "_" + passfail.v + "-" + (testDT.replace(/[a-z]*:/, "")) + "-" + testHeading; // 1integerMininclusiveDECIMALLeadTrail_pass-equalLead
	  var testValues = type in testProfile ? testProfile[type] : testProfile.values;
          var testValue = 
	    testHeading in testValues ?
            testValues[testHeading] :                                // 05
            warn("undefined feature dataProfiles["+testDT+"].values["+testHeading+"]");
          var suffix = testProfile.label+testValue.                  // INT05
	    replace(/\./g, "_").                                     // DEC5_5
	    replace(/\:/g, "_");                                     // 2015-12-25T01_23_45Z
	  var dataRepresentation = testProfile.datatype ?
	    "'"+testValue+"'^^"+testProfile.datatype :               // "lexical"^^<datatype>
	    testValue;                                               // Turtle bareword
          var shexFile = "../schemas/" + shex.v + ".shex";
          var dataFile = "Is1_Ip1_" + suffix + ".ttl";
	  var resultsFile = testName + ".val";
          var typeRes = passfail.v !== "fail" ?
            [ "sht:ValidationTest", "    mf:result <" + resultsFile + ">\n" ] :
            [ "sht:ValidationFailure", "" ];

          testList.push(testName);
	  var dfn =
            "<#" + testName + "> a " + typeRes[0] + " ;\n" +
            "    mf:name \"" + testName + "\" ;\n" +
            "    prov:wasDerivedFrom <" + args[0] + "#" + worksheetName + '.' + testAddress + "> ;\n" +
            "    rdfs:comment \"<S1> { <p1> "+nodeKind+" " + facet + " "+quote(shexRepresentation)+" } / { <s1> <p1> " + dataRepresentation + " }\" ;\n" +
            "    mf:status mf:proposed ;\n" +
            "    mf:action [\n" +
            "      sht:schema <" + shexFile + "> ;\n" +
            "      sht:shape <http://a.example/S1> ;\n" +
            "      sht:data <" + dataFile + "> ;\n" +
            "      sht:focus <http://a.example/s1>\n" +
            "    ] ;\n" +
            typeRes[1] +
            "    .\n";
	  if (testName in testDfns) {
	    warn("ignoring " + testName + " defined at " + testDfns[testName].src)
	  } else {
            testDfns[testName] = { src: testAddress, dfn: dfn };
	  }

	  if (args[1] === "-f" || args[1] === "-c") {
	    function expand (pname) {
	      var m = pname.match(/^([a-z]*):(.*?)$/);
	      if (!m) {
		throw Error(worksheetName + '.' + testAddress + ": expected prefixed name; got " + pname);
	      }
	      var exp = {
		xsd: "http://www.w3.org/2001/XMLSchema#",
		roman: "http://roman.example/"
	      }[m[1]];
	      if (!exp) {
		throw Error(worksheetName + '.' + testAddress + ": unknown prefix: " + pname);
	      }
	      return exp + m[2];
	    }
	    function testFile (name, type, expectedContents) {
	      if (!(name in checked)) {
		if (fs.existsSync(name)) {
		  var curContents = fs.readFileSync(name, "utf8");
		  if (curContents !== expectedContents) {
		    console.warn("unexpected contents in ", type, name, ": saw\n" + curContents + "expected:\n" + expectedContents);
		  }
		} else {
		  console.warn(verb, type, name);
		  if (args[1] === "-c") {
		    fs.writeFileSync(name, expectedContents);
		  }
		}
		checked[name] = testAddress;
	      }
	    }
	    var verb = args[1] === "-f" ? "missing" : "creating";
	    testFile(shexFile, "ShExC", "<http://a.example/S1> {\n" +
		     "   <http://a.example/p1> <"+expand(nodeKind)+"> "+facet+" "+shexRepresentation+"\n" +
		     "}\n");
	    testFile(dataFile, "data", "<http://a.example/s1> <http://a.example/p1> " + dataRepresentation + " .\n");

	    if (passfail.v !== "fail" && !fs.existsSync(resultsFile)) {
	      console.warn(verb, "results", resultsFile);
	      if (args[1] === "-c") {
		(function (testAddress, testName, shexFile, dataFile, resultsFile) {
		  function warn (msg, obj, key) {
		    var m = worksheetName + '.' + testAddress + ": " + msg;
		    console.warn(m);
		    return key ? extend(obj, {key: m}) : "[["+m+"]]";
		  }
		  var shex = require("shex");
		  shex.Loader.load([shexFile], [], [dataFile]).then(function (loaded) {
		    var res = shex.Validator(loaded.schema).validate(loaded.data, "http://a.example/s1", "http://a.example/S1");
		    if (!res) {
		      warn(testName + ": error validating <http://a.example/s1> in " + dataFile + " as <http://a.example/S1> in " + shexFile + " : tested invalid");
		    } else {
		      testFile(resultsFile, "results", JSON.stringify(res, null, "  "));
		    }
		  }).catch(function (e) {
		    warn(testName + ": error validating <http://a.example/s1> in " + dataFile + " as <http://a.example/S1> in " + shexFile + " :" + e);
		  });
		})(testAddress, testName, shexFile, dataFile, resultsFile);
	      }
	    }
	  }
        }
      }
    }
  }
}


gen('MinInclusive', "O", 6, 175, 3);
gen('MinExclusive', "O", 6, 175, 3);
gen('MaxInclusive', "O", 6, 175, 3);
gen('MaxExclusive', "O", 6, 175, 3);

testList.forEach(function (test) {
  console.log(args[1] === "-l" ? "        <#" + test + ">" : testDfns[test].dfn);
});

// debugger;
// var c = addrToCol(args[1]);
// console.log(c);
// var a = colToAddr(c);
// console.log(a);
// process.exit(0);

function addrToCol (a) {
    return a.match(/^[A-Z]+/g)[0].
    split("").
    reduce(function (r, c, i, l) {
      return r + Math.pow(26, l.length - i - 1)*(c.charCodeAt(0) - "A".charCodeAt(0) + 1);
    }, -1); // 0-based column number
}

// JS double (IEE754) works up to 9007199254740991 (column BKTXHSOGHKKF)
function colToAddr (column) {
  ++column; // 1-based column-letters
  var ret = '';

  do {
    ret = String.fromCharCode(64 + // charCode of 'A'
                              (column % 26 || 26) // 1-based modulus in 1's digit
                              ) + ret; // Shift letters in at left
  } while ((column = Math.floor((column - 1) / 26)) > 0); // Chop off 1's digit.
    
  return ret
}

function extend (base) {
  if (!base) base = {};
  for (var i = 1, l = arguments.length, arg; i < l && (arg = arguments[i] || {}); i++)
    for (var name in arg)
      base[name] = arg[name];
  return base;
}

function quote (s) {
  return s ? s.replace(/"/g, "\\\"") : s;
}

