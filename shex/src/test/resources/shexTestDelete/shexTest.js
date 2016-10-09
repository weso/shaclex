var ret = {};
['schemas', 'negativeSyntax', 'illDefined', 'validation', "ASTs"].forEach(function (dir) {
  ret[dir] = __dirname + '/' + dir + '/';
});
ret['parsedSchemas'] = __dirname + '/' + 'schemas' + '/';;

if (typeof require !== 'undefined' && typeof exports !== 'undefined')
  module.exports = ret;
else
  shexTest = ret;

