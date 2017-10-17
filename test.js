var EsyOpam = require('./index.js');
var fs = require('fs');

var opamName = process.argv[2];
var opamVersion = process.argv[3];
var opamFilename = process.argv[4];

var data = fs.readFileSync(opamFilename, 'utf8');
var opam = EsyOpam.parseOpam(data);
var res = EsyOpam.renderOpam(opamName, opamVersion, opam);

console.log(JSON.stringify(res, null, 2));
