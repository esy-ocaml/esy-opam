var EsyOpam = require('./index.js');
var fs = require('fs');

var data = fs.readFileSync(process.argv[2], 'utf8');
var opam = EsyOpam.parseOpam(data);
var res = EsyOpam.renderOpamToJs('package', opam);

console.log(res);
