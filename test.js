var EsyOpam = require('./index.js');
console.log(EsyOpam);
var fs = require('fs');

var data = fs.readFileSync(process.argv[2], 'utf8');
var opam = EsyOpam.parseOpam(data);
var res = EsyOpam.renderOpam('package', '0.1.1', opam);

console.log(JSON.stringify(res, null, 2));
