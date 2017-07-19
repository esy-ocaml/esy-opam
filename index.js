var EsyOpam = require('./lib/js/src/main.js');

function renderOpamToJs(packageName, opam) {
  return EsyOpam.render_opam_to_js(packageName, opam);
}

function parseOpam(data) {
  return EsyOpam.parse_opam(data);
}

module.exports = {
  renderOpamToJs: renderOpamToJs,
  parseOpam: parseOpam,
};
