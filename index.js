var EsyOpam = require('./lib/js/src/main.js');

function renderOpam(packageName, packageVersion, opam) {
  return EsyOpam.render_opam(packageName, packageVersion, opam);
}

function parseOpam(data) {
  return EsyOpam.parse_opam(data);
}

function renderOpamUrl(opamUrl) {
  return EsyOpam.render_opam_url(opamUrl);
}

function parseOpamUrl(data) {
  return EsyOpam.parse_opam_url(data);
}

module.exports = {
  renderOpam: renderOpam,
  parseOpam: parseOpam,

  renderOpamUrl: renderOpamUrl,
  parseOpamUrl: parseOpamUrl,
};
