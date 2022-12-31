const { join } = require('path');
const { readFile } = require('fs/promises');

function rows(html) {
  return [...html.matchAll(/<tr[^>]*>(.*?)<\/tr>/g)].map(m => m[1]);
}

function cells(html) {
  return [...html.matchAll(/<td[^>]*>(.*?)<\/td>/g)].map(m => m[1]);
}

function removeLink(html) {
  const m = html.match(/<a[^>]*>(.*?)<\/a>/);
  return m ? m[1] : html;
}

module.exports = async ({github, context, coveragePath}) => {
  const { owner, repo, number } = context.issue;
  if (!number) {
    return;
  }

  const html = await readFile(join(coveragePath, 'cover-index.html'), { encoding: 'utf8' });

  const table = rows(html)
    .slice(2)
    .map(row => cells(row).map(removeLink))
    .filter(row => row.length === 7)
    .map(cells => '| ' + cells.join(' | ') + ' |')
    .join('\n');

  const body = `[Coverage report](https://github.com/${owner}/${repo}/actions/runs/${context.runId})

| Source file | Covered expressions | Total expressions | % | Covered branches | Total branches | % |
| --- | --- | --- | --- | --- | --- | --- |
${table}
`;

  await github.rest.issues.createComment({ issue_number: number, owner, repo, body });
};
