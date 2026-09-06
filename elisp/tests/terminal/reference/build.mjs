import {mkdir,writeFile} from 'node:fs/promises';
import './renderer.js';
import './scenarios.js';
const sizes=[[96,32],[120,40],[400,80]];
const cases=globalThis.AIBO_SCENARIOS;
const galleryOnly=process.argv.includes('--gallery-only');
const html=(item,root=false)=>`<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Aibo · ${item.title}</title><link rel="stylesheet" href="${root?'':'../'}gallery.css"></head>
<body data-scenario="${item.id}" data-root="${root}"><div class="gallery"><aside class="catalog"><a class="brand" href="${root?'index.html':'../index.html'}">aibo / terminal study</a><div class="draft-label">Terminal reference · fixture data</div><div id="catalog-links"></div><div class="catalog-footer"><a href="${root?'':'../'}README.md">Snapshot contract</a></div></aside><main class="review"><div class="title-row"><h1 id="title"></h1><nav class="arrows"><a id="previous">← Previous</a><a id="next">Next →</a></nav></div><div class="toolbar"><label>View <select id="view-select"></select></label><label>Terminal <select id="size-select"><option value="96x32">96 × 32</option><option value="120x40" selected>120 × 40</option><option value="400x80">400 × 80</option></select></label><button id="zoom">Fit frame</button><button id="reset">Reset fixture</button><span class="spacer"></span><a id="text-link">Text</a><a id="cells-link">Cells</a></div><div class="stage" id="stage"><div class="terminal" id="terminal" aria-label="Aibo terminal prototype"></div></div><div class="status" id="status"></div><div class="notes"><p id="notes"></p><div class="requirements" id="requirements"></div></div></main></div><script src="${root?'':'../'}renderer.js"></script><script src="${root?'':'../'}scenarios.js"></script><script src="${root?'':'../'}gallery.js"></script></body></html>`;
await mkdir(new URL('views/',import.meta.url),{recursive:true});
for(const item of cases){
 await writeFile(new URL(`views/${item.id}.html`,import.meta.url),html(item));
 if(galleryOnly) continue;
 for(const [cols,rows] of sizes){
  const directory=new URL(`snapshots/${cols}x${rows}/`,import.meta.url);await mkdir(directory,{recursive:true});
  const frame=globalThis.AiboTerminal.render(item.state,cols,rows);
  await writeFile(new URL(`${item.id}.txt`,directory),frame.lines.join('\n')+'\n');
  await writeFile(new URL(`${item.id}.json`,directory),JSON.stringify({scenario:item.id,columns:cols,rows,cursor:frame.cursor,regions:frame.regions,runs:frame.runs},null,2)+'\n');
 }
}
await writeFile(new URL('index.html',import.meta.url),html(cases[0],true));
if(!galleryOnly){
 await writeFile(new URL('palette.json',import.meta.url),JSON.stringify(globalThis.AiboTerminal.palette,null,2)+'\n');
 await writeFile(new URL('manifest.json',import.meta.url),JSON.stringify(cases.map(({state,...item})=>item),null,2)+'\n');
 await writeFile(new URL('fixtures.json',import.meta.url),JSON.stringify(cases,null,2)+'\n');
}
console.log(`Built ${cases.length} views${galleryOnly?'':` and ${cases.length*sizes.length} terminal snapshots`}.`);
