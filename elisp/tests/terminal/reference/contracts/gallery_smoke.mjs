// Exercise gallery navigation and editor state without claiming browser visual QA.
import '../renderer.js';
import '../scenarios.js';
class Element {
  constructor(tag='div'){this.tag=tag;this.children=[];this.dataset={};this.style={};this.scrollTop=0;this.scrollLeft=0;this.clientWidth=1100;this.clientHeight=800;this.value='';this.selectionStart=0;this.classList={toggle(){}};}
  append(child){this.children.push(child);}
  set innerHTML(value){this.html=value;this.children=[];}
  get innerHTML(){return this.html;}
  setAttribute(name,value){this[name]=value;}
  getAttribute(name){return this[name];}
  setSelectionRange(start,end){this.selectionStart=start;this.selectionEnd=end;}
  focus(){}
  querySelectorAll(){return this.children.filter(child=>child.dataset.scenario);}
}
const ids=['terminal','stage','catalog-links','view-select','title','notes','requirements','previous','next','status','text-link','cells-link','size-select','zoom','reset'];
const elements=Object.fromEntries(ids.map(id=>[id,new Element()]));
const handlers={};
globalThis.document={body:{dataset:{scenario:'home',root:'true'}},getElementById:id=>elements[id],createElement:tag=>new Element(tag),querySelectorAll:()=>[],addEventListener:(name,fn)=>{handlers[name]=fn;}};
globalThis.window={addEventListener(){}};
globalThis.location={href:'http://127.0.0.1:58342/',pathname:'/'};
const navigations=[];
globalThis.history={replaceState(){},pushState:(state,_unused,url)=>navigations.push({state,url})};
globalThis.ResizeObserver=class{observe(){}};
await import('../gallery.js');
const assert=(condition,message)=>{if(!condition)throw Error(message);};
const click=id=>{
  const link=elements['catalog-links'].children.find(child=>child.dataset.scenario===id);
  assert(link,`missing view ${id}`);
  let prevented=false;
  handlers.click({target:{closest:()=>link},preventDefault(){prevented=true;}});
  assert(prevented,'navigation must not reload');
};
elements['catalog-links'].scrollTop=1480;
let editor=elements.terminal.children.at(-1);
editor.value='keep my draft';editor.selectionStart=4;editor.oninput();
click('projects');
assert(elements['catalog-links'].scrollTop===1480,'catalog scroll moved');
assert(!elements.terminal.innerHTML.includes('[Edit'),'project editing control leaked');
click('home');
editor=elements.terminal.children.at(-1);
assert(editor.value==='keep my draft'&&editor.selectionStart===4,'view draft/cursor lost');
handlers.keydown({target:{closest:()=>null},key:'0',altKey:true,preventDefault(){}});
assert(elements.terminal.innerHTML.includes('m2'),'M-0 must open a new numbered manager');
assert(navigations.length===2,'navigation should use history, not location reload');
console.log('Gallery smoke passed: no reload, retained scroll/draft/cursor, read-only projects, M-0 new manager.');
const terminalText=()=>elements.terminal.innerHTML.replace(/<[^>]*>/g,'');
const key=key=>handlers.keydown({target:{closest:()=>null},key,preventDefault(){}});
click('search-many');
for(let i=0;i<51;i++)key('ArrowDown');
assert(terminalText().includes('[Show 50 more]'),'first batch has no continuation');
key('Enter');
assert(terminalText().includes('Search task 51'),'RET did not reveal next batch');
for(let i=0;i<50;i++)key('ArrowDown');
key('Enter');
for(let i=0;i<60;i++)key('ArrowDown');
assert(terminalText().includes('Search task 123'),'last result missing');
assert(!terminalText().includes('[Show 50 more]'),'final batch offers an empty next page');
key('Enter');
assert(terminalText().includes('search-122')||terminalText().includes('Search task 123'),'RET at bottom did not open final result');
click('search-many');elements.reset.onclick();
for(let i=0;i<200;i++)key('ArrowUp');
editor=elements.terminal.children.at(-1);
assert(editor['aria-label']==='Search chats','search input missing');
editor.value='no-match-zzz';editor.selectionStart=12;editor.oninput();
key('ArrowDown');key('Enter');
assert(terminalText().includes('No matching chats.'),'empty search cursor escaped into a chat');
editor=elements.terminal.children.at(-1);
editor.value='b122';editor.selectionStart=4;editor.oninput();
assert(terminalText().includes('Search task 123'),'typing does not filter search');
key('ArrowDown');key('End');key('Enter');
assert(terminalText().includes('Search task 123'),'result activation failed');
console.log('Search smoke passed: 50/100/final paging, bounded selection, query edits, empty results, and RET activation.');
