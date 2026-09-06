(function(){
'use strict';
const cases=globalThis.AIBO_SCENARIOS;
let id=document.body.dataset.scenario||'home';
let specimen=cases.find(item=>item.id===id)||cases[0];
let state=structuredClone(specimen.state),columns=120,rows=40,fit=true,frame=null,chord=[];
const terminal=document.getElementById('terminal'),stage=document.getElementById('stage');
const escape=text=>String(text).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
const baseUrl=new URL(document.body.dataset.root==='true'?'./':'../',location.href);
const viewUrl=id=>new URL(`views/${id}.html`,baseUrl).href;
const savedViews=new Map();
for(const link of document.querySelectorAll('.brand, .catalog-footer a'))link.href=new URL(link.getAttribute('href'),location.href).href;
function navigate(nextId,push=true){
  const next=cases.find(item=>item.id===nextId);if(!next)return;
  savedViews.set(id,{state:structuredClone(state),top:stage.scrollTop,left:stage.scrollLeft});
  id=nextId;specimen=next;
  const saved=savedViews.get(id);state=structuredClone(saved?.state||specimen.state);
  if(push)history.pushState({scenario:id},'',viewUrl(id));
  updateTitle();draw(state.page==='search');
  stage.scrollTop=saved?.top||0;stage.scrollLeft=saved?.left||0;
}
const catalog=document.getElementById('catalog-links');
for(const category of [...new Set(cases.map(item=>item.category))]){
  const heading=document.createElement('h2');heading.textContent=category;catalog.append(heading);
  for(const item of cases.filter(item=>item.category===category)){
    const link=document.createElement('a');link.className='case-link';link.dataset.scenario=item.id;link.href=viewUrl(item.id);link.textContent=item.title;catalog.append(link);
  }
}
document.getElementById('view-select').innerHTML=cases.map(item=>`<option value="${item.id}">${escape(item.category+' / '+item.title)}</option>`).join('');
document.getElementById('view-select').onchange=event=>navigate(event.target.value);
function updateTitle(){
  document.getElementById('title').textContent=specimen.title;
  document.getElementById('notes').textContent=specimen.notes;
  document.getElementById('requirements').textContent='Contracts: '+specimen.requirements.join(' · ');
  document.getElementById('view-select').value=id;
  document.title='Aibo · '+specimen.title;
  for(const link of catalog.querySelectorAll('[data-scenario]'))link.classList.toggle('current',link.dataset.scenario===id);
  const index=cases.indexOf(specimen);
  for(const [name,offset] of [['previous',-1],['next',1]]){
    const next=cases[(index+cases.length+offset)%cases.length];const link=document.getElementById(name);link.href=viewUrl(next.id);link.dataset.scenario=next.id;
  }
}
document.addEventListener('click',event=>{
  const link=event.target.closest('a[data-scenario]');
  if(!link||event.metaKey||event.ctrlKey||event.shiftKey||event.altKey)return;
  event.preventDefault();navigate(link.dataset.scenario);
});
window.addEventListener('popstate',event=>navigate(event.state?.scenario||location.pathname.split('/').at(-1).replace('.html','')||'home',false));
history.replaceState({scenario:id},'');
updateTitle();
function draw(focusEditor=false){
  frame=AiboTerminal.render(state,columns,rows);
  terminal.style.width=`${columns}ch`;
  terminal.innerHTML=frame.runs.map(row=>'<div class="terminal-row">'+row.map(run=>{
    const color=frame.palette[run.face];return `<span style="color:${color[0]};background:${color[1]}"${run.action?' data-action="'+escape(run.action)+'"':''}>${escape(run.text)}</span>`;
  }).join('')+'</div>').join('');
  if(frame.regions.input){
    const region=frame.regions.input;const editor=document.createElement('textarea');
    editor.className='capture';editor.setAttribute('aria-label','Terminal draft');editor.spellcheck=false;editor.value=state.draft||'';
    editor.style.left=`${region.x}ch`;editor.style.top=`${region.y*1.45}em`;editor.style.width=`${region.width}ch`;editor.style.height=`${region.height*1.45}em`;
    terminal.append(editor);editor.setSelectionRange(state.point??editor.value.length,state.point??editor.value.length);
    editor.oninput=()=>{state.draft=editor.value;state.point=editor.selectionStart;state.focus='input';draw(true);};
    editor.onkeyup=event=>{if(['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','Home','End'].includes(event.key)){state.point=editor.selectionStart;draw(true);}};
    if(focusEditor && state.focus==='input')editor.focus({preventScroll:true});
  }
  if(frame.regions.searchInput){
    const region=frame.regions.searchInput,editor=document.createElement('input');
    editor.className='capture';editor.setAttribute('aria-label','Search chats');editor.spellcheck=false;editor.value=state.query||'';
    editor.style.left=`${region.x}ch`;editor.style.top=`${region.y*1.45}em`;editor.style.width=`${region.width}ch`;editor.style.height='1.45em';
    terminal.append(editor);editor.setSelectionRange(state.searchPoint??editor.value.length,state.searchPoint??editor.value.length);
    editor.oninput=()=>{state.query=editor.value;state.searchPoint=editor.selectionStart;state.searchLimit=50;state.searchSelection=-1;state.mainScroll=0;state.noResults=false;state.searchLoading=false;state.focus='main';draw(true);};
    editor.onclick=()=>{state.searchPoint=editor.selectionStart;state.searchSelection=-1;state.focus='main';draw(true);};
    editor.onkeyup=event=>{if(['ArrowLeft','ArrowRight','Home','End'].includes(event.key)){state.searchPoint=editor.selectionStart;draw(true);}};
    if(focusEditor && state.focus==='main' && (state.searchSelection??-1)<0)editor.focus({preventScroll:true});
  }
  size();document.getElementById('status').textContent=`${columns} × ${rows} cells · ${state.mode} · focus: ${state.focus||'input'} · ${frame.regions.input?.height||0} editable lines`;
  document.getElementById('text-link').href=new URL(`snapshots/${columns}x${rows}/${id}.txt`,baseUrl).href;
  document.getElementById('cells-link').href=new URL(`snapshots/${columns}x${rows}/${id}.json`,baseUrl).href;
}
function size(){
  if(!frame)return;
  if(fit){const font=Math.min(15,(stage.clientWidth-2)/(columns*.603),(stage.clientHeight-2)/(rows*1.45));terminal.style.fontSize=`${Math.max(4,font)}px`;}
  else terminal.style.fontSize='13px';
}
new ResizeObserver(size).observe(stage);
document.getElementById('size-select').onchange=event=>{[columns,rows]=event.target.value.split('x').map(Number);draw();};
document.getElementById('zoom').onclick=event=>{fit=!fit;event.currentTarget.textContent=fit?'Fit frame':'100% · scroll';size();};
document.getElementById('reset').onclick=()=>{state=structuredClone(specimen.state);draw(state.page==='search');};
function moveSearch(selection){
  const results=AiboTerminal.searchResults(state),limit=state.searchLimit||50;
  const count=Math.min(results.length,limit)+(results.length>limit?1:0);
  state.searchSelection=Math.max(-1,Math.min(selection,count-1));state.focus='main';
  const height=frame.regions.main.height-2;
  if(state.searchSelection>=0){
    if(state.searchSelection<(state.mainScroll||0))state.mainScroll=state.searchSelection;
    if(state.searchSelection>=(state.mainScroll||0)+height)state.mainScroll=state.searchSelection-height+1;
  }
  draw(state.searchSelection<0);
}
function activateSearch(){
  const results=AiboTerminal.searchResults(state),limit=state.searchLimit||50,index=state.searchSelection??-1;
  if(index===limit && results.length>limit){state.searchLimit=limit+50;moveSearch(index);}
  else {
    const chat=results[Math.max(0,index)];if(!chat)return;
    if(!state.chats.some(item=>item.id===chat.id))state.chats.push(chat);
    dispatch('chat:'+chat.id);
  }
}
function dispatch(action){
  const [type,value]=action.split(':');
  if(type==='search'){
    if(value==='input')moveSearch(-1);
    else if(value==='more'){state.searchSelection=state.searchLimit||50;activateSearch();}
    else moveSearch(Number(action.split(':')[2]));
  }
  else if(type==='scenario')navigate(value);
  else if(type==='page')navigate(value);
  else if(type==='hidden'){state.expanded=!state.expanded;draw();}
  else if(type==='chat'){state.page='chat';state.current=value;state.target=value;state.focus='main';state.mainScroll=0;draw();}
  else if(type==='target'){state.target=value;state.focus='input';draw(true);}
  else if(type==='attachment'){state.attachments.splice(Number(value),1);draw(true);}
  else if(type==='new-manager'){
    const number=state.nextManagerNumber??((Math.max(-1,...state.chats.filter(chat=>chat.kind==='manager').map(chat=>Number(chat.label.slice(1))))+1)%256);
    state.nextManagerNumber=(number+1)%256;
    const chat={id:`new-manager-${number}-${state.chats.length}`,label:`m${number}`,kind:'manager',title:'New chat',location:'aibo',status:'idle',active:false,time:'now'};
    state.chats.unshift(chat);state.current=chat.id;state.target=chat.id;state.page='chat';state.messages=[];
    state.focus='input';state.inputOpen=true;draw(true);
  }
  else if(type==='sidebar'){state.sidebarScroll=Math.max(0,(state.sidebarScroll||0)+(value==='down'?5:-5));draw();}
  else if(type==='main'){state.mainScroll=Math.max(0,(state.mainScroll||0)+(value==='down'?5:-5));draw();}
  else if(type==='tabs'){const key=value==='bot'?'botTabScroll':'managerTabScroll';state[key]=((state[key]||0)+1)%5;draw();}
}
terminal.onclick=event=>{const action=event.target.closest('[data-action]')?.dataset.action;if(action)dispatch(action);};
terminal.onwheel=event=>{
  const bounds=terminal.getBoundingClientRect();const cellWidth=bounds.width/columns;
  const column=Math.floor((event.clientX-bounds.left)/cellWidth);
  const region=column<35&&state.mode!=='cowork'?'sidebarScroll':'mainScroll';
  if(region==='mainScroll' && state.page==='search')state.searchSelection=-1;
  state[region]=Math.max(0,(state[region]||0)+Math.sign(event.deltaY)*3);event.preventDefault();draw();
};
document.addEventListener('keydown',event=>{
  if(event.target.closest('.toolbar')&&!event.ctrlKey&&!event.altKey)return;
  const key=event.key.toLowerCase(),meta=event.altKey||event.metaKey;
  if(chord.length){
    event.preventDefault();chord.push(key);
    if(chord.length===2&&['b','n','p','x'].includes(key))return;
    const value=chord.at(-1),command=chord.join(' ');chord=[];
    if(command==='c p h')navigate('help');else if(command==='c p l')navigate('locations');else if(command==='c p p')navigate('projects');else if(command==='c p s')navigate('search');
    else if(command==='c p n'){state.focus='sidebar';draw();}
    else if(command.startsWith('c b ')){const chat=state.chats.filter(c=>c.kind==='manager')[Number(value)];if(chat)dispatch('chat:'+chat.id);}
    else if(command==='c i')navigate('attachments');
    else if(command.startsWith('x ')){state.mode='cowork';state.inputOpen=false;state.page='chat';state.current=state.current||'b2';state.focus='main';state.split=value==='3'?'right':value==='2'?'below':null;draw();}
    return;
  }
  if(state.page==='search' && state.focus==='main' && !event.ctrlKey && !meta){
    const selected=state.searchSelection??-1;
    if(key==='arrowdown' || key==='arrowup' || key==='tab'){
      event.preventDefault();moveSearch(selected+((key==='arrowup'||event.shiftKey)?-1:1));return;
    }
    if(key==='enter'){event.preventDefault();activateSearch();return;}
    if(selected>=0){
      event.preventDefault();
      if(key==='home')moveSearch(-1);
      else if(key==='end'){const count=AiboTerminal.searchResults(state).length,limit=state.searchLimit||50;moveSearch(Math.min(count-1,limit));}
      else if(key==='pageup'||key==='pagedown')moveSearch(selected+(key==='pageup'?-1:1)*(frame.regions.main.height-2));
      return;
    }
  }
  if(event.ctrlKey&&meta&&key==='h'){event.preventDefault();navigate('home');}
  else if(event.ctrlKey&&key==='c'){event.preventDefault();chord=['c'];}
  else if(event.ctrlKey&&key==='x'){event.preventDefault();chord=['x'];}
  else if(event.ctrlKey&&key==='o'){event.preventDefault();state.focus=state.focus==='input'?'main':'input';state.inputOpen=true;draw(state.focus==='input');}
  else if(meta&&key==='/'){event.preventDefault();if(state.focus!=='input')state.target=state.current||'m0';else state.target=state.target===state.current?'m0':state.current;state.focus='input';state.inputOpen=true;draw(true);}
  else if(meta&&key==='0'){event.preventDefault();dispatch('new-manager');}
  else if(meta&&/^[1-9]$/.test(key)){event.preventDefault();const chat=state.chats.filter(c=>c.kind==='bot')[Number(key)-1];if(chat)dispatch('chat:'+chat.id);}
  else if(event.ctrlKey&&key==='g'){event.preventDefault();state.draft='';state.point=0;state.attachments=[];if(state.mode==='cowork'){state.inputOpen=false;state.focus='main';}draw(state.mode!=='cowork');}
  else if(meta&&key==='enter'){event.preventDefault();if(state.draft||state.attachments?.length){state.echo='Prototype: message sent · no API call was made';state.messages.push({kind:'user',content:state.draft||'[Image attachment]'});state.draft='';state.point=0;draw(true);}}
});
draw(state.page==='search');
})();
